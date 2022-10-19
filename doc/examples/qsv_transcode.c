/*
 * Quick Sync Video (video transcoding) transcode sample
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/**
 * @file
 * Intel QSV-accelerated transcoding example.
 *
 * @example qsv_transcode.c
 * This example shows how to do QSV-accelerated transcoding and how to
 * dynamically change encoder's option.
 * Usage: qsv_transcode input_stream codec output_stream initial option
 *                      { frame_number new_option }
 * e.g: - qsv_transcode input.mp4 h264_qsv output_h264.mp4 "g 60"
 *      - qsv_transcode input.mp4 hevc_qsv output_hevc.mp4 "g 60 async_depth 1"
 *                      100 "g 120"
 *         (initialize codec with gop_size 60 and change it to 120 after 100
 *          frames)
 */

#include <stdio.h>
#include <errno.h>
#include <libavutil/hwcontext.h>
#include <libavcodec/avcodec.h>
#include <libavcodec/packet_internal.h>
#include <libavformat/avformat.h>
#include <libavutil/opt.h>
#include <libavutil/time.h>

#define REQUIRED_OPTION_NUMBER 8

static AVFormatContext *ifmt_ctx[2] = {NULL}, *ofmt_ctx = {NULL};
static AVBufferRef *hw_device_ctx[2] = {NULL};
static AVCodecContext *decoder_ctx[2] = {NULL}, *encoder_ctx[2] = {NULL};
static AVDictionary *enc_opts = NULL;
static AVDictionary *dec_opts = NULL;
static AVDictionary *device_opts[2] = {NULL};
PacketList pkt_list[2] = {NULL};
AVPacket *tmp_pkt = NULL;
static int frame_count[2] = {0};
static int video_stream = -1;
static int initialized = 0;
static int64_t timer_start = 0;
static uint64_t frame_number = 0;


typedef struct DynamicSetting {
    int frame_number;
    char* optstr;
} DynamicSetting;
static DynamicSetting *dynamic_setting;
static int setting_number;
static int current_setting_number;
static int enable_concat;

static void print_fps(void)
{
    float time = (av_gettime_relative() - timer_start) / 1000000.0;
    av_log(NULL, AV_LOG_VERBOSE, "Frame:%ld, time:%.2f, fps:%.2f\n",
           frame_number, time, frame_number / time);
}

static int str_to_dict(char* optstr, AVDictionary **opt)
{
    char str[1024];
    char *key, *value;
    strcpy(str, optstr);
    key = strtok(str, " ");
    if (key == NULL)
        return AVERROR(ENAVAIL);
    value = strtok(NULL, " ");
    if (value == NULL)
        return AVERROR(ENAVAIL);
    av_dict_set(opt, key, value, 0);
    do {
        key = strtok(NULL, " ");
        if (key == NULL)
            return 0;
        value = strtok(NULL, " ");
        if (value == NULL)
            return AVERROR(ENAVAIL);
        av_dict_set(opt, key, value, 0);
    } while(key != NULL);
    return 0;
}

static int dynamic_set_parameter(AVCodecContext *avctx)
{
    AVDictionary *opts = NULL;
    int ret = 0;
    static int frame_number = 0;
    frame_number++;
    if (current_setting_number < setting_number &&
        frame_number == dynamic_setting[current_setting_number].frame_number) {
        AVDictionaryEntry *e = NULL;
        ret = str_to_dict(dynamic_setting[current_setting_number].optstr, &opts);
        if (ret < 0) {
            fprintf(stderr, "The dynamic parameter is wrong\n");
            goto fail;
        }
        /* Set common option. The dictionary will be freed and replaced
         * by a new one containing all options not found in common option list.
         * Then this new dictionary is used to set private option. */
        if ((ret = av_opt_set_dict(avctx, &opts)) < 0)
            goto fail;
        /* Set codec specific option */
        if ((ret = av_opt_set_dict(avctx->priv_data, &opts)) < 0)
            goto fail;
        /* There is no "framerate" option in commom option list. Use "-r" to set
         * framerate, which is compatible with ffmpeg commandline. The video is
         * assumed to be average frame rate, so set time_base to 1/framerate. */
        e = av_dict_get(opts, "r", NULL, 0);
        if (e) {
            avctx->framerate = av_d2q(atof(e->value), INT_MAX);
            encoder_ctx[0]->time_base = av_inv_q(encoder_ctx[0]->framerate);
        }
    }
fail:
    av_dict_free(&opts);
    return ret;
}

static int get_format(AVCodecContext *avctx, const enum AVPixelFormat *pix_fmts)
{
    while (*pix_fmts != AV_PIX_FMT_NONE) {
        if (*pix_fmts == AV_PIX_FMT_QSV) {
            return AV_PIX_FMT_QSV;
        }

        pix_fmts++;
    }

    fprintf(stderr, "The QSV pixel format not offered in get_format()\n");

    return AV_PIX_FMT_NONE;
}

static int open_input_file(char *filename, int idx, char *hw_frame, char *dec_async_depth)
{
    int ret;
    const AVCodec *decoder = NULL;
    AVStream *video = NULL;

    if ((ret = avformat_open_input(&ifmt_ctx[idx], filename, NULL, NULL)) < 0) {
        fprintf(stderr, "Cannot open input file '%s', Error code: %s\n",
                filename, av_err2str(ret));
        return ret;
    }

    if ((ret = avformat_find_stream_info(ifmt_ctx[idx], NULL)) < 0) {
        fprintf(stderr, "Cannot find input stream information. Error code: %s\n",
                av_err2str(ret));
        return ret;
    }

    ret = av_find_best_stream(ifmt_ctx[idx], AVMEDIA_TYPE_VIDEO, -1, -1, NULL, 0);
    if (ret < 0) {
        fprintf(stderr, "Cannot find a video stream in the input file. "
                "Error code: %s\n", av_err2str(ret));
        return ret;
    }
    video_stream = ret;
    video = ifmt_ctx[idx]->streams[video_stream];

    switch(video->codecpar->codec_id) {
    case AV_CODEC_ID_H264:
        decoder = avcodec_find_decoder_by_name("h264_qsv");
        break;
    case AV_CODEC_ID_HEVC:
        decoder = avcodec_find_decoder_by_name("hevc_qsv");
        break;
    case AV_CODEC_ID_VP9:
        decoder = avcodec_find_decoder_by_name("vp9_qsv");
        break;
    case AV_CODEC_ID_VP8:
        decoder = avcodec_find_decoder_by_name("vp8_qsv");
        break;
    case AV_CODEC_ID_AV1:
        decoder = avcodec_find_decoder_by_name("av1_qsv");
        break;
    case AV_CODEC_ID_MPEG2VIDEO:
        decoder = avcodec_find_decoder_by_name("mpeg2_qsv");
        break;
    case AV_CODEC_ID_MJPEG:
        decoder = avcodec_find_decoder_by_name("mjpeg_qsv");
        break;
    default:
        fprintf(stderr, "Codec is not supportted by qsv\n");
        return AVERROR(ENAVAIL);
    }

    if (!(decoder_ctx[idx] = avcodec_alloc_context3(decoder)))
        return AVERROR(ENOMEM);

    if ((ret = avcodec_parameters_to_context(decoder_ctx[idx], video->codecpar)) < 0) {
        fprintf(stderr, "avcodec_parameters_to_context error. Error code: %s\n",
                av_err2str(ret));
        return ret;
    }
    decoder_ctx[idx]->framerate = av_guess_frame_rate(ifmt_ctx[idx], video, NULL);

    decoder_ctx[idx]->hw_device_ctx = av_buffer_ref(hw_device_ctx[idx]);
    if (!decoder_ctx[idx]->hw_device_ctx) {
        fprintf(stderr, "A hardware device reference create failed.\n");
        return AVERROR(ENOMEM);
    }
    decoder_ctx[idx]->get_format    = get_format;
    decoder_ctx[idx]->pkt_timebase = video->time_base;

    av_dict_set(&dec_opts, "async_depth", dec_async_depth, 0);
    av_dict_set(&dec_opts, "extra_hw_frames", hw_frame, 0);

    if ((ret = avcodec_open2(decoder_ctx[idx], decoder, &dec_opts)) < 0)
        fprintf(stderr, "Failed to open codec for decoding. Error code: %s\n",
                av_err2str(ret));

    return ret;
}

static int encode_write(AVPacket *enc_pkt, AVFrame *frame, int idx)
{
    int ret = 0;

    av_packet_unref(enc_pkt);

    if((ret = dynamic_set_parameter(encoder_ctx[idx])) < 0) {
        fprintf(stderr, "Failed to set dynamic parameter. Error code: %s\n",
                av_err2str(ret));
        goto end;
    }

    if ((ret = avcodec_send_frame(encoder_ctx[idx], frame)) < 0) {
        fprintf(stderr, "Error during encoding. Error code: %s\n", av_err2str(ret));
        goto end;
    }
    while (1) {
        if (ret = avcodec_receive_packet(encoder_ctx[idx], enc_pkt))
            break;
        enc_pkt->stream_index = 0;
        av_packet_rescale_ts(enc_pkt, encoder_ctx[idx]->time_base,
                             ofmt_ctx->streams[0]->time_base);

        if (enable_concat) {
            if (frame_number / 60 % 2 == idx) {
                while (1) {
                    avpriv_packet_list_get(&pkt_list[idx], tmp_pkt);
                    if (tmp_pkt->size) {
                        if ((ret = av_interleaved_write_frame(ofmt_ctx, tmp_pkt)) < 0) {
                            fprintf(stderr, "Error during writing data to output file. "
                                    "Error code: %s\n", av_err2str(ret));
                            return ret;
                        }
                        if (++frame_number % 200 == 0)
                            print_fps();
                        av_packet_unref(tmp_pkt);
                    } else
                        break;
                }
                if ((ret = av_interleaved_write_frame(ofmt_ctx, enc_pkt)) < 0) {
                    fprintf(stderr, "Error during writing data to output file. "
                            "Error code: %s\n", av_err2str(ret));
                    return ret;
                }
                if (++frame_number % 200 == 0)
                    print_fps();
            } else {
                ret = avpriv_packet_list_put(&pkt_list[idx], enc_pkt, NULL, 0);
                if (ret < 0) {
                    fprintf(stderr, "Error during writing data to output file. "
                            "Error code: %s\n", av_err2str(ret));
                    avpriv_packet_list_free(&pkt_list[idx]);
                    return ret;
                }
            }
        } else {
            if ((ret = av_interleaved_write_frame(ofmt_ctx, enc_pkt)) < 0) {
                fprintf(stderr, "Error during writing data to output file. "
                        "Error code: %s\n", av_err2str(ret));
                return ret;
            }
            if (++frame_number % 200 == 0)
                print_fps();
        }
    }

end:
    if (ret == AVERROR_EOF)
        return 0;
    ret = ((ret == AVERROR(EAGAIN)) ? 0:-1);
    return ret;
}

static int dec_enc(AVPacket *pkt, const AVCodec *enc_codec,  char *optstr, int idx)
{
    AVDictionaryEntry *e = NULL;
    AVStream *ost;
    AVFrame *frame;
    int ret = 0;

    ret = avcodec_send_packet(decoder_ctx[idx], pkt);
    if (ret < 0) {
        fprintf(stderr, "Error during decoding. Error code: %s\n", av_err2str(ret));
        return ret;
    }

    while (ret >= 0) {
        if (!(frame = av_frame_alloc()))
            return AVERROR(ENOMEM);

        ret = avcodec_receive_frame(decoder_ctx[idx], frame);
        if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF) {
            av_frame_free(&frame);
            return 0;
        } else if (ret < 0) {
            fprintf(stderr, "Error while decoding. Error code: %s\n", av_err2str(ret));
            goto fail;
        }

        if (!encoder_ctx[idx]->hw_frames_ctx) {
            /* we need to ref hw_frames_ctx of decoder to initialize encoder's codec.
               Only after we get a decoded frame, can we obtain its hw_frames_ctx */
            encoder_ctx[idx]->hw_frames_ctx = av_buffer_ref(decoder_ctx[idx]->hw_frames_ctx);
            if (!encoder_ctx[idx]->hw_frames_ctx) {
                ret = AVERROR(ENOMEM);
                goto fail;
            }
            /* set AVCodecContext Parameters for encoder, here we keep them stay
             * the same as decoder.
             * xxx: now the sample can't handle resolution change case.
             */
            encoder_ctx[idx]->time_base = av_inv_q(decoder_ctx[idx]->framerate);
            encoder_ctx[idx]->pix_fmt   = AV_PIX_FMT_QSV;
            encoder_ctx[idx]->width     = decoder_ctx[idx]->width;
            encoder_ctx[idx]->height    = decoder_ctx[idx]->height;

            if (!enc_opts) {
                if ((ret = str_to_dict(optstr, &enc_opts)) < 0) {
                    fprintf(stderr, "Failed to set encoding parameter.\n");
                    goto fail;
                }
            }
            e = av_dict_get(enc_opts, "r", NULL, 0);
            if (e) {
                encoder_ctx[idx]->framerate = av_d2q(atof(e->value), INT_MAX);
                encoder_ctx[idx]->time_base = av_inv_q(encoder_ctx[idx]->framerate);
            }

            if ((ret = avcodec_open2(encoder_ctx[idx], enc_codec, &enc_opts)) < 0) {
                fprintf(stderr, "Failed to open encode codec. Error code: %s\n",
                        av_err2str(ret));
                goto fail;
            }

            if (!initialized && idx == 0) {
                if (!(ost = avformat_new_stream(ofmt_ctx, enc_codec))) {
                    fprintf(stderr, "Failed to allocate stream for output format.\n");
                    ret = AVERROR(ENOMEM);
                    goto fail;
                }

                ost->time_base = encoder_ctx[0]->time_base;
                ret = avcodec_parameters_from_context(ost->codecpar, encoder_ctx[0]);
                if (ret < 0) {
                    fprintf(stderr, "Failed to copy the stream parameters. "
                            "Error code: %s\n", av_err2str(ret));
                    goto fail;
                }

                /* write the stream header */
                if ((ret = avformat_write_header(ofmt_ctx, NULL)) < 0) {
                    fprintf(stderr, "Error while writing stream header. "
                            "Error code: %s\n", av_err2str(ret));
                    goto fail;
                }

                initialized = 1;
            }
        }

        frame->pts = av_rescale_q(frame->pts, decoder_ctx[idx]->pkt_timebase,
                                  encoder_ctx[idx]->time_base);

        if (enable_concat && ((frame_count[idx]++ / 60 % 2) != idx))
            goto fail;

        if ((ret = encode_write(pkt, frame, idx)) < 0)
            fprintf(stderr, "Error during encoding and writing.\n");

fail:
        av_frame_free(&frame);
        if (ret < 0)
            return ret;
    }
    return 0;
}

int main(int argc, char **argv)
{
    const AVCodec *enc_codec;
    AVPacket *dec_pkt, *dec_pkt1;
    char device_optstr[2][100] = {"child_device /dev/dri/renderD128", "child_device /dev/dri/renderD129"};
    int ret = 0;
    int i;

    if (argc < REQUIRED_OPTION_NUMBER || (argc - REQUIRED_OPTION_NUMBER) % 2) {
        av_log(NULL, AV_LOG_ERROR, "Usage: %s <input file> <encoder> <output file> <enable concat> <extra hw frames> <dec async depth>\"<encoding option>\""
               "{<frame-number> \"<encoding options>\"}\n", argv[0]);
        return 1;
    }
    enable_concat = atoi(argv[4]);
    setting_number = (argc - REQUIRED_OPTION_NUMBER) / 2;
    dynamic_setting = av_malloc(setting_number * sizeof(*dynamic_setting));
    current_setting_number = 0;
    for (int i = 0; i < setting_number; i++) {
        dynamic_setting[i].frame_number = atoi(argv[i*2 + REQUIRED_OPTION_NUMBER]);
        dynamic_setting[i].optstr = argv[i*2 + REQUIRED_OPTION_NUMBER];
    }

    av_log_set_level(AV_LOG_VERBOSE);


    for (i = 0; i <= (enable_concat ? 1 : 0); i++) {
        if (enable_concat && !device_opts[i]) {
            if ((ret = str_to_dict(&device_optstr[i][0], &device_opts[i])) < 0) {
                    fprintf(stderr, "Failed to set device parameter.\n");
                    goto end;
                }
            }
        ret = av_hwdevice_ctx_create(&hw_device_ctx[i], AV_HWDEVICE_TYPE_QSV, NULL, device_opts[i], 0);
        if (ret < 0) {
            fprintf(stderr, "Failed to create a QSV device. Error code: %s\n", av_err2str(ret));
            goto end;
        }

        if ((ret = open_input_file(argv[1], i, argv[5], argv[6])) < 0)
            goto end;

        if (!(enc_codec = avcodec_find_encoder_by_name(argv[2]))) {
            fprintf(stderr, "Could not find encoder '%s'\n", argv[2]);
            ret = -1;
            goto end;
        }

        if ((ret = (avformat_alloc_output_context2(&ofmt_ctx, NULL, NULL, argv[3]))) < 0) {
            fprintf(stderr, "Failed to deduce output format from file extension. Error code: "
                    "%s\n", av_err2str(ret));
            goto end;
        }

        if (!(encoder_ctx[i] = avcodec_alloc_context3(enc_codec))) {
            ret = AVERROR(ENOMEM);
            goto end;
        }
    }

    ret = avio_open(&ofmt_ctx->pb, argv[3], AVIO_FLAG_WRITE);
    if (ret < 0) {
        fprintf(stderr, "Cannot open output file. "
                "Error code: %s\n", av_err2str(ret));
        goto end;
    }

    dec_pkt = av_packet_alloc();
    if (!dec_pkt) {
        fprintf(stderr, "Failed to allocate decode packet\n");
        goto end;
    }

    if (enable_concat) {
        dec_pkt1 = av_packet_alloc();
        if (!dec_pkt1) {
            fprintf(stderr, "Failed to allocate decode packet1\n");
            goto end;
        }

        tmp_pkt = av_packet_alloc();
        if (!tmp_pkt) {
            fprintf(stderr, "Failed to allocate decode tmp pkt\n");
            goto end;
        }

    }

    timer_start = av_gettime_relative();
    /* read all packets and only transcoding video */
    while (ret >= 0) {
        if (enable_concat)
        {
            ret = av_read_frame(ifmt_ctx[0], dec_pkt);
            if (ret < 0 && ret != AVERROR_EOF) {
                fprintf(stderr, "Failed to read frame packet\n");
                goto end;
            }

            ret = av_read_frame(ifmt_ctx[1], dec_pkt1);
            if (ret < 0 && ret != AVERROR_EOF) {
                fprintf(stderr, "Failed to read frame packet1\n");
                goto end;
            }

            if (!dec_pkt->size && !dec_pkt1->size)
                break;

            if (dec_pkt->size && (video_stream == dec_pkt->stream_index)) {
                ret = dec_enc(dec_pkt, enc_codec, argv[7], 0);
                av_packet_unref(dec_pkt);
            }

            if (dec_pkt1->size && (video_stream == dec_pkt1->stream_index)) {
                ret = dec_enc(dec_pkt1, enc_codec, argv[7], 1);
                av_packet_unref(dec_pkt1);
            }
        } else {
            if ((ret = av_read_frame(ifmt_ctx[0], dec_pkt)) < 0)
                break;

            if (video_stream == dec_pkt->stream_index)
                ret = dec_enc(dec_pkt, enc_codec, argv[7], 0);

            av_packet_unref(dec_pkt);
        }
    }

    /* flush decoder */
    av_packet_unref(dec_pkt);
    if ((ret = dec_enc(dec_pkt, enc_codec, argv[7], 0)) < 0) {
        fprintf(stderr, "Failed to flush decoder %s\n", av_err2str(ret));
        goto end;
    }

    /* flush encoder */
    if ((ret = encode_write(dec_pkt, NULL, 0)) < 0) {
        fprintf(stderr, "Failed to flush encoder %s\n", av_err2str(ret));
        goto end;
    }

    if (enable_concat) {
        /* flush decoder */
        av_packet_unref(dec_pkt1);
        if ((ret = dec_enc(dec_pkt1, enc_codec, argv[7], 1)) < 0) {
            fprintf(stderr, "Failed to flush decoder %s\n", av_err2str(ret));
            goto end;
        }

        /* flush encoder */
        if ((ret = encode_write(dec_pkt1, NULL, 1)) < 0) {
            fprintf(stderr, "Failed to flush encoder %s\n", av_err2str(ret));
            goto end;
        }
    }

    /* write the trailer for output stream */
    if ((ret = av_write_trailer(ofmt_ctx)) < 0)
        fprintf(stderr, "Failed to write trailer %s\n", av_err2str(ret));

    print_fps();

end:
    for (i = 0; i <= (enable_concat ? 1 : 0); i++) {
        avformat_close_input(&ifmt_ctx[i]);
        avcodec_free_context(&decoder_ctx[i]);
        avcodec_free_context(&encoder_ctx[i]);
        av_buffer_unref(&hw_device_ctx[i]);
        avpriv_packet_list_free(&pkt_list[i]);
        av_dict_free(&device_opts[i]);
    }
    avformat_close_input(&ofmt_ctx);
    av_packet_free(&dec_pkt);
    av_packet_free(&tmp_pkt);
    if (enable_concat)
        av_packet_free(&dec_pkt1);
    av_dict_free(&enc_opts);
    av_dict_free(&dec_opts);
    av_freep(&dynamic_setting);
    return ret;
}
