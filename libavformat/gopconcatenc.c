/*
 * Stream merge muxer
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "avformat.h"
#include "internal.h"
#include "mux.h"

#include "libavutil/opt.h"
#include "libavcodec/packet_internal.h"

typedef struct GopConcatMuxContext {
    const AVClass *class;
    AVFormatContext *avf;
    AVDictionary *format_options;
    PacketList pkt_list;
    unsigned int stream_idx;
    int output_number;
    unsigned int gop_counter;

    uint8_t header_written;
    // user options
    char *format;
    int gop_size;
} GopConcatMuxContext;

static av_cold int gop_concat_init(AVFormatContext *ctx)
{
    GopConcatMuxContext *s = ctx->priv_data;
    const AVOutputFormat *oformat;
    AVFormatContext *avf2;
    AVStream *st;
    int ret;

    oformat = av_guess_format(s->format, ctx->url, NULL);
    if (!oformat) {
        ret = AVERROR_MUXER_NOT_FOUND;
        return ret;
    }

    ret = avformat_alloc_output_context2(&avf2, oformat, NULL, ctx->url);
    if (ret < 0)
        return ret;

    s->avf = avf2;

    avf2->interrupt_callback = ctx->interrupt_callback;
    avf2->max_delay = ctx->max_delay;
    ret = av_dict_copy(&avf2->metadata, ctx->metadata, 0);
    if (ret < 0)
        return ret;
    avf2->opaque = ctx->opaque;
    avf2->io_close = ctx->io_close;
    avf2->io_close2 = ctx->io_close2;
    avf2->io_open = ctx->io_open;
    avf2->flags = ctx->flags;

    // create one output stream.
    st = ff_stream_clone(avf2, ctx->streams[0]);
    if (!st)
        return AVERROR(ENOMEM);

    return 0;
}

static int gop_concat_write_header(AVFormatContext *ctx)
{
    GopConcatMuxContext *s = ctx->priv_data;
    AVFormatContext *avf2 = s->avf;
    AVDictionary *format_options = NULL;
    int ret, i;

    ret = av_dict_copy(&format_options, s->format_options, 0);
    if (ret < 0)
        goto end;

    ret = ff_format_output_open(avf2, ctx->url, &format_options);
    if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "Error opening %s: %s\n", ctx->url,
               av_err2str(ret));
        goto end;
    }

    for (i = 0;i < avf2->nb_streams; i++)
        ffstream(avf2->streams[i])->cur_dts = 0;

    ret = avformat_write_header(avf2, &format_options);
    if (!ret)
        s->header_written = 1;

    // Check for options unrecognized by underlying muxer
    if (format_options) {
        AVDictionaryEntry *entry = NULL;
        while ((entry = av_dict_get(format_options, "", entry, AV_DICT_IGNORE_SUFFIX)))
            av_log(avf2, AV_LOG_ERROR, "Unknown option '%s'\n", entry->key);
        ret = AVERROR(EINVAL);
    }

end:
    av_dict_free(&format_options);
    return ret;
}

static void packet_list_remove(PacketList *pkt_list, PacketListEntry *pktl, PacketListEntry *prev)
{
    if (prev) {
        if (pktl->next)
            prev->next = pktl->next;
        else {
            prev->next = NULL;
            pkt_list->tail = prev;
        }
    } else {
        if (pktl->next)
            pkt_list->head = pktl->next;
        else {
            pkt_list->head = NULL;
            pkt_list->tail = NULL;
        }
    }
    av_freep(&pktl);
}

static int send_output_pkt(AVFormatContext *ctx)
{
    GopConcatMuxContext *s = ctx->priv_data;
    AVFormatContext *avf2 = s->avf;
    PacketListEntry *pktl, *prev_pktl = NULL;
    AVPacket *pkt = NULL;
    AVRational src_tb, dst_tb;
    int ret;

    pktl = s->pkt_list.head;
    while (pktl) {
        pkt = &pktl->pkt;
        if (pkt->stream_index == s->stream_idx) {

            av_log(s, AV_LOG_DEBUG, "send output pkt pts:%ld, dts:%ld, idx:%d\n", pkt->pts, pkt->dts, pkt->stream_index);

            // force to push pkt to one output stream.
            if (pkt->stream_index)
                pkt->stream_index = 0;

            pkt->pts += (s->gop_counter/ctx->nb_streams + s->stream_idx) * s->gop_size;
            pkt->dts += (s->gop_counter/ctx->nb_streams + s->stream_idx) * s->gop_size;

            src_tb = ctx->streams[0]->time_base;
            dst_tb = avf2->streams[0]->time_base;
            av_packet_rescale_ts(pkt, src_tb, dst_tb);

            ret = av_write_frame(avf2, pkt);
            if (ret < 0) {
                av_log(s, AV_LOG_ERROR, "Failed to send output pkt.\n");
                return ret;
            }

            // remove from list
            packet_list_remove(&s->pkt_list, pktl, prev_pktl);
            if (++s->output_number == s->gop_size) {
                s->gop_counter++;
                s->output_number = 0;
                if (++s->stream_idx == ctx->nb_streams)
                    s->stream_idx = 0;
            }
        } else
            prev_pktl = pktl;

        pktl = prev_pktl ? prev_pktl->next : s->pkt_list.head;
    }

    return 0;
}

static int gop_concat_write_packet(AVFormatContext *ctx, AVPacket *pkt)
{
    GopConcatMuxContext *s = ctx->priv_data;
    int ret;

    av_log(s, AV_LOG_DEBUG, "input pkt pts:%ld, dts:%ld, idx:%d, size:%d-\n", pkt->pts, pkt->dts, pkt->stream_index, pkt->size);

    ret = avpriv_packet_list_put(&s->pkt_list, pkt, NULL, 0);
    if (ret < 0) {
        av_log(s, AV_LOG_ERROR, "Failed to put pkt to list.\n");
        avpriv_packet_list_free(&s->pkt_list);
        return ret;
    }

    ret = send_output_pkt(ctx);
    if (ret < 0) {
        av_log(s, AV_LOG_ERROR, "Failed to send output pkt.\n");
        return ret;
    }

    return 0;
}

static av_cold int gop_concat_write_trailer(AVFormatContext *ctx)
{
    GopConcatMuxContext *s = ctx->priv_data;
    AVFormatContext *avf2 = s->avf;
    PacketListEntry *pktl, *prev_pktl;
    AVPacket *pkt = NULL;
    AVRational src_tb, dst_tb;
    int ret, i;

    if (!s->header_written)
        return 0;
    for (i = 0; i < ctx->nb_streams; i++) {
        pktl = s->pkt_list.head;
        prev_pktl = NULL;
        while (pktl) {
            pkt = &pktl->pkt;
            if (pkt->stream_index == i) {
                // avio_write(ctx->pb, pkt->data, pkt->size);
                if (pkt->stream_index)
                    pkt->stream_index = 0;

                pkt->pts += (s->gop_counter/ctx->nb_streams + s->stream_idx) * s->gop_size;
                pkt->dts += (s->gop_counter/ctx->nb_streams + s->stream_idx) * s->gop_size;

                src_tb = ctx->streams[0]->time_base;
                dst_tb = avf2->streams[0]->time_base;
                av_packet_rescale_ts(pkt, src_tb, dst_tb);

                ret = av_write_frame(avf2, pkt);
                if (ret < 0) {
                    av_log(s, AV_LOG_ERROR, "Failed to send output pkt.\n");
                    return ret;
                }
                packet_list_remove(&s->pkt_list, pktl, prev_pktl);
            } else
                prev_pktl = pktl;

            pktl = prev_pktl ? prev_pktl->next : s->pkt_list.head;
       }
    }
    ret = av_write_trailer(avf2);
    ff_format_io_close(avf2, &avf2->pb);

    return ret;
}

static void gop_concat_deinit(AVFormatContext *ctx)
{
    GopConcatMuxContext *s = ctx->priv_data;

    avformat_free_context(s->avf);
}

#define OFFSET(x) offsetof(GopConcatMuxContext, x)
#define ENC AV_OPT_FLAG_ENCODING_PARAM
static const AVOption options[] = {
    {"concat_format", "Target muxer", OFFSET(format),
      AV_OPT_TYPE_STRING, {.str = NULL}, 0, 0, ENC},
    { "gop_size", "Number of frames in each gop.", OFFSET(gop_size),
      AV_OPT_TYPE_INT, { .i64 = 1 }, 0, UINT16_MAX, ENC },
    { NULL },
};

static const AVClass gop_concat_muxer_class = {
    .class_name = "gop_concat",
    .item_name  = av_default_item_name,
    .option     = options,
    .version    = LIBAVUTIL_VERSION_INT
};

const AVOutputFormat ff_gop_concat_muxer = {
    .name              = "gop_concat",
    .long_name         = NULL_IF_CONFIG_SMALL("gop concat muxer"),
    .priv_data_size    = sizeof(GopConcatMuxContext),
    .init              = gop_concat_init,
    .write_header      = gop_concat_write_header,
    .write_packet      = gop_concat_write_packet,
    .write_trailer     = gop_concat_write_trailer,
    .deinit            = gop_concat_deinit,
    .priv_class        = &gop_concat_muxer_class,
    .flags             = AVFMT_GLOBALHEADER,
};
