/*
 * Intel MediaSDK QSV based AV1 encoder
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


#include <stdint.h>
#include <sys/types.h>

#include <mfxvideo.h>

#include "libavutil/common.h"
#include "libavutil/opt.h"

#include "avcodec.h"
#include "internal.h"
#include "qsv.h"
#include "qsvenc.h"

typedef struct QSVAV1EncContext {
    AVClass *class;
    QSVEncContext qsv;
    int load_plugin;
} QSVAV1EncContext;

static av_cold int qsv_enc_init(AVCodecContext *avctx)
{
    QSVAV1EncContext *q = avctx->priv_data;

    return ff_qsv_enc_init(avctx, &q->qsv);
}

static int qsv_enc_frame(AVCodecContext *avctx, AVPacket *pkt,
                         const AVFrame *frame, int *got_packet)
{
    QSVAV1EncContext *q = avctx->priv_data;

    return ff_qsv_encode(avctx, &q->qsv, pkt, frame, got_packet);
}

static av_cold int qsv_enc_close(AVCodecContext *avctx)
{
    QSVAV1EncContext *q = avctx->priv_data;

    return ff_qsv_enc_close(avctx, &q->qsv);
}

#define OFFSET(x) offsetof(QSVAV1EncContext, x)
#define VE AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_ENCODING_PARAM
static const AVOption options[] = {
    QSV_COMMON_OPTS
    { "profile", NULL, OFFSET(qsv.profile), AV_OPT_TYPE_INT, { .i64 = MFX_PROFILE_UNKNOWN }, 0, INT_MAX, VE, "profile" },
        { "unknown" , NULL, 0, AV_OPT_TYPE_CONST, { .i64 = MFX_PROFILE_UNKNOWN      }, INT_MIN, INT_MAX,     VE, "profile" },
        { "main"    , NULL, 0, AV_OPT_TYPE_CONST, { .i64 = MFX_PROFILE_AV1_MAIN     }, INT_MIN, INT_MAX,     VE, "profile" },
    { "tile_cols",  "Number of columns for tiled encoding",   OFFSET(qsv.tile_cols),    AV_OPT_TYPE_INT, { .i64 = 0 }, 0, UINT16_MAX, VE },
    { "tile_rows",  "Number of rows for tiled encoding",      OFFSET(qsv.tile_rows),    AV_OPT_TYPE_INT, { .i64 = 0 }, 0, UINT16_MAX, VE },
    { NULL },
};

static const AVClass class = {
    .class_name = "av1_qsv encoder",
    .item_name  = av_default_item_name,
    .option     = options,
    .version    = LIBAVUTIL_VERSION_INT,
};

static const AVCodecDefault qsv_enc_defaults[] = {
    { NULL },
};

AVCodec ff_av1_qsv_encoder = {
    .name           = "av1_qsv",
    .long_name      = NULL_IF_CONFIG_SMALL("AV1 (Intel Quick Sync Video acceleration)"),
    .priv_data_size = sizeof(QSVAV1EncContext),
    .type           = AVMEDIA_TYPE_VIDEO,
    .id             = AV_CODEC_ID_AV1,
    .init           = qsv_enc_init,
    .encode2        = qsv_enc_frame,
    .close          = qsv_enc_close,
    .capabilities   = AV_CODEC_CAP_DELAY | AV_CODEC_CAP_HYBRID,
    .pix_fmts       = (const enum AVPixelFormat[]){ AV_PIX_FMT_NV12,
                                                    AV_PIX_FMT_P010,
                                                    AV_PIX_FMT_QSV,
                                                    AV_PIX_FMT_NONE },
    .priv_class     = &class,
    .defaults       = qsv_enc_defaults,
    .caps_internal  = FF_CODEC_CAP_INIT_CLEANUP,
    .wrapper_name   = "qsv",
    .hw_configs     = ff_qsv_enc_hw_configs,
};
