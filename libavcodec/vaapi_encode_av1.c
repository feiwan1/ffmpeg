/*
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

#include <va/va.h>
#include <va/va_enc_av1.h>

#include "libavutil/pixdesc.h"
#include "libavutil/avassert.h"
#include "libavutil/opt.h"
#include "internal.h"
#include "av1_profile_level.h"
#include "cbs.h"
#include "cbs_av1.h"
#include "av1.h"
#include "vaapi_encode.h"

#define AV1_MAX_QUANT 255

typedef struct VAAPIEncodeAV1Picture {
    int64_t last_idr_frame;
    int slot;
} VAAPIEncodeAV1Picture;

typedef struct VAAPIEncodeAV1Context {
    VAAPIEncodeContext common;
    AV1RawOBU sh; /**< sequence header.*/
    AV1RawOBU fh; /**< frame header.*/
    CodedBitstreamContext *cbc;
    CodedBitstreamFragment current_obu;
    VAConfigAttribValEncAV1 attr;
    VAConfigAttribValEncAV1Ext1 attr_ext1;
    VAConfigAttribValEncAV1Ext2 attr_ext2;

    char sh_data[MAX_PARAM_BUFFER_SIZE]; /**< coded sequence header data. */
    size_t sh_data_len; /**< bit length of sh_data. */
    char fh_data[MAX_PARAM_BUFFER_SIZE]; /**< coded frame header data. */
    size_t fh_data_len; /**< bit length of fh_data. */

    int enable_128x128_superblock;
    int sb_cols;
    int sb_rows;
    int tile_cols;
    int tile_rows;
    int tile_width_sb;
    int tile_height_sb;

    int q_idx_idr;
    int q_idx_p;
    int q_idx_b;

    /** user options */
    int profile;
    int level;
    int tier;
    int tile_cols_log2;
    int tile_rows_log2;
    int tile_groups;
} VAAPIEncodeAV1Context;

static av_cold int vaapi_encode_av1_configure(AVCodecContext *avctx)
{
    VAAPIEncodeContext     *ctx = avctx->priv_data;
    VAAPIEncodeAV1Context *priv = avctx->priv_data;
    int ret;
    ret = ff_cbs_init(&priv->cbc, AV_CODEC_ID_AV1, avctx);
    if (ret < 0)
        return ret;

    if (ctx->rc_mode->quality) {
        priv->q_idx_p = av_clip(ctx->rc_quality, 0, AV1_MAX_QUANT);
        if (avctx->i_quant_factor > 0.0)
            priv->q_idx_idr =
                av_clip((avctx->i_quant_factor * priv->q_idx_p  +
                         avctx->i_quant_offset) + 0.5,
                        0, AV1_MAX_QUANT);
        else
            priv->q_idx_idr = priv->q_idx_p;

        if (avctx->b_quant_factor > 0.0)
            priv->q_idx_b =
                av_clip((avctx->b_quant_factor * priv->q_idx_p  +
                         avctx->b_quant_offset) + 0.5,
                        0, AV1_MAX_QUANT);
        else
            priv->q_idx_b = priv->q_idx_p;
    } else {
        // Arbitrary value.
        priv->q_idx_idr = priv->q_idx_p = priv->q_idx_b = 128;
    }

    return 0;
}

static int vaapi_encode_av1_add_obu(AVCodecContext *avctx,
                                    CodedBitstreamFragment *au,
                                    uint8_t type,
                                    void *obu_unit)
{
    int ret;

    ret = ff_cbs_insert_unit_content(au, -1,
                                     type, obu_unit, NULL);
    if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Failed to add OBU unit: "
               "type = %d.\n", type);
        return ret;
    }

    return 0;
}

static int vaapi_encode_av1_write_obu(AVCodecContext *avctx,
                                      char *data, size_t *data_len,
                                      CodedBitstreamFragment *au)
{
    VAAPIEncodeAV1Context *priv = avctx->priv_data;
    int ret;

    ret = ff_cbs_write_fragment_data(priv->cbc, au);
    if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Failed to write packed header.\n");
        return ret;
    }

    if ((size_t)8 * MAX_PARAM_BUFFER_SIZE < 8 * au->data_size - au->data_bit_padding) {
        av_log(avctx, AV_LOG_ERROR, "Access unit too large: "
               "%zu < %zu.\n", (size_t)8 * MAX_PARAM_BUFFER_SIZE,
               8 * au->data_size - au->data_bit_padding);
        return AVERROR(ENOSPC);
    }

    memcpy(data, au->data, au->data_size);
    *data_len = 8 * au->data_size - au->data_bit_padding;

    return 0;
}

static int tile_log2(int blkSize, int target) {
    int k;
    for (k = 0; (blkSize << k) < target; k++);
    return k;
}

static int vaapi_encode_av1_set_tile(AVCodecContext *avctx)
{
    VAAPIEncodeAV1Context *priv = avctx->priv_data;
    int mi_cols, mi_rows, sb_shift, sb_size;
    int max_tile_width_sb, max_tile_area_sb;
    int min_log2_tile_cols, max_log2_tile_cols, max_log2_tile_rows;
    int min_log2_tiles, min_log2_tile_rows;

    /** TODO: consider to add an option of using 128x128 block. */
    priv->enable_128x128_superblock = 0;

    mi_cols = 2 * ((avctx->width + 7) >> 3);
    mi_rows = 2 * ((avctx->height + 7) >> 3);
    priv->sb_cols = priv->enable_128x128_superblock ?
                    ((mi_cols + 31) >> 5) : ((mi_cols + 15) >> 4);
    priv->sb_rows = priv->enable_128x128_superblock ?
                    ((mi_rows + 31) >> 5) : ((mi_rows + 15) >> 4);
    sb_shift = priv->enable_128x128_superblock ? 5 : 4;
    sb_size  = sb_shift + 2;
    max_tile_width_sb = AV1_MAX_TILE_WIDTH >> sb_size;
    max_tile_area_sb  = AV1_MAX_TILE_AREA  >> (2 * sb_size);

    min_log2_tile_cols = tile_log2(max_tile_width_sb, priv->sb_cols);
    max_log2_tile_cols = tile_log2(1, FFMIN(priv->sb_cols, AV1_MAX_TILE_COLS));
    max_log2_tile_rows = tile_log2(1, FFMIN(priv->sb_rows, AV1_MAX_TILE_ROWS));
    min_log2_tiles     = FFMAX(min_log2_tile_cols,
                               tile_log2(max_tile_area_sb, priv->sb_rows * priv->sb_cols));
    min_log2_tile_rows = FFMAX(min_log2_tiles - priv->tile_cols_log2, 0);

    if (priv->tile_cols_log2 != av_clip(priv->tile_cols_log2, min_log2_tile_cols, max_log2_tile_cols) ||
        priv->tile_rows_log2 != av_clip(priv->tile_rows_log2, min_log2_tile_rows, max_log2_tile_rows)) {
        priv->tile_cols_log2 = av_clip(priv->tile_cols_log2, min_log2_tile_cols, max_log2_tile_cols);
        priv->tile_rows_log2 = av_clip(priv->tile_rows_log2, min_log2_tile_rows, max_log2_tile_rows);
        av_log(avctx, AV_LOG_WARNING, "Correct tile cols/rows log2 to %d/%d.\n",
               priv->tile_cols_log2,
               priv->tile_rows_log2);
    }

    /** only support uniformed tile mode. */
    priv->tile_width_sb  = (priv->sb_cols + (1 << priv->tile_cols_log2) - 1) >>
                            priv->tile_cols_log2;
    priv->tile_height_sb = (priv->sb_rows + (1 << priv->tile_rows_log2) - 1) >>
                            priv->tile_rows_log2;
    priv->tile_cols = (priv->sb_cols + priv->tile_width_sb - 1) / priv->tile_width_sb;
    priv->tile_rows = (priv->sb_rows + priv->tile_height_sb - 1) / priv->tile_height_sb;

    /** check if tile cols/rows is supported by driver. */
    if ((priv->tile_cols * priv->tile_rows - 1) > priv->attr_ext2.bits.max_tile_num_minus1) {
        av_log(avctx, AV_LOG_ERROR, "Unsupported tile num %d * %d = %d, "
               "should be less than %d.\n", priv->tile_cols, priv->tile_rows,
               priv->tile_cols * priv->tile_rows,
               priv->attr_ext2.bits.max_tile_num_minus1 + 1);
        return AVERROR(EINVAL);
    }
    av_log(avctx, AV_LOG_DEBUG, "Setting tile cols/rows to %d/%d.\n",
           priv->tile_cols, priv->tile_rows);

    /** check if tile group numbers is valid. */
    if (priv->tile_groups > priv->tile_cols * priv->tile_rows) {
        av_log(avctx, AV_LOG_ERROR, "Invalid tile groups %d, "
        "should be less than %d.\n", priv->tile_groups, priv->tile_cols * priv->tile_rows);
        return AVERROR(EINVAL);
    }

    return 0;
}

static int vaapi_encode_av1_write_sequence_header(AVCodecContext *avctx,
                                                  char *data, size_t *data_len)
{
    VAAPIEncodeAV1Context *priv = avctx->priv_data;

    memcpy(data, &priv->sh_data, MAX_PARAM_BUFFER_SIZE * sizeof(char));
    *data_len = priv->sh_data_len;

    return 0;
}

static int vaapi_encode_av1_init_sequence_params(AVCodecContext *avctx)
{
    VAAPIEncodeContext               *ctx = avctx->priv_data;
    VAAPIEncodeAV1Context           *priv = avctx->priv_data;
    AV1RawOBU                     *sh_obu = &priv->sh;
    AV1RawSequenceHeader              *sh = &sh_obu->obu.sequence_header;
    VAEncSequenceParameterBufferAV1 *vseq = ctx->codec_sequence_params;
    CodedBitstreamFragment           *obu = &priv->current_obu;
    const AVPixFmtDescriptor *desc;
    int ret;

    memset(sh_obu, 0, sizeof(*sh_obu));
    sh_obu->header.obu_type = AV1_OBU_SEQUENCE_HEADER;

    desc = av_pix_fmt_desc_get(priv->common.input_frames->sw_format);
    av_assert0(desc);

    sh->seq_profile  = avctx->profile;
    if (!sh->seq_force_screen_content_tools)
        sh->seq_force_integer_mv = AV1_SELECT_INTEGER_MV;
    sh->frame_width_bits_minus_1  = av_log2(avctx->width);
    sh->frame_height_bits_minus_1 = av_log2(avctx->height);
    sh->max_frame_width_minus_1   = avctx->width - 1;
    sh->max_frame_height_minus_1  = avctx->height - 1;
    sh->enable_order_hint         = 1;
    sh->order_hint_bits_minus_1   = av_log2(avctx->gop_size);
    sh->seq_tier[0]               = priv->tier;
    sh->use_128x128_superblock    = priv->enable_128x128_superblock;

    sh->color_config = (AV1RawColorConfig) {
        .high_bitdepth                  = desc->comp[0].depth == 8 ? 0 : 1,
        .color_primaries                = avctx->color_primaries,
        .transfer_characteristics       = avctx->color_trc,
        .matrix_coefficients            = avctx->colorspace,
        .color_description_present_flag = (avctx->color_primaries != AVCOL_PRI_UNSPECIFIED ||
                                           avctx->color_trc       != AVCOL_TRC_UNSPECIFIED ||
                                           avctx->colorspace      != AVCOL_SPC_UNSPECIFIED),
        .subsampling_x                  = desc->log2_chroma_w,
        .subsampling_y                  = desc->log2_chroma_h,
    };

    if (avctx->level != FF_LEVEL_UNKNOWN) {
        sh->seq_level_idx[0] = avctx->level;
    } else {
        const AV1LevelDescriptor *level;
        float framerate;

        if (avctx->framerate.num > 0 && avctx->framerate.den > 0)
            framerate = avctx->framerate.num / avctx->framerate.den;
        else
            framerate = 0;

        level = ff_av1_guess_level(avctx->bit_rate, priv->tier,
                                   ctx->surface_width, ctx->surface_height,
                                   priv->tile_rows * priv->tile_cols,
                                   priv->tile_cols, framerate);
        if (level) {
            av_log(avctx, AV_LOG_VERBOSE, "Using level %s.\n", level->name);
            sh->seq_level_idx[0] = level->level_idx;
        } else {
            av_log(avctx, AV_LOG_VERBOSE, "Stream will not conform to "
                   "any normal level, using level 6.3 by default.\n");
            sh->seq_level_idx[0] = 19;
            sh->seq_tier[0] = 1;
        }
    }
    vseq->seq_profile             = sh->seq_profile;
    vseq->seq_level_idx           = sh->seq_level_idx[0];
    vseq->seq_tier                = sh->seq_tier[0];
    vseq->order_hint_bits_minus_1 = sh->order_hint_bits_minus_1;
    vseq->intra_period            = ctx->gop_size;
    vseq->ip_period               = ctx->b_per_p + 1;
    vseq->order_hint_bits_minus_1 = sh->order_hint_bits_minus_1;

    vseq->seq_fields.bits.enable_order_hint = sh->enable_order_hint;

    if (!(ctx->va_rc_mode & VA_RC_CQP)) {
        vseq->bits_per_second = ctx->va_bit_rate;
        vseq->seq_fields.bits.enable_cdef = sh->enable_cdef = 1;
    }

    ret = vaapi_encode_av1_add_obu(avctx, obu, AV1_OBU_SEQUENCE_HEADER, &priv->sh);
    if (ret < 0)
        goto end;

    ret = vaapi_encode_av1_write_obu(avctx, priv->sh_data, &priv->sh_data_len, obu);
    if (ret < 0)
        goto end;

end:
    ff_cbs_fragment_reset(obu);
    return ret;
}

static int vaapi_encode_av1_init_picture_params(AVCodecContext *avctx,
                                                VAAPIEncodePicture *pic)
{
    VAAPIEncodeContext              *ctx = avctx->priv_data;
    VAAPIEncodePicture             *prev = pic->prev;
    VAAPIEncodeAV1Picture         *hprev = prev ? prev->priv_data : NULL;
    VAAPIEncodeAV1Context          *priv = avctx->priv_data;
    VAAPIEncodeAV1Picture          *hpic = pic->priv_data;
    AV1RawOBU                    *fh_obu = &priv->fh;
    AV1RawFrameHeader                *fh = &fh_obu->obu.frame.header;
    VAEncPictureParameterBufferAV1 *vpic = pic->codec_picture_params;
    CodedBitstreamAV1Context      *cbctx = priv->cbc->priv_data;
    CodedBitstreamFragment          *obu = &priv->current_obu;
    VAAPIEncodeAV1Picture *href0, *href1;
    int offset, slot, i;
    int ret;
    static const int8_t default_loop_filter_ref_deltas[AV1_TOTAL_REFS_PER_FRAME] =
        { 1, 0, 0, 0, -1, 0, -1, -1 };

    memset(fh_obu, 0, sizeof(*fh_obu));
    pic->nb_slices = priv->tile_groups;
    fh_obu->header.obu_type = AV1_OBU_FRAME_HEADER;

    switch (pic->type) {
    case PICTURE_TYPE_IDR:
        av_assert0(pic->nb_refs == 0);
        fh->frame_type = AV1_FRAME_KEY;
        fh->refresh_frame_flags = 0xff;
        fh->base_q_idx = priv->q_idx_idr;
        hpic->slot = 0;
        hpic->last_idr_frame = pic->display_order;
        break;
    case PICTURE_TYPE_P:
        av_assert0(pic->nb_refs == 1);
        fh->frame_type = AV1_FRAME_INTER;
        fh->base_q_idx = priv->q_idx_p;
        hpic->last_idr_frame = hprev->last_idr_frame;
        href0 = pic->refs[0]->priv_data;
        vpic->ref_frame_ctrl_l0.fields.search_idx0 = 1;

        av_assert0(href0->slot == 0 || href0->slot == 1);

        if (ctx->max_b_depth > 0) {
            hpic->slot = !href0->slot;
            fh->refresh_frame_flags = 1 << hpic->slot | 0xfc;
            for (i=0; i < AV1_NUM_REF_FRAMES; i++) {
                fh->ref_order_hint[i] = pic->refs[0]->display_order - hpic->last_idr_frame;
            }

            if (hpic->slot) {
                for (i=0; i <= AV1_REFS_PER_FRAME; i++)
                    fh->ref_frame_idx[i] = 0;
                if (pic->refs[0]->refs[0])
                    fh->ref_order_hint[1] = pic->refs[0]->refs[0]->display_order - hpic->last_idr_frame;
            } else {
                for (i=0; i <= AV1_REFS_PER_FRAME; i++)
                    fh->ref_frame_idx[i] = 1;
                fh->primary_ref_frame = 1;
                if (pic->refs[0]->refs[0])
                    fh->ref_order_hint[0] = pic->refs[0]->refs[0]->display_order - hpic->last_idr_frame;
            }
        } else {
            hpic->slot = 0;
            fh->refresh_frame_flags = 0xff;
            for (i=0; i < AV1_NUM_REF_FRAMES; i++)
                fh->ref_order_hint[i] = pic->refs[0]->display_order - hpic->last_idr_frame;
        }
        break;
    case PICTURE_TYPE_B:
        av_assert0(pic->nb_refs == 2);
        href0 = pic->refs[0]->priv_data;
        href1 = pic->refs[1]->priv_data;
        av_assert0(href0->slot < pic->b_depth + 1 &&
                   href1->slot < pic->b_depth + 1);
        fh->frame_type = AV1_FRAME_INTER;
        fh->base_q_idx = priv->q_idx_b;
        fh->refresh_frame_flags = 0x0;
        fh->reference_select = 1;
        hpic->last_idr_frame = hprev->last_idr_frame;
        vpic->ref_frame_ctrl_l0.fields.search_idx0 = 1;
        vpic->ref_frame_ctrl_l1.fields.search_idx0 = 5;

        if (href0->slot) {
            for (i=0; i < 4; i++) {
                fh->ref_frame_idx[i] = 1;
            }
            for (i=4; i < 7; i++) {
                fh->ref_frame_idx[i] = 0;
            }
            for (i=0; i < AV1_NUM_REF_FRAMES; i++)
                fh->ref_order_hint[i] = pic->refs[1]->display_order - hpic->last_idr_frame;
            fh->ref_order_hint[1] = pic->refs[0]->display_order - hpic->last_idr_frame;
            fh->primary_ref_frame = 1;
        } else {
            for (i=0; i < 4; i++) {
                fh->ref_frame_idx[i] = 0;
            }
            for (i=4; i < 7; i++) {
                fh->ref_frame_idx[i] = 1;
            }

            for (i=0; i < AV1_NUM_REF_FRAMES; i++) {
                fh->ref_order_hint[i] = pic->refs[1]->display_order - hpic->last_idr_frame;
            }
            fh->ref_order_hint[0] = pic->refs[0]->display_order - hpic->last_idr_frame;
        }
        break;
    default:
        av_assert0(0 && "invalid picture type");
    }

    fh->show_frame                = pic->display_order <= pic->encode_order;
    fh->showable_frame            = fh->frame_type != AV1_FRAME_KEY;
    fh->frame_width_minus_1       = avctx->width - 1;
    fh->frame_height_minus_1      = avctx->height - 1;
    fh->render_width_minus_1      = fh->frame_width_minus_1;
    fh->render_height_minus_1     = fh->frame_height_minus_1;
    fh->order_hint                = pic->display_order - hpic->last_idr_frame;
    fh->tile_cols                 = priv->tile_cols;
    fh->tile_rows                 = priv->tile_rows;
    fh->tile_cols_log2            = priv->tile_cols_log2;
    fh->tile_rows_log2            = priv->tile_rows_log2;
    fh->uniform_tile_spacing_flag = 1;
    fh->tile_size_bytes_minus1    = priv->attr_ext2.bits.tile_size_bytes_minus1;
    pic->cache_coded_frame        = !fh->show_frame;

    if (priv->attr_ext2.bits.tx_mode_support)
        fh->tx_mode = priv->attr_ext2.bits.tx_mode_support;

    for (i=0; i < fh->tile_cols - 1; i++)
        fh->width_in_sbs_minus_1[i] = priv->tile_width_sb - 1;
    fh->width_in_sbs_minus_1[i] = priv->sb_cols - (fh->tile_cols - 1) * priv->tile_width_sb - 1;

    for (i=0; i < fh->tile_rows - 1; i++)
        fh->height_in_sbs_minus_1[i] = priv->tile_height_sb - 1;
    fh->height_in_sbs_minus_1[i] = priv->sb_rows - (fh->tile_rows - 1) * priv->tile_height_sb - 1;

    for (i=0; i < fh->tile_cols; i++)
        vpic->width_in_sbs_minus_1[i] = fh->width_in_sbs_minus_1[i];
    for (i=0; i < fh->tile_rows; i++)
        vpic->height_in_sbs_minus_1[i] = fh->height_in_sbs_minus_1[i];

    memcpy(fh->loop_filter_ref_deltas, default_loop_filter_ref_deltas,
           AV1_TOTAL_REFS_PER_FRAME * sizeof(int8_t));

    if (fh->frame_type == AV1_FRAME_KEY) {
        fh->error_resilient_mode = 1;
        fh->refresh_frame_flags = (1 << AV1_NUM_REF_FRAMES) - 1;
    }

    if (fh->frame_type == AV1_FRAME_KEY || fh->error_resilient_mode)
        fh->primary_ref_frame = AV1_PRIMARY_REF_NONE;

    vpic->base_qindex          = fh->base_q_idx;
    vpic->frame_width_minus_1  = fh->frame_width_minus_1;
    vpic->frame_height_minus_1 = fh->frame_height_minus_1;
    vpic->primary_ref_frame    = fh->primary_ref_frame;
    vpic->reconstructed_frame  = pic->recon_surface;
    vpic->coded_buf            = pic->output_buffer;
    vpic->tile_cols            = fh->tile_cols;
    vpic->tile_rows            = fh->tile_rows;
    vpic->order_hint           = fh->order_hint;

    vpic->picture_flags.bits.enable_frame_obu     = 0;
    vpic->picture_flags.bits.frame_type           = fh->frame_type;
    vpic->picture_flags.bits.reduced_tx_set       = fh->reduced_tx_set;
    vpic->picture_flags.bits.error_resilient_mode = fh->error_resilient_mode;

    vpic->mode_control_flags.bits.reference_mode = fh->reference_select ? 2 : 0;
    vpic->mode_control_flags.bits.tx_mode = fh->tx_mode;

    vpic->tile_group_obu_hdr_info.bits.obu_has_size_field = 1;

    /** set reference. */
    for (i = 0; i < AV1_REFS_PER_FRAME; i++)
        vpic->ref_frame_idx[i] = fh->ref_frame_idx[i];

    for (i = 0; i < FF_ARRAY_ELEMS(vpic->reference_frames); i++)
        vpic->reference_frames[i] = VA_INVALID_SURFACE;
    for (i = 0; i < pic->nb_refs; i++) {
        VAAPIEncodePicture *ref_pic = pic->refs[i];

        slot = ((VAAPIEncodeAV1Picture*)ref_pic->priv_data)->slot;
        av_assert0(vpic->reference_frames[slot] == VA_INVALID_SURFACE);

        vpic->reference_frames[slot] = ref_pic->recon_surface;
    }

    /** pack frame header, then set va params offset like bit_offset_qindex etc. */
    ret = vaapi_encode_av1_add_obu(avctx, obu, AV1_OBU_FRAME_HEADER , &priv->fh);
    if (ret < 0)
        goto end;

    ret = vaapi_encode_av1_write_obu(avctx, priv->fh_data, &priv->fh_data_len, obu);
    if (ret < 0)
        goto end;

    if (!(ctx->va_rc_mode & VA_RC_CQP)) {
        vpic->min_base_qindex = av_clip(avctx->qmin, 1, AV1_MAX_QUANT);
        vpic->max_base_qindex = av_clip(avctx->qmax, 1, AV1_MAX_QUANT);

        offset = cbctx->frame_header_data_offset - cbctx->frame_header_start_position;
        vpic->bit_offset_qindex            = offset + cbctx->qindex_offset;
        vpic->bit_offset_loopfilter_params = offset + cbctx->loopfilter_offset;
        vpic->bit_offset_cdef_params       = offset + cbctx->cdef_start_offset;
        vpic->size_in_bits_cdef_params     = cbctx->cdef_end_offset - cbctx->cdef_start_offset;
        vpic->size_in_bits_frame_hdr_obu   = priv->fh_data_len;
    }

    vpic->byte_offset_frame_hdr_obu_size = (((pic->type == PICTURE_TYPE_IDR) ?
                                            priv->sh_data_len/8 : 0) +
                                            (fh_obu->header.obu_extension_flag ?
                                            2 : 1));

end:
    ff_cbs_fragment_reset(obu);
    return ret;
}

static int vaapi_encode_av1_init_slice_params(AVCodecContext *avctx,
                                               VAAPIEncodePicture *pic,
                                               VAAPIEncodeSlice *slice)
{
    VAAPIEncodeAV1Context      *priv = avctx->priv_data;
    VAEncTileGroupBufferAV1  *vslice = slice->codec_slice_params;
    CodedBitstreamAV1Context  *cbctx = priv->cbc->priv_data;
    int div;

    /** Set tile group info. */
    div = priv->tile_cols * priv->tile_rows / priv->tile_groups;
    vslice->tg_start = slice->index * div;
    if (slice->index == (priv->tile_groups - 1)) {
        vslice->tg_end = priv->tile_cols * priv->tile_rows - 1;
        cbctx->seen_frame_header = 0;
    } else {
        vslice->tg_end = (slice->index + 1) * div - 1;
    }

    return 0;
}

static int vaapi_encode_av1_write_picture_header(AVCodecContext *avctx,
                                                 VAAPIEncodePicture *pic,
                                                 char *data, size_t *data_len)
{
    AV1RawOBU fh_obu;
    VAAPIEncodeAV1Context     *priv = avctx->priv_data;
    CodedBitstreamFragment     *obu = &priv->current_obu;
    AV1RawFrameHeader       *rep_fh = &fh_obu.obu.frame_header;
    CodedBitstreamAV1Context *cbctx = priv->cbc->priv_data;
    VAAPIEncodeAV1Picture *href0;
    int ret;

    pic->tail_size = 0;
    /** Pack repeat frame header. */
    if ((pic->type == PICTURE_TYPE_B) && (pic->next->type == PICTURE_TYPE_P)) {
        memset(&fh_obu, 0, sizeof(fh_obu));
        cbctx->seen_frame_header = 0;
        href0 = pic->refs[0]->priv_data;
        pic->tail_size = MAX_PARAM_BUFFER_SIZE;
        fh_obu.header.obu_type = AV1_OBU_FRAME_HEADER;
        rep_fh->show_existing_frame   = 1;
        rep_fh->frame_to_show_map_idx = href0->slot == 0;
        rep_fh->frame_type            = AV1_FRAME_INTER;
        rep_fh->frame_width_minus_1   = avctx->width - 1;
        rep_fh->frame_height_minus_1  = avctx->height - 1;
        rep_fh->render_width_minus_1  = rep_fh->frame_width_minus_1;
        rep_fh->render_height_minus_1 = rep_fh->frame_height_minus_1;

        ret = vaapi_encode_av1_add_obu(avctx, obu, AV1_OBU_FRAME_HEADER, &fh_obu);
        if (ret < 0)
            goto end;

        ret = vaapi_encode_av1_write_obu(avctx, pic->tail_data, &pic->tail_size, obu);
        if (ret < 0)
            goto end;

        pic->tail_size /= 8;
    }

    memcpy(data, &priv->fh_data, MAX_PARAM_BUFFER_SIZE * sizeof(char));
    *data_len = priv->fh_data_len;

end:
    ff_cbs_fragment_reset(obu);
    return ret;
}

static const VAAPIEncodeProfile vaapi_encode_av1_profiles[] = {
    { FF_PROFILE_AV1_MAIN,  8, 3, 1, 1, VAProfileAV1Profile0 },
    { FF_PROFILE_AV1_MAIN, 10, 3, 1, 1, VAProfileAV1Profile0 },
    { FF_PROFILE_UNKNOWN }
};

static const VAAPIEncodeType vaapi_encode_type_av1 = {
    .profiles        = vaapi_encode_av1_profiles,
    .flags           = FLAG_B_PICTURES |
                       FLAG_B_PICTURE_REFERENCES,
    .default_quality = 25,
    .configure       = &vaapi_encode_av1_configure,

    .sequence_header_type  = VAEncPackedHeaderSequence,
    .sequence_params_size  = sizeof(VAEncSequenceParameterBufferAV1),
    .init_sequence_params  = &vaapi_encode_av1_init_sequence_params,
    .write_sequence_header = &vaapi_encode_av1_write_sequence_header,

    .picture_priv_data_size = sizeof(VAAPIEncodeAV1Picture),
    .picture_header_type    = VAEncPackedHeaderPicture,
    .picture_params_size    = sizeof(VAEncPictureParameterBufferAV1),
    .init_picture_params    = &vaapi_encode_av1_init_picture_params,
    .write_picture_header   = &vaapi_encode_av1_write_picture_header,

    .slice_params_size = sizeof(VAEncTileGroupBufferAV1),
    .init_slice_params = &vaapi_encode_av1_init_slice_params,
};

static av_cold int vaapi_encode_av1_init(AVCodecContext *avctx)
{
    VAAPIEncodeContext      *ctx = avctx->priv_data;
    VAAPIEncodeAV1Context  *priv = avctx->priv_data;
    VAConfigAttrib attr;
    int ret;

    ctx->codec = &vaapi_encode_type_av1;

    ctx->surface_width  = FFALIGN(avctx->width,  16);
    ctx->surface_height = FFALIGN(avctx->height, 16);

    ctx->desired_packed_headers =
        VA_ENC_PACKED_HEADER_SEQUENCE |
        VA_ENC_PACKED_HEADER_PICTURE;
        //VA_ENC_PACKED_HEADER_SLICE;

    if (avctx->profile == FF_PROFILE_UNKNOWN)
        avctx->profile = priv->profile;
    if (avctx->level == FF_LEVEL_UNKNOWN)
        avctx->level = priv->level;

    if (avctx->level != FF_LEVEL_UNKNOWN && avctx->level & ~0x1f) {
        av_log(avctx, AV_LOG_ERROR, "Invalid level %d\n", avctx->level);
        return AVERROR(EINVAL);
    }

    ret = ff_vaapi_encode_init(avctx);
    if (ret < 0)
        return ret;

    attr.type = VAConfigAttribEncAV1;
    ret = ff_vaapi_encode_get_attributs(avctx, &attr);
    if (ret < 0)
        return ret;
    if (attr.value == VA_ATTRIB_NOT_SUPPORTED)
        priv->attr.value = 0;
    else
        priv->attr.value = attr.value;

    attr.type = VAConfigAttribEncAV1Ext1;
    ret = ff_vaapi_encode_get_attributs(avctx, &attr);
    if (ret < 0)
        return ret;
    if (attr.value == VA_ATTRIB_NOT_SUPPORTED)
        priv->attr_ext1.value = 0;
    else
        priv->attr_ext1.value = attr.value;

    attr.type = VAConfigAttribEncAV1Ext2;
    ret = ff_vaapi_encode_get_attributs(avctx, &attr);
    if (ret < 0)
        return ret;
    if (attr.value == VA_ATTRIB_NOT_SUPPORTED)
        priv->attr_ext2.value = 0;
    else
        priv->attr_ext2.value = attr.value;

    ret = vaapi_encode_av1_set_tile(avctx);
    if (ret < 0)
        return ret;

    return 0;
}

static av_cold int vaapi_encode_av1_close(AVCodecContext *avctx)
{
    VAAPIEncodeAV1Context *priv = avctx->priv_data;

    ff_cbs_fragment_free(&priv->current_obu);
    ff_cbs_close(&priv->cbc);

    return ff_vaapi_encode_close(avctx);
}

#define OFFSET(x) offsetof(VAAPIEncodeAV1Context, x)
#define FLAGS (AV_OPT_FLAG_VIDEO_PARAM | AV_OPT_FLAG_ENCODING_PARAM)

static const AVOption vaapi_encode_av1_options[] = {
    VAAPI_ENCODE_COMMON_OPTIONS,
    VAAPI_ENCODE_RC_OPTIONS,
    { "profile", "Set profile (seq_profile)",
      OFFSET(profile), AV_OPT_TYPE_INT,
      { .i64 = FF_PROFILE_UNKNOWN }, FF_PROFILE_UNKNOWN, 0xff, FLAGS, "profile" },

#define PROFILE(name, value)  name, NULL, 0, AV_OPT_TYPE_CONST, \
    { .i64 = value }, 0, 0, FLAGS, "profile"
    { PROFILE("main",               FF_PROFILE_AV1_MAIN) },
    { PROFILE("high",               FF_PROFILE_AV1_HIGH) },
    { PROFILE("professional",       FF_PROFILE_AV1_PROFESSIONAL) },
#undef PROFILE

    { "tier", "Set tier (seq_tier)",
      OFFSET(tier), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, 1, FLAGS, "tier" },
    { "main", NULL, 0, AV_OPT_TYPE_CONST,
      { .i64 = 0 }, 0, 0, FLAGS, "tier" },
    { "high", NULL, 0, AV_OPT_TYPE_CONST,
      { .i64 = 1 }, 0, 0, FLAGS, "tier" },
    { "level", "Set level (seq_level_idx)",
      OFFSET(level), AV_OPT_TYPE_INT,
      { .i64 = FF_LEVEL_UNKNOWN }, FF_LEVEL_UNKNOWN, 0x1f, FLAGS, "level" },

#define LEVEL(name, value) name, NULL, 0, AV_OPT_TYPE_CONST, \
      { .i64 = value }, 0, 0, FLAGS, "level"
    { LEVEL("2.0",  0) },
    { LEVEL("2.1",  1) },
    { LEVEL("3.0",  4) },
    { LEVEL("3.1",  5) },
    { LEVEL("4.0",  8) },
    { LEVEL("4.1",  9) },
    { LEVEL("5.0", 12) },
    { LEVEL("5.1", 13) },
    { LEVEL("5.2", 14) },
    { LEVEL("5.3", 15) },
    { LEVEL("6.0", 16) },
    { LEVEL("6.1", 17) },
    { LEVEL("6.2", 18) },
    { LEVEL("6.3", 19) },
#undef LEVEL

    { "tile_cols_log2", "Log2 of columns number for tiled encoding",
      OFFSET(tile_cols_log2), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, av_log2(AV1_MAX_TILE_COLS), FLAGS },
    { "tile_rows_log2", "Log2 of rows number for tiled encoding",
      OFFSET(tile_rows_log2), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, av_log2(AV1_MAX_TILE_ROWS), FLAGS },
    { "tile_groups", "Number of tile groups for encoding",
      OFFSET(tile_groups), AV_OPT_TYPE_INT, { .i64 = 1 }, 1, AV1_MAX_TILE_ROWS * AV1_MAX_TILE_COLS, FLAGS },

    { NULL },
};

static const AVCodecDefault vaapi_encode_av1_defaults[] = {
    { "b",              "0"   },
    { "bf",             "0"   },
    { "g",              "120" },
    { "i_qfactor",      "1"   },
    { "i_qoffset",      "0"   },
    { "b_qfactor",      "6/5" },
    { "b_qoffset",      "0"   },
    { "qmin",           "1"   },
    { "qmax",           "255" },
    { NULL },
};

static const AVClass vaapi_encode_av1_class = {
    .class_name = "av1_vaapi",
    .item_name  = av_default_item_name,
    .option     = vaapi_encode_av1_options,
    .version    = LIBAVUTIL_VERSION_INT,
};

AVCodec ff_av1_vaapi_encoder = {
    .name           = "av1_vaapi",
    .long_name      = NULL_IF_CONFIG_SMALL("AV1 (VAAPI)"),
    .type           = AVMEDIA_TYPE_VIDEO,
    .id             = AV_CODEC_ID_AV1,
    .priv_data_size = sizeof(VAAPIEncodeAV1Context),
    .init           = &vaapi_encode_av1_init,
    .receive_packet = &ff_vaapi_encode_receive_packet,
    .close          = &vaapi_encode_av1_close,
    .priv_class     = &vaapi_encode_av1_class,
    .capabilities   = AV_CODEC_CAP_DELAY | AV_CODEC_CAP_HARDWARE,
    .caps_internal  = FF_CODEC_CAP_INIT_CLEANUP,
    .defaults       = vaapi_encode_av1_defaults,
    .pix_fmts = (const enum AVPixelFormat[]) {
        AV_PIX_FMT_VAAPI,
        AV_PIX_FMT_NONE,
    },
    .hw_configs     = ff_vaapi_encode_hw_configs,
    .wrapper_name   = "vaapi",
};
