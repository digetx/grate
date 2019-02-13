/*
 * Copyright (c) 2012, 2013 Erik Faye-Lund
 * Copyright (c) 2013 Avionic Design GmbH
 * Copyright (c) 2013 Thierry Reding
 * Copyright (c) 2017 Dmitry Osipenko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include <string.h>

#include "libgrate-private.h"
#include "grate-3d.h"
#include "tgr_3d.xml.h"

static uint32_t float_to_fp20(float f)
{
	uint32_t sign, mantissa, exponent;
	union {
		uint32_t u;
		float f;
	} value;

	if (f == 0.0f)
		return 0;

	value.f = f;

	sign = (value.u >> 31) & 0x1;
	exponent = (value.u >> 23) & 0xff;
	mantissa = (value.u >>  0) & 0x7fffff;

	if (exponent == 0xff)
		exponent = 0x3f;
	else
		exponent = (exponent - 127 + 31) & 0x3f;

	return (sign << 19) | (exponent << 13) | (mantissa >> (23 - 13));
}

static uint32_t float_to_fx10(float f)
{
	uint32_t u = f * 256.0f;
	return u & 0x3ff;
}

struct grate_3d_ctx * grate_3d_alloc_ctx(struct grate *grate)
{
	struct grate_3d_ctx *ctx = calloc(1, sizeof(struct grate_3d_ctx));
	struct host1x *host1x = grate->host1x;
	uint32_t *ptr;

	if (!ctx) {
		grate_error("Failed to allocate context\n");
		return NULL;
	}

	ctx->grate = grate;

	if (host1x->drm_v2) {
		ctx->uniforms_bo = HOST1X_BO_CREATE(host1x, (256 * 4 + 32) * 4,
						    HOST1X_BO_CREATE_FLAG_GATHER);

		host1x_bo_mmap(ctx->uniforms_bo, (void**)&ptr);

		ctx->gather_vs_uniforms.data_offset = 0;
		ctx->gather_vs_uniforms.reg_offset = TGR3D_VP_UPLOAD_CONST;
		ctx->gather_vs_uniforms.count = 256 * 4;
		ctx->gather_vs_uniforms.incr = 0;

		ctx->gather_fs_uniforms.data_offset = 256 * 4;
		ctx->gather_fs_uniforms.reg_offset = TGR3D_FP_CONST(0);
		ctx->gather_fs_uniforms.count = 32;
		ctx->gather_fs_uniforms.incr = 1;

		ctx->vs_uniforms = ptr;
		ctx->fs_uniforms = ptr + 256 * 4;
	} else {
		ctx->vs_uniforms = ctx->vs_uniforms_data;
		ctx->fs_uniforms = ctx->fs_uniforms_data;
	}

	return ctx;
}

int grate_3d_ctx_vertex_attrib_pointer(struct grate_3d_ctx *ctx,
				       unsigned location, unsigned size,
				       unsigned type, unsigned stride,
				       struct host1x_bo *data_bo)
{
	struct grate_vtx_attribute *attr;

	switch (type) {
	case TGR3D_ATTRIB_TYPE_UBYTE:
	case TGR3D_ATTRIB_TYPE_UBYTE_NORM:
	case TGR3D_ATTRIB_TYPE_SBYTE:
	case TGR3D_ATTRIB_TYPE_SBYTE_NORM:
	case TGR3D_ATTRIB_TYPE_USHORT:
	case TGR3D_ATTRIB_TYPE_USHORT_NORM:
	case TGR3D_ATTRIB_TYPE_SSHORT:
	case TGR3D_ATTRIB_TYPE_SSHORT_NORM:
	case TGR3D_ATTRIB_TYPE_FIXED16:
	case TGR3D_ATTRIB_TYPE_FLOAT16:
	case TGR3D_ATTRIB_TYPE_UINT:
	case TGR3D_ATTRIB_TYPE_UINT_NORM:
	case TGR3D_ATTRIB_TYPE_SINT:
	case TGR3D_ATTRIB_TYPE_SINT_NORM:
	case TGR3D_ATTRIB_TYPE_FLOAT32:
		break;
	default:
		grate_error("Invalid type %u\n", type);
		return -1;
	}

	if (location >= 16) {
		grate_error("Invalid location %u\n", location);
		return -1;
	}

	if (!data_bo) {
		grate_error("Invalid data BO ptr\n");
		return -1;
	}

	attr = calloc(1, sizeof(*attr));
	if (!attr) {
		grate_error("Allocation failed\n");
		return -1;
	}

	attr->stride = stride;
	attr->type = type;
	attr->size = size;
	attr->bo = data_bo;

	ctx->vtx_attributes[location] = attr;

	return 0;
}

int grate_3d_ctx_enable_vertex_attrib_array(struct grate_3d_ctx *ctx,
					    unsigned location)
{
	if (location >= 16) {
		grate_error("Invalid location %u\n", location);
		return -1;
	}

	ctx->attributes_enable_mask |= 1u << location;

	return 0;
}

int grate_3d_ctx_disable_vertex_attrib_array(struct grate_3d_ctx *ctx,
					     unsigned target)
{
	if (target >= 16) {
		grate_error("Invalid target location %u\n", target);
		return -1;
	}

	ctx->attributes_enable_mask &= ~(1u << target);

	return 0;
}

int grate_3d_ctx_bind_render_target(struct grate_3d_ctx *ctx,
				    unsigned target,
				    struct host1x_pixelbuffer *pixbuf)
{
	switch (pixbuf->format) {
	case PIX_BUF_FMT_A8:
	case PIX_BUF_FMT_L8:
	case PIX_BUF_FMT_S8:
	case PIX_BUF_FMT_LA88:
	case PIX_BUF_FMT_RGB565:
	case PIX_BUF_FMT_RGBA5551:
	case PIX_BUF_FMT_RGBA4444:
	case PIX_BUF_FMT_D16_LINEAR:
	case PIX_BUF_FMT_D16_NONLINEAR:
	case PIX_BUF_FMT_RGBA8888:
	case PIX_BUF_FMT_RGBA_FP32:
		break;
	default:
		grate_error("Invalid format %u\n", pixbuf->format);
		return -1;
	}

	if (target >= 16) {
		grate_error("Invalid target location %u\n", target);
		return -1;
	}

	ctx->render_targets[target].pixbuf = pixbuf;

	return 0;
}

int grate_3d_ctx_set_render_target_dither(struct grate_3d_ctx *ctx,
					  unsigned target,
					  bool enable)
{
	if (target >= 16) {
		grate_error("Invalid target location %u\n", target);
		return -1;
	}

	ctx->render_targets[target].dither_enabled = enable;

	return 0;
}

int grate_3d_ctx_enable_render_target(struct grate_3d_ctx *ctx,
				      unsigned target)
{
	if (target >= 16) {
		grate_error("Invalid target location %u\n", target);
		return -1;
	}

	ctx->render_targets_enable_mask |= 1u << target;

	return 0;
}

int grate_3d_ctx_disable_render_target(struct grate_3d_ctx *ctx,
				       unsigned target)
{
	if (target >= 16) {
		grate_error("Invalid target location %u\n", target);
		return -1;
	}

	ctx->render_targets_enable_mask &= ~(1u << target);

	return 0;
}

int grate_3d_ctx_bind_program(struct grate_3d_ctx *ctx,
			      struct grate_program *program)
{
	if (!program) {
		grate_error("Bad program ptr\n");
		return -1;
	}

	ctx->program = program;

	memcpy(ctx->vs_uniforms, program->vs_constants,
	       sizeof(ctx->vs_uniforms_data));

	memcpy(ctx->fs_uniforms, program->fs_constants,
	       sizeof(ctx->fs_uniforms_data));

	return 0;
}

int grate_3d_ctx_set_vertex_uniform(struct grate_3d_ctx *ctx,
				    unsigned location, unsigned nb,
				    float *values)
{
	if (!ctx->program) {
		grate_error("No program bound\n");
		return -1;
	}

	if (location >= 256 || location + nb > 256) {
		grate_error("Invalid location %u\n", location);
		return -1;
	}

	memcpy(&ctx->vs_uniforms[location * 4], values, nb * sizeof(float));

	return 0;
}

int grate_3d_ctx_set_vertex_mat4_uniform(struct grate_3d_ctx *ctx,
					 unsigned location, struct mat4 *mat)
{
	return grate_3d_ctx_set_vertex_uniform(ctx, location,
					       16, (float *) mat);
}

int grate_3d_ctx_set_fragment_uniform(struct grate_3d_ctx *ctx,
				      unsigned location, unsigned nb,
				      float *value)
{
	bool lowp = !!(location & 0x8000);
	unsigned components_mask = (location >> 8) & 0xF;
	unsigned components_nb = 0;
	unsigned i;

	if (!ctx->program) {
		grate_error("No program bound\n");
		return -1;
	}

	if (location == ~0u) {
		grate_error("Invalid location %u\n", location);
		return -1;
	}

	if (components_mask == 0) {
		grate_error("Empty components mask\n");
		return -1;
	}

	if (BIT(0) & components_mask)
		components_nb = 1;

	if (BIT(1) & components_mask)
		components_nb = 2;

	if (BIT(2) & components_mask)
		components_nb = 3;

	if (BIT(3) & components_mask)
		components_nb = 4;

	location &= 0xFF;

	if (components_nb > nb || nb > 4) {
		grate_error("Invalid components number %u - should be at "
			    "least %u, 4 maximum\n",
			    nb, components_nb);
		return -1;
	}

	if (location >= 64) {
		grate_error("Invalid location %u\n", location);
		return -1;
	}

	for (i = 0; i < 4; i++) {
		unsigned position = location >> 1;
		bool fx10_high = !!(location & 1);

		if (!(components_mask & BIT(i)))
			continue;

		if (lowp) {
			uint32_t fx10_val = float_to_fx10(value[i]);

			if (fx10_high) {
				ctx->fs_uniforms[position] &= 0x3ff;
				ctx->fs_uniforms[position] |= fx10_val << 10;
			} else {
				ctx->fs_uniforms[position] &= ~0x3ff;
				ctx->fs_uniforms[position] |= fx10_val;
			}
		} else {
			ctx->fs_uniforms[position] = float_to_fp20(value[i]);
		}

		location += lowp ? 1 : 2;
	}

	return 0;
}

int grate_3d_ctx_set_fragment_float_uniform(struct grate_3d_ctx *ctx,
					    unsigned location, float value)
{
	return grate_3d_ctx_set_fragment_uniform(ctx, location, 1, &value);
}

void grate_3d_ctx_set_depth_range(struct grate_3d_ctx *ctx,
				  float near, float far)
{
	ctx->depth_range_near = near;
	ctx->depth_range_far = far;
}

void grate_3d_ctx_set_dither(struct grate_3d_ctx *ctx, uint32_t unk)
{
	ctx->dither_unk = unk;
}

void grate_3d_ctx_set_viewport_bias(struct grate_3d_ctx *ctx,
				    float x, float y, float z)
{
	ctx->viewport_x_bias = x;
	ctx->viewport_y_bias = y;
	ctx->viewport_z_bias = z;
}

void grate_3d_ctx_set_viewport_scale(struct grate_3d_ctx *ctx,
				     float width, float height, float depth)
{
	ctx->viewport_x_scale = width;
	ctx->viewport_y_scale = height;
	ctx->viewport_z_scale = depth;
}

void grate_3d_ctx_set_point_params(struct grate_3d_ctx *ctx, uint32_t params)
{
	ctx->point_params = params;
}

void grate_3d_ctx_set_point_size(struct grate_3d_ctx *ctx, float size)
{
	ctx->point_size = size;
}

void grate_3d_ctx_set_line_params(struct grate_3d_ctx *ctx, uint32_t params)
{
	ctx->line_params = params;
}

void grate_3d_ctx_set_line_width(struct grate_3d_ctx *ctx, float width)
{
	ctx->line_width = width;
}

void grate_3d_ctx_use_guardband(struct grate_3d_ctx *ctx, bool enabled)
{
	ctx->guarband_enabled = enabled;
}

void grate_3d_ctx_set_front_direction_is_cw(struct grate_3d_ctx *ctx,
					    bool front_cw)
{
	switch (ctx->cull_face) {
	case TGR3D_CULL_FACE_NONE:
	case TGR3D_CULL_FACE_BOTH:
		break;

	case TGR3D_CULL_FACE_CW:
		if (ctx->tri_face_front_cw != front_cw)
			ctx->cull_face = TGR3D_CULL_FACE_CCW;
		break;

	case TGR3D_CULL_FACE_CCW:
		if (ctx->tri_face_front_cw != front_cw)
			ctx->cull_face = TGR3D_CULL_FACE_CW;
		break;

	default:
		grate_error("Invalid cull face %u\n", ctx->cull_face);
		abort();
	}

	ctx->tri_face_front_cw = front_cw;
}

void grate_3d_ctx_set_cull_face(struct grate_3d_ctx *ctx,
                                enum grate_3d_ctx_cull_face cull_face)
{
	bool front_cw = ctx->tri_face_front_cw;

	switch (cull_face) {
	case GRATE_3D_CTX_CULL_FACE_NONE:
		ctx->cull_face = TGR3D_CULL_FACE_NONE;
		break;

	case GRATE_3D_CTX_CULL_FACE_FRONT:
		ctx->cull_face =
			front_cw ? TGR3D_CULL_FACE_CW : TGR3D_CULL_FACE_CCW;
		break;

	case GRATE_3D_CTX_CULL_FACE_BACK:
		ctx->cull_face =
			front_cw ?TGR3D_CULL_FACE_CCW : TGR3D_CULL_FACE_CW;
		break;

	case GRATE_3D_CTX_CULL_FACE_BOTH:
		ctx->cull_face = TGR3D_CULL_FACE_BOTH;
		break;

	default:
		grate_error("Invalid cull face %u\n", cull_face);
	}
}

void grate_3d_ctx_set_scissor(struct grate_3d_ctx *ctx,
			      unsigned x, unsigned width,
			      unsigned y, unsigned height)
{
	ctx->scissor_x = x;
	ctx->scissor_y = y;
	ctx->scissor_width = width;
	ctx->scissor_heigth = height;
}

void grate_3d_ctx_set_point_coord_range(struct grate_3d_ctx *ctx,
					float min_s, float max_s,
					float min_t, float max_t)
{
	ctx->point_coord_range_min_s = min_s;
	ctx->point_coord_range_max_s = max_s;
	ctx->point_coord_range_min_t = min_t;
	ctx->point_coord_range_max_t = max_t;
}

void grate_3d_ctx_set_polygon_offset(struct grate_3d_ctx *ctx,
				     float units, float factor)
{
	ctx->polygon_offset_units = units;
	ctx->polygon_offset_factor = factor;
}

void grate_3d_ctx_set_provoking_vtx_last(struct grate_3d_ctx *ctx, bool last)
{
	ctx->provoking_vtx_last = last;
}

int grate_3d_ctx_bind_texture(struct grate_3d_ctx *ctx,
			      unsigned location,
			      struct grate_texture *tex)
{
	if (location >= 16) {
		grate_error("Invalid location %u\n", location);
		return -1;
	}

	ctx->textures[location] = tex;

	return 0;
}

void grate_3d_ctx_set_depth_func(struct grate_3d_ctx *ctx,
				 enum grate_3d_ctx_depth_function func)
{
	switch (func) {
	case GRATE_3D_CTX_DEPTH_FUNC_NEVER:
		ctx->depth_func = TGR3D_DEPTH_FUNC_NEVER;
		break;

	case GRATE_3D_CTX_DEPTH_FUNC_LESS:
		ctx->depth_func = TGR3D_DEPTH_FUNC_LESS;
		break;

	case GRATE_3D_CTX_DEPTH_FUNC_EQUAL:
		ctx->depth_func = TGR3D_DEPTH_FUNC_EQUAL;
		break;

	case GRATE_3D_CTX_DEPTH_FUNC_LEQUAL:
		ctx->depth_func = TGR3D_DEPTH_FUNC_LEQUAL;
		break;

	case GRATE_3D_CTX_DEPTH_FUNC_GREATER:
		ctx->depth_func = TGR3D_DEPTH_FUNC_GREATER;
		break;

	case GRATE_3D_CTX_DEPTH_FUNC_NOTEQUAL:
		ctx->depth_func = TGR3D_DEPTH_FUNC_NOTEQUAL;
		break;

	case GRATE_3D_CTX_DEPTH_FUNC_GEQUAL:
		ctx->depth_func = TGR3D_DEPTH_FUNC_GEQUAL;
		break;

	case GRATE_3D_CTX_DEPTH_FUNC_ALWAYS:
		ctx->depth_func = TGR3D_DEPTH_FUNC_ALWAYS;
		break;
	default:
		grate_error("Invalid depth function %u\n", func);
	}
}

void grate_3d_ctx_perform_depth_test(struct grate_3d_ctx *ctx, bool enable)
{
	ctx->depth_test = enable;
}

void grate_3d_ctx_perform_depth_write(struct grate_3d_ctx *ctx, bool enable)
{
	ctx->depth_write = enable;
}

int grate_3d_ctx_bind_depth_buffer(struct grate_3d_ctx *ctx,
				   struct host1x_pixelbuffer *pixbuf)
{
	ctx->render_targets[0].pixbuf = NULL;

	switch (pixbuf->format) {
	case PIX_BUF_FMT_D16_LINEAR:
	case PIX_BUF_FMT_D16_NONLINEAR:
		break;
	default:
		grate_error("Invalid format %u\n", pixbuf->format);
		return -1;
	}

	switch (pixbuf->layout) {
	case PIX_BUF_LAYOUT_LINEAR:
	case PIX_BUF_LAYOUT_TILED_16x16:
		break;
	default:
		grate_error("Invalid layout %u\n", pixbuf->layout);
		return -1;
	}

	ctx->render_targets[0].pixbuf = pixbuf;

	return 0;
}

void grate_3d_ctx_perform_stencil_test(struct grate_3d_ctx *ctx, bool enable)
{
	ctx->stencil_test = enable;
}

void grate_3d_ctx_set_stencil_func(struct grate_3d_ctx *ctx,
				   enum grate_3d_ctx_stencil_test_face face,
				   enum grate_3d_ctx_stencil_test_func func,
				   unsigned ref, unsigned mask)
{
	unsigned stencil_func;

	if (mask > 255)
		grate_error("Invalid mask %u, max 255\n", mask);

	if (ref > 255)
		grate_error("Invalid ref %u, max 255\n", ref);

	switch (func) {
	case GRATE_3D_CTX_STENCIL_TEST_NEVER:
		stencil_func = TGR3D_STENCIL_FUNC_NEVER;
		break;

	case GRATE_3D_CTX_STENCIL_TEST_ALWAYS:
		stencil_func = TGR3D_STENCIL_FUNC_ALWAYS;
		break;

	case GRATE_3D_CTX_STENCIL_TEST_EQUAL:
		stencil_func = TGR3D_STENCIL_FUNC_EQUAL;
		break;

	case GRATE_3D_CTX_STENCIL_TEST_NOTEQUAL:
		stencil_func = TGR3D_STENCIL_FUNC_NOTEQUAL;
		break;

	case GRATE_3D_CTX_STENCIL_TEST_LEQUAL:
		stencil_func = TGR3D_STENCIL_FUNC_LESS_EQUAL;
		break;

	case GRATE_3D_CTX_STENCIL_TEST_GEQUAL:
		stencil_func = TGR3D_STENCIL_FUNC_GREATER_EQUAL;
		break;

	case GRATE_3D_CTX_STENCIL_TEST_GREATER:
		stencil_func = TGR3D_STENCIL_FUNC_GREATER;
		break;

	case GRATE_3D_CTX_STENCIL_TEST_LESS:
		stencil_func = TGR3D_STENCIL_FUNC_LESS;
		break;
	default:
		grate_error("Invalid stencil function %u\n", func);
		return;
	}

	if (face == GRATE_3D_CTX_STENCIL_TEST_FRONT ||
	    face == GRATE_3D_CTX_STENCIL_TEST_BOTH) {
		ctx->stencil_func_front = stencil_func;
		ctx->stencil_mask_front = mask;
		ctx->stencil_ref_front  = ref;
	}

	if (face == GRATE_3D_CTX_STENCIL_TEST_BACK ||
	    face == GRATE_3D_CTX_STENCIL_TEST_BOTH) {
		ctx->stencil_func_back = stencil_func;
		ctx->stencil_mask_back = mask;
		ctx->stencil_ref_back  = ref;
	}
}

static int get_stencil_op(enum grate_3d_ctx_stencil_operation op)
{
	switch (op) {
	case GRATE_3D_CTX_STENCIL_OP_ZERO:
		return TGR3D_STENCIL_OP_ZERO;

	case GRATE_3D_CTX_STENCIL_OP_KEEP:
		return TGR3D_STENCIL_OP_KEEP;

	case GRATE_3D_CTX_STENCIL_OP_INVERT:
		return TGR3D_STENCIL_OP_INVERT;

	case GRATE_3D_CTX_STENCIL_OP_REPLACE:
		return TGR3D_STENCIL_OP_REPLACE;

	case GRATE_3D_CTX_STENCIL_OP_INCR:
		return TGR3D_STENCIL_OP_INCR;

	case GRATE_3D_CTX_STENCIL_OP_DECR:
		return TGR3D_STENCIL_OP_DECR;

	case GRATE_3D_CTX_STENCIL_OP_INCR_WRAP:
		return TGR3D_STENCIL_OP_INCR_WRAP;

	case GRATE_3D_CTX_STENCIL_OP_DECR_WRAP:
		return TGR3D_STENCIL_OP_DECR_WRAP;
	}

	return -1;
}

void grate_3d_ctx_set_stencil_ops(struct grate_3d_ctx *ctx,
				enum grate_3d_ctx_stencil_test_face face,
				enum grate_3d_ctx_stencil_operation fail_op,
				enum grate_3d_ctx_stencil_operation zfail_op,
				enum grate_3d_ctx_stencil_operation zpass_op)
{
	int stencil_fail_op = get_stencil_op(fail_op);
	int stencil_zfail_op = get_stencil_op(zfail_op);
	int stencil_zpass_op = get_stencil_op(zpass_op);

	if (stencil_fail_op < 0) {
		grate_error("Invalid stencil fail operation %u\n", fail_op);
		return;
	}

	if (stencil_zfail_op < 0) {
		grate_error("Invalid stencil depth fail operation %u\n",
			    zfail_op);
		return;
	}

	if (stencil_zpass_op < 0) {
		grate_error("Invalid stencil depth pass operation %u\n",
			    zpass_op);
		return;
	}

	if (face == GRATE_3D_CTX_STENCIL_TEST_FRONT ||
	    face == GRATE_3D_CTX_STENCIL_TEST_BOTH) {
		ctx->stencil_fail_op_front  = stencil_fail_op;
		ctx->stencil_zfail_op_front = stencil_zfail_op;
		ctx->stencil_zpass_op_front = stencil_zpass_op;
	}

	if (face == GRATE_3D_CTX_STENCIL_TEST_BACK ||
	    face == GRATE_3D_CTX_STENCIL_TEST_BOTH) {
		ctx->stencil_fail_op_back  = stencil_fail_op;
		ctx->stencil_zfail_op_back = stencil_zfail_op;
		ctx->stencil_zpass_op_back = stencil_zpass_op;
	}
}

int grate_3d_ctx_bind_stencil_buffer(struct grate_3d_ctx *ctx,
				     struct host1x_pixelbuffer *pixbuf)
{
	ctx->render_targets[2].pixbuf = NULL;

	switch (pixbuf->format) {
	case PIX_BUF_FMT_S8:
		break;
	default:
		grate_error("Invalid format %u\n", pixbuf->format);
		return -1;
	}

	switch (pixbuf->layout) {
	case PIX_BUF_LAYOUT_LINEAR:
	case PIX_BUF_LAYOUT_TILED_16x16:
		break;
	default:
		grate_error("Invalid layout %u\n", pixbuf->layout);
		return -1;
	}

	ctx->render_targets[2].pixbuf = pixbuf;

	return 0;
}
