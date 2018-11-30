/*
 * Copyright (c) 2012, 2013 Erik Faye-Lund
 * Copyright (c) 2013 Thierry Reding
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

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <sys/ioctl.h>
#include <sys/mman.h>

#include <libdrm/drm_fourcc.h>
#include <xf86drm.h>
#include <xf86drmMode.h>

#include "host1x.h"
#include "tegra_drm.h"
#include "x11-display.h"

/*
 * for simplicity we allocate one single-common buffer and then assume
 * that job won't ever overflow the hardcoded offset.
 */
#define DRM_JOB_BOS_OFFSET	(0x3000 / sizeof(uint32_t))

struct drm;

struct drm_bo {
	struct host1x_bo base;
	struct drm *drm;
};

struct drm_v2_pushbuf {
	struct host1x_pushbuf base;
	uint32_t cmds[4096];
};

static inline struct drm_bo *to_drm_bo(struct host1x_bo *bo)
{
	return container_of(bo, struct drm_bo, base);
}

struct drm_display {
	struct host1x_display base;
	struct drm *drm;
	drmModeModeInfo mode;
	uint32_t connector;
	unsigned int pipe;
	uint32_t plane;
	uint32_t crtc;
	bool reflected;
};

static inline struct drm_display *to_drm_display(struct host1x_display *display)
{
	return container_of(display, struct drm_display, base);
}

struct drm_overlay {
	struct host1x_overlay base;
	struct drm_display *display;
	uint32_t plane;

	unsigned int x;
	unsigned int y;
	unsigned int width;
	unsigned int height;
	uint32_t format;
	bool reflected;
};

static inline struct drm_overlay *to_drm_overlay(struct host1x_overlay *overlay)
{
	return container_of(overlay, struct drm_overlay, base);
}

struct drm_gr2d {
	struct host1x_gr2d base;
};

struct drm_gr3d {
	struct host1x_gr3d base;
};

struct drm {
	struct host1x base;
	struct host1x_client client;

	struct drm_display *display;
	struct drm_gr2d *gr2d;
	struct drm_gr3d *gr3d;

	int fd;

	struct host1x_syncpt syncpt_stub;
	int syncobj_handle;
};

static struct drm *to_drm(struct host1x *host1x)
{
	return container_of(host1x, struct drm, base);
}

static inline struct drm *to_drm_client(struct host1x_client *client)
{
	return container_of(client, struct drm, client);
}

static int drm_plane_type(struct drm *drm, drmModePlane *p)
{
#ifdef DRM_CLIENT_CAP_UNIVERSAL_PLANES
	drmModeObjectPropertiesPtr props;
	drmModePropertyPtr prop;
	int type = -EINVAL;
	unsigned int i;

	props = drmModeObjectGetProperties(drm->fd, p->plane_id,
					   DRM_MODE_OBJECT_PLANE);
	if (!props)
		return -ENODEV;

	for (i = 0; i < props->count_props && type == -EINVAL; i++) {
		prop = drmModeGetProperty(drm->fd, props->props[i]);
		if (prop) {
			if (!strcmp(prop->name, "type"))
				type = props->prop_values[i];

			drmModeFreeProperty(prop);
		}
	}

	drmModeFreeObjectProperties(props);

	return type;
#else
	return 0;
#endif
}

static int drm_display_find_plane(struct drm_display *display,
				  uint32_t *plane, uint32_t type)
{
	struct drm *drm = display->drm;
	drmModePlaneRes *res;
	uint32_t id = 0, i;

	res = drmModeGetPlaneResources(drm->fd);
	if (!res)
		return -errno;

	for (i = 0; i < res->count_planes && !id; i++) {
		drmModePlane *p = drmModeGetPlane(drm->fd, res->planes[i]);
		if (!p) {
			continue;
		}

		if ((p->possible_crtcs & (1u << display->pipe)) &&
		    (drm_plane_type(drm, p) == type))
			id = p->plane_id;

		drmModeFreePlane(p);
	}

	drmModeFreePlaneResources(res);

	if (!id)
		return -ENODEV;

	if (plane)
		*plane = id;

	return 0;
}

static int drm_overlay_reflect_y(struct drm *drm, uint32_t plane_id,
				 bool reflect)
{
#ifdef DRM_MODE_REFLECT_Y
	drmModeObjectPropertiesPtr properties;
	drmModePropertyPtr property;
	drmModeAtomicReqPtr req;
	unsigned int reflect_flag;
	unsigned int i;
	int ret;

	req = drmModeAtomicAlloc();
	if (!req) {
		host1x_error("drmModeAtomicAlloc() failed\n");
		return -ENOMEM;
	}

	properties = drmModeObjectGetProperties(drm->fd, plane_id,
						DRM_MODE_OBJECT_PLANE);
	if (!properties) {
		host1x_error("drmModeObjectGetProperties() failed\n");
		ret = -EINVAL;
		goto atomic_free;
	}

	for (i = 0, ret = -100; i < properties->count_props; i++) {
		property = drmModeGetProperty(drm->fd, properties->props[i]);
		if (!property)
			continue;

		if (reflect)
			reflect_flag = DRM_MODE_REFLECT_Y;
		else
			reflect_flag = 0;

		if (!strcmp(property->name, "rotation")) {
			ret = drmModeAtomicAddProperty(req, plane_id,
						       property->prop_id,
						       DRM_MODE_ROTATE_0 |
						       reflect_flag);
			if (ret < 0)
				host1x_error("drmModeAtomicAddProperty() failed: %d\n",
					     ret);
		}

		free(property);
	}

	free(properties);

	if (ret >= 0) {
		ret = drmModeAtomicCommit(drm->fd, req, 0, NULL);
		if (ret < 0)
			host1x_error("drmModeAtomicCommit() failed: %d\n", ret);
	}

atomic_free:
	drmModeAtomicFree(req);

	if (ret == -100)
		host1x_error("couldn't get DRM plane \"rotation\" property\n");

	return ret;
#else
	return 0;
#endif
}

static int drm_overlay_close(struct host1x_overlay *overlay)
{
	struct drm_overlay *plane = to_drm_overlay(overlay);
	struct drm_display *display = plane->display;
	struct drm *drm = display->drm;

	drmModeSetPlane(drm->fd, plane->plane, display->crtc, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0);

	free(plane);
	return 0;
}

static int drm_overlay_set(struct host1x_overlay *overlay,
			   struct host1x_framebuffer *fb, unsigned int x,
			   unsigned int y, unsigned int width,
			   unsigned int height, bool vsync, bool reflect_y)
{
	struct drm_overlay *plane = to_drm_overlay(overlay);
	struct drm_display *display = plane->display;
	struct drm *drm = display->drm;
	int err;

	if (plane->reflected != reflect_y) {
		drm_overlay_reflect_y(drm, plane->plane, reflect_y);
		plane->reflected = reflect_y;
	}

	if (vsync) {
		drmVBlank vblank = {
			.request = {
				.type = DRM_VBLANK_RELATIVE,
				.sequence = 1,
			},
		};

		vblank.request.type |=
				display->pipe << DRM_VBLANK_HIGH_CRTC_SHIFT;

		err = drmWaitVBlank(drm->fd, &vblank);
		if (err < 0) {
			host1x_error("drmWaitVBlank() failed: %m\n");
			return -errno;
		}
	}

	err = drmModeSetPlane(drm->fd, plane->plane, display->crtc,
			      fb->handle, 0, x, y, width, height, 0, 0,
			      fb->pixbuf->width << 16,
			      fb->pixbuf->height << 16);
	if (err < 0)
		return -errno;

	return 0;
}

static int drm_overlay_create(struct host1x_display *display,
			      struct host1x_overlay **overlayp)
{
	struct drm_display *drm = to_drm_display(display);
	struct drm_overlay *overlay;
	uint32_t plane = 0;
	int err;

#ifndef DRM_PLANE_TYPE_OVERLAY
#define DRM_PLANE_TYPE_OVERLAY 0
#endif
	err = drm_display_find_plane(drm, &plane, DRM_PLANE_TYPE_OVERLAY);
	if (err < 0)
		return err;

	overlay = calloc(1, sizeof(*overlay));
	if (!overlay)
		return -ENOMEM;

	overlay->base.close = drm_overlay_close;
	overlay->base.set = drm_overlay_set;

	overlay->display = drm;
	overlay->plane = plane;

	*overlayp = &overlay->base;

	return 0;
}

static void drm_display_on_page_flip(int fd, unsigned int frame,
				     unsigned int sec, unsigned int usec,
				     void *data)
{
}

static void drm_display_on_vblank(int fd, unsigned int frame,
				  unsigned int sec, unsigned int usec,
				  void *data)
{
}

static int drm_display_set(struct host1x_display *display,
			   struct host1x_framebuffer *fb,
			   bool vsync, bool reflect_y)
{
	struct drm_display *drm = to_drm_display(display);
	int err;

	if (drm->reflected != reflect_y) {
		drm_overlay_reflect_y(drm->drm, drm->plane, reflect_y);
		drm->reflected = reflect_y;
	}

	if (vsync) {
		struct timeval timeout;
		fd_set fds;

		err = drmModePageFlip(drm->drm->fd, drm->crtc, fb->handle,
				      DRM_MODE_PAGE_FLIP_EVENT, drm);
		if (err < 0) {
			err = drmModeSetCrtc(drm->drm->fd, drm->crtc,
					     fb->handle, 0, 0, &drm->connector,
					     1, &drm->mode);
		}

		if (err < 0) {
			host1x_error("drmModePageFlip() failed: %m\n");
			return -errno;
		}

		memset(&timeout, 0, sizeof(timeout));
		timeout.tv_sec = 1;
		timeout.tv_usec = 0;

		FD_ZERO(&fds);
		FD_SET((unsigned)drm->drm->fd, &fds);

		err = select(drm->drm->fd + 1, &fds, NULL, NULL, &timeout);
		if (err <= 0) {
		}

		if (FD_ISSET((unsigned)drm->drm->fd, &fds)) {
			drmEventContext context;

			memset(&context, 0, sizeof(context));
			context.version = DRM_EVENT_CONTEXT_VERSION;
			context.page_flip_handler = drm_display_on_page_flip;
			context.vblank_handler = drm_display_on_vblank;

			drmHandleEvent(drm->drm->fd, &context);
		}
	} else {
		err = drmModeSetCrtc(drm->drm->fd, drm->crtc, fb->handle, 0,
				     0, &drm->connector, 1, &drm->mode);
		if (err < 0)
			return -errno;
	}

	return 0;
}

static int drm_display_setup(struct drm_display *display)
{
	struct drm *drm = display->drm;
	int ret = -ENODEV;
	drmModeRes *res;
	uint32_t i;

	res = drmModeGetResources(drm->fd);
	if (!res)
		return -ENODEV;

	for (i = 0; i < res->count_connectors; i++) {
		drmModeConnector *connector;
		drmModeEncoder *encoder;

		connector = drmModeGetConnector(drm->fd, res->connectors[i]);
		if (!connector)
			continue;

		if (connector->connection != DRM_MODE_CONNECTED) {
			drmModeFreeConnector(connector);
			continue;
		}

		encoder = drmModeGetEncoder(drm->fd, connector->encoder_id);
		if (!encoder) {
			drmModeFreeConnector(connector);
			continue;
		}

		display->connector = res->connectors[i];
		display->mode = connector->modes[0];
		display->crtc = encoder->crtc_id;

		drmModeFreeEncoder(encoder);
		drmModeFreeConnector(connector);
		ret = 0;
		break;
	}

	for (i = 0; i < res->count_crtcs; i++) {
		drmModeCrtc *crtc;

		crtc = drmModeGetCrtc(drm->fd, res->crtcs[i]);
		if (!crtc)
			continue;

		if (crtc->crtc_id == display->crtc) {
			drmModeFreeCrtc(crtc);
			display->pipe = i;
			break;
		}

		drmModeFreeCrtc(crtc);
	}

	drmModeFreeResources(res);

	if (ret == 0)
		ret = drm_display_find_plane(display, &display->plane,
					     DRM_PLANE_TYPE_PRIMARY);

	return ret;
}

static int drm_display_create(struct drm_display **displayp, struct drm *drm)
{
	struct drm_display *display;
	int err;

	display = calloc(1, sizeof(*display));
	if (!display)
		return -ENOMEM;

	display->drm = drm;

	err = drmSetMaster(drm->fd);
	if (err < 0)
		goto try_x11;

#ifdef DRM_CLIENT_CAP_ATOMIC
	err = drmSetClientCap(drm->fd, DRM_CLIENT_CAP_ATOMIC, 1);
	if (err)
		host1x_error("drmSetClientCap(ATOMIC) failed: %d\n", err);
#endif

#ifdef DRM_CLIENT_CAP_UNIVERSAL_PLANES
	err = drmSetClientCap(drm->fd, DRM_CLIENT_CAP_UNIVERSAL_PLANES, 1);
	if (err)
		host1x_error("drmSetClientCap(UNIVERSAL_PLANES) failed: %d\n",
			     err);
#endif

	err = drm_display_setup(display);
	if (err < 0)
		goto try_x11;

	display->base.width = display->mode.hdisplay;
	display->base.height = display->mode.vdisplay;
	display->base.create_overlay = drm_overlay_create;
	display->base.set = drm_display_set;

	*displayp = display;

	return 0;
try_x11:
	err = x11_display_create(&drm->base, &display->base);
	if (err < 0) {
		free(display);
		return err;
	}

	*displayp = display;

	return 0;
}

static int drm_display_close(struct drm_display *display)
{
	struct drm *drm;

	if (!display)
		return -EINVAL;

	drm = display->drm;

	drmDropMaster(drm->fd);
	free(display);

	return 0;
}

static int drm_bo_mmap(struct host1x_bo *bo)
{
	struct drm_bo *drm = to_drm_bo(bo);
	struct drm_tegra_gem_mmap args;
	struct host1x_bo *orig;
	void *ptr;
	int err;

	orig = bo->wrapped ?: bo;

	if (orig->ptr) {
		bo->ptr = orig->ptr;
		return 0;
	}

	memset(&args, 0, sizeof(args));
	args.handle = bo->handle;

	err = ioctl(drm->drm->fd, DRM_IOCTL_TEGRA_GEM_MMAP, &args);
	if (err < 0)
		return -errno;

	ptr = mmap(NULL, orig->size, PROT_READ | PROT_WRITE, MAP_SHARED,
		   drm->drm->fd, (__off_t)args.offset);
	if (ptr == MAP_FAILED)
		return -errno;

	orig->ptr = ptr;
	bo->ptr = ptr;

	return 0;
}

static int drm_bo_invalidate(struct host1x_bo *bo, unsigned long offset,
			     size_t length)
{
	return 0;
}

static int drm_bo_flush(struct host1x_bo *bo, unsigned long offset,
			size_t length)
{
	return 0;
}

static void drm_bo_free(struct host1x_bo *bo)
{
	struct drm_bo *drm_bo = to_drm_bo(bo);
	struct drm_gem_close args;
	int err;

	if (bo->wrapped)
		return free(drm_bo);

	if (bo->ptr)
		munmap(bo->ptr, bo->size);

	memset(&args, 0, sizeof(args));
	args.handle = bo->handle;

	err = ioctl(drm_bo->drm->fd, DRM_IOCTL_GEM_CLOSE, &args);
	if (err < 0)
		host1x_error("failed to delete buffer object: %m\n");

	free(drm_bo);
}

static struct host1x_bo *drm_bo_clone(struct host1x_bo *bo)
{
	struct drm_bo *dbo = to_drm_bo(bo);
	struct drm_bo *clone = malloc(sizeof(*dbo));

	if (!clone)
		return NULL;

	memcpy(clone, dbo, sizeof(*dbo));

	return &clone->base;
}

static int drm_bo_export(struct host1x_bo *bo, uint32_t *handle)
{
	struct drm_bo *dbo = to_drm_bo(bo);
	struct drm_gem_flink args;
	int err;

	memset(&args, 0, sizeof(args));
	args.handle = bo->handle;

	err = drmIoctl(dbo->drm->fd, DRM_IOCTL_GEM_FLINK, &args);
	if (err < 0)
		return -errno;

	*handle = args.name;

	return 0;
}

static struct host1x_bo *drm_bo_create(struct host1x *host1x,
				       struct host1x_bo_priv *priv,
				       size_t size, unsigned long flags)
{
	struct drm_tegra_gem_create args;
	struct drm *drm = to_drm(host1x);
	struct drm_bo *bo;
	int err;

	bo = calloc(1, sizeof(*bo));
	if (!bo)
		return NULL;

	bo->drm = drm;
	bo->base.priv = priv;

	if (size < 0x4000)
		size = 0x4000;

	memset(&args, 0, sizeof(args));
	args.size = size;

	if (flags & HOST1X_BO_CREATE_FLAG_BOTTOM_UP)
		args.flags |= DRM_TEGRA_GEM_CREATE_BOTTOM_UP;

	if (flags & HOST1X_BO_CREATE_FLAG_TILED)
		args.flags |= DRM_TEGRA_GEM_CREATE_TILED;

	if (flags & HOST1X_BO_CREATE_FLAG_GATHER)
		args.flags |= DRM_TEGRA_GEM_CREATE_HOST1X_GATHER;

	err = ioctl(drm->fd, DRM_IOCTL_TEGRA_GEM_CREATE, &args);
	if (err < 0) {
		free(bo);
		return NULL;
	}

	bo->base.handle = args.handle;

	bo->base.priv->mmap = drm_bo_mmap;
	bo->base.priv->invalidate = drm_bo_invalidate;
	bo->base.priv->flush = drm_bo_flush;
	bo->base.priv->free = drm_bo_free;
	bo->base.priv->clone = drm_bo_clone;
	bo->base.priv->export = drm_bo_export;

	return &bo->base;
}

static struct host1x_bo *drm_bo_import(struct host1x *host1x,
				       struct host1x_bo_priv *priv,
				       uint32_t handle)
{
	struct drm_gem_open args;
	struct drm *drm = to_drm(host1x);
	struct drm_bo *bo;
	int err;

	bo = calloc(1, sizeof(*bo));
	if (!bo)
		return NULL;

	bo->drm = drm;
	bo->base.priv = priv;

	memset(&args, 0, sizeof(args));
	args.name = handle;

	err = ioctl(drm->fd, DRM_IOCTL_GEM_OPEN, &args);
	if (err < 0) {
		free(bo);
		return NULL;
	}

	bo->base.handle = args.handle;

	bo->base.priv->mmap = drm_bo_mmap;
	bo->base.priv->invalidate = drm_bo_invalidate;
	bo->base.priv->flush = drm_bo_flush;
	bo->base.priv->free = drm_bo_free;
	bo->base.priv->clone = drm_bo_clone;
	bo->base.priv->export = drm_bo_export;

	return &bo->base;
}

static int drm_framebuffer_init(struct host1x *host1x,
				struct host1x_framebuffer *fb)
{
	uint32_t handles[4], pitches[4], offsets[4], format;
#ifdef DRM_FORMAT_MOD_NVIDIA_TEGRA_TILED
	uint64_t modifiers[4];
#endif
	struct host1x_pixelbuffer *pixbuf = fb->pixbuf;
	struct drm *drm = to_drm(host1x);
	int err = -1;

	/* XXX: support other formats */
	switch (pixbuf->format)
	{
	case PIX_BUF_FMT_RGB565:
		format = DRM_FORMAT_RGB565;
		break;
	case PIX_BUF_FMT_RGBA8888:
		format = DRM_FORMAT_XBGR8888;
		break;
	case PIX_BUF_FMT_BGRA8888:
		format = DRM_FORMAT_XRGB8888;
		break;
	default:
		host1x_error("Unsupported framebuffer format\n");
		return -EINVAL;
	}

	memset(handles, 0, sizeof(handles));
	memset(pitches, 0, sizeof(pitches));
	memset(offsets, 0, sizeof(offsets));

	handles[0] = pixbuf->bo->handle;
	pitches[0] = pixbuf->pitch;
	offsets[0] = pixbuf->bo->offset;

#ifdef DRM_FORMAT_MOD_NVIDIA_TEGRA_TILED
	memset(modifiers, 0, sizeof(modifiers));

	if (pixbuf->layout == PIX_BUF_LAYOUT_TILED_16x16)
		modifiers[0] = DRM_FORMAT_MOD_NVIDIA_TEGRA_TILED;
	else
		modifiers[0] = DRM_FORMAT_MOD_LINEAR;

	err = drmModeAddFB2WithModifiers(drm->fd, pixbuf->width, pixbuf->height,
					 format, handles, pitches, offsets,
					 modifiers, &fb->handle,
					 DRM_MODE_FB_MODIFIERS);
	if (!err)
		return 0;
#endif
	err = drmModeAddFB2(drm->fd, pixbuf->width, pixbuf->height, format,
			    handles, pitches, offsets, &fb->handle, 0);
	if (err < 0)
		return -errno;

	return 0;
}

static int drm_push_reloc(struct host1x_pushbuf *pb,
			  struct host1x_bo *target,
			  unsigned long offset, unsigned long shift)
{
	struct drm_tegra_cmdstream_reloc reloc;
	struct drm_tegra_bo_table_entry *bo_table;
	struct drm_tegra_bo_table_entry entry;
	unsigned int i;

	reloc.bo_index = pb->num_relocs;
	reloc.bo_offset = offset;

	entry.handle = target->handle;
	entry.flags = 0;

	bo_table = (struct drm_tegra_bo_table_entry *)
			(pb->start_ptr + DRM_JOB_BOS_OFFSET);

	for (i = 0; i < pb->num_relocs; i++) {
		if (bo_table[i].handle == target->handle) {
			reloc.bo_index = i;
			break;
		}
	}

	if (i == pb->num_relocs)
		bo_table[pb->num_relocs++] = entry;

	*pb->ptr++ = reloc.u_data;

	pb->length++;

	return 0;
}

static struct host1x_pushbuf * drm_job_append(struct host1x_job *job,
					      struct host1x_bo *bo,
					      unsigned long offset)
{
	struct drm_v2_pushbuf *drm_pb;
	struct host1x_pushbuf *pb;

	if (!bo->ptr)
		return NULL;

	if (job->num_pushbufs)
		return job->pushbufs;

	drm_pb = calloc(1, sizeof(*drm_pb));
	if (!drm_pb)
		return NULL;

	pb = &drm_pb->base;

	job->pushbufs = pb;
	job->num_pushbufs = 1;

	pb->start_ptr = drm_pb->cmds;
	pb->ptr = pb->start_ptr;
	pb->offset = 0;
	pb->bo = NULL;

	pb->push_reloc = drm_push_reloc;
	pb->num_relocs = 0;

	return pb;
}

static int drm_submit(struct host1x_client *client, struct host1x_job *job)
{
	struct drm *drm = to_drm_client(client);
	struct drm_tegra_submit_v2 args;
	int err;

	if (job->num_pushbufs != 1) {
		host1x_error("drm_submit() num_pushbufs %u != 1\n",
			     job->num_pushbufs);
		return -EINVAL;
	}

	memset(&args, 0, sizeof(args));
	args.out_fence			= drm->syncobj_handle;
	args.cmdstream_ptr		= (uintptr_t)job->pushbufs[0].start_ptr;
	args.bo_table_ptr		= (uintptr_t)(job->pushbufs[0].start_ptr +
							DRM_JOB_BOS_OFFSET);
	args.num_cmdstream_words	= job->pushbufs[0].length;
	args.num_bos			= job->pushbufs[0].num_relocs;
	args.pipes			= (1 << DRM_TEGRA_PIPE_ID_2D) |
					  (1 << DRM_TEGRA_PIPE_ID_3D);

	err = drmIoctl(drm->fd, DRM_IOCTL_TEGRA_SUBMIT_V2, &args);
	if (err < 0)
		return -errno;

	return 0;
}

static int drm_flush(struct host1x_client *client, uint32_t *fence)
{
	return 0;
}

static uint64_t gettime_ns(void)
{
	struct timespec current;
	clock_gettime(CLOCK_MONOTONIC, &current);
	return (uint64_t)current.tv_sec * 1000000000ull + current.tv_nsec;
}

static int drm_wait(struct host1x_client *client, uint32_t fence,
		    uint32_t timeout)
{
#ifdef DRM_IOCTL_SYNCOBJ_WAIT
	struct drm *drm = to_drm_client(client);
	struct drm_syncobj_wait args;
	int err;

	memset(&args, 0, sizeof(args));

	args.handles = (uintptr_t) &drm->syncobj_handle;
	args.count_handles = 1;
	args.timeout_nsec = gettime_ns() + timeout * 1000000;
	args.flags = DRM_SYNCOBJ_WAIT_FLAGS_WAIT_FOR_SUBMIT;

	err = drmIoctl(drm->fd, DRM_IOCTL_SYNCOBJ_WAIT, &args);
	if (err < 0)
		return -errno;

	return 0;
#else
	return -1;
#endif
}

static int drm_gr2d_create(struct drm_gr2d **gr2dp, struct drm *drm)
{
	struct drm_gr2d *gr2d;
	int err;

	gr2d = calloc(1, sizeof(*gr2d));
	if (!gr2d)
		return -ENOMEM;

	gr2d->base.client = &drm->client;

	err = host1x_gr2d_init(&drm->base, &gr2d->base);
	if (err < 0) {
		free(gr2d);
		return err;
	}

	*gr2dp = gr2d;

	return 0;
}

static void drm_gr2d_close(struct drm_gr2d *gr2d)
{
	if (gr2d)
		host1x_gr2d_exit(&gr2d->base);

	free(gr2d);
}

static int drm_gr3d_create(struct drm_gr3d **gr3dp, struct drm *drm)
{
	struct drm_gr3d *gr3d;
	int err;

	gr3d = calloc(1, sizeof(*gr3d));
	if (!gr3d)
		return -ENOMEM;

	gr3d->base.client = &drm->client;

	err = host1x_gr3d_init(&drm->base, &gr3d->base);
	if (err < 0) {
		free(gr3d);
		return err;
	}

	*gr3dp = gr3d;

	return 0;
}

static void drm_gr3d_close(struct drm_gr3d *gr3d)
{
	if (gr3d)
		host1x_gr3d_exit(&gr3d->base);

	free(gr3d);
}

static int drm_create_syncobj(struct drm *drm)
{
#ifdef DRM_IOCTL_SYNCOBJ_CREATE
	struct drm_syncobj_create args;
	int err;

	memset(&args, 0, sizeof(args));

	err = drmIoctl(drm->fd, DRM_IOCTL_SYNCOBJ_CREATE, &args);
	if (err < 0)
		return -errno;

	return args.handle;
#else
	return -1;
#endif
}

static int drm_destroy_syncobj(struct drm *drm, int handle)
{
#ifdef DRM_IOCTL_SYNCOBJ_DESTROY
	struct drm_syncobj_destroy args;
	int err;

	memset(&args, 0, sizeof(args));
	args.handle = handle;

	err = drmIoctl(drm->fd, DRM_IOCTL_SYNCOBJ_DESTROY, &args);
	if (err < 0)
		return -errno;

	return 0;
#else
	return -1;
#endif
}

static void drm_close(struct host1x *host1x)
{
	struct drm *drm = to_drm(host1x);

	drm_gr3d_close(drm->gr3d);
	drm_gr2d_close(drm->gr2d);
	drm_display_close(drm->display);
	drm_destroy_syncobj(drm, drm->syncobj_handle);

	close(drm->fd);
	free(drm);
}

struct host1x *host1x_drm_open_v2(int fd)
{
	struct drm *drm;
	bool close_fd;
	int err;

	if (fd < 0) {
		fd = open("/dev/dri/card0", O_RDWR);
		if (fd < 0)
			return NULL;

		close_fd = true;
	}

	drm = calloc(1, sizeof(*drm));
	if (!drm) {
		if (close_fd)
			close(fd);
		return NULL;
	}

	drm->fd = fd;

	drm->base.bo_create = drm_bo_create;
	drm->base.framebuffer_init = drm_framebuffer_init;
	drm->base.close = drm_close;
	drm->base.bo_import = drm_bo_import;

	drm->client.job_append = drm_job_append;
	drm->client.num_syncpts = 1;
	drm->client.syncpts = &drm->syncpt_stub;
	drm->client.submit = drm_submit;
	drm->client.flush = drm_flush;
	drm->client.wait = drm_wait;

	err = drm_create_syncobj(drm);
	if (err < 0) {
		host1x_error("drm_create_syncobj() failed: %d\n", err);
		free(drm);
		if (close_fd)
			close(fd);
		return NULL;
	}

	drm->syncobj_handle = err;

	err = drm_gr2d_create(&drm->gr2d, drm);
	if (err < 0) {
		host1x_error("drm_gr2d_create() failed: %d\n", err);
		drm_destroy_syncobj(drm, drm->syncobj_handle);
		free(drm);
		if (close_fd)
			close(fd);
		return NULL;
	}

	err = drm_gr3d_create(&drm->gr3d, drm);
	if (err < 0) {
		host1x_error("drm_gr3d_create() failed: %d\n", err);
		drm_gr2d_close(drm->gr2d);
		drm_destroy_syncobj(drm, drm->syncobj_handle);
		free(drm);
		if (close_fd)
			close(fd);
		return NULL;
	}

	drm->base.gr2d = &drm->gr2d->base;
	drm->base.gr3d = &drm->gr3d->base;
	drm->base.drm_v2 = true;

	return &drm->base;
}

void host1x_drm_display_init_v2(struct host1x *host1x)
{
	struct drm *drm = to_drm(host1x);
	int err;

	err = drm_display_create(&drm->display, drm);
	if (err < 0) {
		host1x_error("drm_display_create() failed: %d\n", err);
	} else {
		drm->base.display = &drm->display->base;
	}
}
