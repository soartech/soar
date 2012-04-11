#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <asm/types.h>
#include <assert.h>
#include <linux/videodev2.h>
#include <errno.h>
#include <stdint.h>

#define IMAGE_SOURCE_UTILS
#include "image_source.h"

#define IMPL_TYPE 0x56344c32
#define NUM_BUFFERS 4

struct buffer {
        void *                  start;
        size_t                  length;
};

typedef struct impl_v4l2 impl_v4l2_t;
struct impl_v4l2
{
    char                    *path;

    int fd;

    int                     nformats;
    image_source_format_t  **formats;
    // formats: we put a pointer to struct v4l2_format in the priv field

    int                     current_format_idx;

    struct buffer           buffers[NUM_BUFFERS];
};

/** Creates a fourcc string **/
static char* int2fourcc(uint32_t fcc)
{
    char *result = malloc(5);
    result[0] = fcc & 0xff;
    result[1] = (fcc >> 8) & 0xff;
    result[2] = (fcc >> 16) & 0xff;
    result[3] = (fcc >> 24) & 0xff;
    result[4] = 0;

    return result;
}

static int num_formats(image_source_t *isrc)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    return impl->nformats;
}

static image_source_format_t *get_format(image_source_t *isrc, int idx)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    assert(idx>=0 && idx < impl->nformats);
    return impl->formats[idx];
}

static int get_current_format(image_source_t *isrc)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    return impl->current_format_idx;
}

static int set_format(image_source_t *isrc, int idx)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    // work around broken cameras that don't have any driver-specified formats.
    if (impl->nformats == 0)
        return 0;

    assert(idx>=0 && idx < impl->nformats);

    // XXX Hack. Some cameras don't seem to like changing formats
    // after they've been opened and started. So close and re-open it!
    close(impl->fd);
    impl->fd = open(impl->path, O_RDWR, 0); // | O_NONBLOCK, 0);

    if (impl->fd < 0) {
        printf("reopening device failed\n");
        return -1;
    }

    if (ioctl (impl->fd, VIDIOC_S_FMT, impl->formats[idx]->priv) < 0) {
        printf("set format failed\n");
        return -1;
    }

    impl->current_format_idx = idx;

    return 0;
}

static int set_named_format(image_source_t *isrc, const char *desired_format)
{
    printf("***This feature (set_named_format) is not currently supported by v4l2 cameras in jcam.  Exiting.");
    exit(-1);

    return -1;
}

static int num_features(image_source_t *isrc)
{
    return 0;
}

static const char* get_feature_name(image_source_t *isrc, int idx)
{
    return NULL;
}

static double get_feature_min(image_source_t *isrc, int idx)
{
    return 0;
}

static double get_feature_max(image_source_t *isrc, int idx)
{
    return 0;
}

static double get_feature_value(image_source_t *isrc, int idx)
{
    return 0;
}

static int set_feature_value(image_source_t *isrc, int idx, double v)
{
    return 0;
}

static int stop(image_source_t *isrc)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

    if (-1 == ioctl (impl->fd, VIDIOC_STREAMOFF, &type)) {
        perror("VIDIOC_STREAMOFF");
        return -1;
    }

    for (int i = 0; i < NUM_BUFFERS; i++) {
        if (-1 == munmap (impl->buffers[i].start, impl->buffers[i].length)) {
            perror("munmap");
            return -1;
        }
    }

    return 0;
}

static int start(image_source_t *isrc)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    struct v4l2_requestbuffers req;
    memset (&req, 0, sizeof (req));
    req.count               = NUM_BUFFERS;
    req.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    req.memory              = V4L2_MEMORY_MMAP;

    if (ioctl(impl->fd, VIDIOC_REQBUFS, &req) < 0) {
        perror("v4l2: VIDIOC_REQBUFS");
        return -1;
    }

    // capture.c fails here (insufficient buffer memory on device)
    assert(req.count == NUM_BUFFERS);

    for (int i = 0; i < NUM_BUFFERS; i++) {
        struct v4l2_buffer buf;
        memset(&buf, 0, sizeof(struct v4l2_buffer));

        buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        buf.memory      = V4L2_MEMORY_MMAP;
        buf.index       = i;

        if (-1 == ioctl (impl->fd, VIDIOC_QUERYBUF, &buf)) {
            perror("VIDIOC_QUERYBUF");
            return -1;
        }

        impl->buffers[i].length = buf.length;
        impl->buffers[i].start = mmap (NULL /* start anywhere */,
                                       buf.length,
                                       PROT_READ | PROT_WRITE /* required */,
                                       MAP_SHARED /* recommended */,
                                       impl->fd, buf.m.offset);

        if (MAP_FAILED == impl->buffers[i].start) {
            perror("mmap");
            return -1;
        }

        if (-1 == ioctl (impl->fd, VIDIOC_QBUF, &buf)) {
            perror ("VIDIOC_QBUF");
            break;
        }
    }

    enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if (ioctl(impl->fd, VIDIOC_STREAMON, &type) < 0) {
        perror("VIDIOC_STREAMON");
        return -1;
    }

    return 0;
}

static int get_frame(image_source_t *isrc, void **imbuf, int *buflen)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    fd_set fds;

    FD_ZERO (&fds);
    FD_SET (impl->fd, &fds);

    int r = select(impl->fd + 1, &fds, NULL, NULL, NULL);

    if (-1 == r) {
        perror("select");
        return -2;
    }

    if (0 == r) {
        // timeout
        return -1;
    }

    // Read the frame
    struct v4l2_buffer buf;
    memset(&buf, 0, sizeof(struct v4l2_buffer));

    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;

    if (-1 == ioctl (impl->fd, VIDIOC_DQBUF, &buf)) {
        switch (errno) {
        case EAGAIN:
            return 0;

        case EIO:
            // Could ignore EIO, see spec.
            // fall through

        default:
            perror("VIDIOC_DQBUF");
            return -1;
        }
    }

    *buflen = buf.bytesused;
    *imbuf = impl->buffers[buf.index].start;

    return 0;
}

static int release_frame(image_source_t *isrc, void *imbuf)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    struct v4l2_buffer buf;
    memset(&buf, 0, sizeof(struct v4l2_buffer));

    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;
    buf.index = -1;

    int found = 0;
    for (int i = 0; i < NUM_BUFFERS; i++) {
        if (impl->buffers[i].start == imbuf) {
            buf.index = i;
            found = 1;
            break;
        }
    }

    if (!found) {
        printf("release frame called with bogus imbuf: %p\n", imbuf);
        return -1;
    }

    // done with this frame, re-enqueue it
    if (-1 == ioctl (impl->fd, VIDIOC_QBUF, &buf)) {
        perror("VIDIOC_QBUF");
        return -1;
    }

    return 0;
}

static int add_format(image_source_t *isrc, struct v4l2_fmtdesc *rfmt, int width, int height)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    // Try the format with the driver. We might get a slightly
    // different format out than we requested.
    struct v4l2_format *vfmt = calloc(1, sizeof(struct v4l2_format));
    memset(vfmt, 0, sizeof(struct v4l2_format));
    vfmt->type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    vfmt->fmt.pix.width = width;
    vfmt->fmt.pix.height = height;
    vfmt->fmt.pix.pixelformat = rfmt->pixelformat;
    vfmt->fmt.pix.field = V4L2_FIELD_ANY;
    vfmt->fmt.pix.bytesperline = 0;

    if (ioctl(impl->fd, VIDIOC_TRY_FMT, vfmt) < 0) {
        perror("ioctl");
        fprintf(stderr, "Error: VIDIOC_TRY_FMT failed\n");
        return -1;
    }

    if (vfmt->fmt.pix.height*vfmt->fmt.pix.bytesperline > vfmt->fmt.pix.sizeimage) {
        fprintf(stderr, "Error: v4l2 driver is reporting bogus row stride. Assuming 8bpp.\n");
        int bpp = 8; // XXX, compute correct bpp via pixel format.
        vfmt->fmt.pix.bytesperline =  vfmt->fmt.pix.width * bpp / 8;
    }

    impl->formats = realloc(impl->formats, (impl->nformats+1) * sizeof(image_source_format_t*));

    image_source_format_t *ifmt = calloc(1, sizeof(image_source_format_t));
    ifmt->width = vfmt->fmt.pix.width;
    ifmt->height = vfmt->fmt.pix.height;
    ifmt->format = int2fourcc((uint32_t) rfmt->pixelformat);

    impl->formats[impl->nformats] = ifmt;

    impl->formats[impl->nformats]->priv = vfmt;

    impl->nformats++;

    return 0;
}

int my_close(image_source_t *isrc)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_v4l2_t *impl = (impl_v4l2_t*) isrc->impl;

    return close(impl->fd);
}

image_source_t *image_source_v4l2_open(const char *path)
{
    image_source_t *isrc = calloc(1, sizeof(image_source_t));
    impl_v4l2_t *impl = calloc(1, sizeof(impl_v4l2_t));

    isrc->impl_type = IMPL_TYPE;
    isrc->impl = impl;
    isrc->num_formats = num_formats;
    isrc->get_format = get_format;
    isrc->get_current_format = get_current_format;
    isrc->set_format = set_format;
    isrc->set_named_format = set_named_format;
    isrc->num_features = num_features;
    isrc->get_feature_name = get_feature_name;
    isrc->get_feature_min = get_feature_min;
    isrc->get_feature_max = get_feature_max;
    isrc->get_feature_value = get_feature_value;
    isrc->set_feature_value = set_feature_value;
    isrc->start = start;
    isrc->get_frame = get_frame;
    isrc->release_frame = release_frame;
    isrc->stop = stop;
    isrc->close = my_close;

    impl->path = strdup(path);

    impl->fd = open(path, O_RDWR, 0); // | O_NONBLOCK, 0);
    if (impl->fd < 0)
        goto fail;

    struct v4l2_capability cap;
    if (-1 == ioctl (impl->fd, VIDIOC_QUERYCAP, &cap)) {
        if (EINVAL == errno) {
            fprintf (stderr, "%s is not a V4L2 device\n", path);
            exit (EXIT_FAILURE);
        } else {
            perror("VIDIOC_QUERYCAP");
            goto fail;
        }
    }

    if (0) {
        struct v4l2_cropcap cropcap;
        struct v4l2_crop crop;
        memset(&crop, 0, sizeof(struct v4l2_crop));
        memset(&cropcap, 0, sizeof(struct v4l2_cropcap));

        cropcap.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        if (0 == ioctl (impl->fd, VIDIOC_CROPCAP, &cropcap)) {
            crop.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
            crop.c = cropcap.defrect; /* reset to default */

            if (-1 == ioctl (impl->fd, VIDIOC_S_CROP, &crop)) {
                switch (errno) {
                case EINVAL:
                    // Cropping not supported.
                    break;
                default:
                    // Errors ignored.
                    break;
                }
            }
        } else {
            // Errors ignored.
        }
    }

    struct v4l2_fmtdesc fmt;
    memset (&fmt, 0, sizeof (fmt));
    fmt.index = 0;
    fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    int oldfindex = fmt.index;

    while (ioctl(impl->fd, VIDIOC_ENUM_FMT, &fmt) == 0) {
        // With some Logitech Quickcams the only way we know there are
        // no more formats is that the index gets modified under us.
        if (fmt.index != oldfindex && oldfindex != 0)
            break;

//        if (f.pixelformat == 0x32435750) { // 'PWC2'
//            cam_pixelformat = CAM_PIXEL_FORMAT_I420;
//        }

        struct v4l2_frmsizeenum framesize;
        memset (&framesize, 0, sizeof(framesize));
        framesize.index = 0;
        framesize.pixel_format = fmt.pixelformat;

        int got_frame_size = 0;

        while (ioctl(impl->fd, VIDIOC_ENUM_FRAMESIZES, &framesize) == 0) {
            int width, height;

            got_frame_size = 1;

            if (framesize.type == V4L2_FRMSIZE_TYPE_STEPWISE || framesize.type == V4L2_FRMSIZE_TYPE_CONTINUOUS) {
                width = framesize.stepwise.max_width;
                height = framesize.stepwise.max_height;
            } else {
                width = framesize.discrete.width;
                height = framesize.discrete.height;
            }

            add_format(isrc, &fmt, width, height);

            framesize.index++;
        }

        if (!got_frame_size) {
            // just add a big format. The try_fmt will correct the resolution.
            add_format(isrc, &fmt, 4096, 4096);
        }

        fmt.index++;
        oldfindex = fmt.index;
    }

    if (impl->nformats == 0) {
        printf("image_source_v4l2: camera has no image formats.\n");
    }

    int res = set_format(isrc, 0);
    if (res < 0) {
        printf("Unable to set format 0\n");
    }

    return isrc;

fail:
    free(isrc);
    free(impl);

    return NULL;
}

char** image_source_enumerate_v4l2(char **urls)
{
    for (int i = 0; i < 16; i++) {
        char buf[1024];
        sprintf(buf, "/dev/video%d", i);
        struct stat s;
        int res = stat(buf,  &s);
        if (res)
            continue;
        sprintf(buf, "v4l2:///dev/video%d", i);
        urls = string_array_add(urls, buf);
    }

    return urls;
}
