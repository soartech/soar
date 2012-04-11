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
#include <inttypes.h>
#include <errno.h>
#include <stdint.h>
#include <sys/types.h>

#include <dc1394/control.h>
#include <dc1394/vendor/avt.h>

#define IMAGE_SOURCE_UTILS
#include "image_source.h"

// XXX: We only attempt to support dc1394 Format 7 cameras

#define IMPL_TYPE 0x44431394

typedef struct impl_dc1394 impl_dc1394_t;
struct impl_dc1394
{
    int                   fd;

    dc1394_t              *dc1394;
    dc1394camera_t        *cam;

    int                   nformats;
    image_source_format_t **formats;
    int                   current_format_idx;

    int                   num_buffers;

    dc1394video_frame_t   *current_frame;

    dc1394featureset_t    features;

    uint32_t              packet_size;

    uint32_t              started;
};

struct format_priv
{
    dc1394video_mode_t dc1394_mode;
    int format7_mode_idx;
    int color_coding_idx;
};

static int strposat(const char *haystack, const char *needle, int haystackpos)
{
    int idx = haystackpos;
    int needlelen = strlen(needle);

    while (haystack[idx] != 0) {
        if (!strncmp(&haystack[idx], needle, needlelen))
            return idx;

        idx++;
    }

    return -1; // not found.
}

static int strpos(const char *haystack, const char *needle)
{
    return strposat(haystack, needle, 0);
}

// convert a base-16 number in ASCII ('len' characters long) to a 64
// bit integer. Result is written to *ov, 0 is returned if parsing is
// successful. Otherwise -1 is returned.
static int strto64(const char *s, int maxlen, int64_t *ov)
{
    int64_t acc = 0;
    for (int i = 0; i < maxlen; i++) {
        char c = s[i];
        if (c==0)
            break;
        int ic = 0;
        if (c >= 'a' && c <='f')
            ic = c - 'a' + 10;
        else if (c >= 'A' && c <= 'F')
            ic = c - 'A' + 10;
        else if (c >= '0' && c <= '9')
            ic = c - '0';
        else
            printf("%c", c); //return -1;
        acc = (acc<<4) + ic;
    }

    *ov = acc;
    return 0;
}


static const char *toformat(dc1394color_coding_t color, dc1394color_filter_t filter)
{
    switch (color) {
        case DC1394_COLOR_CODING_MONO8:
            return "GRAY8";
        case DC1394_COLOR_CODING_RAW8:
            switch (filter) {
                case DC1394_COLOR_FILTER_RGGB:
                    return "BAYER_RGGB";
                case DC1394_COLOR_FILTER_GBRG:
                    return "BAYER_GBRG";
                case DC1394_COLOR_FILTER_GRBG:
                    return "BAYER_GRBG";
                case DC1394_COLOR_FILTER_BGGR:
                    return "BAYER_BGGR";
                default:
                    return "GRAY";
            }
        case DC1394_COLOR_CODING_YUV411:
            return "YUV422";
        case DC1394_COLOR_CODING_YUV422:
            return "UYVY";
        case DC1394_COLOR_CODING_YUV444:
            return "IYU2";
        case DC1394_COLOR_CODING_RGB8:
            return "RGB";
        case DC1394_COLOR_CODING_MONO16:
            return "GRAY16";
        case DC1394_COLOR_CODING_RGB16:
            return "BE_RGB16";
        case DC1394_COLOR_CODING_MONO16S:
            return "BE_SIGNED_GRAY16";
        case DC1394_COLOR_CODING_RGB16S:
            return "BE_SIGNED_RGB16";
        case DC1394_COLOR_CODING_RAW16:
            return "BE_GRAY16";
    }
    return "UNKNOWN";
}

static int num_formats(image_source_t *isrc)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    return impl->nformats;
}

static image_source_format_t *get_format(image_source_t *isrc, int idx)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    assert(idx>=0 && idx < impl->nformats);
    return impl->formats[idx];
}

static int get_current_format(image_source_t *isrc)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    return impl->current_format_idx;
}

static int set_format(image_source_t *isrc, int idx)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    assert(idx>=0 && idx < impl->nformats);

    impl->current_format_idx = idx;

    return 0;
}

static int set_named_format(image_source_t *isrc, const char *desired_format)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    const char *format_name = desired_format;
    int colonpos = strpos(desired_format, ":");
    int xpos = strpos(desired_format, "x");
    int width = -1;
    int height = -1;
    if (colonpos >= 0 && xpos > colonpos) {
        format_name = strndup(desired_format, colonpos);
        char *swidth = strndup(&desired_format[colonpos+1], xpos-colonpos-1);
        char *sheight = strdup(&desired_format[xpos+1]);

        width = atoi(swidth);
        height = atoi(sheight);

        free(swidth);
        free(sheight);
    }

    int nformats = num_formats(isrc);
    int fidx = -1;

    for (int i=0; i < nformats; i++)
    {
        image_source_format_t *fmt = get_format(isrc, i);

        if (!strcmp(fmt->format, format_name)) {
            if (width == -1 || height == -1 || (fmt->width == width && fmt->height == height)) {
                fidx = i;
                break;
            }
        }
    }

    // if no matching format found...
    if (fidx < 0 || fidx >= impl->nformats) {
        printf("Matching format '%s' not found. Valid formats are:\n", desired_format);
        for (int i=0; i < nformats; i++)
        {
            image_source_format_t *fmt = get_format(isrc, i);
            printf("\t[fidx: %d] width: %d height: %d name: '%s'\n",
                   i, fmt->width, fmt->height, fmt->format);
        }
        printf("\tFormat resolution not required.  Exiting.\n");
        exit(-1);
    }

    impl->current_format_idx = fidx;

    return 0;
}

static int num_features(image_source_t *isrc)
{
    // don't forget: feature index starts at 0
    return 18;
}

// Features we may want to implement (from dc1394feature_t enum)
//
//    DC1394_FEATURE_TRIGGER
//    DC1394_FEATURE_TRIGGER_DELAY

static const char* get_feature_name(image_source_t *isrc, int idx)
{
    switch(idx)
    {
    case 0:
        return "white-balance-manual";
    case 1:
        return "white-balance-red";
    case 2:
        return "white-balance-blue";
    case 3:
        return "exposure-manual";
    case 4:
        return "exposure";
    case 5:
        return "brightness-manual";
    case 6:
        return "brightness";
    case 7:
        return "shutter-manual";
    case 8:
        return "shutter";
    case 9:
        return "gain-manual";
    case 10:
        return "gain";
    case 11:
        return "gamma-manual";
    case 12:
        return "gamma";
    case 13:
        return "hdr";
    case 14:
        return "frame-rate-manual";
    case 15:
        return "frame-rate";
    case 16:
        return "timestamps-enable";
    case 17:
        return "frame-counter-enable";
    default:
        return NULL;
    }
}

static dc1394feature_info_t *find_feature(image_source_t *isrc, dc1394feature_t id)
{
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    for (int i = 0; i < DC1394_FEATURE_NUM; i++) {
        if (impl->features.feature[i].id == id)
            return &impl->features.feature[i];
    }

    return NULL;
}

static double get_feature_min(image_source_t *isrc, int idx)
{
    switch(idx)
    {
    case 0: // white-balance-manual
        return 0;
    case 1: // white-balance-red
    case 2: // white-balance-blue
        return find_feature(isrc, DC1394_FEATURE_WHITE_BALANCE)->min;
    case 3: // exposure-manual
        return 0;
    case 4: // exposure
        return find_feature(isrc, DC1394_FEATURE_EXPOSURE)->min;
    case 5: // brightness-manual
        return 0;
    case 6: // brightness
        return find_feature(isrc, DC1394_FEATURE_BRIGHTNESS)->min;
    case 7: // shutter-manual
        return 0;
    case 8: // shutter
        return find_feature(isrc, DC1394_FEATURE_SHUTTER)->min;
    case 9: // gain-manual
        return 0;
    case 10: // gain
        return find_feature(isrc, DC1394_FEATURE_GAIN)->min;
    case 11: // gamma-manual
        return 0;
    case 12: // gamma
        return find_feature(isrc, DC1394_FEATURE_GAMMA)->min;
    case 13: // hdr
        return 0;
    case 14: // frame-rate-mode
        return 0;
    case 15: // frame-rate
        return find_feature(isrc, DC1394_FEATURE_FRAME_RATE)->abs_min;
    case 16: // timestamps-enable
        return 0;
    case 17: // frame-counter-enable
        return 0;
    default:
        return 0;
    }
}

static double get_feature_max(image_source_t *isrc, int idx)
{
    switch(idx)
    {
    case 0: // white-balance-manual
        return 1;
    case 1: // white-balance-red
    case 2: // white-balance-blue
        return find_feature(isrc, DC1394_FEATURE_WHITE_BALANCE)->max;
    case 3: // exposure-manual
        return 1;
    case 4: // exposure
        return find_feature(isrc, DC1394_FEATURE_EXPOSURE)->max;
    case 5: // brightness-manual
        return 1;
    case 6: // brightness
        return find_feature(isrc, DC1394_FEATURE_BRIGHTNESS)->max;
    case 7: // shutter-manual
        return 1;
    case 8: // shutter
        return find_feature(isrc, DC1394_FEATURE_SHUTTER)->max;
    case 9: // gain-manual
        return 1;
    case 10: // gain
        return find_feature(isrc, DC1394_FEATURE_GAIN)->max;
    case 11: // gamma-manual
        return 1;
    case 12: // gamma
        return find_feature(isrc, DC1394_FEATURE_GAMMA)->max;
    case 13: // hdr
        return 1;
    case 14: // frame-rate-mode
        return 1;
    case 15: // frame-rate
        return find_feature(isrc, DC1394_FEATURE_FRAME_RATE)->abs_max;
    case 16: // timestamps-enable
        return 1;
    case 17: // frame-counter-enable
        return 1;
    default:
        return 0;
    }
}

// Some features are controlled via ON/OFF (e.g., WHITE_BALANCE), some
// by mode AUTO/MANUAL (e.g., most of the rest). We try to hide this
// variation and just "make it work".
static double get_feature_value(image_source_t *isrc, int idx)
{
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    switch (idx)
    {
    case 0: { // white-balance-manual
        dc1394switch_t mode = DC1394_OFF;
        dc1394_feature_get_power(impl->cam, DC1394_FEATURE_WHITE_BALANCE, &mode);
        return mode == DC1394_ON;
    }

    case 1:   // white-balance-red
    case 2: { // white-balance-blue
        uint32_t b=0, r=0;

        dc1394_feature_whitebalance_get_value(impl->cam, &b, &r);

        if (idx == 1)
            return r;

        return b;
    }

    case 3: { // exposure-manual
        dc1394feature_mode_t mode = DC1394_FEATURE_MODE_AUTO;
        dc1394_feature_get_mode(impl->cam, DC1394_FEATURE_EXPOSURE, &mode);
        return mode == DC1394_FEATURE_MODE_MANUAL;
    }
    case 4: { // exposure
        uint32_t v = 0;
        dc1394_feature_get_value(impl->cam, DC1394_FEATURE_EXPOSURE, &v); // XXX error checking
        return v;
    }

    case 5: {// brightness-manual
        dc1394feature_mode_t mode = DC1394_FEATURE_MODE_AUTO;
        dc1394_feature_get_mode(impl->cam, DC1394_FEATURE_BRIGHTNESS, &mode);
        return mode == DC1394_FEATURE_MODE_MANUAL;
    }
    case 6: { // brightness
        uint32_t v = 0;
        dc1394_feature_get_value(impl->cam, DC1394_FEATURE_BRIGHTNESS, &v); // XXX error checking
        return v;
    }

    case 7: { // shutter-manual
        dc1394feature_mode_t mode = DC1394_FEATURE_MODE_AUTO;
        dc1394_feature_get_mode(impl->cam, DC1394_FEATURE_SHUTTER, &mode);

        return mode == DC1394_FEATURE_MODE_MANUAL;
    }
    case 8: { // shutter
        uint32_t v = 0;
        dc1394_feature_get_value(impl->cam, DC1394_FEATURE_SHUTTER, &v); // XXX error checking
        return v;
    }

    case 9: { // gain-manual
        dc1394feature_mode_t mode = DC1394_FEATURE_MODE_AUTO;
        dc1394_feature_get_mode(impl->cam, DC1394_FEATURE_GAIN, &mode);

        return mode == DC1394_FEATURE_MODE_MANUAL;
    }
    case 10: { // gain
        uint32_t v = 0;
        dc1394_feature_get_value(impl->cam, DC1394_FEATURE_GAIN, &v); // XXX error checking
        return v;
    }

    case 11: { // gamma-manual
        dc1394switch_t mode = DC1394_OFF;
        dc1394_feature_get_power(impl->cam, DC1394_FEATURE_GAMMA, &mode);
        return mode == DC1394_ON;
    }

    case 12: { // gamma
        uint32_t v = 0;
        dc1394_feature_get_value(impl->cam, DC1394_FEATURE_GAMMA, &v); // XXX error checking
        return v;
    }

    case 13: { // hdr
         uint64_t offset = 0x0F00000ULL;

       // write address of imager register to pointgrey pass-through register
        if (dc1394_set_register(impl->cam, offset + 0x1a00, 0x0f) != DC1394_SUCCESS)
            return -1;

        uint32_t value;
        if (dc1394_get_register(impl->cam, offset + 0x1a04, &value) != DC1394_SUCCESS)
            return -1;

        return (value & 0x40) ? 1 : 0;
    }

    case 14: { // frame-rate-mode
        dc1394feature_mode_t mode = DC1394_FEATURE_MODE_AUTO;
        dc1394_feature_get_mode(impl->cam, DC1394_FEATURE_FRAME_RATE, &mode);
        return mode == DC1394_FEATURE_MODE_MANUAL;
    }

    case 15: { // frame rate
        float v = 0;
        dc1394_feature_get_absolute_value(impl->cam, DC1394_FEATURE_FRAME_RATE, &v); // XXX error checking
        return v;
    }

    case 16: { // timestamps-enable
        uint32_t value;
        if (dc1394_get_adv_control_register(impl->cam, 0x2F8, &value) != DC1394_SUCCESS)
            return 0;

        return value & 0x01;
    }

    case 17: { // frame-counter-enable
        uint32_t value;
        if (dc1394_get_adv_control_register(impl->cam, 0x2F8, &value) != DC1394_SUCCESS)
            return 0;

        return value & 0x40;
    }

    default:
        return 0;
    }
}

static int set_feature_value(image_source_t *isrc, int idx, double v)
{
    uint32_t r, b;
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    dc1394_feature_whitebalance_get_value(impl->cam, &b, &r);

    switch (idx)
    {
    case 0: { // white-balance-manual
        if (v==1) {
            dc1394_feature_set_power(impl->cam, DC1394_FEATURE_WHITE_BALANCE, DC1394_ON);
            dc1394_feature_set_mode(impl->cam, DC1394_FEATURE_WHITE_BALANCE, DC1394_FEATURE_MODE_MANUAL);
        } else {
            dc1394_feature_set_power(impl->cam, DC1394_FEATURE_WHITE_BALANCE, DC1394_OFF);
        }

        break;
    }

    case 1: // white-balance-red
    case 2: { // white-balance-blue
        uint32_t b=0, r=0;

        dc1394_feature_whitebalance_get_value(impl->cam, &b, &r);

        if (idx==1)
            r = (uint32_t) v;
        if (idx==2)
            b = (uint32_t) v;

        dc1394_feature_whitebalance_set_value(impl->cam, (uint32_t) b, (uint32_t) r);
        break;
    }

    case 3: { // exposure-manual
        dc1394_feature_set_power(impl->cam, DC1394_FEATURE_EXPOSURE, DC1394_ON);
        dc1394_feature_set_mode(impl->cam, DC1394_FEATURE_EXPOSURE, v!=0 ? DC1394_FEATURE_MODE_MANUAL :
                                DC1394_FEATURE_MODE_AUTO);
        break;
    }

    case 4: { // exposure
        dc1394_feature_set_value(impl->cam, DC1394_FEATURE_EXPOSURE, (uint32_t) v);
        break;
    }

    case 5: { // brightness-manual
        dc1394_feature_set_power(impl->cam, DC1394_FEATURE_BRIGHTNESS, DC1394_ON);
        dc1394_feature_set_mode(impl->cam, DC1394_FEATURE_BRIGHTNESS, v!=0 ? DC1394_FEATURE_MODE_MANUAL :
                                DC1394_FEATURE_MODE_AUTO);
        break;
    }

    case 6: // brightness
        dc1394_feature_set_value(impl->cam, DC1394_FEATURE_BRIGHTNESS, (uint32_t) v);
        break;

    case 7: { // shutter-manual
        if (v == 1) {
            dc1394_feature_set_power(           impl->cam, DC1394_FEATURE_SHUTTER, DC1394_ON);
            dc1394_feature_set_mode(            impl->cam, DC1394_FEATURE_SHUTTER, DC1394_FEATURE_MODE_MANUAL);
            dc1394_feature_set_absolute_control(impl->cam, DC1394_FEATURE_SHUTTER, DC1394_OFF);
        } else {
            dc1394_feature_set_power(           impl->cam, DC1394_FEATURE_SHUTTER, DC1394_ON);
            dc1394_feature_set_mode(            impl->cam, DC1394_FEATURE_SHUTTER, DC1394_FEATURE_MODE_AUTO);
            dc1394_feature_set_absolute_control(impl->cam, DC1394_FEATURE_SHUTTER, DC1394_ON);
        }
        break;
    }
    case 8: { // shutter
        dc1394_feature_set_value(impl->cam, DC1394_FEATURE_SHUTTER, (uint32_t) v);
        break;
    }

    case 9: { // gain-manual
        if (v == 1) {
            dc1394_feature_set_power(           impl->cam, DC1394_FEATURE_GAIN, DC1394_ON);
            dc1394_feature_set_mode(            impl->cam, DC1394_FEATURE_GAIN, DC1394_FEATURE_MODE_MANUAL);
            dc1394_feature_set_absolute_control(impl->cam, DC1394_FEATURE_GAIN, DC1394_OFF);
        } else {
            dc1394_feature_set_power(           impl->cam, DC1394_FEATURE_GAIN, DC1394_ON);
            dc1394_feature_set_mode(            impl->cam, DC1394_FEATURE_GAIN, DC1394_FEATURE_MODE_AUTO);
            dc1394_feature_set_absolute_control(impl->cam, DC1394_FEATURE_GAIN, DC1394_ON);
        }
        break;
    }
    case 10: { // gain
        dc1394_feature_set_value(impl->cam, DC1394_FEATURE_GAIN, (uint32_t) v);
        break;
    }

    case 11: { // gamma-manual
        if (v==1) {
            dc1394_feature_set_power(impl->cam, DC1394_FEATURE_GAMMA, DC1394_ON);
            dc1394_feature_set_mode(impl->cam, DC1394_FEATURE_GAMMA, DC1394_FEATURE_MODE_MANUAL);
        } else {
            dc1394_feature_set_power(impl->cam, DC1394_FEATURE_GAMMA, DC1394_OFF);
        }
    }

    case 12: // gamma
        return dc1394_feature_set_value(impl->cam, DC1394_FEATURE_GAMMA, (uint32_t) v);

    case 13: { // hdr

        uint64_t offset = 0x0F00000ULL;

        // write address of imager register to pointgrey pass-through register
        if (dc1394_set_register(impl->cam, offset + 0x1a00, 0x0f) != DC1394_SUCCESS)
            return -1;

        // enable HDR mode
        uint32_t value;
        if (dc1394_get_register(impl->cam, offset + 0x1a04, &value) != DC1394_SUCCESS)
            return -1;

        if (v == 1) {
            value |= 0x40;
        } else {
            value &= (~0x40);
        }

        if (dc1394_set_register(impl->cam,  offset + 0x1a04, value) != DC1394_SUCCESS)
            return -1;

        // enable automatic knee point timing
        if (dc1394_set_register(impl->cam,  offset + 0x1a00, 0x0a) != DC1394_SUCCESS)
            return -1;

        if (dc1394_get_register(impl->cam,  offset + 0x1a04, &value) != DC1394_SUCCESS)
            return -1;

        if (v == 1) {
            value |= 0x100;
        } else {
            value &= (~0x100);
        }

        if (dc1394_set_register(impl->cam,  offset + 0x1a04, value) != DC1394_SUCCESS)
            return -1;

        break;
    }

    case 14: { // frame-rate-mode
        dc1394_feature_set_power(impl->cam, DC1394_FEATURE_FRAME_RATE, DC1394_ON);
        dc1394_feature_set_mode(impl->cam, DC1394_FEATURE_FRAME_RATE, v!=0 ? DC1394_FEATURE_MODE_MANUAL :
                                DC1394_FEATURE_MODE_AUTO);

        break;
    }

    case 15: { // frame rate
        dc1394_feature_set_absolute_value(impl->cam, DC1394_FEATURE_FRAME_RATE, (float) v);
        break;
    }

    case 16: { // timestamps-enable
        uint32_t value;
        if (dc1394_get_adv_control_register(impl->cam, 0x2F8, &value) != DC1394_SUCCESS)
            return -1;

        value &= (~0x01);
        value |= ((int) v) << 0;

        if (dc1394_set_adv_control_register(impl->cam, 0x2F8, value) != DC1394_SUCCESS)
            return -1;

        break;
    }

    case 17: { // frame-counter-enable
        uint32_t value;
        if (dc1394_get_adv_control_register(impl->cam, 0x2F8, &value) != DC1394_SUCCESS)
            return -1;

        value &= (~0x40);
        value |= ((int) v) << 6;

        if (dc1394_set_adv_control_register(impl->cam, 0x2F8, value) != DC1394_SUCCESS)
            return -1;

        break;
    }

    default:
        return 0;
    }

    return 0;
}

static int start(image_source_t *isrc)
{
    int have_reset_bus = 0;

restart:
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    image_source_format_t *format = impl->formats[impl->current_format_idx];
    struct format_priv *format_priv = format->priv;

    dc1394_video_set_mode(impl->cam, format_priv->dc1394_mode);
    dc1394_video_set_iso_speed(impl->cam, DC1394_ISO_SPEED_400);

    assert(dc1394_is_video_mode_scalable(format_priv->dc1394_mode));

    dc1394format7modeset_t info;
    dc1394_format7_get_modeset(impl->cam, &info);

    dc1394format7mode_t *mode = info.mode + format_priv->format7_mode_idx;
    dc1394color_coding_t color_coding = mode->color_codings.codings[format_priv->color_coding_idx];

    dc1394_format7_set_image_size(impl->cam, format_priv->dc1394_mode,
                                  format->width, format->height);

    dc1394_format7_set_image_position(impl->cam, format_priv->dc1394_mode, 0, 0);

    dc1394_format7_set_color_coding(impl->cam, format_priv->dc1394_mode, color_coding);

    uint32_t psize_unit, psize_max;
    dc1394_format7_get_packet_parameters(impl->cam, format_priv->dc1394_mode, &psize_unit, &psize_max);

    if (impl->packet_size == 0) {
        impl->packet_size = psize_max; //4096;
    } else {
        impl->packet_size = psize_unit * (impl->packet_size / psize_unit);
        if (impl->packet_size > psize_max)
            impl->packet_size = psize_max;
        if (impl->packet_size < psize_unit)
            impl->packet_size = psize_unit;
    }

    // printf("psize_unit: %d, psize_max: %d, packet_size: %d\n", psize_unit, psize_max, impl->packet_size);

    dc1394_format7_set_packet_size(impl->cam, format_priv->dc1394_mode, impl->packet_size);
    uint64_t bytes_per_frame;
    dc1394_format7_get_total_bytes(impl->cam, format_priv->dc1394_mode, &bytes_per_frame);

    if (bytes_per_frame * impl->num_buffers > 25000000) {
        printf ("Reducing dc1394 buffers from %d to ", impl->num_buffers);
        impl->num_buffers = 25000000 / bytes_per_frame;
        printf ("%d\n", impl->num_buffers);
    }

    /* Using libdc1394 for iso streaming */
    if (dc1394_capture_setup(impl->cam, impl->num_buffers,
                             DC1394_CAPTURE_FLAGS_DEFAULT) != DC1394_SUCCESS)
        goto fail;

    if (dc1394_video_set_transmission(impl->cam, DC1394_ON) != DC1394_SUCCESS)
        goto fail;

    impl->fd = dc1394_capture_get_fileno (impl->cam);

    impl->started = 1;

    return 0;

fail:
    if (have_reset_bus) {
        fprintf(stderr, "----------------------------------------------------------------\n");
        fprintf(stderr, "Error: failed to initialize dc1394 stream\n");
        fprintf(stderr, "\nIF YOU HAVE HAD A CAMERA FAIL TO EXIT CLEANLY OR\n");
        fprintf(stderr, " THE BANDWIDTH HAS BEEN OVER SUBSCRIBED TRY (to reset):\n");
        fprintf(stderr, "dc1394_reset_bus\n\n");
        fprintf(stderr, "----------------------------------------------------------------\n");
        return -1;
    } else {
        fprintf(stderr, "----------------------------------------------------------------\n");
        fprintf(stderr, "image_source_dc1394: Camera startup failed, reseting bus.\n");
        fprintf(stderr, "(this is harmless if the last program didn't quit cleanly,\n");
        fprintf(stderr, "but things may not work well if bandwidth is over-subscribed.\n");
        fprintf(stderr, "----------------------------------------------------------------\n");

        dc1394_reset_bus(impl->cam);

        have_reset_bus = 1;
        goto restart;
    }
}

static int get_frame(image_source_t *isrc, void **imbuf, int *buflen)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    assert(impl->current_frame == NULL);

    while (1) {

        if (dc1394_capture_dequeue(impl->cam, DC1394_CAPTURE_POLICY_WAIT, &impl->current_frame) != DC1394_SUCCESS) {
            printf("DC1394 dequeue failed\n");
            return -1;
        }

        if (impl->current_frame->frames_behind > 0 || dc1394_capture_is_frame_corrupt(impl->cam, impl->current_frame) == DC1394_TRUE) {
            dc1394_capture_enqueue(impl->cam, impl->current_frame);
            continue;
        }

        break;
    }

    *imbuf = impl->current_frame->image;
    *buflen = impl->current_frame->image_bytes;

    return 0;
}

static int release_frame(image_source_t *isrc, void *imbuf)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    dc1394_capture_enqueue(impl->cam, impl->current_frame);
    impl->current_frame = NULL;

    return 0;
}

static int stop(image_source_t *isrc)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    dc1394_video_set_transmission (impl->cam, DC1394_OFF);

    dc1394_capture_stop(impl->cam);

    impl->started = 0;

    return 0;
}

static int my_close(image_source_t *isrc)
{
    assert(isrc->impl_type == IMPL_TYPE);
    impl_dc1394_t *impl = (impl_dc1394_t*) isrc->impl;

    return close(impl->fd);
}

/** Open the given guid, or if -1, open the first camera available. **/
image_source_t *image_source_dc1394_open(url_parser_t *urlp)
{
    const char *protocol = url_parser_get_protocol(urlp);
    const char *location = url_parser_get_location(urlp);

    int64_t guid = 0;

    if (strlen(location) == 0) {
        // use the first dc1394 camera in the system
        dc1394_t *dc1394 = dc1394_new();
        if (dc1394 == NULL)
            return NULL;

        dc1394camera_list_t *list;

        if (dc1394_camera_enumerate(dc1394, &list)) {
            dc1394_free(dc1394);
            return NULL;
        }

        if (list->num > 0) {
            guid = list->ids[0].guid;
        }

        dc1394_camera_free_list(list);
    } else if (strto64(location, strlen(location), &guid)) {
        printf("image_source_open: dc1394 guid '%s' is not a valid integer.\n", location);
        return NULL;
    }

    image_source_t *isrc = calloc(1, sizeof(image_source_t));
    impl_dc1394_t *impl = calloc(1, sizeof(impl_dc1394_t));

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

    impl->num_buffers = 10;

    impl->dc1394 = dc1394_new();
    if (!impl->dc1394)
        return NULL;

    // now open our desired camera.
    impl->cam = dc1394_camera_new(impl->dc1394, guid);
    if (impl->cam == NULL)
        goto fail;

    dc1394format7modeset_t info;
    if (dc1394_format7_get_modeset(impl->cam, &info) != DC1394_SUCCESS)
        goto fail;

    for (int i = 0; i < DC1394_VIDEO_MODE_FORMAT7_NUM; i++) {

        dc1394format7mode_t *mode = info.mode + i;

        if (!info.mode[i].present)
            continue;

        for (int j = 0; j < mode->color_codings.num; j++) {

            impl->formats = realloc(impl->formats, (impl->nformats+1) * sizeof(image_source_format_t*));
            impl->formats[impl->nformats] = calloc(1, sizeof(image_source_format_t));

            impl->formats[impl->nformats]->width = mode->max_size_x;
            impl->formats[impl->nformats]->height = mode->max_size_y;
            impl->formats[impl->nformats]->format = strdup(toformat(mode->color_codings.codings[j], mode->color_filter));

            struct format_priv *format_priv = calloc(1, sizeof(struct format_priv));
            impl->formats[impl->nformats]->priv = format_priv;

            format_priv->dc1394_mode = DC1394_VIDEO_MODE_FORMAT7_0 + i;
            format_priv->format7_mode_idx = i;
            format_priv->color_coding_idx = j;

            impl->nformats++;
        }
    }

    dc1394_feature_get_all(impl->cam, &impl->features);

    if (1) {
        // ptgrey cameras don't seem to have any cases of this...
        int reread = 0;

        for (int i = 0; i < DC1394_FEATURE_NUM; i++) {
            dc1394feature_info_t *f = &impl->features.feature[i];
            if (f->available && f->absolute_capable && !f->abs_control) {
                printf("absolute mode\n");
                dc1394_feature_set_absolute_control(impl->cam, f->id, DC1394_ON);
                reread = 1;
            }
        }

        if (reread)
            dc1394_feature_get_all(impl->cam, &impl->features);
    }

    if (0) {
        // work around an intermittent bug where sometimes some
        // garbage causes the camera data to be offset by about a
        // third of a scanline.
        isrc->start(isrc);

        void *imbuf = NULL;
        int imbuflen = 0;

        if (!isrc->get_frame(isrc, &imbuf, &imbuflen)) {
            isrc->release_frame(isrc, imbuf);
        }

        isrc->stop(isrc);
    }

    return isrc;

fail:
    printf("image_source_dc1394_open: failure\n");
    return NULL;
}

char** image_source_enumerate_dc1394(char **urls)
{
    dc1394_t *dc1394;
    dc1394camera_list_t *list;

    dc1394 = dc1394_new();
    if (dc1394 == NULL)
        return urls;

    if (dc1394_camera_enumerate (dc1394, &list) < 0)
        goto exit;

    // display all cameras for convenience
    for (int i = 0; i < list->num; i++) {
        dc1394camera_t *cam = dc1394_camera_new(dc1394, list->ids[i].guid);
        if (cam == NULL)
            continue;

        char buf[1024];

        // other useful fields: cam->vendor, cam->model);
        snprintf(buf, 1024, "dc1394://%"PRIx64, list->ids[i].guid);
        urls = string_array_add(urls, buf);
        dc1394_camera_free(cam);
    }

    dc1394_camera_free_list(list);

exit:
    dc1394_free(dc1394);
    return urls;
}
