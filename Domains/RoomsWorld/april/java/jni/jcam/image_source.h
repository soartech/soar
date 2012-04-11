#ifndef _IMAGE_SOURCE_H
#define _IMAGE_SOURCE_H

#include <stdint.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "url_parser.h"

typedef struct image_source_format image_source_format_t;
struct image_source_format
{
    int   width, height;
    char  *format; // human readable string. Use fourcc codes whenever possible

    void  *priv; // for use by the implementation.
};


typedef struct image_source image_source_t;
struct image_source
{
    int   impl_type;
    void *impl;

    int (*num_formats)(image_source_t *isrc);
    image_source_format_t *(*get_format)(image_source_t *isrc, int idx);
    int (*set_format)(image_source_t *isrc, int idx);
    int (*set_named_format)(image_source_t *isrc, const char *desired_format);
    int (*get_current_format)(image_source_t *isrc);

    int (*start)(image_source_t *isrc);
    int (*get_frame)(image_source_t *isrc, void **buf, int *buflen);
    int (*release_frame)(image_source_t *isrc, void *buf);
    int (*stop)(image_source_t *isrc);

    int (*num_features)(image_source_t *isrc);
    const char* (*get_feature_name)(image_source_t *isrc, int idx);
    double (*get_feature_min)(image_source_t *isrc, int idx);
    double (*get_feature_max)(image_source_t *isrc, int idx);
    double (*get_feature_value)(image_source_t *isrc, int idx);
    // returns non-zero on error
    int (*set_feature_value)(image_source_t *isrc, int idx, double v);

    int (*close)(image_source_t *isrc);
};

image_source_t *image_source_open(const char *url);
image_source_t *image_source_v4l2_open(const char *path);
image_source_t *image_source_dc1394_open(url_parser_t *urlp);
image_source_t *image_source_islog_open(url_parser_t *urlp);

char** image_source_enumerate();
void image_source_enumerate_free(char **b);

/** Adds URLs to 'urls' **/
char** image_source_enumerate_v4l2(char **urls);
char** image_source_enumerate_dc1394(char **urls);

#endif

#ifdef IMAGE_SOURCE_UTILS

// for use by image sources during enumeration.
static char** string_array_add(char **strs, char *s)
{
    // how long is the array now? (How many non-null entries?)
    int len;
    for (len = 0; strs[len]!=NULL; len++);

    // make room for one more entry (plus the NULL).
    strs = realloc(strs, sizeof(char*)*(len+2));
    strs[len] = strdup(s);
    strs[len+1] = NULL;

    return strs;
}

#endif

