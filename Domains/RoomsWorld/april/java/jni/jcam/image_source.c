#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdio.h>

#include "image_source.h"
#include "url_parser.h"

image_source_t *image_source_open(const char *url)
{
    image_source_t *isrc = NULL;

    url_parser_t *urlp = url_parser_create(url);
    if (urlp == NULL) // bad URL format
        return NULL;

    const char *protocol = url_parser_get_protocol(urlp);
    const char *location = url_parser_get_location(urlp);

    if (!strcmp(protocol, "v4l2://"))
        isrc = image_source_v4l2_open(location);
    else if (!strcmp(protocol, "dc1394://")) {
        isrc = image_source_dc1394_open(urlp);
    } else if (!strcmp(protocol, "islog://")) {
        isrc = image_source_islog_open(urlp);
    }

    if (isrc != NULL) {

        // handle parameters
        for (int idx = 0; idx < url_parser_num_parameters(urlp); idx++) {
            const char *key = url_parser_get_parameter_name(urlp, idx);
            const char *value = url_parser_get_parameter_value(urlp, idx);

//            printf("%s => %s\n", key, value);

            if (!strcmp(key, "fidx")) {
                int fidx = atoi(url_parser_get_parameter(urlp, "fidx", "0"));
                isrc->set_format(isrc, fidx);
                continue;
            }

            if (!strcmp(key, "format")) {
                isrc->set_named_format(isrc, value);
                continue;
            }

            // pass through a device-specific parameter.
            int found = 0;
            for (int idx = 0; idx < isrc->num_features(isrc); idx++) {

                if (!strcmp(isrc->get_feature_name(isrc, idx), key)) {
                    char *endptr = NULL;
                    double dv = strtod(value, &endptr);
                    if (endptr != value + strlen(value)) {
                        printf("Parameter for key '%s' is invalid. Must be a number.\n",
                               isrc->get_feature_name(isrc, idx));
                        goto cleanup;
                    }
                    int res = isrc->set_feature_value(isrc, idx, dv);
                    found = 1;
                    break;
                }
            }

            if (!found)
                printf("Unhandled parameter %s\n", key);
        }
    }

cleanup:
    url_parser_destroy(urlp);

    return isrc;
}

char** image_source_enumerate()
{
    char **urls = calloc(1, sizeof(char*));

    urls = image_source_enumerate_v4l2(urls);
    urls = image_source_enumerate_dc1394(urls);

    return urls;
}

void image_source_enumerate_free(char **urls)
{
    if (urls == NULL)
        return;

    for (int i = 0; urls[i] != NULL; i++)
        free(urls[i]);
    free(urls);
}
