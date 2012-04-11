#include <stdio.h>
#include <string.h>

#include "image_source.h"

#define BUFSIZE (1024*1024)

int main(int argc, char *argv[])
{
    char *url = NULL;

    if (argc > 1) {
        // if URL is provided on command line, use that.
        url = argv[1];
    } else {
        // otherwise, show all cameras and use the first one.

        char **urls = image_source_enumerate();
        printf("Cameras:\n");
        for (int i = 0; urls[i] != NULL; i++)
            printf("  %3d: %s\n", i, urls[i]);

        if (urls[0]==NULL) {
            printf("Found no cameras.\n");
            return -1;
        }

        url = urls[0];
    }

    image_source_t *isrc = image_source_open(url);

    if (isrc == NULL) {
        printf("Error opening device.\n");
        return -1;
    }

    for (int i = 0; i < isrc->num_formats(isrc); i++) {
        image_source_format_t *ifmt = isrc->get_format(isrc, i);
        printf("%3d: %4d x %4d (%s)\n", i, ifmt->width, ifmt->height, ifmt->format);
    }

    isrc->set_format(isrc, 0);

    if (1) {
        int res = isrc->start(isrc);
        printf("start: res = %d\n", res);
    }

    int nframes = 0;

//    setlinebuf(stdout);

    while(1) {
        void *imbuf = NULL;
        int imbuflen = 0;
        int res = isrc->get_frame(isrc, &imbuf, &imbuflen);
        if (res < 0) {
            printf("get_frame fail: %d\n", res);
            continue;
        } else {
            nframes++;
        }

        printf("get_frame: res = %d count = %10d (%10d bytes)\r", res, nframes, imbuflen);
        fflush(stdout);
        isrc->release_frame(isrc, imbuf);
    }
    return 0;
}

