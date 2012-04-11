#include "url_parser.h"
#define _GNU_SOURCE
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

struct url_parser
{
    char *protocol; // e.g. http://
    char *location; // e.g. www.google.com/foobar
    int  nparams;
    char **keys;
    char **values;
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

url_parser_t *url_parser_create(const char *s)
{
    url_parser_t *urlp = (url_parser_t*) calloc(1, sizeof(url_parser_t));

    int slashpos = strpos(s, "://");
    urlp->protocol = strndup(s, slashpos+3);

    int parampos = strpos(s, "?");
    urlp->location = strndup(&s[slashpos+3], parampos < 0 ? 9999 : parampos - slashpos - 3);

    while (parampos >= 0) {
        int nextparampos = strposat(s, "&", parampos+1);

        int eqpos = strposat(s, "=", parampos+1);
        char *key = strndup(&s[parampos+1], eqpos - parampos-1);
        char *val = strndup(&s[eqpos+1], nextparampos < 0 ? 9999 : nextparampos - eqpos - 1);

        urlp->nparams++;
        urlp->keys   = realloc(urlp->keys, urlp->nparams * sizeof(char*));
        urlp->values = realloc(urlp->values, urlp->nparams * sizeof(char*));
        urlp->keys[urlp->nparams-1] = key;
        urlp->values[urlp->nparams-1] = val;

        parampos = nextparampos;
    }
    return urlp;
}

void url_parser_destroy(url_parser_t *urlp)
{
    free(urlp->protocol);
    free(urlp->location);

    for (int i = 0; i < urlp->nparams; i++) {
        free(urlp->keys[i]);
        free(urlp->values[i]);
    }

    if (urlp->nparams > 0) {
        free(urlp->keys);
        free(urlp->values);
    }

    free(urlp);
}

const char* url_parser_get_protocol(url_parser_t *urlp)
{
    return urlp->protocol;
}

const char* url_parser_get_location(url_parser_t *urlp)
{
    return urlp->location;
}

const char* url_parser_get_parameter(url_parser_t *urlp, const char *key, const char *def)
{
    for (int i = 0; i < urlp->nparams; i++) {
        if (!strcmp(key, urlp->keys[i]))
            return urlp->values[i];
    }
    return def;
}

int url_parser_num_parameters(url_parser_t *urlp)
{
    return urlp->nparams;
}

const char* url_parser_get_parameter_name(url_parser_t *urlp, int idx)
{
    return urlp->keys[idx];
}

const char* url_parser_get_parameter_value(url_parser_t *urlp, int idx)
{
    return urlp->values[idx];
}
