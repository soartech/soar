#ifndef _URL_PARSER_H
#define _URL_PARSER_H

typedef struct url_parser url_parser_t;

// returns null if error.
url_parser_t *url_parser_create(const char *s);
void url_parser_destroy(url_parser_t *urlp);

// e.g., http://.
const char* url_parser_get_protocol(url_parser_t *urlp);

// e.g., www.google.com/foobar
const char* url_parser_get_location(url_parser_t *urlp);

// returns null def if no parameter specified.
const char* url_parser_get_parameter(url_parser_t *urlp, const char *key, const char *def);

// how many parameters were manually specified?
int url_parser_num_parameters(url_parser_t *urlp);

// what was the name of the nth specified parameter?
const char* url_parser_get_parameter_name(url_parser_t *urlp, int idx);
const char* url_parser_get_parameter_value(url_parser_t *urlp, int idx);

#endif
