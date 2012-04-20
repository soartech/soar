/* 
 Copyright (C) 2009, Scott Livingston

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

*/


#include <stdlib.h>
#include <time.h>
#include <stdio.h>
#include <string.h>

#include <rlglue/utils/C/TaskSpec_Parser.h>
#include <rlglue/utils/C/RLStruct_util.h>

#define BUF_SIZE 1024


/* Add "noise" to a portion of the characters in string s. */
void string_noise( char *s, float noise_fraction );


int main( int argc, char **argv )
{
	FILE *test_fp;
	char *cp;
	char test_str[BUF_SIZE];
	char out_str[BUF_SIZE];
	taskspec_t ts;
	int dec_result;
	float noise_frac;

	/* attempt to open the file of test task spec strings */
	if (argc == 1) {
		test_fp = fopen( "test_cases.txt", "r" );
		noise_frac = 0.;
	} else if (argc == 2) {
		test_fp = fopen( argv[1], "r" );
		noise_frac = 0.;
	} else if (argc == 3) {
		test_fp = fopen( argv[1], "r" );
		noise_frac = atof( argv[2] );
	} else {
		printf( "Usage: %s [test-strings-file] [noise-fraction]\n", argv[0] );
		return 1;
	}
	if (test_fp == NULL) {
		perror( "main, fopen" );
		return -1;
	}

	srand(time(NULL));

	/* the read, test loop */
	while (!feof( test_fp )) {

		if (fgets( test_str, BUF_SIZE, test_fp ) == NULL)
			break;

		/* strip read line of a new-line character, if present */
		if ((cp = strchr( test_str, '\n' )) != NULL)
			*cp = '\0';

		string_noise( test_str, noise_frac );
		printf( "Test string:      %s\n", test_str );
		dec_result = decode_taskspec( &ts, test_str );
		switch( dec_result ) {

		case 0:
			if (encode_taskspec( &ts, out_str, BUF_SIZE ) == -1) {
				fprintf( stderr, "encode_taskspec (task spec string generation) failed.\n\n" );
			} else {
				printf( "Resulting string: %s\n\n", out_str );
			}
			free_taskspec_struct( &ts );
			break;

		case 1:
			printf( "Unrecognized or unsupported task spec string version.\n\n" );
			break;

		case -1:
			fprintf( stderr, "decode_taskspec (task spec string parsing) failed.\n\n" );
			break;

		default:
			fprintf( stderr, "decode_taskspec returned: %d\n\n", dec_result );
			break;

		}

	}

	fclose( test_fp );

	return 0;
}


void string_noise( char *s, float noise_fraction )
{
	int i, total_corrupt, s_len;
	int ind_to_alter;

	if (s == NULL
		|| noise_fraction > 1 || noise_fraction < 0)
		return;

	s_len = strlen( s );

	total_corrupt = (int)(noise_fraction*s_len);

	for (i = 0; i < total_corrupt; i++) {
		ind_to_alter = (int)((double)s_len*rand()/(RAND_MAX+1.0));
		*(s+ind_to_alter) = 32 + (int)(94.*rand()/(RAND_MAX+1.0)); /* random ASCII char */
	}
}
