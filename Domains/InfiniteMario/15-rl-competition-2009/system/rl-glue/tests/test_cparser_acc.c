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

/* FILE: test_cparser_acc.c
 *
 * DESC: Test the accessor methods for the C task spec parser. The
 * task spec string is read from each line of a file called
 * test_cases.txt (by default) or from the given file name. A file
 * name of - will cause the lines to be read from standard input. The
 * process terminates upon reaching the end-of-file indicator.
 *
 */


#include <stdio.h>
#include <string.h>

/* #include <rlglue/utils/C/TaskSpec_Parser.h> */
#include "TaskSpec_Parser.h"


#define BUF_SIZE 1024


/* test every accessor and print results to stdout */
void test_accessors( taskspec_t *ts );


int main( int argc, char **argv )
{
	FILE *test_fp;
	char *cp;
	char test_str[BUF_SIZE];
	taskspec_t ts;
	int dec_result;

	/* attempt to open the file of test task spec strings */
	if (argc == 1) {
		test_fp = fopen( "test_cases.txt", "r" );
	} else if (argc == 2) {
		if (strlen(argv[1]) == 1 && argv[1][0] == '-')
			test_fp = stdin;
		else
			test_fp = fopen( argv[1], "r" );
	} else {
		printf( "Usage: %s [test-strings-file]\n", argv[0] );
		return 1;
	}
	if (test_fp == NULL) {
		perror( "main, fopen" );
		return -1;
	}

	/* the read, test loop */
	while (!feof( test_fp )) {

		if (fgets( test_str, BUF_SIZE, test_fp ) == NULL)
			break;

		/* strip read line of a new-line character, if present */
		if ((cp = strchr( test_str, '\n' )) != NULL)
			*cp = '\0';

		printf( "Test string:      %s\n", test_str );
		dec_result = decode_taskspec( &ts, test_str );
		switch( dec_result ) {

		case 0:
			printf( "taskspec_t accessor method results:\n" );
			test_accessors( &ts );
			free_taskspec_struct( &ts );
			printf( "\n" );
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

	if (test_fp != stdin)
		fclose( test_fp );

	return 0;
}

void test_accessors( taskspec_t *ts )
{
	int_range_t ir;
	double_range_t dr;
	int i, array_len;

	if (ts == NULL)
		return;

	/* determine problem type */
	printf( "Problem type: " );
	if (isEpisodic( ts ) == 1) {
		printf( "episodic\n" );
	} else if (isContinuing( ts ) == 1) {
		printf( "continuing\n" );
	} else if (isOtherType( ts ) == 1) {
		printf( "other\n" );
	} else {
		printf( "ERROR\n" );
	}

	/* observation types and ranges */
	printf( "OBSERVATIONS:\n" );
	array_len = getNumIntObs( ts );
	printf( "  INTS (%d total): ", array_len );
	for (i = 0; i < array_len; i++) {
		ir = getIntObs( ts, i );
		printf( "(%d ", ir.repeat_count );
		
		if (isIntObsMin_special( ts, i ) == 1) {
			if (isIntObsMin_negInf( ts, i ) == 1) {
				printf( "NEGINF " );
			} else if (isIntObsMin_unspec( ts, i ) == 1) {
				printf( "UNSPEC " );
			} else {
				printf( "ERROR " );
			}
		} else {
			/* printf( "%d ", ir.min ); */
			printf( "%d ", getIntObsMin( ts, i ) );
		}

		if (isIntObsMax_special( ts, i ) == 1) {
			if (isIntObsMax_posInf( ts, i ) == 1) {
				printf( "POSINF) " );
			} else if (isIntObsMax_unspec( ts, i ) == 1) {
				printf( "UNSPEC) " );
			} else {
				printf( "ERROR) " );
			}
		} else {
			/* printf( "%d) ", ir.max ); */
			printf( "%d) ", getIntObsMax( ts, i ) );
		}
	}
	
	array_len = getNumDoubleObs( ts );
	printf( "\n  DOUBLES (%d total): ", array_len );
	for (i = 0; i < array_len; i++) {
		dr = getDoubleObs( ts, i );
		printf( "(%d ", dr.repeat_count );
		
		if (isDoubleObsMin_special( ts, i ) == 1) {
			if (isDoubleObsMin_negInf( ts, i ) == 1) {
				printf( "NEGINF " );
			} else if (isDoubleObsMin_unspec( ts, i ) == 1) {
				printf( "UNSPEC " );
			} else {
				printf( "ERROR " );
			}
		} else {
			/* printf( "%g ", dr.min ); */
			printf( "%g ", getDoubleObsMin( ts, i ) );
		}

		if (isDoubleObsMax_special( ts, i ) == 1) {
			if (isDoubleObsMax_posInf( ts, i ) == 1) {
				printf( "POSINF) " );
			} else if (isDoubleObsMax_unspec( ts, i ) == 1) {
				printf( "UNSPEC) " );
			} else {
				printf( "ERROR) " );
			}
		} else {
			/* printf( "%g) ", dr.max ); */
			printf( "%g) ", getDoubleObsMax( ts, i ) );
		}
	}

	printf( "\n  CHARCOUNT: %d", getCharcountObs( ts ) );

	/* action types and ranges */
	printf( "\nACTIONS:\n" );
	array_len = getNumIntAct( ts );
	printf( "  INTS (%d total): ", array_len );
	for (i = 0; i < array_len; i++) {
		ir = getIntAct( ts, i );
		printf( "(%d ", ir.repeat_count );
		
		if (isIntActMin_special( ts, i ) == 1) {
			if (isIntActMin_negInf( ts, i ) == 1) {
				printf( "NEGINF " );
			} else if (isIntActMin_unspec( ts, i ) == 1) {
				printf( "UNSPEC " );
			} else {
				printf( "ERROR " );
			}
		} else {
			/* printf( "%d ", ir.min ); */
			printf( "%d ", getIntActMin( ts, i ) );
		}

		if (isIntActMax_special( ts, i ) == 1) {
			if (isIntActMax_posInf( ts, i ) == 1) {
				printf( "POSINF) " );
			} else if (isIntActMax_unspec( ts, i ) == 1) {
				printf( "UNSPEC) " );
			} else {
				printf( "ERROR) " );
			}
		} else {
			/* printf( "%d) ", ir.max ); */
			printf( "%d) ", getIntActMax( ts, i ) );
		}
	}
	
	array_len = getNumDoubleAct( ts );
	printf( "\n  DOUBLES (%d total): ", array_len );
	for (i = 0; i < array_len; i++) {
		dr = getDoubleAct( ts, i );
		printf( "(%d ", dr.repeat_count );
		
		if (isDoubleActMin_special( ts, i ) == 1) {
			if (isDoubleActMin_negInf( ts, i ) == 1) {
				printf( "NEGINF " );
			} else if (isDoubleActMin_unspec( ts, i ) == 1) {
				printf( "UNSPEC " );
			} else {
				printf( "ERROR " );
			}
		} else {
			/* printf( "%g ", dr.min ); */
			printf( "%g ", getDoubleActMin( ts, i ) );
		}

		if (isDoubleActMax_special( ts, i ) == 1) {
			if (isDoubleActMax_posInf( ts, i ) == 1) {
				printf( "POSINF) " );
			} else if (isDoubleActMax_unspec( ts, i ) == 1) {
				printf( "UNSPEC) " );
			} else {
				printf( "ERROR) " );
			}
		} else {
			/* printf( "%g) ", dr.max ); */
			printf( "%g) ", getDoubleActMax( ts, i ) );
		}
	}

	printf( "\n  CHARCOUNT: %d", getCharcountAct( ts ) );

	/* reward range */
	printf( "\nREWARD: (" );
		
	if (isRewardMin_special( ts ) == 1) {
		if (isRewardMin_negInf( ts ) == 1) {
			printf( "NEGINF " );
		} else if (isRewardMin_unspec( ts ) == 1) {
			printf( "UNSPEC " );
		} else {
			printf( "ERROR " );
		}
	} else {
		printf( "%g ", getRewardMin( ts ) );
	}

	if (isRewardMax_special( ts ) == 1) {
		if (isRewardMax_posInf( ts ) == 1) {
			printf( "POSINF) " );
		} else if (isRewardMax_unspec( ts ) == 1) {
			printf( "UNSPEC) " );
		} else {
			printf( "ERROR) " );
		}
	} else {
		printf( "%g) ", getRewardMax( ts ) );
	}
	printf( "\n" );
}
