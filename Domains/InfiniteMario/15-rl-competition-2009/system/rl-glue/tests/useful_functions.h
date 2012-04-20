/* 
* Copyright (C) 2007, Brian Tanner

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.

* 
*  $Revision: 837 $
*  $Date: 2008-09-15 15:02:45 -0400 (Mon, 15 Sep 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue.googlecode.com/svn/trunk/tests/useful_functions.h $
* 
*/

#ifndef USEFUL_FUNCTIONS_H
#define USEFUL_FUNCTIONS_H

#include <rlglue/RL_common.h>

void copy_structure_to_structure(rl_abstract_type_t *dst, const rl_abstract_type_t *src);
void clean_abstract_type(rl_abstract_type_t *theStruct);
int compare_abstract_types(const rl_abstract_type_t *struct1,const rl_abstract_type_t *struct2);

void set_k_ints_in_abstract_type(rl_abstract_type_t *the_struct, int num_ints);
void set_k_doubles_in_abstract_type(rl_abstract_type_t *the_struct, int num_doubles);
void set_k_chars_in_abstract_type(rl_abstract_type_t *the_struct, int num_chars);

/* Deprecated */
void makeKInts(rl_abstract_type_t *theStruct, int numInts);
void makeKDoubles(rl_abstract_type_t *theStruct, int numDoubles);
void makeKChars(rl_abstract_type_t *theStruct, int numChars);
#endif
