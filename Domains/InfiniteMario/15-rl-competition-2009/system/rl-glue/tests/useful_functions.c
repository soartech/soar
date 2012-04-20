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
*  $Revision: 914 $
*  $Date: 2008-10-11 14:09:33 -0400 (Sat, 11 Oct 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue.googlecode.com/svn/trunk/tests/useful_functions.c $
* 
*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "useful_functions.h"

#include <stdlib.h>

void makeKInts(rl_abstract_type_t *theStruct, int numInts){
	set_k_ints_in_abstract_type(theStruct,numInts);
}

void makeKDoubles(rl_abstract_type_t *theStruct, int numDoubles){
	set_k_doubles_in_abstract_type(theStruct,numDoubles);
}
void makeKChars(rl_abstract_type_t *theStruct, int numChars){
	set_k_chars_in_abstract_type(theStruct,numChars);
}

void set_k_ints_in_abstract_type(rl_abstract_type_t *the_struct, int num_ints){
	int i;
	
	the_struct->numInts=num_ints;
	
	if(the_struct->intArray!=0){
		free(the_struct->intArray);
		the_struct->intArray=0;
	}	
	
	if(num_ints==0){
		the_struct->intArray=0;
	}else{
		the_struct->intArray=(int *)calloc(num_ints,sizeof(int));
		for(i=0;i<num_ints;i++) the_struct->intArray[i]=i;
	}
	
}
void set_k_doubles_in_abstract_type(rl_abstract_type_t *the_struct, int num_doubles){
	int i;
	
	the_struct->numDoubles=num_doubles;

	if(the_struct->doubleArray!=0){
		free(the_struct->doubleArray);
		the_struct->doubleArray=0;
	}	

	if(num_doubles==0){
		the_struct->doubleArray=0;
	}else{
		the_struct->doubleArray=(double *)calloc(num_doubles,sizeof(double));
		for(i=0;i<num_doubles;i++) the_struct->doubleArray[i]=(double)i/(double)num_doubles;		
	}
}
void set_k_chars_in_abstract_type(rl_abstract_type_t *the_struct, int num_chars){
	int i;
	
	the_struct->numChars=num_chars;
	if(the_struct->charArray!=0){
		free(the_struct->charArray);
		the_struct->charArray=0;
	}	

	if(num_chars==0){
		the_struct->charArray=0;
	}else{
		the_struct->charArray=(char *)calloc(num_chars,sizeof(char));
		for(i=0;i<num_chars;i++) the_struct->charArray[i]='a'+i;
	}
}





int compare_abstract_types(const rl_abstract_type_t *struct1,const rl_abstract_type_t *struct2){
	int i;
	if(struct1->numInts!=struct2->numInts)return 1;
	if(struct1->numDoubles!=struct2->numDoubles)return 2;
	if(struct1->numChars!=struct2->numChars)return 3;
	
	for(i=0;i<struct1->numInts;i++)
		if(struct1->intArray[i]!=struct2->intArray[i]){
			printf("Index %d, %d != %d\n",i,struct1->intArray[i],struct2->intArray[i]);
			return 4;
		}
	for(i=0;i<struct1->numDoubles;i++)
		if(struct1->doubleArray[i]!=struct2->doubleArray[i])
		return 5;
	for(i=0;i<struct1->numChars;i++)
		if(struct1->charArray[i]!=struct2->charArray[i])
		return 6;
	
	return 0;
}
