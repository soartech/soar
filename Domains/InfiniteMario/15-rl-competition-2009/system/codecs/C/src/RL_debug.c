/* 
* Copyright (C) 2008, Brian Tanner

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
*  $Revision: 576 $
*  $Date: 2009-02-03 17:34:44 -0500 (Tue, 03 Feb 2009) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/src/RL_debug.c $
* 
*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <assert.h>
#include <string.h>
#include <rlglue/RL_common.h>
#include <stdio.h>

/*This is an easier trick to get the version */
static char svnVersionCodecString[1024];
char* __rlglue_get_codec_svn_version(){
	int howMuchToCopy=0;
	char *theVersion="$Revision: 576 $";
	howMuchToCopy=strlen(theVersion+11) - 2;
	assert(howMuchToCopy>0);
	memcpy(svnVersionCodecString,  theVersion+11, howMuchToCopy);
    svnVersionCodecString[howMuchToCopy] = '\0';
	return svnVersionCodecString;
}
