/* 
* Copyright (C) 2008, Brian Taner

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
*  $Revision: 277 $
*  $Date: 2008-10-01 14:15:48 -0600 (Wed, 01 Oct 2008) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: https://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/src/RL_client_environment.c $
* 
*/

/**
This is a copy of RL_client_environment with a couple 
of specific calls filled in.  To customize the codec, just 
look for places where default calls are happening, like:
env_init, env_start, env_step, etc, etc.. and then write a bit of code to 
emulate the same behavior using whatever project you are hooking into.


All of the inserted code is commented.
All of the previous code in here has been commented out like:*/

/* CUT-FOR-CUSTOMIZATION: env_init();*/



#include <assert.h> /* assert  */
#include <unistd.h> /* sleep   */
#include <string.h> /* strlen */ /* I'm sorry about using strlen. */
#include <stdio.h>  /* fprintf */
#include <stdlib.h> /* calloc, getenv, exit */

#include <ctype.h> /* isdigit */
#include <netdb.h> /* gethostbyname */
#include <arpa/inet.h> /* inet_ntoa */

#include <rlglue/Environment_common.h>
#include <rlglue/network/RL_network.h>

/* Our project specific include */
#include "TheGame.h"
/* Include the utility methods*/
#include <rlglue/utils/C/RLStruct_util.h>

/* State variable for TheGame that is not exposed with function calls */
extern int gameState;

static const char* kUnknownMessage = "Unknown Message: %s\n";

static action_t theAction                 = {0};
static rlBuffer theBuffer                 = {0};
static char* theInMessage = 0;
static unsigned int theInMessageCapacity = 0;

/*Added as a global variable because now we must allocate and fill
up the data structures instead of the environment.*/
static observation_t globalObservation		  = {0};

static void onEnvInit(int theConnection) {
 /* CUT-FOR-CUSTOMIZATION:  char* theTaskSpec = 0;*/
  unsigned int theTaskSpecLength = 0;
  unsigned int offset = 0;

   /* CUT-FOR-CUSTOMIZATION: theTaskSpec = env_init();*/

  /* You could give a real RL-Glue task spec or a custom one*/
  /* TheGame conveniently is the same as SkeletonEnvironment.c,
	 our other example environment*/
  char* theTaskSpec = "2:e:1_[i]_[0,20]:1_[i]_[0,1]:[-1,1]";

  if (theTaskSpec != NULL) {
    theTaskSpecLength = strlen(theTaskSpec);
  }

  /* Prepare the buffer for sending data back to the server */
  rlBufferClear(&theBuffer);
  offset = rlBufferWrite(&theBuffer, offset, &theTaskSpecLength, 1, sizeof(int));
  if (theTaskSpecLength > 0) {
    offset = rlBufferWrite(&theBuffer, offset, theTaskSpec, theTaskSpecLength, sizeof(char));
  }
}

static void onEnvStart(int theConnection) {
   	/* CUT-FOR-CUSTOMIZATION: observation_t globalObservation = {0}; */
  	unsigned int offset = 0;

 	/* CUT-FOR-CUSTOMIZATION: globalObservation=env_start(); */

	/* Call our hook into TheGame to start a new game */
	new_game();
	/* Allocate space to store the observation (gameState)*/
	allocateRLStruct(&globalObservation,1,0,0);

	/* Get the int observation from a global variable we
								extern'd from TheGame.c */
	globalObservation.intArray[0]=gameState;
	__RL_CHECK_STRUCT(&globalObservation)
  rlBufferClear(&theBuffer);
  offset = rlCopyADTToBuffer(&globalObservation, &theBuffer, offset);
}

static void onEnvStep(int theConnection) {
	static reward_observation_terminal_t ro = {0};
	unsigned int offset = 0;
	/* Create an integer variable to hold the action from the agent*/
  	int theIntAction=0;
	ro.terminal=0;
  ro.reward=0;

	offset = rlCopyBufferToADT(&theBuffer, offset, &theAction);
 	__RL_CHECK_STRUCT(&theAction);

	/*I know to only expect 1 integer action*/
	theIntAction=theAction.intArray[0];

	/*This is our hook into TheGame */
	play_one_step(theIntAction);
	
  	/* CUT-FOR-CUSTOMIZATION: ro = env_step(theAction);	*/
  
	/************************** ALL NEW CODE HERE **************************/
	/* Allocate space to store the observation (gameState)*/
	allocateRLStruct(&globalObservation,1,0,0);
	/* Get the int observation from a global variable we
								extern'd from TheGame.c */
	globalObservation.intArray[0]=gameState;

	/* TheGame doesn't know about rewards, and it doesn't
		have a "terminal" flag, so we have to write code
					   to make TheGame fit with RL-Glue*/
	if(gameState==0){
		ro.reward =-1.0;
		ro.terminal=1;
	}

	if(gameState==20){
		ro.reward =1.0;
		ro.terminal=1;
	}

	ro.observation =&globalObservation;

	/************************** NEW CODE OVER **************************/
	
  __RL_CHECK_STRUCT(ro.observation)


  rlBufferClear(&theBuffer);
  offset = 0;
  offset = rlBufferWrite(&theBuffer, offset, &ro.terminal, 1, sizeof(int));
  offset = rlBufferWrite(&theBuffer, offset, &ro.reward, 1, sizeof(double));
  offset = rlCopyADTToBuffer(ro.observation, &theBuffer, offset);
}

static void onEnvCleanup(int theConnection) {
	/*No game specific cleanup to do*/
	/* CUT-FOR-CUSTOMIZATION: env_cleanup();*/

	rlBufferClear(&theBuffer);
	
	/* Clean up globalObservation global we created*/
	clearRLStruct(&globalObservation);

	clearRLStruct(&theAction);
	
	/*It's ok to free null pointers, so this is safe */
	free(theInMessage);
	theInMessage = 0;
	theInMessageCapacity = 0;
}


static void onEnvMessage(int theConnection) {
  unsigned int inMessageLength = 0;
  unsigned int outMessageLength = 0;
  char* inMessage = 0;
  /*We set this to a string constant instead of null*/
  char* outMessage = "sample custom codec integration has no messages!";
  unsigned int offset = 0;

  offset = 0;
  offset = rlBufferRead(&theBuffer, offset, &inMessageLength, 1, sizeof(int));
  if (inMessageLength >= theInMessageCapacity) {
    inMessage = (char*)calloc(inMessageLength+1, sizeof(char));
    free(theInMessage);

    theInMessage = inMessage;
    theInMessageCapacity = inMessageLength;
  }

  if (inMessageLength > 0) {
    offset = rlBufferRead(&theBuffer, offset, theInMessage, inMessageLength, sizeof(char));
  }
/*Make sure to null terminate the string */
   theInMessage[inMessageLength]='\0';

	/* CUT-FOR-CUSTOMIZATION: outMessage = env_message(theInMessage);*/

  if (outMessage != NULL) {
   outMessageLength = strlen(outMessage);
  }

  
  /* we want to start sending, so we're going to reset the offset to 0 so we write the the beginning of the buffer */
  rlBufferClear(&theBuffer);
  offset = 0;
  offset = rlBufferWrite(&theBuffer, offset, &outMessageLength, 1, sizeof(int));
  if (outMessageLength > 0) {
    offset = rlBufferWrite(&theBuffer, offset, outMessage, outMessageLength, sizeof(char));
  }
}

void runEnvironmentEventLoop(int theConnection) {
  int envState = 0;

  do { 
    rlBufferClear(&theBuffer);
    rlRecvBufferData(theConnection, &theBuffer, &envState);

    switch(envState) {
    case kEnvInit:
      onEnvInit(theConnection);
      break;

    case kEnvStart:
      onEnvStart(theConnection);
      break;

    case kEnvStep:
      onEnvStep(theConnection);
      break;

    case kEnvCleanup:
      onEnvCleanup(theConnection);
      break;

    case kEnvMessage:
      onEnvMessage(theConnection);
      break;

    case kRLTerm:
      break;

    default:
      fprintf(stderr, kUnknownMessage, envState);
      exit(0);
      break;
    };

    rlSendBufferData(theConnection, &theBuffer, envState);
  } while (envState != kRLTerm);
}

/*This used to be the main method, I've renamed it and cut a bunch of stuff out*/
int setup_rlglue_network() {
	int theConnection = 0;

	char* host = kLocalHost;
	short port = kDefaultPort;

	printf("RL-Glue sample env custom codec integration.\n");

	/* Allocate what should be plenty of space for the buffer - it will dynamically resize if it is too small */
	rlBufferCreate(&theBuffer, 4096);

	theConnection = rlWaitForConnection(host, port, kRetryTimeout);

	printf("\tSample custom env codec :: Connected\n");
	rlBufferClear(&theBuffer);
	rlSendBufferData(theConnection, &theBuffer, kEnvironmentConnection);

	return theConnection;
}

void teardown_rlglue_network(int theConnection){
	rlClose(theConnection);
	rlBufferDestroy(&theBuffer);
}
