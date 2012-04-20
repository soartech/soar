/* 
* Copyright (C) 2007, Andrew Butcher

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
*  $Revision: 574 $
*  $Date: 2009-02-03 11:23:41 -0500 (Tue, 03 Feb 2009) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/src/RL_client_experiment.c $
* 
*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h> /* calloc */
#include <string.h> /* strlen */
#include <stdio.h>  /* fprintf */
#include <assert.h> /* assert */

#include <ctype.h> /* isdigit */
#include <netdb.h> /* gethostbyname */
#include <arpa/inet.h> /* inet_ntoa */

#include <rlglue/RL_glue.h>
#include <rlglue/network/RL_network.h>
/* Include the utility methods*/
#include <rlglue/utils/C/RLStruct_util.h>

/* Gives us __rlglue_get_codec_svn_version */
#include <RL_codec_common.h>

int theExperimentConnection = 0;

observation_t clientexp_observation       = {0};
action_t clientexp_action                 = {0};
rlBuffer clientexp_rlbuffer               = {0};

char* clientexp_message = 0;
unsigned int clientexp_messagecapacity = 0;

void cleanupExperimentAtExit(void)
{
  rlBufferDestroy(&clientexp_rlbuffer);
}

static void forceConnection()
{
  struct hostent *host_ent;

  char* host = kLocalHost;
  short port = kDefaultPort;
  char* envptr = 0;

  if (theExperimentConnection == 0) {
    host = getenv("RLGLUE_HOST");
    if (host == 0) {
      host = kLocalHost;
    }

    envptr = getenv("RLGLUE_PORT");  
    if (envptr != 0) {
      port = strtol(envptr, 0, 10);
      if (port == 0) {
	port = kDefaultPort;
      }
    }
    
	if (isalpha(host[0])) {
		/*This method is apparently deprecated, we should update at some point*/
		host_ent = gethostbyname(host); 
		if(host_ent==0){
			fprintf(stderr,"Couldn't find IP address for host: %s\n",host);
			exit(55);
		}
	  	host = inet_ntoa(*(struct in_addr*)host_ent->h_addr_list[0]);
	}

  	fprintf(stdout, "RL-Glue C Experiment Codec Version %s, Build %s\n\tConnecting to host=%s on port=%d...\n", VERSION,__rlglue_get_codec_svn_version(),host, port);
	fflush(stdout);
    theExperimentConnection = rlWaitForConnection(host, port, kRetryTimeout);
	fprintf(stdout, "\tRL-Glue C Experiment Codec :: Connected\n");
    /* Send the connection type */
    atexit(cleanupExperimentAtExit);
    rlBufferCreate(&clientexp_rlbuffer, 65536);
    rlBufferClear(&clientexp_rlbuffer);
    rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, kExperimentConnection);
  }
}

const char* RL_init() {
  unsigned int offset=0;
  unsigned int messageLength=0;
  int experimentState = kRLInit;

  forceConnection();

  /* Remote call RL_init */
  rlBufferClear(&clientexp_rlbuffer);
  rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);

  /* Recv back a reply from RL_init */
  rlBufferClear(&clientexp_rlbuffer);
  rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState);
  assert(experimentState == kRLInit);

 /* Brian added Sept 8 so that RL_init returns the task spec */
 /* We'll reuse messageLength and clientexp_message from Agent_message*/
  offset = rlBufferRead(&clientexp_rlbuffer, offset, &messageLength, 1, sizeof(unsigned int));
  if (messageLength >= clientexp_messagecapacity) {
	if(clientexp_message!=0){
    	free(clientexp_message);
		clientexp_message=0;
	}	

    clientexp_message = (char*)calloc(messageLength+1, sizeof(char));
    clientexp_messagecapacity = messageLength;
  }

  if (messageLength > 0) {
    offset = rlBufferRead(&clientexp_rlbuffer, offset, clientexp_message, messageLength, sizeof(char));
  }
  /*Need to move this outside of the if statement, so that we get null termination for empty messages*/
  clientexp_message[messageLength] = '\0';

  return clientexp_message;
}

const observation_action_t *RL_start() {
  int experimentState = kRLStart;
  static observation_action_t oa = { 0,0};
  unsigned int offset = 0;

  assert(theExperimentConnection != 0);

  rlBufferClear(&clientexp_rlbuffer);
  rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);

  rlBufferClear(&clientexp_rlbuffer);
  rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState); 
  assert(experimentState == kRLStart);

  offset = rlCopyBufferToADT(&clientexp_rlbuffer, offset, &clientexp_observation);
  offset = rlCopyBufferToADT(&clientexp_rlbuffer, offset, &clientexp_action);
	__RL_CHECK_STRUCT(&clientexp_observation)
	__RL_CHECK_STRUCT(&clientexp_action)

  oa.observation = &clientexp_observation;
  oa.action = &clientexp_action;

  return &oa;
}

const reward_observation_action_terminal_t* RL_step() {
  int experimentState = kRLStep;
  static reward_observation_action_terminal_t roat = {0, 0,0, 0};
  unsigned int offset = 0;
  
  assert(theExperimentConnection != 0);

  rlBufferClear(&clientexp_rlbuffer);
  rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);

  /* Recv Data from Server */
  rlBufferClear(&clientexp_rlbuffer);
  rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState);
  assert(experimentState == kRLStep);

  offset = rlBufferRead(&clientexp_rlbuffer, offset, &roat.terminal, 1, sizeof(int));
  offset = rlBufferRead(&clientexp_rlbuffer, offset, &roat.reward, 1, sizeof(double));
  offset = rlCopyBufferToADT(&clientexp_rlbuffer, offset, &clientexp_observation);
  offset = rlCopyBufferToADT(&clientexp_rlbuffer, offset, &clientexp_action);
	__RL_CHECK_STRUCT(&clientexp_observation)
	__RL_CHECK_STRUCT(&clientexp_action)

  roat.observation = &clientexp_observation;
  roat.action = &clientexp_action;

  return &roat;
}

void RL_cleanup() {
	int experimentState = kRLCleanup;

	assert(theExperimentConnection != 0);

	rlBufferClear(&clientexp_rlbuffer);
	rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);

	rlBufferClear(&clientexp_rlbuffer);
	rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState);
	assert(experimentState == kRLCleanup);

	clearRLStruct(&clientexp_observation);
	clearRLStruct(&clientexp_action);

	/*safe even if it is null */
	free(clientexp_message);
	clientexp_message = 0;

	clientexp_messagecapacity = 0;
}

double RL_return() {
  int experimentState = kRLReturn;
  double theReward = 0;
  unsigned int offset = 0;

  assert(theExperimentConnection != 0);

  rlBufferClear(&clientexp_rlbuffer);
  rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);

  rlBufferClear(&clientexp_rlbuffer);
  rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState);
  assert(experimentState == kRLReturn);

  offset = rlBufferRead(&clientexp_rlbuffer, offset, &theReward, 1, sizeof(double));

  return theReward;
}

int RL_num_steps() {
  int experimentState = kRLNumSteps;
  int numSteps = 0;
  unsigned int offset = 0;

  assert(theExperimentConnection != 0);

  rlBufferClear(&clientexp_rlbuffer);
  rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);

  rlBufferClear(&clientexp_rlbuffer);
  rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState);
  assert(experimentState == kRLNumSteps);

  offset = rlBufferRead(&clientexp_rlbuffer, offset, &numSteps, 1, sizeof(int));

  return numSteps;
}


const char* RL_agent_message(const char* message) {
  int experimentState = kRLAgentMessage;
  unsigned int messageLength = 0;
  unsigned int offset = 0;

  if (message != 0)
    messageLength = strlen(message);

  forceConnection();

  rlBufferClear(&clientexp_rlbuffer);
  offset = 0;
  offset = rlBufferWrite(&clientexp_rlbuffer, offset, &messageLength, 1, sizeof(int));
  if (messageLength > 0) {
    offset = rlBufferWrite(&clientexp_rlbuffer, offset, message, messageLength, sizeof(char));
  }
  rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);
  
  offset = 0;
  rlBufferClear(&clientexp_rlbuffer);
  rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState);
  assert(experimentState == kRLAgentMessage);

  offset = rlBufferRead(&clientexp_rlbuffer, offset, &messageLength, 1, sizeof(int));
  /* Sept 12 2008 made this >= instead of > so that we'd at least have size 1 */
  if (messageLength >= clientexp_messagecapacity) {
    if(clientexp_message!=0){
    	free(clientexp_message);
		clientexp_message=0;
	}	
    clientexp_message = (char*)calloc(messageLength+1, sizeof(char));
    clientexp_messagecapacity = messageLength;
  }

  if (messageLength > 0) {
    offset = rlBufferRead(&clientexp_rlbuffer, offset, clientexp_message, messageLength, sizeof(char));
  }
  /* Sept 12 2008 moved this out of the if statement so we actually null terminate at the right place if we get a "" message */
  clientexp_message[messageLength] = '\0';

  return clientexp_message;
}


const char* RL_env_message(const char *message) {
	int experimentState = kRLEnvMessage;
	unsigned int messageLength = 0;
	unsigned int offset = 0;

	if (message != 0){
		messageLength = strlen(message);
	}
	forceConnection();

	rlBufferClear(&clientexp_rlbuffer);
	offset = 0;
	offset = rlBufferWrite(&clientexp_rlbuffer, offset, &messageLength, 1, sizeof(int));
	if (messageLength > 0) {
		offset = rlBufferWrite(&clientexp_rlbuffer, offset, message, messageLength, sizeof(char));
	}
	rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);

	rlBufferClear(&clientexp_rlbuffer);
	rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState);
	assert(experimentState == kRLEnvMessage);

	offset = 0;
	offset = rlBufferRead(&clientexp_rlbuffer, offset, &messageLength, 1, sizeof(int));

	/* Sept 12 2008 made this >= instead of > so that we'd at least have size 1 */
	if (messageLength >= clientexp_messagecapacity) {
		if(clientexp_message!=0){
			free(clientexp_message);
			clientexp_message=0;
		}	
		clientexp_message = (char*)calloc(messageLength+1, sizeof(char));
		clientexp_messagecapacity = messageLength;
	}

	if (messageLength > 0) {
		offset = rlBufferRead(&clientexp_rlbuffer, offset, clientexp_message, messageLength, sizeof(char));
	}
	/* Sept 12 2008 moved this out of the if statement so we actually null terminate at the right place if we get a "" message */
	clientexp_message[messageLength] = '\0';

	return clientexp_message;
}

int RL_num_episodes() {
	int experimentState = kRLNumEpisodes;
	int numEpisodes = 0;
	unsigned int offset = 0;

	assert(theExperimentConnection != 0);

	rlBufferClear(&clientexp_rlbuffer);
	rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);

	rlBufferClear(&clientexp_rlbuffer);
	rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState);
	assert(experimentState == kRLNumEpisodes);

	offset = rlBufferRead(&clientexp_rlbuffer, offset, &numEpisodes, 1, sizeof(int));

	return numEpisodes;
}

int RL_episode(unsigned int numSteps) {
	int terminal=0;
	unsigned int offset = 0;
	int experimentState = kRLEpisode;

	assert(theExperimentConnection != 0);

	rlBufferClear(&clientexp_rlbuffer);
	offset = 0;
	offset = rlBufferWrite(&clientexp_rlbuffer, offset, &numSteps, 1, sizeof(int));
	rlSendBufferData(theExperimentConnection, &clientexp_rlbuffer, experimentState);

	rlBufferClear(&clientexp_rlbuffer);
	/*Brian Sept 8 2008 :: Not really sure if I should be resetting offset to 0 here.  Seems to work as is*/
	offset=0;
	rlRecvBufferData(theExperimentConnection, &clientexp_rlbuffer, &experimentState);
	offset = rlBufferRead(&clientexp_rlbuffer, offset, &terminal, 1, sizeof(int));
	assert(experimentState == kRLEpisode);
	return terminal;
}

