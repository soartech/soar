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
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/C/src/RL_client_agent.c $
* 
*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <assert.h> /* assert  */
#include <stdlib.h> /* malloc, exit */
#include <stdio.h>  /* fprintf */
#include <unistd.h> /* sleep   */
#include <string.h> /* strlen */

#include <ctype.h> /* isdigit */
#include <netdb.h> /* gethostbyname */
#include <arpa/inet.h> /* inet_ntoa */

#include <rlglue/Agent_common.h>
#include <rlglue/network/RL_network.h>
/* Include the utility methods*/
#include <rlglue/utils/C/RLStruct_util.h>

/* Gives us __rlglue_get_codec_svn_version */
#include <RL_codec_common.h>

#define DEBUGAGENT 1
/*
This is the core of the agent C codec. It is very similar to RL_client_environment.c, so
it may be slightly more sparsely commented than that file */

static const char* kUnknownMessage = "Unknown Message: %d\n";

static char* theTaskSpec = 0;
static observation_t clientagent_observation={0};
static rlBuffer theBuffer = {0};
static char *clientagent_inmessage = 0;
static unsigned int clientagent_inmessagecapacity = 0;

static void onAgentInit(int theConnection) {
	unsigned int theTaskSpecLength = 0;
	unsigned int offset = 0;

	/* Read the data in the buffer (data from server) */
	offset = rlBufferRead(&theBuffer, offset, &theTaskSpecLength, 1, sizeof(int));
	if (theTaskSpecLength > 0) {
		theTaskSpec = (char*)calloc(theTaskSpecLength+1, sizeof(char));
		offset = rlBufferRead(&theBuffer, offset, theTaskSpec, theTaskSpecLength, sizeof(char));
		/*Make sure the string is null terminated */
		theTaskSpec[theTaskSpecLength]='\0';
	}

	/* Call RL method on the recv'd data */
	agent_init(theTaskSpec);

	/* Prepare the buffer for sending data back to the server */
	rlBufferClear(&theBuffer);
}

static void onAgentStart(int theConnection) {
	const action_t *theAction;
	unsigned int offset = 0;

	/* Read the data in the buffer (data from server) */
	offset = rlCopyBufferToADT(&theBuffer, offset, &clientagent_observation);
	__RL_CHECK_STRUCT(&clientagent_observation)

	/* Call RL method on the recv'd data */
	theAction = agent_start(&clientagent_observation);
	__RL_CHECK_STRUCT(theAction)

	/* Prepare the buffer for sending data back to the server */
	rlBufferClear(&theBuffer);
	offset = 0;
	offset = rlCopyADTToBuffer(theAction, &theBuffer, offset);

}

static void onAgentStep(int theConnection) {
	double theReward = 0;
	const action_t *theAction;
	unsigned int offset = 0;

	/* Read the data in the buffer (data from server) */
	offset = rlBufferRead(&theBuffer, offset, &theReward, 1, sizeof(theReward));
	offset = rlCopyBufferToADT(&theBuffer, offset, &clientagent_observation);
	__RL_CHECK_STRUCT(&clientagent_observation)

	/* Call RL method on the recv'd data */
	theAction = agent_step(theReward, &clientagent_observation);
	__RL_CHECK_STRUCT(theAction)

	/* Prepare the buffer for sending data back to the server */
	rlBufferClear(&theBuffer);
	offset = 0;

	rlCopyADTToBuffer(theAction, &theBuffer, offset);
}

static void onAgentEnd(int theConnection) {
	double theReward = 0;

	/* Read the data in the buffer (data from server) */
	rlBufferRead(&theBuffer, 0, &theReward, 1, sizeof(double));

	/* Call RL method on the recv'd data */
	agent_end(theReward);

	/* Prepare the buffer for sending data back to the server */
	rlBufferClear(&theBuffer);
}

static void onAgentCleanup(int theConnection) {
	/* Read the data in the buffer (data from server) */
	/* No data sent for agent cleanup */

	/* Call RL method on the recv'd data */
	agent_cleanup();

	/* Prepare the buffer for sending data back to the server */
	rlBufferClear(&theBuffer);

	/* Cleanup our resources */
	clearRLStruct(&clientagent_observation);
	free(theTaskSpec);
	free(clientagent_inmessage);

	theTaskSpec = 0;
	clientagent_inmessage = 0;
	clientagent_inmessagecapacity = 0;
}


static void onAgentMessage(int theConnection) {
	unsigned int inMessageLength = 0;
	unsigned int outMessageLength = 0;
	char* inMessage = 0;
	const char* outMessage = 0;
	unsigned int offset = 0;

	/* Read the data in the buffer (data from server) */
	offset = 0;
	offset = rlBufferRead(&theBuffer, offset, &inMessageLength, 1, sizeof(int));

	if (inMessageLength >= clientagent_inmessagecapacity) {
		inMessage = (char*)calloc(inMessageLength+1, sizeof(char));
		free(clientagent_inmessage);

		clientagent_inmessage = inMessage;
		clientagent_inmessagecapacity = inMessageLength;
	}

	if (inMessageLength > 0) {
		offset = rlBufferRead(&theBuffer, offset, clientagent_inmessage, inMessageLength, sizeof(char));
	}
	/*Make sure to null terminate the string */
	 clientagent_inmessage[inMessageLength]='\0';

	/* Call RL method on the recv'd data */
	outMessage = agent_message(clientagent_inmessage);
	if (outMessage != NULL) {
		outMessageLength = strlen(outMessage);
	}

	/* Prepare the buffer for sending data back to the server */
	/* we want to start sending, so we're going to reset the offset to 0 so we write to the beginning of the buffer */
	rlBufferClear(&theBuffer);
	offset = 0;

	offset = rlBufferWrite(&theBuffer, offset, &outMessageLength, 1, sizeof(int)); 
	if (outMessageLength > 0) {
		offset = rlBufferWrite(&theBuffer, offset, outMessage, outMessageLength, sizeof(char));
	}
}

static void runAgentEventLoop(int theConnection) {
  int agentState = 0;

  do {
    rlBufferClear(&theBuffer);
    rlRecvBufferData(theConnection, &theBuffer, &agentState);

    switch(agentState) {
    case kAgentInit:
      onAgentInit(theConnection);
      break;

    case kAgentStart:
      onAgentStart(theConnection);
      break;

    case kAgentStep:
      onAgentStep(theConnection);
      break;

    case kAgentEnd:
      onAgentEnd(theConnection);
      break;

    case kAgentCleanup:
      onAgentCleanup(theConnection);
      break;

    case kAgentMessage:
      onAgentMessage(theConnection);
      break;

    case kRLTerm:
      break;
    
    default:
      fprintf(stderr, kUnknownMessage, agentState);
      exit(0);
      break;
    };

    rlSendBufferData(theConnection, &theBuffer, agentState);
  } while (agentState != kRLTerm);
}


int main(int argc, char** argv) {
	int theConnection = 0;

	const char *usage = "The following environment variables are used by the agent to control its function:\n"
	  	"RLGLUE_HOST  : If set the agent will use this ip or hostname to connect to rather than %s\n"
		"RLGLUE_PORT  : If set the agent will use this port to connect on rather than %d\n";

	struct hostent *host_ent;

	char* host = kLocalHost;
	short port = kDefaultPort;

	char* envptr = 0;

	if (argc > 1) {
		fprintf(stderr, usage, kLocalHost, kDefaultPort);
		exit(1);
	}

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

	fprintf(stdout, "RL-Glue C Agent Codec Version %s, Build %s\n\tConnecting to host=%s on port=%d...\n", VERSION,__rlglue_get_codec_svn_version(),host, port);
	fflush(stdout);

/* Allocate what should be plenty of space for the buffer - it will dynamically resize if it is too small */
	rlBufferCreate(&theBuffer, 4096);

	theConnection = rlWaitForConnection(host, port, kRetryTimeout);
	fprintf(stdout, "\tRL-Glue C Agent Codec :: Connected\n");
	rlBufferClear(&theBuffer);
	rlSendBufferData(theConnection, &theBuffer, kAgentConnection);
	runAgentEventLoop(theConnection);
	rlClose(theConnection);

	rlBufferDestroy(&theBuffer);

	return 0;
}
