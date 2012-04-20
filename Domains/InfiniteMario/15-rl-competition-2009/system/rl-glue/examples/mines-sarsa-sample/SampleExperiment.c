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


	*  $Revision: 1007 $
	*  $Date: 2009-02-09 17:30:05 -0500 (Mon, 09 Feb 2009) $
	*  $Author: brian@tannerpages.com $
	*  $HeadURL: http://rl-library.googlecode.com/svn/trunk/projects/packages/examples/mines-sarsa-c-directcompile/SampleExperiment.c $

 */

#include <stdio.h>	/* for printf */
#include <math.h> /* for sqrt */
#include <rlglue/RL_glue.h> /* RL_ function prototypes and RL-Glue types */


/*
* Experiment program that does some of the things that might be important when
* running an experiment.  It runs an agent on the environment and periodically
* asks the agent to "freeze learning": to stop updating its policy for a number
* of episodes in order to get an estimate of the quality of what has been learned
* so far.
*
* The experiment estimates statistics such as the mean and standard deviation of
* the return gathered by the policy and writes those to a comma-separated value file
* called results.csv.
*
* This experiment also shows off some other features that can be achieved easily
* through the RL-Glue env/agent messaging system by freezing learning (described
* above), having the environment start in specific starting states, and saving
* and loading the agent's value function to/from a binary data file.
*/
typedef struct {
	double mean;
	double standard_dev;
} evaluation_point_t;

evaluation_point_t *evaluate_agent();
void single_evaluation();
void print_score(int afterEpisodes, evaluation_point_t *the_score);
void save_results_csv(evaluation_point_t *the_score[], char *fileName);
void offline_demo();



int main(int argc, char *argv[]) {
	printf("Starting offline demo\n----------------------------\nWill alternate learning for 25 episodes, then freeze policy and evaluate for 10 episodes.\n\n");
	printf("After Episode\tMean Return\tStandard Deviation\n-------------------------------------------------------------------------\n");
	RL_init();
	offline_demo();
	
	
	printf("\nNow we will save the agent's learned value function to a file....\n");

	RL_agent_message("save_policy results.dat");

	printf("\nCalling RL_cleanup and RL_init to clear the agent's memory...\n");

	RL_cleanup();
	RL_init();


	printf("Evaluating the agent's default policy:\n\t\tMean Return\tStandardDeviation\n------------------------------------------------------\n");
	single_evaluation();
	
	printf("\nLoading up the value function we saved earlier.\n");
	RL_agent_message("load_policy results.dat");

	printf("Evaluating the agent after loading the value function:\n\t\tMean Return\tStandardDeviation\n------------------------------------------------------\n");
	single_evaluation();

	printf("Telling the environment to use fixed start state of 2,3.\n");
	RL_env_message("set-start-state 2 3");
	RL_start();
	printf("Telling the environment to print the current state to the screen.\n");
	RL_env_message("print-state");
    printf("Evaluating the agent a few times from a fixed start state of 2,3:\n\t\tMean Return\tStandardDeviation\n-------------------------------------------\n");
	single_evaluation();

	printf("Evaluating the agent again with the random start state:\n\t\tMean Return\tStandardDeviation\n-----------------------------------------------------\n");
    RL_env_message("set-random-start-state");
	single_evaluation();


	RL_cleanup();
	printf("\nProgram Complete.\n");

	return 0;
}


void save_result_csv(evaluation_point_t *the_score[], char *fileName){
	FILE *fp;
	int i=0;

	fp=fopen(fileName, "w");
	fprintf(fp, "#Results from SampleExperiment.c.  First line is means, second line is standard deviations.\n");
	
	for(i=0;i<21;i++){
		fprintf(fp,"%.2f,",the_score[i]->mean);
	}
	fprintf(fp,"\n");
	
	for(i=0;i<21;i++){
		fprintf(fp,"%.2f,",the_score[i]->standard_dev);
	}
	fprintf(fp,"\n");

	fclose(fp);
}

/*
	This function will freeze the agent's policy and test it after every 25 episodes.
*/
void offline_demo(){
	int i=0;
	int j=0;
	evaluation_point_t *this_score=0;
	evaluation_point_t *statistics[21];
	
	this_score=evaluate_agent();
	print_score(0,this_score);
	statistics[0]=this_score;
	
	for(i=0;i<20;i++){
		for(j=0;j<25;j++){
			RL_episode(0);
		}
		this_score=evaluate_agent();
		print_score((i+1)*25,this_score);
		statistics[i+1]=this_score;
	}
	
	save_result_csv(statistics,"results.csv");
	
	for(i=0;i<21;i++){
		free(statistics[i]);
	}
	
}



/**
 * Tell the agent to stop learning, then execute n episodes with his current
 * policy.  Estimate the mean and variance of the return over these episodes.
 */
evaluation_point_t *evaluate_agent(){
	int i=0;
	double sum=0;
	double sum_of_squares=0;
	double this_return=0;
	double mean;
	double variance;
	int n=10;
	evaluation_point_t *eval_point=0;
	
	RL_agent_message("freeze learning");
	for(i=0;i<n;i++){
		/* We use a cutoff here in case the policy is bad
		   and will never end an episode */
		RL_episode(5000);
		this_return=RL_return();
		sum+=this_return;
		sum_of_squares+=this_return*this_return;
	}
	
	mean=sum/(double)n;
	variance = (sum_of_squares - (double)n*mean*mean)/((double)n - 1.0f);
	eval_point=(evaluation_point_t *)malloc(sizeof(evaluation_point_t));
	eval_point->mean=mean;
	eval_point->standard_dev=sqrt(variance);

	RL_agent_message("unfreeze learning");
	return eval_point;
}

/**
* Just do a single evaluate_agent and print it
**/
void single_evaluation(){
	evaluation_point_t *this_score=0;
	this_score=evaluate_agent();
	print_score(0,this_score);
	free(this_score);
}


void print_score(int afterEpisodes, evaluation_point_t *the_score) {
    printf("%d\t\t%.2f\t\t%.2f\n", afterEpisodes, the_score->mean, the_score->standard_dev);
}

