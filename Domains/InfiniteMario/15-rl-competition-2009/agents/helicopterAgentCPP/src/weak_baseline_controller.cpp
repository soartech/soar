////////////////////////////////////////////////////////////////////////////////
// Authors: Pieter Abbeel, Adam Coates, Andrew Y. Ng --- Stanford University ///
////////////////////////////////////////////////////////////////////////////////

// --
// Updated for Reinforcement Learning Competition 2009 by Chris Rayner,
// University of Alberta.   http://www.rl-competition.org
// --

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <rlglue/utils/C/TaskSpec_Parser.h>

#include "weak_baseline_controller.h"

action_t action;
int timestep;

std::string const agentName = "Weak Baseline Controller";

/**
 * Output a text representation of observation 'o' for debugging purposes.
 */
void print_observation (observation_t * o) {
    std::cout << "Observation = [";
    for (int i = 0; i < 12; ++ i)
        std::cout << o->doubleArray[i] << ",";
    std::cout << "]" << std::endl;
}

/**
 * Initializes agent to a naive (pre-learning) state. The task_spec, if given,
 * is a description on the environment's i/o interface according to a standard
 * description language.  ...  The agent may ignore the task_specification.
 * -- http://rlai.cs.ualberta.ca/RLBB/RL-Framework-concepts.html
 */
void agent_init (const char * task_spec) {  

    // TODO: repair this task spec parsing---once it's working for CPP
    //task_spec_struct tss;
    //parse_task_spec(task_spec, & tss);

    // allocate memory for a single action
    action.numInts    = 0; // tss.num_discrete_action_dims;
    action.numDoubles = 4; // tss.num_continuous_action_dims;

    if (action.numInts)
        action.intArray = (int*)malloc(sizeof(int)*action.numInts);
    if (action.numDoubles)
        action.doubleArray = (double*)malloc(sizeof(double)*action.numDoubles);

    if (action.numInts != 0 || action.numDoubles != 4) {
        std::cerr << agentName << ": unexpected action structure in task_spec."
            << std::endl
            << "Expected: numInts = 0, numDoubles = 4; received: "
            << "numInts = "    << action.numInts    << ", "
            << "numDoubles = " << action.numDoubles << "." << std::endl
            << "Exiting ..." << std::endl;
        exit(0);
    }

}

/**
 * Return a first action for the agent based on the supplied observation.  You
 * may also choose to reset variables (trace values, timestep counters, etc.)
 * so that subsequent episodic runs are "clean."
 */
const action_t * agent_start (const observation_t * o) {
    timestep = 0;
    agent_policy(o, action);
    return & action;
}

/**
 * Return an action for the agent based on the supplied observation---i.e.,
 * perform one step.
 */
const action_t * agent_step (double reward, const observation_t * o) {
    ++ timestep;
    agent_policy(o, action);
    return & action;	
}

/**
 * The final step of the episode (for episodic environments).
 */
void agent_end (double reward) { }

/**
 * If memory or other resources are allocated when the agent is initialized,
 * then the following routine can be used to release them.
 * -- http://rlai.cs.ualberta.ca/RLBB/RL-Framework-concepts.html
 */
void agent_cleanup () {
    // free all the memory
    free(action.doubleArray);
    // clear all values in the actions (TODO: necessary?)
    action.numInts     = 0;
    action.numDoubles  = 0;
    action.intArray    = 0;
    action.doubleArray = 0;
}

/**
 * A call to agent_freeze should halt any learning the agent is doing, as well
 * as remove any randomness from the agent's policy. Agent_freeze is around to
 * allow for training and testing phases. Typically an agent will train for
 * some period, freeze it's value function and policy, and then "test" by
 * running the agent through the environment and gathering results.
 * -- http://rlai.cs.ualberta.ca/RLBB/QuickStart.html
 */
void agent_freeze () { }

/**
 * Send the agent a message.  A "free-form" communication line between the
 * environment and the agent.  This example agent does not currently implement
 * any messages.
 */
const char * agent_message (const char * message) {
    return (agentName + "does not respond to any messages.").c_str();
}

/**
 * Modifies "action" via reference based on a static policy that maps
 * observations to actions.
 */
void agent_policy (const observation_t * o, action_t & a) {

    const double weights[12] = {0.0196, 0.7475, 0.0367, 0.0185,
        0.7904, 0.0322, 0.1969, 0.0513, 0.1348, 0.02, 0, 0.23};

    const int y_w = 0;
    const int roll_w = 1;
    const int v_w = 2;
    const int x_w = 3;
    const int pitch_w = 4;
    const int u_w = 5;
    const int yaw_w = 6;
    const int z_w = 7;
    const int w_w = 8;
    const int ail_trim = 9;
    const int el_trim = 10;
    const int coll_trim = 11;

    //x/y/z_error = body(x - x_target)
    //q_error = inverse(Q_target) * Q, where Q is the orientation of the helicopter
    //roll/pitch/yaw_error = scaled_axis(q_error)

    // collective control
    double coll = weights[z_w] * o->doubleArray[z_err] +
        weights[w_w] * o->doubleArray[w_err] +
        weights[coll_trim];

    // forward-backward control
    double elevator =  -weights[x_w] * o->doubleArray[x_err] +
        -weights[u_w] * o->doubleArray[u_err] +
        weights[pitch_w] * o->doubleArray[qy_err] +
        weights[el_trim];

    // left-right control
    double aileron =
        -weights[y_w] * o->doubleArray[y_err] +
        -weights[v_w] * o->doubleArray[v_err] +
        -weights[roll_w] * o->doubleArray[qx_err] +
        weights[ail_trim];

    double rudder =
        -weights[yaw_w] * o->doubleArray[qz_err];

    a.doubleArray[0] = aileron;
    a.doubleArray[1] = elevator;
    a.doubleArray[2] = rudder;
    a.doubleArray[3] = coll;
}

