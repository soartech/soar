////////////////////////////////////////////////////////////////////////////////
// Authors: Pieter Abbeel, Adam Coates, Andrew Y. Ng --- Stanford University ///
////////////////////////////////////////////////////////////////////////////////

// --
// Updated for Reinforcement Learning Competition 2009 by Chris Rayner,
// University of Alberta.   http://www.rl-competition.org
// --

#ifndef WEAK_BASELINE_CONTROLLER_H_
#define WEAK_BASELINE_CONTROLLER_H_

#include <rlglue/RL_common.h>
#include <rlglue/Agent_common.h>

/**
 * Modifies "action" via reference based on a static policy that maps
 * observations to actions.
 */
void agent_policy(const observation_t * o, action_t & action);

// Indices into observation_t.doubleArray...
const unsigned int u_err = 0,   // forward velocity
      v_err = 1,   // sideways velocity
      w_err = 2,   // downward velocity
      x_err = 3,   // forward error
      y_err = 4,   // sideways error
      z_err = 5,   // downward error
      p_err = 6,   // angular rate around forward axis
      q_err = 7,   // angular rate around sideways (to the right) axis
      r_err = 8,   // angular rate around vertical (downward) axis
      qx_err = 9,  // <-- quaternion entries, x,y,z,w   q = [ sin(theta/2) * axis; cos(theta/2)],
      qy_err = 10, // where axis = axis of rotation; theta is amount of rotation around that axis
      qz_err = 11; // [recall: any rotation can be represented by a single rotation around some axis]

#endif // #ifndef WEAK_BASELINE_CONTROLLER_H_

