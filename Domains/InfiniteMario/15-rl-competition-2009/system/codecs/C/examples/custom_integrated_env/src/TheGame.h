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
 */
#ifndef THE_GAME_H
#define THE_GAME_H

/*This is a fake-o game that has the same dynamics
as the skeleton_environment but has a very non-rl-glue 
style calling setup. 
*/

/*These are implemented in game.c*/
void game_startup();
void new_game();
void play_one_step(int whichAction);


/*This is implemented in Customized_client_environment.c*/
int setup_rlglue_network();
void teardown_rlglue_network(int theConnection);
void runEnvironmentEventLoop(int theConnection);

#endif
