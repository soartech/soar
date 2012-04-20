/* console Trainer for RL Competition
* Copyright (C) 2007, Brian Tanner brian@tannerpages.com (http://brian.tannerpages.com/)
* 
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public License
* as published by the Free Software Foundation; either version 2
* of the License, or (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. */
	#ifndef CONSOLETRAINERHELPER_H
	#define CONSOLETRAINERHELPER_H
	
	#include <string>
	class ParameterHolder;
	
	
	void load(std::string envNameString, ParameterHolder *theParams);
	ParameterHolder *preload(std::string envNameString);

	void preloadAndLoad(std::string envNameString);

	/*
	* Tetris has an integer parameter called pnum that takes values in [0,9]
	* Setting this parameter changes the exact tetris problem you are solving
	*/
	void loadTetris(int whichParamSet);

	/*
	* Acrobot has an integer parameter called pnum that takes values in [0,39]
	* Setting this parameter changes the exact acrobot problem you are solving
	*/
	void loadAcrobot(int whichParamSet);	

	/*
	* Octopus has no parameters
	*/
	void loadOctopus();	

	/*
	* Mario has 4 parameters! 
	*   levelSeed - determines randomization of level
	*   levelType - 0..2 outdoors, subterranean, other
	*   levelDifficylty - 0..10 determines how hard it gets!
	*   instance - 0..9 provides different (hidden) parameterizations
	*/
        void loadMario(int levelSeed, int levelType, int levelDifficulty, int instance);

	/*
	* Helicopter has no user controllable parameters
	*/
	void loadHelicopter(int whichParamSet);	

        void loadPolyathlon(int whichParamSet);

#endif
