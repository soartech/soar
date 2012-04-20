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

#include "consoleTrainerHelper.h"

#include <sstream>
#include "ParameterHolder.h"
#include <rlglue/RL_glue.h>
#include <cstring>

#define BENCHMARK 0
#define ENVSHELL 1
#define LISTQUERY 1
#define NOVALUE 3
#define STRINGLIST 0
#define LOADQUERY 2
#define UNLOADQUERY 3

void load(std::string envNameString, ParameterHolder *theParams){
	char theRequest[2048]={0};
	std::string loadPayLoad=envNameString+":"+theParams->stringSerialize();

	printf("Payload we are sending is:\n%s\n",loadPayLoad.c_str());
	sprintf(theRequest,"TO=%d FROM=%d CMD=%d VALTYPE=%d VALS=%s",ENVSHELL, BENCHMARK, LOADQUERY, STRINGLIST,loadPayLoad.c_str());
	printf("The request is: \n%s\nLength:%d\n",theRequest,(int)strlen(theRequest));


	RL_env_message(theRequest);
}
ParameterHolder *preload(std::string envNameString){
	char theRequest[2048]={0};
	sprintf(theRequest,"TO=%d FROM=%d CMD=%d VALTYPE=%d VALS=NULL",ENVSHELL, BENCHMARK, LISTQUERY, NOVALUE);

	std::string theResponse=std::string(RL_env_message(theRequest));
	std::string::size_type lastColonPos = theResponse.find_last_of ("=");
	std::string thePayLoad=theResponse.substr(lastColonPos+1);

	std::vector<std::string> payLoadVector;
	std::string thisItem;

	std::istringstream iss(thePayLoad);
	while (getline(iss,thisItem,':'))
	{
		payLoadVector.push_back (thisItem);
	}

	std::vector<std::string> theNames;
	std::vector<ParameterHolder *> theParamHolders;

	for(size_t i=1;i<payLoadVector.size();i+=2){
		theNames.push_back(payLoadVector[i]);
		theParamHolders.push_back(new ParameterHolder(payLoadVector[i+1]));
	}

	unsigned int indexOfMyEnv=0;

	for(unsigned int i=0;i<theNames.size();i++){
		if(theNames[i]==envNameString){
			indexOfMyEnv=i;
		}
	}

	return theParamHolders[indexOfMyEnv];
}

void preloadAndLoad(std::string envNameString){
	ParameterHolder *p=preload(envNameString);
	load(envNameString,p);
}

/*
	* Tetris has an integer parameter called pnum that takes values in [0,19]
	* Setting this parameter changes the exact tetris problem you are solving
	*/
void loadTetris(int whichParamSet){
	std::string theEnvString="GeneralizedTetris - Java";
	ParameterHolder *theParams=preload(theEnvString);
	theParams->setIntegerParam("pnum",whichParamSet);

	load(theEnvString, theParams);
}

void loadMario(int levelSeed, int levelType, int levelDifficulty, int instance){
	std::string theEnvString="GeneralizedMario - Java";
	ParameterHolder *theParams=preload(theEnvString);

	theParams->setIntegerParam("fast",1);
	theParams->setIntegerParam("dark",1);
        theParams->setIntegerParam("level seed",levelSeed);
        theParams->setIntegerParam("level difficulty",levelType);
        theParams->setIntegerParam("level type",levelDifficulty);
        theParams->setIntegerParam("instance",instance);

	load(theEnvString, theParams);
}

void loadOctopus(){
	std::string theEnvString="Octopus - Java";
	ParameterHolder *theParams=preload(theEnvString);

	load(theEnvString, theParams);
}

/*
	* Acrobot has an integer parameter called pnum that takes values in [0,39]
	* Setting this parameter changes the exact acrobot problem you are solving
	*/
void loadAcrobot(int whichParamSet){
	std::string theEnvString="GeneralizedAcrobot - Java";
	ParameterHolder *theParams=preload(theEnvString);
	theParams->setIntegerParam("pnum",whichParamSet);

	load(theEnvString, theParams);

}

/*
	* Helicopter has an integer parameter called pnum that takes values in [0,9]
	* Setting this parameter changes the exact helicopter problem you are solving
	*/
void loadHelicopter(int whichParamSet){
	std::string theEnvString="GeneralizedHelicopter - Java";
	ParameterHolder *theParams=preload(theEnvString);
	theParams->setIntegerParam("pnum",whichParamSet);

	load(theEnvString, theParams);
}

void loadPolyathlon(int whichParamSet){
	std::string theEnvString="TrainingPolyathlon - Java";
	ParameterHolder *theParams=preload(theEnvString);
	theParams->setIntegerParam("whichDomain",whichParamSet);

	load(theEnvString, theParams);
}
