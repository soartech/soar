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
import rlVizLib.general.ParameterHolder;
import rlVizLib.messaging.environmentShell.EnvShellListRequest;
import rlVizLib.messaging.environmentShell.EnvShellListResponse;
import rlVizLib.messaging.environmentShell.EnvShellLoadRequest;
import rlVizLib.messaging.environmentShell.EnvShellUnLoadRequest;

public class consoleTrainerHelper{
	private static boolean currentlyLoaded=false;
	
	private static void unload(){
		EnvShellUnLoadRequest.Execute();
	}
	private static void load(String envNameString, ParameterHolder theParams){
		if(currentlyLoaded)unload();
		
		EnvShellLoadRequest.Execute(envNameString,theParams);
		currentlyLoaded=true;
	}
	
	private static ParameterHolder preload(String envNameString){
		EnvShellListResponse ListResponse = EnvShellListRequest.Execute();

		int thisEnvIndex=ListResponse.getTheEnvList().indexOf(envNameString);
		
		ParameterHolder p = ListResponse.getTheParamList().get(thisEnvIndex);
		return p;
		
	}
	
	private static void preloadAndLoad(String envNameString){
		ParameterHolder p=preload(envNameString);
		load(envNameString,p);
	}

	/*
	* Octopus has no parameters 
	* 
	*/
	public static void loadOctopus(){
		String theEnvString="Octopus - Java";
		ParameterHolder theParams=preload(theEnvString);
				
		load(theEnvString, theParams);
	}


	/*
	* Tetris has an integer parameter called pnum that takes values in [0,19]
	* Setting this parameter changes the exact tetris problem you are solving
	*/
	public static void loadTetris(int whichParamSet){
		String theEnvString="GeneralizedTetris - Java";
		ParameterHolder theParams=preload(theEnvString);
		theParams.setIntegerParam("pnum",whichParamSet);
		
		load(theEnvString, theParams);
	}

	/*
	* Acrobot has an integer parameter called pnum that takes values in [0,39]
	* Setting this parameter changes the exact Acrobot problem you are solving
	* MDP 0 is standard acrobot
	*/
	public static void loadAcrobot(int whichParamSet){
		String theEnvString="GeneralizedAcrobot - Java";
		ParameterHolder theParams=preload(theEnvString);
		theParams.setIntegerParam("pnum",whichParamSet);
		
		load(theEnvString, theParams);
	}
	

    public static void loadMario(int levelSeed, int levelType, int levelDifficulty, int instance){
		String theEnvString="GeneralizedMario - Java";
		ParameterHolder theParams=preload(theEnvString);

		theParams.setBooleanParam("fast",true);
		theParams.setBooleanParam("dark",true);
		theParams.setIntegerParam("level seed",levelSeed);
		theParams.setIntegerParam("level difficulty",levelType);
		theParams.setIntegerParam("level type",levelDifficulty);
		theParams.setIntegerParam("instance",instance);
		
		load(theEnvString, theParams);
	}
	/*
	* Helicopter has an integer parameter called pnum that takes values in [0,9]
	* Setting this parameter changes the exact helicopter problem you are solving
	*/
	public static void loadHelicopter(int whichParamSet){
		String theEnvString="GeneralizedHelicopter - Java";
		ParameterHolder theParams=preload(theEnvString);
		theParams.setIntegerParam("pnum",whichParamSet);
		
		load(theEnvString, theParams);
	}

	/*
	* Polyathlon has an integer parameter called pnum that takes values in [0,5]
	* Setting this parameter changes the exact helicopter problem you are solving
	*/
	public static void loadPolyathlon(int whichParamSet){
		String theEnvString="TrainingPolyathlon - Java";
		ParameterHolder theParams=preload(theEnvString);
		theParams.setIntegerParam("whichDomain",whichParamSet);
		
		load(theEnvString, theParams);
	}
	
	/*
	* Task spec tester takes no parameters
	*/
	public static void loadTaskSpecTester(){
		String theEnvString="TaskSpecTester - Java";
		preloadAndLoad(theEnvString);
	}
}
