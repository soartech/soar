/*
Copyright 2008 Matt Radkie

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
*  $Revision: 638 $
*  $Date: 2009-02-07 16:17:29 -0500 (Sat, 07 Feb 2009) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/taskspec/TaskSpecV2.java $
* 
*/


  
package org.rlcommunity.rlglue.codec.taskspec;

import java.util.StringTokenizer;

class TaskSpecV2 extends TaskSpecDelegate
{
    public double version;			
    public char episodic;			
    public int obs_dim;
    public int num_discrete_obs_dims;
    public int num_continuous_obs_dims;		
    public char [] obs_types;	    
    public double [] obs_mins;           
    public double [] obs_maxs;			
    public int action_dim;
	public int num_discrete_action_dims;
	public int num_continuous_action_dims;			
    public char [] action_types;		
    public double [] action_mins;		
    public double [] action_maxs;
    public double reward_max;
    public double reward_min;
    
    static final int parser_version = 2;
    
    //Test program
    public static void main(String [] args) throws Exception
	{
//Write some test code here?
//                 String taskSpec = "2:e:2_[f,f]_[-1.2,0.6]_[-0.07,0.07]:1_[i]_[0,2]";
//		TaskSpecObject taskObject = new TaskSpecObject(taskSpec);
	}
    
    //As we discussed, the TaskSpecObject should parse in its constructor
    public TaskSpecV2(String taskSpecString){
		/* Break the task spec into its four component parts
		 * The version number
		 * The task style (episodic/continuous)
		 * The observation data
		 * The action data 
		 */
        
        
        
    	taskSpecString = this.removeWhiteSpace(taskSpecString);
	StringTokenizer tokenizer = new StringTokenizer(taskSpecString, ":");		
        
        int numberOfTokens=tokenizer.countTokens();
        if(numberOfTokens>5)throw new IllegalArgumentException("TaskSpecV2 shouldn't parse task specs with more than 5 sections");
        if(numberOfTokens<4)throw new IllegalArgumentException("TaskSpecV2 shouldn't parse task specs with less than 4 sections");

		String versionString = tokenizer.nextToken();
		String taskStyle = tokenizer.nextToken();
		String observationString = tokenizer.nextToken();
		String actionString = tokenizer.nextToken();
		String rewardString;

		version = Double.parseDouble(versionString);
		
		// Task specs of version > 1 have reward info that needs to be parsed
		if(version >= parser_version){
			if(tokenizer.hasMoreTokens())
				rewardString = tokenizer.nextToken();
			else
				rewardString = "[]";
		}
		else{
			System.err.println("WARNING: task spec parser is version: " + parser_version + " Your task spec is: " + version);
			System.err.println("Attempting to parse anyway!");
			rewardString = "";
		}
		
		episodic = taskStyle.charAt(0);
		// check to make sure this is a valid task type
		if(episodic != 'e' && episodic != 'c')
		{
			System.err.println("Invalid task type. Specify episodic (e) or continuous (c)");
			System.out.println("WOULD HAVE EXITED JAVA");//System.exit(1);
		}

		try {
			parseObservations(observationString);
			parseActions(actionString);
			if(version >= parser_version )
				parseRewards(rewardString);
			constraintCheck();
		} catch (Exception e) {
			System.err.println("Error parsing the Task Spec");
			System.err.println("Task Spec was: "+taskSpecString);
			System.err.println("Exception was: "+e);
			e.printStackTrace();
		}
    }
    
	protected  void parseObservationTypesAndDimensions(String obsTypesString) throws Exception
	{
		// Discard the [ ] around the types string
		obsTypesString = obsTypesString.substring(1, obsTypesString.length() - 1);
				
		// Split up the observation types
		StringTokenizer obsTypesTokenizer = new StringTokenizer(obsTypesString, ",");
		
		/* Parse the data out of obsTypesString.
		 * Allocate and fill the obs_types array, and set the number 
		 * of discrete and continuous observation dimensions.
		 */
		this.obs_types = new char[obsTypesTokenizer.countTokens()];
		this.num_discrete_obs_dims   = 0;
		this.num_continuous_obs_dims = 0;
		
		/* We get the observation type from the tokenizer, 
		 * add it to the obs_types array, and update the discrete and continuous dimensions
		 */
		int currentObservationTypeIndex = 0;
		while (obsTypesTokenizer.hasMoreTokens())
		{
			char obsType  = obsTypesTokenizer.nextToken().charAt(0);
			this.obs_types[currentObservationTypeIndex] = obsType;
			switch(obsType)
			{
			case 'i':
				this.num_discrete_obs_dims += 1;
				break;
				
			case 'f':
				this.num_continuous_obs_dims += 1;
				break;
				
			default: 
				throw new Exception ("Unknown Observation Type: " + obsType);
			}
			currentObservationTypeIndex += 1;
		}
	}
	
	protected void parseObservationRanges(StringTokenizer observationTokenizer)
	{
		// Now we can allocate our obs mins and obs maxs arrays
		this.obs_mins = new double[this.obs_types.length];
		this.obs_maxs = new double[this.obs_types.length];
		int currentRange = 0;
		while(observationTokenizer.hasMoreTokens())
		{
			String observationRange = observationTokenizer.nextToken();
			if(this.rangeKnown(observationRange))
			{
				//observationRange = observationRange.substring(1, observationRange.length() - 1);
				StringTokenizer rangeTokenizer = new StringTokenizer(observationRange, ",");	
				this.obs_mins[currentRange] = this.validValue(rangeTokenizer.nextToken());
				this.obs_maxs[currentRange] = this.validValue(rangeTokenizer.nextToken());
			}
			else
			{
				this.obs_mins[currentRange] = Double.NaN;
				this.obs_maxs[currentRange] = Double.NaN;
			}
			currentRange += 1;
		}		
	}

	protected void parseActionTypesAndDimensions(String actionTypesString) throws Exception
	{
		// Discard the [ ] around the types string
		actionTypesString = actionTypesString.substring(1, actionTypesString.length() - 1);
		
		// Split up the observation types
		StringTokenizer actionTypesTokenizer = new StringTokenizer(actionTypesString, ",");
		
		/* Parse the data out of obsTypesString.
		 * Allocate and fill the obs_types array, and set the number 
		 * of discrete and continuous observation dimensions.
		 */
		this.action_types = new char[actionTypesTokenizer.countTokens()];
		this.num_discrete_action_dims = 0;
		this.num_continuous_action_dims = 0;
		
		/* We get the observation type from the tokenizer, 
		 * add it to the obs_types array, and update the discrete and continuous dimensions
		 */
		int currentActionTypeIndex = 0;
		while (actionTypesTokenizer.hasMoreTokens())
		{
			char actionType = actionTypesTokenizer.nextToken().charAt(0);
			this.action_types[currentActionTypeIndex] = actionType;
			switch(actionType)
			{
			case 'i':
				this.num_discrete_action_dims += 1;
				break;
				
			case 'f':
				this.num_continuous_action_dims += 1;
				break;
				
			default: 
				throw new Exception ("Unknown Action Type: " + actionType);
			}
			currentActionTypeIndex += 1;
		}
	}
	
	protected void parseActionRanges(StringTokenizer actionTokenizer)
	{
		// Now we can allocate our obs mins and obs maxs arrays
		this.action_mins = new double[this.action_types.length];
		this.action_maxs = new double[this.action_types.length];
		int currentRange = 0;
		while(actionTokenizer.hasMoreTokens())
		{
			String actionRange = actionTokenizer.nextToken();
			if(this.rangeKnown(actionRange))
			{
				//actionRange = actionRange.substring(1, actionRange.length() - 1);
				StringTokenizer rangeTokenizer = new StringTokenizer(actionRange, ",");
				this.action_mins[currentRange] = this.validValue(rangeTokenizer.nextToken());
				//System.err.print(rangeTokenizer.nextToken() + "\n");
				this.action_maxs[currentRange] = this.validValue(rangeTokenizer.nextToken());
			}
			else
			{
				this.action_mins[currentRange] = Double.NaN;
				this.action_maxs[currentRange] = Double.NaN;
			}
			currentRange += 1;
		}		
	}
	
	protected void parseObservations(String observationString) throws Exception
	{
		/* Break the observation into its three component parts
		 * The number of dimensions to the observation
		 * The types of the observation
		 * The ranges of the observations
		 */
		StringTokenizer observationTokenizer = new StringTokenizer(observationString, "_");
		String obsDimensionString = observationTokenizer.nextToken();
		String obsTypesString = observationTokenizer.nextToken();
		
		this.obs_dim = Integer.parseInt(obsDimensionString);
		parseObservationTypesAndDimensions(obsTypesString);
		parseObservationRanges(observationTokenizer);
	}
	
	protected void parseActions(String actionString) throws Exception
	{
		StringTokenizer actionTokenizer = new StringTokenizer(actionString, "_");
		String actionDimensionString = actionTokenizer.nextToken();
		String actionTypesString = actionTokenizer.nextToken();	
		
		this.action_dim = Integer.parseInt(actionDimensionString);
		parseActionTypesAndDimensions(actionTypesString);
		parseActionRanges(actionTokenizer);
	}
	
	protected void parseRewards(String rewardString) throws Exception
	{
		//if both min and max rewards are defined
		if(this.rangeKnown(rewardString))
		{
			//rewardString = rewardString.substring(1, rewardString.length()-1);
			StringTokenizer rewardTokenizer = new StringTokenizer(rewardString, ",");
			this.reward_min = this.validValue(rewardTokenizer.nextToken());
			this.reward_max = this.validValue(rewardTokenizer.nextToken());	
		}
		else
		{
			this.reward_min = Double.NaN;
			this.reward_max = Double.NaN;
		}
	}
	
	protected double validValue(String valueString)
	{
		if(valueString.equalsIgnoreCase("[-inf"))
			return Double.NEGATIVE_INFINITY;
		else if(valueString.equalsIgnoreCase("inf]"))
			return Double.POSITIVE_INFINITY;
		else if(valueString.equals("["))
			return Double.NaN;
		else if(valueString.equals("]"))
			return Double.NaN;
		else{
			if(valueString.charAt(0) == '[')
				valueString = valueString.substring(1);
			else if(valueString.charAt(valueString.length()-1) == ']')
			{
				if(valueString.length() == 1)
					return Double.NaN;
				valueString = valueString.substring(0, valueString.length()-1);
			}
			return Double.parseDouble(valueString);
		}
	}
	
	protected boolean rangeKnown(String valueRange)
	{
		if(valueRange.equals("[,]"))
			return false;
		else if( valueRange.equals("[]"))
			return false;
		else
			return true;
	}
	
	protected String removeWhiteSpace(String input)
	{
		StringTokenizer whiteTokens = new StringTokenizer(input, " ");  
		String output = whiteTokens.nextToken();
		while (whiteTokens.hasMoreTokens()) 
		{
			output += whiteTokens.nextToken();
		}
		return output; 
	}
	
	protected void constraintCheck() throws Exception
	{
		for(int i=0; i < this.obs_dim; i++)
		{
			if(this.obs_mins[i] > this.obs_maxs[i]){
				throw new Exception ("Observation min>max at index: " + i);
			}
		}
		for(int i=0; i < this.action_dim; i++)
		{
			if(this.action_mins[i] > this.action_maxs[i])
				throw new Exception ("Action min>max at index: " + i);
		}
		if(this.reward_min > this.reward_max)
			throw new Exception ("Reward min>max: " + this.reward_min);
	}
	//check if obs_min[index] is negative infinity
	public boolean isObsMinNegInfinity(int index)
	{
		return (this.obs_mins[index] == Double.NEGATIVE_INFINITY);
	}
	//check if action_min[index] is negative infinity
	public boolean isActionMinNegInfinity(int index)
	{
		return (this.action_mins[index] == Double.NEGATIVE_INFINITY);
	}
	//check if obs_max[index] is positive infinity
	public boolean isObsMaxPosInfinity(int index)
	{
		return (this.obs_maxs[index] == Double.POSITIVE_INFINITY);
	}
	//check if action_max[index] is positive infinity
	public boolean isActionMaxPosInfinity(int index)
	{
		return (this.action_maxs[index] == Double.POSITIVE_INFINITY);
	}
	//check if the value range for observation[index] is known
	public boolean isObsMinUnknown(int index)
	{
		return new Double(obs_mins[index]).isNaN();
	}
	public boolean isObsMaxUnknown(int index)
	{
		return new Double(obs_maxs[index]).isNaN();
	}
	//check if the value range for action[index] is known
	public boolean isActionMinUnknown(int index)
	{
		return new Double(action_mins[index]).isNaN();
	}
	public boolean isActionMaxUnknown(int index)
	{
		return new Double(action_maxs[index]).isNaN();
	}
	public boolean isMinRewardNegInf()
	{
		return new Double(reward_min).isInfinite();
		
	}
	public boolean isMaxRewardInf()
	{
		return new Double(reward_max).isInfinite();
		
	}
	public boolean isMinRewardUnknown()
	{
		return new Double(reward_min).isNaN();
		
	}
	public boolean isMaxRewardUnknown()
	{
		return new Double(reward_max).isNaN();
		
	}
	


    
    public String toString()
    {
    	String obs_types_string = "";
    	for (int i = 0; i < obs_types.length; ++i)
    		obs_types_string += obs_types[i] + " ";
    	
    	String obs_mins_string = "";
    	for (int i = 0; i < obs_mins.length; ++i)
    		obs_mins_string += obs_mins[i] + " ";
    	
    	String obs_maxs_string = "";
    	for (int i = 0; i < obs_maxs.length; ++i)
    		obs_maxs_string += obs_maxs[i] + " ";
    	
       	String action_types_string = "";
    	for (int i = 0; i < action_types.length; ++i)
    		action_types_string += action_types[i] + " ";
    	
    	String action_mins_string = "";
    	for (int i = 0; i < action_mins.length; ++i)
    		action_mins_string += action_mins[i] + " ";
    	
    	String action_maxs_string = "";
    	for (int i = 0; i < action_maxs.length; ++i)
    		action_maxs_string += action_maxs[i] + " ";
 
    	
    	String taskSpecObject = "version: " + version + "\n" +
    		   "episodic: " + episodic + "\n" + 
    		   "obs_dim: " + obs_dim + "\n" + 
    		   "num_discrete_obs_dims: " + num_discrete_obs_dims + "\n" +
    		   "num_continuous_obs_dims: " + num_continuous_obs_dims + "\n" +
    		   "obs_types: " + obs_types_string + "\n" +
    		   "obs_mins: " + obs_mins_string + "\n" +
    		   "obs_maxs: " + obs_maxs_string + "\n" +
    		   "action_dim: " + action_dim + "\n" + 
    		   "num_discrete_action_dims: " + num_discrete_action_dims + "\n" +
    		   "num_continuous_action_dims: " + num_continuous_action_dims + "\n" +
    		   "action_types: " + action_types_string + "\n" +
    		   "action_mins: " + action_mins_string + "\n" +
    		   "action_maxs: " + action_maxs_string + "\n" +
    		   "reward_min: " + this.reward_min + "\n" +
    		   "reward_max: " + this.reward_max;
    	
    	return taskSpecObject;
    }
    
        public double getVersion() {
        return this.version;
    }

    public void setVersion(int version) {
        this.version = version;
    }

    public char getEpisodic() {
        return this.episodic;
    }

    public void setEpisodic(char episodic) {
        this.episodic = episodic;
    }

    public int getObsDim() {
        return this.obs_dim;
    }

    public void setObsDim(int dim) {
        this.obs_dim = dim;
    }

    public int getNumDiscreteObsDims() {
        return this.num_discrete_obs_dims;
    }

    public void setNumDiscreteObsDims(int numDisc) {
        this.num_discrete_obs_dims = numDisc;
    }

    public int getNumContinuousObsDims() {
        return this.num_continuous_obs_dims;
    }

    public void setNumContinuousObsDims(int numCont) {
        this.num_continuous_obs_dims = numCont;
    }

    public char[] getObsTypes() {
        return this.obs_types;
    }

    public void setObsTypes(char[] types) {
        this.obs_types = types.clone();
    }

    public double[] getObsMins() {
        return this.obs_mins;
    }

    public void setObsMins(double[] mins) {
        this.obs_mins = mins.clone();
    }

    public double[] getObsMaxs() {
        return this.obs_maxs;
    }

    public void setObsMaxs(double[] maxs) {
        this.obs_maxs = maxs.clone();
    }
    public int getActionDim() {
        return this.action_dim;
    }

    public void setActionDim(int dim) {
        this.action_dim = dim;
    }

    public int getNumDiscreteActionDims() {
        return this.num_discrete_action_dims;
    }

    public void setNumDiscreteActionDims(int numDisc) {
        this.num_discrete_action_dims = numDisc;
    }

    public int getNumContinuousActionDims() {
        return this.num_continuous_action_dims;
    }

    public void setNumContinuousActionDims(int numCont) {
        this.num_continuous_action_dims = numCont;
    }

    public char[] getActionTypes() {
        return this.action_types;
    }

    public void setActionTypes(char[] types) {
        this.action_types = types.clone();
    }

    public double[] getActionMins() {
        return this.action_mins;
    }

    public void setActionMins(double[] mins) {
        this.action_mins = mins.clone();
    }

    public double[] getActionMaxs() {
        return this.action_maxs;
    }

    public void setActionMaxs(double[] maxs) {
        this.action_maxs = maxs.clone();
    }
    public double getRewardMax() {
        return this.reward_max;
    }

    public void setRewardMax(double max) {
        this.reward_max = max;
    }

    public double getRewardMin() {
        return this.reward_min;
    }

    public void setRewardMin(double min) {
        this.reward_min = min;
    }
    public int getParserVersion() {
        return this.parser_version;
    }
}
