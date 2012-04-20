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
* 
*  $Revision: 638 $
*  $Date: 2009-02-07 16:17:29 -0500 (Sat, 07 Feb 2009) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/taskspec/TaskSpecV3.java $
* 
*/

package org.rlcommunity.rlglue.codec.taskspec;
import java.util.StringTokenizer;

/**
 * The newest version of the Task Spec (May 15th 2008). With the release of
 * version 3, the framework of the Task Spec (in Java atleast) was overhauled.
 * The capability of adding more versions of the Task Spec without affecting
 * old versions was addded. @see rlglue.utilities.TaskSpecDelegate for more 
 * info. TaskSpecV3 now has the capability of appending a string of extra data
 * onto the end of the task spec.
 * 
 * @author mradkie
 */
class TaskSpecV3 extends TaskSpecDelegate {

    /**
     * Task Spec version. Should be 3.
     */
    private double version = 3;
    /**
     * Stores whether the environment is episodic or continuous.
     */
    private char episodic;
    /**
     * Total number of observations.
     */
    private int obs_dim;
    /**
     * Number of discrete observations.
     */
    private int num_discrete_obs_dims;
    /**
     * Number of continous observations.
     */
    private int num_continuous_obs_dims;
    /**
     * Array of types for the observations.
     */
    private char[] obs_types;
    /**
     * Array of the minimum value for the observations. (One min per observation)
     */
    private double[] obs_mins;
    /**
     * Array of the maximum value for the observations. (One max per observation)
     */
    private double[] obs_maxs;
    /**
     * Total number of actions
     */
    private int action_dim;
    /**
     * Number of discrete actions
     */
    private int num_discrete_action_dims;
    /**
     * Number of continous actions
     */
    private int num_continuous_action_dims;
    /**
     * Array of types for the actions
     */
    private char[] action_types;
    /**
     * Array of the minimum value for the actions. (One min per action)
     */
    private double[] action_mins;
    /**
     * Array of the maximum value for the actions. (One max per action)
     */
    private double[] action_maxs;
    /**
     * Maximum value for the reward.
     */
    private double reward_max;
    /**
     * Minimum value for the reward.
     */
    private double reward_min;
    /**
     * String of extra data to be appended onto the end of the Task Spec.
     */
    private String extraString;
    /**
     * Version of the parser used for this Task Spec.
     */
    static final int parser_version = 3;
    
    public TaskSpecV3(TaskSpecV2 oldTaskSpec){
        this.episodic=oldTaskSpec.getEpisodic();
        this.obs_dim=oldTaskSpec.getObsDim();
        this.action_dim=oldTaskSpec.getActionDim();
        this.num_continuous_action_dims=oldTaskSpec.getNumContinuousActionDims();
        this.num_continuous_obs_dims=oldTaskSpec.getNumContinuousObsDims();
        this.num_discrete_action_dims=oldTaskSpec.getNumDiscreteActionDims();
        this.num_discrete_obs_dims=oldTaskSpec.getNumDiscreteObsDims();
        this.obs_types=oldTaskSpec.getObsTypes();
        this.obs_mins=oldTaskSpec.getObsMins();
        this.obs_maxs=oldTaskSpec.getObsMaxs();
        this.action_types=oldTaskSpec.getActionTypes();
        this.action_mins=oldTaskSpec.getActionMins();
        this.action_maxs=oldTaskSpec.getActionMaxs();
        this.reward_max=oldTaskSpec.getRewardMax();
        this.reward_min=oldTaskSpec.getRewardMin();
        extraString="";
    }

    /**
     * The constructor for version 3 of the Task Spec taks a string as a 
     * parameter. This string is then parsed out, and the information from this
     * string, such as number of actions or observations can be accessed. The
     * format of the string should follow the conventions of the Task Spec 
     * language. Please refer to {@link rlglue.utilities.TaskSpec Task Spec}
     * for more information.
     * <p>
     * Version 3 of the Task Spec added the capability of appending a string of
     * extra data onto the end of the Task Spec.
     * 
     * @param taskSpecString String format of a Task Spec to be parsed into an
     * object.
     */
    public TaskSpecV3(String taskSpecString) {
        /* Break the task spec into its six component parts
         * The version number
         * The task style (episodic/continuous)
         * The observation data
         * The action data
         * The reward data (if version >= 2)
         * The extra data (if version >=3)
         */
        StringTokenizer tokenizer = new StringTokenizer(taskSpecString, ":");

        int numberOfTokens = tokenizer.countTokens();
        if (numberOfTokens < 6) {
            throw new IllegalArgumentException("TaskSpecV3 shouldn't parse task specs with less than 6 sections");
        }

        String versionString = this.removeWhiteSpace(tokenizer.nextToken());
        String taskStyle = this.removeWhiteSpace(tokenizer.nextToken());
        String observationString = this.removeWhiteSpace(tokenizer.nextToken());
        String actionString = this.removeWhiteSpace(tokenizer.nextToken());
        String rewardString;
        extraString = new String("");
        version = Double.parseDouble(versionString);

        // pull off the reward
        if (tokenizer.hasMoreTokens()) {
            rewardString = this.removeWhiteSpace(tokenizer.nextToken());
        } else {
            rewardString = "[]";
        }

        String thetoken = "";
        while (tokenizer.hasMoreTokens()) {
            thetoken = tokenizer.nextToken();
            extraString += thetoken;
        }

        episodic = taskStyle.charAt(0);
        // check to make sure this is a valid task type
        if (episodic != 'e' && episodic != 'c') {
            System.err.println("Invalid task type. Specify episodic (e) or continuous (c)");
           System.exit(1);
        }

        try {
            parseObservations(observationString);
            parseActions(actionString);
            parseRewards(rewardString);
            constraintCheck();
        } catch (Exception e) {
            System.err.println("Error parsing the Task Spec");
            System.err.println("Task Spec was: " + taskSpecString);
            System.err.println("Exception was: " + e);
            e.printStackTrace();
        }
    }

    TaskSpecV3() {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    /**
     * Parses out the observation information from the string parameter and
     * stores it. Number of observations and observation types are parsed.
     * 
     * @param obsTypesString Observation portion of the Task Spec.
     * @throws java.lang.Exception
     * @return none
     */
    protected void parseObservationTypesAndDimensions(String obsTypesString) throws Exception {
        // Discard the [ ] around the types string
        obsTypesString = obsTypesString.substring(1, obsTypesString.length() - 1);

        // Split up the observation types
        StringTokenizer obsTypesTokenizer = new StringTokenizer(obsTypesString, ",");

        /* Parse the data out of obsTypesString.
         * Allocate and fill the obs_types array, and set the number 
         * of discrete and continuous observation dimensions.
         */
        this.obs_types = new char[obsTypesTokenizer.countTokens()];
        this.num_discrete_obs_dims = 0;
        this.num_continuous_obs_dims = 0;

        /* We get the observation type from the tokenizer, 
         * add it to the obs_types array, and update the discrete and continuous dimensions
         */
        int currentObservationTypeIndex = 0;
        while (obsTypesTokenizer.hasMoreTokens()) {
            char obsType = obsTypesTokenizer.nextToken().charAt(0);
            this.obs_types[currentObservationTypeIndex] = obsType;
            switch (obsType) {
                case 'i':
                    this.num_discrete_obs_dims += 1;
                    break;

                case 'f':
                    this.num_continuous_obs_dims += 1;
                    break;

                default:
                    throw new Exception("Unknown Observation Type: " + obsType);
            }
            currentObservationTypeIndex += 1;
        }
    }

    /**
     * Parses the ranges for the observations, storing the minimum values in one
     * array and the max values in a second array.
     * 
     * @param observationTokenizer Tokenizer on the observation string, 
     * tokenizing on the '_'.
     * @return none 
     */
    protected void parseObservationRanges(StringTokenizer observationTokenizer) {
        // Now we can allocate our obs mins and obs maxs arrays
        this.obs_mins = new double[this.obs_types.length];
        this.obs_maxs = new double[this.obs_types.length];
        int currentRange = 0;
        while (observationTokenizer.hasMoreTokens()) {
            String observationRange = observationTokenizer.nextToken();
            if (this.rangeKnown(observationRange)) {
                //observationRange = observationRange.substring(1, observationRange.length() - 1);
                StringTokenizer rangeTokenizer = new StringTokenizer(observationRange, ",");
                this.obs_mins[currentRange] = this.validValue(rangeTokenizer.nextToken());
                this.obs_maxs[currentRange] = this.validValue(rangeTokenizer.nextToken());
            } else {
                this.obs_mins[currentRange] = Double.NaN;
                this.obs_maxs[currentRange] = Double.NaN;
            }
            currentRange += 1;
        }
    }

    /**
     * Parses out the action information from the string parameter and
     * stores it. Number of actions and action types are parsed.
     * 
     * @param obsTypesString Action portion of the Task Spec.
     * @throws java.lang.Exception
     * @return none
     */
    protected void parseActionTypesAndDimensions(String actionTypesString) throws Exception {
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
        while (actionTypesTokenizer.hasMoreTokens()) {
            char actionType = actionTypesTokenizer.nextToken().charAt(0);
            this.action_types[currentActionTypeIndex] = actionType;
            switch (actionType) {
                case 'i':
                    this.num_discrete_action_dims += 1;
                    break;

                case 'f':
                    this.num_continuous_action_dims += 1;
                    break;

                default:
                    throw new Exception("Unknown Action Type: " + actionType);
            }
            currentActionTypeIndex += 1;
        }
    }

     /**
     * Parses the ranges for the actions, storing the minimum values in one
     * array and the max values in a second array.
     * 
     * @param observationTokenizer Tokenizer on the action string, 
     * tokenizing on the '_'.
     * @return none 
     */
    protected void parseActionRanges(StringTokenizer actionTokenizer) {
        // Now we can allocate our obs mins and obs maxs arrays
        this.action_mins = new double[this.action_types.length];
        this.action_maxs = new double[this.action_types.length];
        int currentRange = 0;
        while (actionTokenizer.hasMoreTokens()) {
            String actionRange = actionTokenizer.nextToken();
            if (this.rangeKnown(actionRange)) {
                //actionRange = actionRange.substring(1, actionRange.length() - 1);
                StringTokenizer rangeTokenizer = new StringTokenizer(actionRange, ",");
                this.action_mins[currentRange] = this.validValue(rangeTokenizer.nextToken());
                //System.err.print(rangeTokenizer.nextToken() + "\n");
                this.action_maxs[currentRange] = this.validValue(rangeTokenizer.nextToken());
            } else {
                this.action_mins[currentRange] = Double.NaN;
                this.action_maxs[currentRange] = Double.NaN;
            }
            currentRange += 1;
        }
    }

    /**
     * Parses all information out of the observation portion of the Task Spec.
     * Observation string is passed in, the number of observations, the 
     * observation types and ranges are all parsed out of this string and stored
     * within the respective variables.
     * 
     * @param observationString Observation portion of the Task Spec string
     * @throws java.lang.Exception
     * @return none
     */
    protected void parseObservations(String observationString) throws Exception {
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
    
    /**
     * Parses all information out of the actions portion of the Task Spec.
     * Action string is passed in, the number of actions, the 
     * action types and ranges are all parsed out of this string and stored
     * within the respective variables.
     * 
     * @param observationString Action portion of the Task Spec string
     * @throws java.lang.Exception
     * @return none
     */  
    protected void parseActions(String actionString) throws Exception {
        StringTokenizer actionTokenizer = new StringTokenizer(actionString, "_");
        String actionDimensionString = actionTokenizer.nextToken();
        String actionTypesString = actionTokenizer.nextToken();

        this.action_dim = Integer.parseInt(actionDimensionString);
        parseActionTypesAndDimensions(actionTypesString);
        parseActionRanges(actionTokenizer);
    }
    /**
     * Parses all information out of the reward portion of the Task Spec.
     * Reward string is passed in, the min and max reward is stored.
     * 
     * @param observationString Reward portion of the Task Spec string
     * @throws java.lang.Exception
     * @return none
     */
    protected void parseRewards(String rewardString) throws Exception {
        //if both min and max rewards are defined
        if (this.rangeKnown(rewardString)) {
            //rewardString = rewardString.substring(1, rewardString.length()-1);
            StringTokenizer rewardTokenizer = new StringTokenizer(rewardString, ",");
            this.reward_min = this.validValue(rewardTokenizer.nextToken());
            this.reward_max = this.validValue(rewardTokenizer.nextToken());
        } else {
            this.reward_min = Double.NaN;
            this.reward_max = Double.NaN;
        }
    }

    /**
     * Parses a double out of a string.  This method acts like an overloaded
     * Double.parseDouble(String) method, conventions for -infinity and infinity
     * were added.
     * 
     * @param valueString String to parse the double out of.
     * @return The double parsed out of the strng.
     */
    protected double validValue(String valueString) {
        if (valueString.equalsIgnoreCase("[-inf")) {
            return Double.NEGATIVE_INFINITY;
        } else if (valueString.equalsIgnoreCase("inf]")) {
            return Double.POSITIVE_INFINITY;
        } else if (valueString.equals("[")) {
            return Double.NaN;
        } else if (valueString.equals("]")) {
            return Double.NaN;
        } else {
            if (valueString.charAt(0) == '[') {
                valueString = valueString.substring(1);
            } else if (valueString.charAt(valueString.length() - 1) == ']') {
                if (valueString.length() == 1) {
                    return Double.NaN;
                }
                valueString = valueString.substring(0, valueString.length() - 1);
            }
            return Double.parseDouble(valueString);
        }
    }

    /**
     * Checks if the range of a given parameter is known. Observations, actions
     * and rewards all follow the convention [min,max]. If the min and max are
     * not specified, the range is unknown.
     * 
     * @param valueRange String of the form "[min,max]" where min and max may
     * not be specified ("[,] or []").
     * @return True if range is known, false otherwise.
     */
    protected boolean rangeKnown(String valueRange) {
        if (valueRange.equals("[,]")) {
            return false;
        } else if (valueRange.equals("[]")) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Removes spaces from a given string.
     * 
     * @param input String to remove spaces from.
     * @return Input string after spaces are removed.
     */
    protected String removeWhiteSpace(String input) {
        StringTokenizer whiteTokens = new StringTokenizer(input, " ");
        String output = whiteTokens.nextToken();
        while (whiteTokens.hasMoreTokens()) {
            output += whiteTokens.nextToken();
        }
        return output;
    }

    /**
     * Checks to make sure that: observation mins < observation maxs;
     * action mins < action maxs and reward mins < reward maxs.
     * 
     * @throws java.lang.Exception Exception thrown if one of these conditions
     * is not met.
     */
    protected void constraintCheck() throws Exception {
        for (int i = 0; i < this.obs_dim; i++) {
            if (this.obs_mins[i] > this.obs_maxs[i]) {
                throw new Exception("Observation min>max at index: " + i);
            }
        }
        for (int i = 0; i < this.action_dim; i++) {
            if (this.action_mins[i] > this.action_maxs[i]) {
                throw new Exception("Action min>max at index: " + i);
            }
        }
        if (this.reward_min > this.reward_max) {
            throw new Exception("Reward min>max: " + this.reward_min);
        }
    }
    /**
     * @see rlglue.utilities.TaskSpec#isObsMinNegInfinity(int index)
     */
    public boolean isObsMinNegInfinity(int index) {
        return (this.obs_mins[index] == Double.NEGATIVE_INFINITY);
    }

    /**
     * @see rlglue.utilities.TaskSpec#isActionMinNegInfinity(int index)
     */
    public boolean isActionMinNegInfinity(int index) {
        return (this.action_mins[index] == Double.NEGATIVE_INFINITY);
    }
    /**
     * @see rlglue.utilities.TaskSpec#isObsMaxPosInfinity(int index)
     */
    public boolean isObsMaxPosInfinity(int index) {
        return (this.obs_maxs[index] == Double.POSITIVE_INFINITY);
    }
    /**
     * @see rlglue.utilities.TaskSpec#isActionMaxPosInfinity(int index)
     */
    public boolean isActionMaxPosInfinity(int index) {
        return (this.action_maxs[index] == Double.POSITIVE_INFINITY);
    }
    /**
     * @see rlglue.utilities.TaskSpec#isObsMinUnknown(int index)
     */
    public boolean isObsMinUnknown(int index) {
        return new Double(obs_mins[index]).isNaN();
    }
    /**
     * @see rlglue.utilities.TaskSpec#isObsMaxUnknown(int index)
     */
    public boolean isObsMaxUnknown(int index) {
        return new Double(obs_maxs[index]).isNaN();
    }
    /**
     * @see rlglue.utilities.TaskSpec#isActionMinUnknown(int index)
     */
    public boolean isActionMinUnknown(int index) {
        return new Double(action_mins[index]).isNaN();
    }
    /**
     * @see rlglue.utilities.TaskSpec#isActionMaxUnknown(int index)
     */
    public boolean isActionMaxUnknown(int index) {
        return new Double(action_maxs[index]).isNaN();
    }
    /**
     * @see rlglue.utilities.TaskSpec#isMinRewardNegInf()
     */
    public boolean isMinRewardNegInf() {
        return new Double(reward_min).isInfinite();

    }
    /**
     * @see rlglue.utilities.TaskSpec#isMaxRewardInf()
     */

    public boolean isMaxRewardInf() {
        return new Double(reward_max).isInfinite();

    }

    /**
     * @see rlglue.utilities.TaskSpec#isMinRewardUnknown()
     */
    public boolean isMinRewardUnknown() {
        return new Double(reward_min).isNaN();

    }

    /**
     * @see rlglue.utilities.TaskSpec#isMaxRewardUnknown()
     */
    public boolean isMaxRewardUnknown() {
        return new Double(reward_max).isNaN();

    }

    /**
     * Builds the string representation of the Task Spec, which follows the 
     * Task Spec language.
     * 
     * @param none
     * @return String representation of the Task Spec
     */
    public String getStringRepresentation() {
        //2:e:2_[f,f]_[-1.2,0.6]_[-0.07,0.07]:1_[i]_[0,2]:[0,3]:extrastringhere
        String taskSpec = "";
        taskSpec += (int) this.version + ":";
        taskSpec += this.episodic + ":";
        //add the observations
        taskSpec += buildObsString();
        //add the actions
        taskSpec += buildActionString();
        //add the reward
        taskSpec += "[" + this.reward_min + "," + this.reward_max + "]";
        //add the extra string
        taskSpec += ":" + this.extraString;

        return taskSpec;

    }

    /**
     * Builds the action portion of the Task Spec string from the information
     * stored within this Task Spec object.
     * 
     * @param none
     * @return String representation of the action information.
     */
    private String buildActionString() {
        String actionsString = "";
        int numactions = num_continuous_action_dims + num_discrete_action_dims;
        actionsString += (numactions) + "_[";

        for (int i = 0; i < numactions; i++) {
            actionsString += action_types[i] + ",";
        }
        actionsString = actionsString.substring(0, actionsString.length() - 1);//pull off extra ,
        actionsString += "]";
        for (int i = 0; i < numactions; i++) {
            actionsString += "_[" + action_mins[i] + "," + action_maxs[i] + "]";
        }
        return actionsString + ":";
    }

    /**
     * Builds the observation portion of the Task Spec string from the information
     * stored within this Task Spec object.
     * 
     * @param none
     * @return String representation of the observation information.
     */
    private String buildObsString() {
        String obsString = "";
        int numObs = num_continuous_obs_dims + num_discrete_obs_dims;
        obsString += (numObs) + "_[";

        int contIndex = 0;
        int descIndex = 0;
        for (int i = 0; i < numObs; i++) {
            obsString += obs_types[i] + ",";
        }
        obsString = obsString.substring(0, obsString.length() - 1);//pull off extra ,
        obsString += "]";
        for (int i = 0; i < numObs; i++) {
            obsString += "_[" + obs_mins[i] + "," + obs_maxs[i] + "]";
        }
        return obsString + ":";
    }

    /**
     * Builds a debug string of all the information stored within this Task Spec.
     * Instead of printing this debug info to the screen, it is returned so the
     * implementer can use it.
     * 
     * @param none
     * @return String full of debug information about the Task Spec object.
     */
    public String dump() {
        String obs_types_string = "";
        for (int i = 0; i < obs_types.length; ++i) {
            obs_types_string += obs_types[i] + " ";
        }

        String obs_mins_string = "";
        for (int i = 0; i < obs_mins.length; ++i) {
            obs_mins_string += obs_mins[i] + " ";
        }

        String obs_maxs_string = "";
        for (int i = 0; i < obs_maxs.length; ++i) {
            obs_maxs_string += obs_maxs[i] + " ";
        }

        String action_types_string = "";
        for (int i = 0; i < action_types.length; ++i) {
            action_types_string += action_types[i] + " ";
        }

        String action_mins_string = "";
        for (int i = 0; i < action_mins.length; ++i) {
            action_mins_string += action_mins[i] + " ";
        }

        String action_maxs_string = "";
        for (int i = 0; i < action_maxs.length; ++i) {
            action_maxs_string += action_maxs[i] + " ";
        }


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

    /**
     * @see rlglue.utilities.TaskSpec#getVersion()
     */
    public double getVersion() {
        return this.version;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setVersion(int version)
     */
    public void setVersion(int version) {
        this.version = version;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getEpisodic()
     */
    public char getEpisodic() {
        return this.episodic;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setEpisodic(char episodic)
     */
    public void setEpisodic(char episodic) {
        this.episodic = episodic;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getObsDim()
     */
    public int getObsDim() {
        return this.obs_dim;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setObsDim(int dim)
     */
    public void setObsDim(int dim) {
        this.obs_dim = dim;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getNumDiscreteObsDims()
     */
    public int getNumDiscreteObsDims() {
        return this.num_discrete_obs_dims;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setNumDiscreteObsDims(int numDisc)
     */
    public void setNumDiscreteObsDims(int numDisc) {
        this.num_discrete_obs_dims = numDisc;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getNumContinuousObsDims()
     */
    public int getNumContinuousObsDims() {
        return this.num_continuous_obs_dims;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setNumContinuousObsDims(int numCont)
     */
    public void setNumContinuousObsDims(int numCont) {
        this.num_continuous_obs_dims = numCont;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getObsTypes()
     */
    public char[] getObsTypes() {
        return this.obs_types;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setObsTypes(char[] types)
     */
    public void setObsTypes(char[] types) {
        this.obs_types = types.clone();
    }

    /**
     * @see rlglue.utilities.TaskSpec#getObsMins()
     */
    public double[] getObsMins() {
        return this.obs_mins;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setObsMins(double[] mins)
     */
    public void setObsMins(double[] mins) {
        this.obs_mins = mins.clone();
    }

    /**
     * @see rlglue.utilities.TaskSpec#getObsMaxs()
     */
    public double[] getObsMaxs() {
        return this.obs_maxs;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setObsMaxs(double[] maxs)
     */
    public void setObsMaxs(double[] maxs) {
        this.obs_maxs = maxs.clone();
    }
    
    /**
     * @see rlglue.utilities.TaskSpec#getActionDim()
     */
    public int getActionDim() {
        return this.action_dim;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setActionDim(int dim)
     */
    public void setActionDim(int dim) {
        this.action_dim = dim;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getNumDiscreteActionDims()
     */
    public int getNumDiscreteActionDims() {
        return this.num_discrete_action_dims;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setNumDiscreteActionDims(int numDisc)
     */
    public void setNumDiscreteActionDims(int numDisc) {
        this.num_discrete_action_dims = numDisc;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getNumContinuousActionDims()
     */
    public int getNumContinuousActionDims() {
        return this.num_continuous_action_dims;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setNumContinuousActionDims(int numCont)
     */
    public void setNumContinuousActionDims(int numCont) {
        this.num_continuous_action_dims = numCont;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getActionTypes()
     */
    public char[] getActionTypes() {
        return this.action_types;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setActionTypes(char[] types)
     */
    public void setActionTypes(char[] types) {
        this.action_types = types.clone();
    }

    /**
     * @see rlglue.utilities.TaskSpec#getActionMins()
     */
    public double[] getActionMins() {
        return this.action_mins;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setActionMins(double[] mins)
     */
    public void setActionMins(double[] mins) {
        this.action_mins = mins.clone();
    }

    /**
     * @see rlglue.utilities.TaskSpec#getActionMaxs()
     */
    public double[] getActionMaxs() {
        return this.action_maxs;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setActionMaxs(double[] maxs)
     */
    public void setActionMaxs(double[] maxs) {
        this.action_maxs = maxs.clone();
    }
    /**
     * @see rlglue.utilities.TaskSpec#getRewardMax()
     */
    public double getRewardMax() {
        return this.reward_max;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setRewardMax(double max)
     */
    public void setRewardMax(double max) {
        this.reward_max = max;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getRewardMin()
     */
    public double getRewardMin() {
        return this.reward_min;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setRewardMin(double min)
     */
    public void setRewardMin(double min) {
        this.reward_min = min;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getExtraString()
     */
    public String getExtraString() {
        return this.extraString;
    }

    /**
     * @see rlglue.utilities.TaskSpec#setExtraString(String newString)
     */
    public void setExtraString(String newString) {
        this.extraString = newString;
    }

    /**
     * @see rlglue.utilities.TaskSpec#getParserVersion()
     */
    public int getParserVersion() {
        return this.parser_version;
    }
}