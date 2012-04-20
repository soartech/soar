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
 *  $Revision: 489 $
 *  $Date: 2009-01-31 16:34:21 -0500 (Sat, 31 Jan 2009) $
 *  $Author: brian@tannerpages.com $
 *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/taskspec/TaskSpec.java $
 *
 */
package org.rlcommunity.rlglue.codec.taskspec;

import org.rlcommunity.rlglue.codec.taskspec.ranges.DoubleRange;
import org.rlcommunity.rlglue.codec.taskspec.ranges.IntRange;

/**
 * This class is used to store and parse the information given to an Agent in
 * the RL-Glue framework. The Task Spec stores information the Agent needs
 * regarding the environment, such as the number of actions, observations, and 
 * their ranges. For more information please read the
 * <a href="http://rlai.cs.ualberta.ca/RLBB/TaskSpecification.html"> 
 * RL-Glue Task Spec info</a>
 * <p>
 * This class was written to handle future revisions to the Task Spec while
 * providing backwards compatibility with old Task Spec versions. This is 
 * accomplished through the use of the factory design pattern. The 
 * TaskSpecDelegate object is the medium through which the TaskSpec communicates
 * with different implementations of the Task Spec versions. So far, each
 * Task Spec version has incrementally added functionality, but this might not
 * always be the case, so this framework was designed to robustly accomodate
 * future versions.
 * 
 * <h2>This most recent Implementation of TaskSpecDelegate is TaskSpecVRLGLUE3</h2>
 * 
 * @author Matt Radkie
 */
public class TaskSpec {

    /**
     * Generic object extended by all versions of TaskSpec parsers 
     */
    private TaskSpecDelegate theTSO = null;
    /**
     * The version of the Task Spec.
     */
    private String TSVersion = "0";

    /**
     * Gets the Task Spec version.
     * @deprecated  Moving to a string version
     * 
     * @return Integer value of the Task Spec version.
     */
    public int getVersion() {
        try {
            return (Integer.parseInt(TSVersion));
        } catch (NumberFormatException ex) {
            System.err.println("Asked for version of task spec through deprecated int method and could not make it work. Version is: " + getVersionString());
            return 0;
        }
    }

    /**
     * Quick sanity check.  It parses the task spec into an Task Spec TSA.  Then, it 
     * uses the string representation of TSA to make TSAB.  Finally, it makes sure that the 
     * string representation of TSA is the same as TSB.
     * @param theTaskSpecString
     * @return Whether this task spec appears to be valid
     */
    public static boolean checkTaskSpec(String theTaskSpecString) {
        TaskSpec TS = new TaskSpec(theTaskSpecString);
        try {
            boolean match = TS.getStringRepresentation().equals(new TaskSpec(TS.getStringRepresentation()).getStringRepresentation());
            if (!match) {
                System.err.println("Task spec check failed: if the task spec means what we think it means, these two should be equal:");
                System.err.println("First Construction:\t" + TS.getStringRepresentation());
                System.err.println("Second Construction:\t" + new TaskSpec(TS.getStringRepresentation()).getStringRepresentation());
            }
            return match;
        } catch (Throwable T) {
            System.err.println("There is a problem parsing the task spec you were checking: " + T);
            return false;
        }
    }

    /**
     * @since RL-Glue3.0
     * @return a string representing the version of the task spec that has been parsed
     */
    public String getVersionString() {
        return theTSO.getVersionString();
    }

    /**
     * Constructor that takes a string adhereing to the Task Spec language
     * protocol. This string is parsed out by the appropriate version of the
     * Task Spec.
     * 
     * @param taskSpec String following the Task Spec language
     */
    public TaskSpec(String taskSpec) {
        String errorAccumulator = "Task Spec Parse Results:";

        try {
            theTSO = new TaskSpecVRLGLUE3(taskSpec);
            TSVersion = theTSO.getVersionString();
        } catch (Exception e) {
            errorAccumulator += "\nParsing as TaskSpecVRLGLUE3: " + e.toString();
        }

        if (theTSO == null) {

            try {
                TaskSpecV3 theV3TSO = new TaskSpecV3(taskSpec);
                //Later in here, make a taskSpecVRLGlue3 constructor that takes a taskspecv3
                theTSO = new TaskSpecVRLGLUE3(theV3TSO);
                TSVersion = "3";
            } catch (Exception e) {
                errorAccumulator += "\nParsing as V3: " + e.toString();
            }
        }

        if (theTSO == null) {
            try {
                TaskSpecV2 oldV2Spec = new TaskSpecV2(taskSpec);
                TaskSpecV3 newerV3Spec = new TaskSpecV3(oldV2Spec);
                theTSO = new TaskSpecVRLGLUE3(newerV3Spec);
                TSVersion = "2";
            } catch (Exception e) {
                errorAccumulator += "\nParsing as V2: " + e.toString();
            }
        }
        if (theTSO == null) {
            try {
                TaskSpecVersionOnly versionOnlySpec = new TaskSpecVersionOnly(taskSpec);
                theTSO = new TaskSpecVRLGLUE3(versionOnlySpec);
                TSVersion = theTSO.getVersionString();
            } catch (Exception e) {
                errorAccumulator += "\nParsing as TaskSpecVersionOnly: " + e.toString();
            }
        }

        if (theTSO == null) {
            System.err.println("Task spec couldn't be parsed");
            throw new IllegalArgumentException(errorAccumulator);
        }

    }

    public TaskSpec(TaskSpecDelegate theTaskSpecDelegate) {
        this.theTSO = theTaskSpecDelegate;
        this.TSVersion = theTaskSpecDelegate.getVersionString();
    }

    /**
     * Returns the string representation of the Task Spec object. This string
     * representation follows the Task Spec language as outlined
     * <a href="http://rlai.cs.ualberta.ca/RLBB/TaskSpecification.html"> 
     * here</a>
     * 
     * 
     * @deprecated We never should have overloaded toString in this way.
     * @return String representation of the Task Spec
     */
    public String toString() {
        return getStringRepresentation();
    }

    /**
     * Returns the string representation of the Task Spec object. This string
     * representation follows the Task Spec language as outlined
     * <a href="http://glue.rl-community.org/Home/rl-glue/task-spec-language"> 
     * http://glue.rl-community.org/Home/rl-glue/task-spec-language</a>
     * 
     * @return String representation of the Task Spec
     */
    public String getStringRepresentation() {
        return theTSO.getStringRepresentation();
    }

    /**
     * Returns a string containing debug information about the Task Spec. This
     * debug information is usually printed to the screen, but returning it as 
     * a string allows the caller to print it out to log files etc as well.
     * @deprecated This is dumb.
     * 
     * @return String containing debug information for the Task Spec.
     */
    public String dump() {
        return theTSO.dump();
    }

    /**
     * Checks if the observation min at index is negative infinity.
     * 
     * @param index Integer index of the obs_min array.
     * @return True if obs_min[index] is negative infinity, false otherwise.
     */
    public boolean isObsMinNegInfinity(int index) {
        return theTSO.isObsMinNegInfinity(index);
    }

    /**
     * Checks if the action min at index is negative infinity.
     * 
     * @param index - integer index of the action_mins array.
     * @return True if action_min[index] is negative infinity, false otherwise.
     */
    public boolean isActionMinNegInfinity(int index) {
        return theTSO.isActionMinNegInfinity(index);
    }

    /**
     * Checks if the observation max at index is positive infinity.
     * 
     * @param index Interger index of the obs_maxs array.
     * @return True if obs_max[index] is positive infinity, false otherwise.
     */
    public boolean isObsMaxPosInfinity(int index) {
        return theTSO.isObsMaxPosInfinity(index);
    }

    /**
     * Checks if the action max at index is positive infinity.
     * 
     * @param index Integer index of the action_maxs array.
     * @return True if action_max[index] is positive infinity, false otherwise.
     */
    public boolean isActionMaxPosInfinity(int index) {
        return theTSO.isActionMaxPosInfinity(index);
    }

    /**
     * Checks if the observation min at index is unknown.
     * 
     * @param index Integer index of the obs_mins array.
     * @return True if the min value for observation[index] is unknown, false 
     * otherwise.
     */
    public boolean isObsMinUnknown(int index) {
        return theTSO.isObsMinUnknown(index);
    }

    /**
     * Checks if the observation max at index is unknown.
     * 
     * @param index Integer index of the obs_max array.
     * @return True if the max value for observation[index] is unknown, false 
     * otherwise.
     */
    public boolean isObsMaxUnknown(int index) {
        return theTSO.isObsMaxUnknown(index);
    }

    /**
     * Checks if the min action at index is unknown.
     * 
     * @param index Integer index of the action_mins array.
     * @return True if the min value for action[index] is unknown, false 
     * otherwise.
     */
    public boolean isActionMinUnknown(int index) {
        return theTSO.isActionMinUnknown(index);
    }

    /**
     * Checks if the action max at index is unknown.
     * 
     * @param index Integer index of the action_maxs array.
     * @return True if the max value for action[index] is unknown, false 
     * otherwise.
     */
    public boolean isActionMaxUnknown(int index) {
        return theTSO.isActionMaxUnknown(index);
    }

    /**
     * Checks if the min reward is negative infinity.
     * 
     * 
     * @return True if the min reward is negative infinity, false 
     * otherwise.
     */
    public boolean isMinRewardNegInf() {
        return theTSO.isMinRewardNegInf();
    }

    /**
     * Checks if the max reward is positive infinity.
     * 
     * 
     * @return True if the max reward is positive infinity, false 
     * otherwise.
     */
    public boolean isMaxRewardInf() {
        return theTSO.isMaxRewardInf();
    }

    /**
     * Checks if the min reward is unknown.
     * 
     * 
     * @return True if the min reward is unknown, false 
     * otherwise.
     */
    public boolean isMinRewardUnknown() {
        return theTSO.isMinRewardUnknown();
    }

    /**
     * Checks if the max reward is unknown.
     * 
     * 
     * @return True if the max reward is unknown, false 
     * otherwise.
     */
    public boolean isMaxRewardUnknown() {
        return theTSO.isMaxRewardUnknown();
    }

    /**
     * Gets the version of the Task spec.
     * 
     * @deprecated Use getVersionString
     * @return the version of the Task Spec used.
     */
    public double getTaskSpecVersion() {
        return theTSO.getVersion();
    }

    /**
     * Gets the episodic characteristic of the Task Spec.
     * 
     * 
     * @return Char value representing if an environment is episodic
     * @deprecated use getProblemType()
     */
    public char getEpisodic() {
        return theTSO.getEpisodic();
    }

    /**
     * Gets the size of the observation array (Number of observations)
     * @deprecated This is useless.
     * 
     * @return The size of the observation array (Number of observations)
     */
    public int getObsDim() {
        return theTSO.getObsDim();
    }

    /**
     * Gets the number of descrete observations.
     * 
     * 
     * @return Integer value for the number of descrete observations
     */
    public int getNumDiscreteObsDims() {
        return theTSO.getNumDiscreteObsDims();
    }

    /**
     * Gets the number of continuous observations.
     * 
     * 
     * @return Integer value for the number of continuous observations.
     */
    public int getNumContinuousObsDims() {
        return theTSO.getNumContinuousObsDims();
    }

    /**
     * Gets the types for the observations.
     * @deprecated  I don't like this anymore.
     * 
     * @return Character array representing the types of the observations.
     */
    public char[] getObsTypes() {
        return theTSO.getObsTypes();
    }

    /**
     * Gets the array of mins for the observations.
     * @deprecated  I don't like this anymore.
     * 
     * @return double[] Array of the min values for the observations.
     */
    public double[] getObsMins() {
        return theTSO.getObsMins();
    }

    /**
     * Gets the array of maxs for the observations.
     * @deprecated  I don't like this anymore.
     * 
     * @return double[] Array of the maxs values for the observations.
     */
    public double[] getObsMaxs() {
        return theTSO.getObsMaxs();
    }

    /**
     * Gets the size of the action array (Number of actions)
     * 
     * 
     * @return The size of the action array (Number of actions)
     */
    public int getActionDim() {
        return theTSO.getActionDim();
    }

    /**
     * Gets the number of descrete actions
     * 
     * 
     * @return Integer number of descrete actions.
     */
    public int getNumDiscreteActionDims() {
        return theTSO.getNumDiscreteActionDims();
    }

    /**
     * Gets the number of continous actions
     * 
     * 
     * @return Integer number of continous actions.
     */
    public int getNumContinuousActionDims() {
        return theTSO.getNumContinuousActionDims();
    }

    /**
     * Gets the types for the actions.
     * 
     * @deprecated  I don't like this anymore.
     * 
     * @return Character array representing the types of the actions.
     */
    public char[] getActionTypes() {
        return theTSO.getActionTypes();
    }

    /**
     * Gets the array of mins for the actions.
     * 
     * @deprecated  I don't like this anymore.
     * 
     * @return double[] Array of the min values for the actions.
     */
    public double[] getActionMins() {
        return theTSO.getActionMins();
    }

    /**
     * Gets the array of maxs for the actions.
     * 
     * @deprecated  I don't like this anymore.
     * 
     * @return double[] Array of the max values for the actions.
     */
    public double[] getActionMaxs() {
        return theTSO.getActionMaxs();
    }

    /**
     * Gets the max reward.
     * 
     * 
     * @return Double value of the max reward.
     */
    public double getRewardMax() {
        return theTSO.getRewardMax();
    }

    /**
     * Gets the min reward.
     * 
     * 
     * @return Double value of the min reward.
     */
    public double getRewardMin() {
        return theTSO.getRewardMin();
    }

    /**
     * Gets the string value for the ExtraString.
     * 
     * 'ExtraString' is new for Task Spec version 3. It allows additional
     * information to be appended to the end of the Task Spec. When environments
     * use this feature, agents will require special code to handle this.
     * 
     * 
     * @return String of additional information appended onto the end of the
     * Task Spec.
     */
    public String getExtraString() {
        return theTSO.getExtraString();
    }

    /**
     * Gets the version of the parser used on the Task Spec.
     * 
     * 
     * @return Integer version of the parser used on the Task Spec.
     */
    public int getParserVersion() {
        return theTSO.getParserVersion();
    }

    /**
     * Main has no purpose in this class other than for debugging. This should
     * have been deleted prior to release, but as it makes on going development
     * easier, it has been left for now. Ideally in the future, this code will
     * be removed and moved into test cases.
     * 
     * @param args
     */
    public static void main(String[] args) {
        /*
        String sampleTS = "2:e:2_[f,f]_[-1.2,0.6]_[-0.07,0.07]:1_[i]_[0,2]";
        TaskSpec theTSO = new TaskSpec(sampleTS);
        
        System.out.println(sampleTS+" is version: "+theTSO.getVersion());
        sampleTS="2:e:2_[f,f]_[-1.2,0.6]_[-0.07,0.07]:1_[i]_[0,2]:[]";
        theTSO=new TaskSpec(sampleTS);
        System.out.println(sampleTS+" is version: "+theTSO.getVersion());
        sampleTS="2:e:2_[f,f]_[-1.2,0.6]_[-0.07,0.07]:1_[i]_[0,2]:[0,3]";
        theTSO=new TaskSpec(sampleTS);
        System.out.println(sampleTS+" is version: "+theTSO.getVersion());
        sampleTS = "2:e:2_[f,f]_[-1.2,0.6]_[-0.07,0.07]:1_[i]_[0,2]:[0,3]:Extra strings and stuff here";
        theTSO = new TaskSpec(sampleTS);
        System.out.println(sampleTS + " is version: " + theTSO.getVersion() + "\n" + theTSO.toString());
        System.out.println(theTSO.dump());
        sampleTS="2:e:2_[f,f]_[-1.2,0.6]_[-0.07,0.07]:1_[i]_[0,2]:[0,3]:";
        theTSO=new TaskSpec(sampleTS);
        System.out.println(sampleTS+" is version: "+theTSO.getVersion());
        sampleTS="2:e:[0,3]";
        theTSO=new TaskSpec(sampleTS);
        System.out.println(sampleTS+" is version: "+theTSO.getVersion());
         */
    }

    /**
     * Get the discount factor.
     * @since RL-Glue-3.0
     */
    public double getDiscountFactor() {
        return theTSO.getDiscountFactor();
    }

    /**
     * Get the min, max, and special information for the i'th integer observation.
     * @since RL-Glue-3.0
     * @param i
     */
    public IntRange getDiscreteObservationRange(int i) {
        return theTSO.getDiscreteObservationRange(i);
    }

    /**
     * Get the min, max, and special information for the i'th integer action.
     * @since RL-Glue-3.0
     * @param i
     */
    public IntRange getDiscreteActionRange(int i) {
        return theTSO.getDiscreteActionRange(i);
    }

    /**
     * Get the min, max, and special information for the i'th double observation.
     * @since RL-Glue-3.0
     * @param i
     */
    public DoubleRange getContinuousObservationRange(int i) {
        return theTSO.getContinuousObservationRange(i);
    }

    /**
     * Get the min, max, and special information for the i'th double action.
     * @since RL-Glue-3.0
     * @param i
     */
    public DoubleRange getContinuousActionRange(int i) {
        return theTSO.getContinuousActionRange(i);
    }

    /**
     * Get the range of rewards
     * @since RL-Glue-3.0
     */
    public DoubleRange getRewardRange() {
        return theTSO.getRewardRange();
    }

    /**
     * Replacement for getEpisodic
     * @return episodic | continuous | something else
     * @since RL-Glue-3.0
     */
    String getProblemType() {
        return theTSO.getProblemType();
    }
}