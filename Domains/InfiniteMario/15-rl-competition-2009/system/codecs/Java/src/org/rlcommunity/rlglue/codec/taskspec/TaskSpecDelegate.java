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
 limitations under the License.rlglue
* 
*  $Revision: 489 $
*  $Date: 2009-01-31 16:34:21 -0500 (Sat, 31 Jan 2009) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/taskspec/TaskSpecDelegate.java $
* 
*/



package org.rlcommunity.rlglue.codec.taskspec;

import org.rlcommunity.rlglue.codec.taskspec.ranges.DoubleRange;
import org.rlcommunity.rlglue.codec.taskspec.ranges.IntRange;

/**
 * TaskSpecDelegate was written to be the abstraction between the Task Spec object
 * agents and environments interact with, and the implementation of each version
 * of the Task Spec. This allows for the addition of functionality to the Task
 * Spec by adding new versions without breaking previous Versions.We'll extend 
 * this class over time, adding more stuff to it, but we'll be careful such that
 * we don't need to *ever* change existing subclasses.
 * <p>
 * TaskSpecDelegate does not implement any of these functions, it acts as an 
 * interface.
 * 
 * Some of these things will be deprecated over time, and some will be added.  If you 
 * try and call one that isn't supported on the exact task spec version you have, you'll 
 * get a UnsupportedOperationException.  I am going to just take out the setter methods though, those should be 
 * used only really through the correct specific object.
 *   
 * @author Matt Radkie
 */
public abstract class TaskSpecDelegate {

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#dump()
     */
    protected String dump() {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getStringRepresentation()
     */
    protected String getStringRepresentation() {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isObsMinNegInfinity(int index)
     */
    public boolean isObsMinNegInfinity(int index) {
        throw new NoSuchMethodError("This version of the task spec does not support: isObsMinNegInfinity");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isActionMinNegInfinity(int index)
     */
    public boolean isActionMinNegInfinity(int index) {
        throw new NoSuchMethodError("This version of the task spec does not support: isActionMinNegInfinity");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isObsMaxPosInfinity(int index)
     */
    public boolean isObsMaxPosInfinity(int index) {
        throw new NoSuchMethodError("This version of the task spec does not support: isObsMaxPosInfinity");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isActionMaxPosInfinity(int index)
     */
    public boolean isActionMaxPosInfinity(int index) {
        throw new NoSuchMethodError("This version of the task spec does not support: isActionMaxPosInfinity");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isObsMinUnknown(int index)
     */
    public boolean isObsMinUnknown(int index) {
        throw new NoSuchMethodError("This version of the task spec does not support: isObsMinUnknown");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isObsMaxUnknown(int index)
     */
    public boolean isObsMaxUnknown(int index) {
        throw new NoSuchMethodError("This version of the task spec does not support: isObsMaxUnknown");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isActionMinUnknown(int index)
     */
    public boolean isActionMinUnknown(int index) {
        throw new NoSuchMethodError("This version of the task spec does not support: isActionMinUnknown");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isActionMaxUnknown(int index)
     */
    public boolean isActionMaxUnknown(int index) {
        throw new NoSuchMethodError("This version of the task spec does not support: isActionMaxUnknown");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isMinRewardNegInf()
     */
    public boolean isMinRewardNegInf() {
        throw new NoSuchMethodError("This version of the task spec does not support: isMinRewardNegInf");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isMaxRewardInf()
     */
    public boolean isMaxRewardInf() {
        throw new NoSuchMethodError("This version of the task spec does not support: isMaxRewardInf");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isMinRewardUnknown()
     */
    public boolean isMinRewardUnknown() {
        throw new NoSuchMethodError("This version of the task spec does not support: isMinRewardUnknown");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isMaxRewardUnknown()
     */
    public boolean isMaxRewardUnknown() {
        throw new NoSuchMethodError("This version of the task spec does not support: isMaxRewardUnknown");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getVersion() 
     */
    public double getVersion() {
        throw new NoSuchMethodError("This version of the task spec does not support: getVersion");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getEpisodic()
     */
    public char getEpisodic() {
        throw new NoSuchMethodError("This version of the task spec does not support: getEpisodic");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getObsDim()
     */
    public int getObsDim() {
        throw new NoSuchMethodError("This version of the task spec does not support: getobsDim");
    }
    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getNumDiscreteObsDims()
     */
    public int getNumDiscreteObsDims() {
        throw new NoSuchMethodError("This version of the task spec does not support: getNumDiscreteObsDims");
    }
    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getNumContinuousObsDims()
     */
    public int getNumContinuousObsDims() {
        throw new NoSuchMethodError("This version of the task spec does not support: getNumContinuousActionDims");
    }
    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getObsTypes()
     */
    public char[] getObsTypes() {
        throw new NoSuchMethodError("This version of the task spec does not support: getObsTypes");
    }
    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getObsMins()
     */
    public double[] getObsMins() {
        throw new NoSuchMethodError("This version of the task spec does not support: getObsMins");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getObsMaxs()
     */
    public double[] getObsMaxs() {
        throw new NoSuchMethodError("This version of the task spec does not support: getObsMaxs");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getActionDim()
     */
    public int getActionDim() {
        throw new NoSuchMethodError("This version of the task spec does not support: getActionDim");
    }
    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getNumDiscreteActionDims()
     */
    public int getNumDiscreteActionDims() {
        throw new NoSuchMethodError("This version of the task spec does not support: getNumDiscreteActionDims");
    }
    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getNumContinuousActionDims()
     */
    public int getNumContinuousActionDims() {
        throw new NoSuchMethodError("This version of the task spec does not support: getNumContinuousActionDims");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getActionTypes()
     */
    public char[] getActionTypes() {
        throw new NoSuchMethodError("This version of the task spec does not support: getActionTypes");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getActionMins()
     */
    public double[] getActionMins() {
        throw new NoSuchMethodError("This version of the task spec does not support: getActionMins");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getActionMaxs()
     */
    public double[] getActionMaxs() {
        throw new NoSuchMethodError("This version of the task spec does not support: getActionMaxs");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getRewardMax()
     */
    public double getRewardMax() {
        throw new NoSuchMethodError("This version of the task spec does not support: getRewardMax");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getRewardMin()
     */
    public double getRewardMin() {
        throw new NoSuchMethodError("This version of the task spec does not support: getRewardMin");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getExtraString()
     */
    public String getExtraString() {
        throw new NoSuchMethodError("This version of the task spec does not support: getExtraString");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getParserVersion()
     */
    public int getParserVersion() {
        throw new NoSuchMethodError("This version of the task spec does not support: getParserVersion");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getVersionString()
     */
    public String getVersionString() {
       throw new NoSuchMethodError("This version of the task spec does not support: getVersionString");
    }

    public IntRange getDiscreteObservationRange(int i) {
       throw new NoSuchMethodError("This version of the task spec does not support: getDiscreteObservationRange");
    }
    public DoubleRange getContinuousObservationRange(int i) {
       throw new NoSuchMethodError("This version of the task spec does not support: getContinuousObservationRange");
    }
    public IntRange getDiscreteActionRange(int i) {
       throw new NoSuchMethodError("This version of the task spec does not support: getDiscreteActionRange");
    }
    public DoubleRange getContinuousActionRange(int i) {
       throw new NoSuchMethodError("This version of the task spec does not support: getContinuousActionRange");
    }

    double getDiscountFactor() {
        throw new NoSuchMethodError("This version of the task spec does not support: getDiscountFactor");
    }


    String getProblemType() {
        throw new NoSuchMethodError("This version of the task spec does not support: getProblemType");
    }

    public DoubleRange getRewardRange() {
        throw new NoSuchMethodError("This version of the task spec does not support: getRewardRange");
    }
}
