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
tmpToken=T.nextToken();
 *  $Date: 2009-02-07 16:17:29 -0500 (Sat, 07 Feb 2009) $
 *  $Author: brian@tannerpages.com $
 *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/taskspec/TaskSpecVRLGLUE3.java $
 * 
 */
package org.rlcommunity.rlglue.codec.taskspec;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import org.rlcommunity.rlglue.codec.taskspec.ranges.AbstractRange;
import org.rlcommunity.rlglue.codec.taskspec.ranges.DoubleRange;
import org.rlcommunity.rlglue.codec.taskspec.ranges.IntRange;

/**
 * The newest version of the Task Spec (Nov 1 2008). 
 * @author Brian Tanner
 */
public class TaskSpecVRLGLUE3 extends TaskSpecDelegate {

    /**
     * Task Spec version. Should be 3.
     */
    private static final String ourVersion = "RL-Glue-3.0";
    private String thisVersion = "";
    private String thisProblemType = "";
    private double discountFactor = 0.0d;
    private List<IntRange> intObservations = new ArrayList<IntRange>();
    private List<DoubleRange> doubleObservations = new ArrayList<DoubleRange>();
    private List<IntRange> intActions = new ArrayList<IntRange>();
    private List<DoubleRange> doubleActions = new ArrayList<DoubleRange>();
    private DoubleRange rewardRange = new DoubleRange();
    private int numObsChars = 0;
    private int numActChars = 0;
    private String extra = "";

    public TaskSpecVRLGLUE3() {
        //defaults
        thisVersion = ourVersion;
        thisProblemType = "episodic";
    }

    /**
     * Create a new updated TaskSpecVRLGLUE3 object from a previous generation task 
     * spec object.
     * @param V3TaskSpec
     */
    TaskSpecVRLGLUE3(TaskSpecV3 V3TaskSpec) {
        thisVersion = ourVersion;
        if (V3TaskSpec.getEpisodic() == 'e') {
            thisProblemType = "episodic";
        } else if (V3TaskSpec.getEpisodic() == 'c') {
            thisProblemType = "continuing";
        } else {
            thisProblemType = "" + V3TaskSpec.getEpisodic();
        }
        discountFactor = 1.0;
        System.out.println("Cannot set discount factor when constructing: " + this.getClass().getName() + " from " + V3TaskSpec.getClass().getName());
        {
            char[] obsTypes = V3TaskSpec.getObsTypes();
            double[] obsMins = V3TaskSpec.getObsMins();
            double[] obsMaxs = V3TaskSpec.getObsMaxs();
            for (int i = 0; i < V3TaskSpec.getObsDim(); i++) {
                AbstractRange newRange = null;
                if (obsTypes[i] == 'i') {
                    newRange = new IntRange((int) obsMins[i], (int) obsMaxs[i]);
                }
                if (obsTypes[i] == 'f') {
                    newRange = new DoubleRange(obsMins[i], obsMaxs[i]);
                }
                if (V3TaskSpec.isObsMinNegInfinity(i)) {
                    newRange.setMinNegInf();
                }
                if (V3TaskSpec.isObsMinUnknown(i)) {
                    newRange.setMinUnspecified();
                }
                if (V3TaskSpec.isObsMaxPosInfinity(i)) {
                    newRange.setMaxInf();
                }
                if (V3TaskSpec.isObsMaxUnknown(i)) {
                    newRange.setMaxUnspecified();
                }
                if (obsTypes[i] == 'i') {
                    intObservations.add((IntRange) newRange);
                }
                if (obsTypes[i] == 'f') {
                    doubleObservations.add((DoubleRange) newRange);
                }
            }
        }
        {
            char[] actTypes = V3TaskSpec.getActionTypes();
            double[] actMins = V3TaskSpec.getActionMins();
            double[] actMaxs = V3TaskSpec.getActionMaxs();
            for (int i = 0; i < V3TaskSpec.getActionDim(); i++) {
                AbstractRange newRange = null;
                if (actTypes[i] == 'i') {
                    newRange = new IntRange((int) actMins[i], (int) actMaxs[i]);
                }
                if (actTypes[i] == 'f') {
                    newRange = new DoubleRange(actMins[i], actMaxs[i]);
                }
                if (V3TaskSpec.isActionMinNegInfinity(i)) {
                    newRange.setMinNegInf();
                }
                if (V3TaskSpec.isActionMinUnknown(i)) {
                    newRange.setMinUnspecified();
                }
                if (V3TaskSpec.isActionMaxPosInfinity(i)) {
                    newRange.setMaxInf();
                }
                if (V3TaskSpec.isActionMaxUnknown(i)) {
                    newRange.setMaxUnspecified();
                }
                if (actTypes[i] == 'i') {
                    intActions.add((IntRange) newRange);
                }
                if (actTypes[i] == 'f') {
                    doubleActions.add((DoubleRange) newRange);
                }
            }
        }
        rewardRange = new DoubleRange(V3TaskSpec.getRewardMin(), V3TaskSpec.getRewardMax());
        if (V3TaskSpec.isMinRewardNegInf()) {
            rewardRange.setMinNegInf();
        }
        if (V3TaskSpec.isMinRewardUnknown()) {
            rewardRange.setMinUnspecified();
        }
        if (V3TaskSpec.isMaxRewardInf()) {
            rewardRange.setMaxInf();
        }
        if (V3TaskSpec.isMaxRewardUnknown()) {
            rewardRange.setMaxUnspecified();
        }
        this.extra = V3TaskSpec.getExtraString();

    }

    TaskSpecVRLGLUE3(TaskSpecVersionOnly versionOnlySpec) {
        this.thisVersion=versionOnlySpec.getVersionString();
        System.err.println("Task spec was parsed as non-standard version.  Only version information is available.");
    }

    public void setEpisodic() {
        thisProblemType = "episodic";
    }

    public void setContinuing() {
        thisProblemType = "continuing";
    }

    public void setProblemTypeCustom(String customType) {
        thisProblemType = customType;
    }

    @Override
    public String getProblemType() {
        return thisProblemType;
    }

    public void setDiscountFactor(double discountFactor) {
        this.discountFactor = discountFactor;
    }

    public void addDiscreteObservation(IntRange newIntRange) {
        intObservations.add(newIntRange);
    }

    public void addContinuousObservation(DoubleRange newDoubleRange) {
        doubleObservations.add(newDoubleRange);
    }

    public void setObservationCharLimit(int charLimit) {
        this.numObsChars = charLimit;
    }

    public void addDiscreteAction(IntRange newIntRange) {
        intActions.add(newIntRange);
    }

    public void addContinuousAction(DoubleRange newDoubleRange) {
        doubleActions.add(newDoubleRange);
    }

    public void setActionCharLimit(int charLimit) {
        this.numActChars = charLimit;
    }
    //Should assert these are all not null
    public void setRewardRange(DoubleRange newRewardRange) {
        this.rewardRange = newRewardRange;
    }

    public void setExtra(String extra) {
        this.extra = extra;
    }

    @Override
    public double getDiscountFactor() {
        return discountFactor;
    }

    /**
     * Parse a task spec string.
     * @param taskSpecString
     */
    public TaskSpecVRLGLUE3(String taskSpecString) {
        String tmpToken;
        //Default token is space, works for me.
        StringTokenizer T = new StringTokenizer(taskSpecString);

        tmpToken = T.nextToken();
        if (!tmpToken.equals("VERSION")) {
            throw new IllegalArgumentException("Expected VERSION token.  This task spec doesn't look like an RL-Glue 3.0+ task spec.");
        }

        thisVersion = T.nextToken();
        if (!thisVersion.equals("RL-Glue-3.0")) {
            throw new IllegalArgumentException("The RL-Glue-3.0 Task spec parser does not know how to parse: " + thisVersion);
        }

        String nextToken = T.nextToken();
        checkLabel("PROBLEMTYPE", nextToken);
        thisProblemType = T.nextToken();

        checkLabel("DISCOUNTFACTOR", T.nextToken());
        discountFactor = Double.parseDouble(T.nextToken());

        checkLabel("OBSERVATIONS", T.nextToken());

        nextToken = T.nextToken();
        if (nextToken.equals("INTS")) {
            //Do the ints
            nextToken = T.nextToken();
            while (nextToken.startsWith("(")) {
                String thisRange = nextToken.substring(1) + " " + T.nextToken(")");
                intObservations.add(new IntRange(thisRange));

                //Advance past the space after the range
                T.nextToken(" ");
                nextToken = T.nextToken();
            }
        }
        if (nextToken.equals("DOUBLES")) {
            //Do the doubles
            nextToken = T.nextToken();
            while (nextToken.startsWith("(")) {
                String thisRange = nextToken.substring(1) + " " + T.nextToken(")");
                doubleObservations.add(new DoubleRange(thisRange));

                //Advance past the space after the range
                T.nextToken(" ");
                nextToken = T.nextToken();
            }
        }
        if (nextToken.equals("CHARCOUNT")) {
            String numCharToken = T.nextToken();
            numObsChars = Integer.parseInt(numCharToken);
            nextToken = T.nextToken();

        }

        checkLabel("ACTIONS", nextToken);
        nextToken = T.nextToken();
        if (nextToken.equals("INTS")) {
            //Do the ints
            nextToken = T.nextToken();
            while (nextToken.startsWith("(")) {
                String thisRange = nextToken.substring(1) + " " + T.nextToken(")");
                intActions.add(new IntRange(thisRange));

                //Advance past the space after the range
                T.nextToken(" ");
                nextToken = T.nextToken();
            }
        }
        if (nextToken.equals("DOUBLES")) {
            //Do the doubles
            nextToken = T.nextToken();
            while (nextToken.startsWith("(")) {
                String thisRange = nextToken.substring(1) + " " + T.nextToken(")");
                doubleActions.add(new DoubleRange(thisRange));

                //Advance past the space after the range
                T.nextToken(" ");
                nextToken = T.nextToken();
            }
        }
        if (nextToken.equals("CHARCOUNT")) {
            String numCharToken = T.nextToken();
            numActChars = Integer.parseInt(numCharToken);
            nextToken = T.nextToken();

        }



        checkLabel("REWARDS", nextToken);
        nextToken = T.nextToken();
        if (nextToken.startsWith("(")) {
            String rewardRangeString = nextToken.substring(1) + " " + T.nextToken(")");
            rewardRange = new DoubleRange(rewardRangeString);
            T.nextToken(" ");
            nextToken = T.nextToken();

        }

        checkLabel("EXTRA", nextToken);
        StringBuilder theRest = new StringBuilder();
        while (T.hasMoreTokens()) {
            theRest.append(T.nextToken());
            if (T.hasMoreTokens()) {
                theRest.append(" ");
            }
        }
        this.extra = theRest.toString();

    }

    public static void main(String[] args) {
        String sampleSpec = "VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 0 OBSERVATIONS INTS (3 0 1) DOUBLES (2 -1.2 0.5) (-.07 .07) CHARCOUNT 1024 ACTIONS INTS (0 4) REWARDS (-5.0 5.0) EXTRA some other stuff goes here";

        //No range for ascii characters.
        TaskSpecVRLGLUE3 theTaskSpec = new TaskSpecVRLGLUE3(sampleSpec);
        System.out.println("orig:\t" + sampleSpec);
        System.out.println("parsed:\t" + theTaskSpec.toTaskSpec());

        //Need to compare the structs also
        assert (theTaskSpec.toTaskSpec().equals(new TaskSpecVRLGLUE3(theTaskSpec.toTaskSpec()).toTaskSpec()));

        TaskSpecVRLGLUE3 sampleTSO = new TaskSpecVRLGLUE3();
        sampleTSO.setEpisodic();
        sampleTSO.setDiscountFactor(0);
        sampleTSO.addDiscreteObservation(new IntRange(0, 1, 3));
        sampleTSO.addContinuousObservation(new DoubleRange(-1.2, .5, 2));
        sampleTSO.addContinuousObservation(new DoubleRange(-.07, .07));
        sampleTSO.setObservationCharLimit(1024);
        sampleTSO.addDiscreteAction(new IntRange(0, 4));
        sampleTSO.setRewardRange(new DoubleRange(-5, 5));
        sampleTSO.setExtra("some other stuff goes here");
        System.out.println(sampleTSO.toTaskSpec());
        assert (sampleTSO.toTaskSpec().equals(new TaskSpecVRLGLUE3(sampleTSO.toTaskSpec()).toTaskSpec()));
        assert (theTaskSpec.toTaskSpec().equals(sampleTSO.toTaskSpec()));
    }

    /**
     * Returns a person-friendly string  version of the object.
     * @return a person friendly, human readable verson of the task spec.
     */
    @Override
    public String toString() {
        StringBuilder SB = new StringBuilder();
        SB.append("Int obs\n");
        for (IntRange intRange : intObservations) {
            SB.append(intRange);
        }
        SB.append("\n");
        SB.append("double obs\n");
        for (DoubleRange doubleRange : doubleObservations) {
            SB.append(doubleRange);
        }
        SB.append("\n");
        SB.append("Char Obs: " + numObsChars + "\n");

        SB.append("Int Act\n");
        for (IntRange intRange : intActions) {
            System.out.print(intRange);
        }
        SB.append("\n");
        SB.append("double act\n");
        for (DoubleRange doubleRange : doubleActions) {
            System.out.print(doubleRange);
        }
        SB.append("\n");
        SB.append("Char act: " + numActChars + "\n");

        SB.append("\n");
        SB.append("Rewards: " + rewardRange.toString() + "\n");

        SB.append("\n");
        SB.append("Extra: " + extra + "\n");
        return SB.toString();

    }

    private static void checkLabel(String expectedLabel, String actualLabel) {
        assert actualLabel.equals(expectedLabel) : "Expected " + expectedLabel + " token instead got: " + actualLabel;
    }

    /**
     * Return a valid RLGLUEV3 task spec for this object.
     * @return a string representing a valid RLGLUEV3 task spec for this object.
     */
    public String toTaskSpec() {
        StringBuilder TS = new StringBuilder();
        TS.append("VERSION ");
        TS.append(ourVersion);

        TS.append(" PROBLEMTYPE ");
        TS.append(this.thisProblemType);

        TS.append(" DISCOUNTFACTOR ");
        TS.append(this.discountFactor);

        TS.append(" OBSERVATIONS ");

        if (intObservations.size() > 0) {
            TS.append("INTS");
            for (IntRange intRange : intObservations) {
                TS.append(intRange.toTaskSpec());
            }
        }
        if (doubleObservations.size() > 0) {
            TS.append(" DOUBLES");
            for (DoubleRange doubleRange : doubleObservations) {
                TS.append(doubleRange.toTaskSpec());
            }
        }
        if (numObsChars > 0) {
            TS.append(" CHARCOUNT ");
            TS.append(numObsChars);
        }

        TS.append(" ACTIONS ");
        if (intActions.size() > 0) {
            TS.append("INTS");
            for (IntRange intRange : intActions) {
                TS.append(intRange.toTaskSpec());
            }
        }
        if (doubleActions.size() > 0) {
            TS.append(" DOUBLES");
            for (DoubleRange doubleRange : doubleActions) {
                TS.append(doubleRange.toTaskSpec());
            }
        }
        if (numActChars > 0) {
            TS.append(" CHARCOUNT ");
            TS.append(numActChars);
        }

        TS.append(" REWARDS");
        TS.append(rewardRange.toTaskSpec());

        TS.append(" EXTRA ");
        TS.append(extra);
        return TS.toString();

    }

    private int getDiscreteObservationCount() {
        int theCount = 0;
        for (AbstractRange thisRange : intObservations) {
            theCount += thisRange.getHowMany();
        }
        return theCount;
    }

    private int getContinuousObservationCount() {
        int theCount = 0;
        for (AbstractRange thisRange : doubleObservations) {
            theCount += thisRange.getHowMany();
        }
        return theCount;
    }

    private int getDiscreteActionCount() {
        int theCount = 0;
        for (AbstractRange thisRange : intActions) {
            theCount += thisRange.getHowMany();
        }
        return theCount;
    }

    private int getContinuousActionCount() {
        int theCount = 0;
        for (AbstractRange thisRange : doubleActions) {
            theCount += thisRange.getHowMany();
        }
        return theCount;
    }

    @Override
    public DoubleRange getRewardRange() {
        return rewardRange;
    }

    @Override
    public IntRange getDiscreteObservationRange(int i) {
        int theCount = 0;
        for (IntRange thisRange : intObservations) {
            theCount += thisRange.getHowMany();
            if (theCount > i) {
                return thisRange;
            }
        }
        //This is a bit ugly, but I'm trying to replace all my System.exit() with
        //exceptions, because they might close Matlab
        throw new RuntimeException("This task spec does not have discrete observation: " + i);
    }

    @Override
    public IntRange getDiscreteActionRange(int i) {
        int theCount = 0;
        for (IntRange thisRange : intActions) {
            theCount += thisRange.getHowMany();
            if (theCount > i) {
                return thisRange;
            }
        }
        //This is a bit ugly, but I'm trying to replace all my System.exit() with
        //exceptions, because they might close Matlab
        throw new RuntimeException("This task spec does not have discrete action: " + i);
    }

    @Override
    public DoubleRange getContinuousObservationRange(int i) {
        int theCount = 0;
        for (DoubleRange thisRange : doubleObservations) {
            theCount += thisRange.getHowMany();
            if (theCount > i) {
                return thisRange;
            }
        }
        //This is a bit ugly, but I'm trying to replace all my System.exit() with
        //exceptions, because they might close Matlab
        throw new RuntimeException("This task spec does not have Continuous observation: " + i);
    }

    public DoubleRange getContinuousActionRange(int i) {
        int theCount = 0;
        for (DoubleRange thisRange : doubleActions) {
            theCount += thisRange.getHowMany();
            if (theCount > i) {
                return thisRange;
            }
        }
        //This is a bit ugly, but I'm trying to replace all my System.exit() with
        //exceptions, because they might close Matlab
        throw new RuntimeException("This task spec does not have discrete action: " + i);
    }

    private AbstractRange getActionRange(int i) {
        int numDiscreteActions = getDiscreteActionCount();
        if (i < numDiscreteActions) {
            return getDiscreteActionRange(i);
        }
        i -= numDiscreteActions;
        return getContinuousActionRange(i);
    }

    private AbstractRange getObservationRange(int i) {
        int numDiscreteObservations = getDiscreteObservationCount();
        if (i < numDiscreteObservations) {
            return getDiscreteObservationRange(i);
        }
        i -= numDiscreteObservations;
        return getContinuousObservationRange(i);
    }
    //I pasted all of the taskSpecDelegate methods below.
    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getStringRepresentation()
     */
    @Override
    protected String getStringRepresentation() {
        return toTaskSpec();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isObsMinNegInfinity(int index)
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isObsMinNegInfinity(int index) {
        return getObservationRange(index).getMinNegInf();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isActionMinNegInfinity(int index)
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isActionMinNegInfinity(int index) {
        return getActionRange(index).getMinNegInf();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isObsMaxPosInfinity(int index)
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isObsMaxPosInfinity(int index) {
        return getObservationRange(index).getMinInf();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isActionMaxPosInfinity(int index)
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isActionMaxPosInfinity(int index) {
        return getActionRange(index).getMinInf();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isObsMinUnknown(int index)
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isObsMinUnknown(int index) {
        return getObservationRange(index).getMinUnspecified();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isObsMaxUnknown(int index)
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isObsMaxUnknown(int index) {
        return getObservationRange(index).getMaxUnspecified();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isActionMinUnknown(int index)
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isActionMinUnknown(int index) {
        return getActionRange(index).getMinUnspecified();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isActionMaxUnknown(int index)
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isActionMaxUnknown(int index) {
        return getActionRange(index).getMaxUnspecified();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isMinRewardNegInf()
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isMinRewardNegInf() {
        return rewardRange.getMinNegInf();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isMaxRewardInf()
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isMaxRewardInf() {
        return rewardRange.getMaxInf();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isMinRewardUnknown()
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isMinRewardUnknown() {
        return rewardRange.getMinUnspecified();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#isMaxRewardUnknown()
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public boolean isMaxRewardUnknown() {
        return rewardRange.getMaxUnspecified();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getVersion() 
     */
    @Override
    public double getVersion() {
        throw new NoSuchMethodError("This version of the task spec does not support: (double) getVersion");
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getEpisodic()
     * @deprecated Use getProblemType
     */
    @Override
    public char getEpisodic() {
        if (thisProblemType.equals("episodic")) {
            return 'e';
        }
        if (thisProblemType.equals("continuing")) {
            return 'c';
        }
        return 'o';
    }

    @Override
    public String getVersionString() {
        return thisVersion;
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getNumContinuousObsDims()
     */
    @Override
    public int getNumContinuousObsDims() {
        return getContinuousObservationCount();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getNumDiscreteObsDims()
     */
    @Override
    public int getNumDiscreteObsDims() {
        return getDiscreteObservationCount();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getNumDiscreteActionDims()
     */
    @Override
    public int getNumDiscreteActionDims() {
        return getDiscreteActionCount();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getNumContinuousActionDims()
     */
    @Override
    public int getNumContinuousActionDims() {
        return getContinuousActionCount();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getRewardMax()
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public double getRewardMax() {
        return rewardRange.getMax();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getRewardMin()
     * @deprecated Just get the range and ask that object this question.
     */
    @Override
    public double getRewardMin() {
        return rewardRange.getMin();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getExtraString()
     */
    @Override
    public String getExtraString() {
        return extra;
    }
}


