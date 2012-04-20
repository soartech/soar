/*
 * Copyright 2008 Brian Tanner
 * http://bt-recordbook.googlecode.com/
 * brian@tannerpages.com
 * http://brian.tannerpages.com
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.rlcommunity.rlglue.codec.taskspec.ranges;

import java.util.StringTokenizer;

/**
 *
 * @author Brian Tanner
 */
public abstract class AbstractRange {

    private String minSpecial = "NONE";
    private String maxSpecial = "NONE";
    private int howMany = 1;

    public AbstractRange(int howMany) {
        this.howMany = howMany;
    }

    public AbstractRange(String rangeString) {
        //Now get a string that has either 2 or 3 tokens ... either num low high or low high
        StringTokenizer rangeTokenizer = new StringTokenizer(rangeString);
        if (rangeTokenizer.countTokens() == 3) {
            howMany = Integer.parseInt(rangeTokenizer.nextToken());
        }
        String firstHalf = rangeTokenizer.nextToken();
        String secondHalf = rangeTokenizer.nextToken();
        parseSpecialMin(firstHalf);
        if (!hasSpecialMinStatus()) {
            parseMin(firstHalf);
        }

        parseSpecialMax(secondHalf);
        if (!hasSpecialMaxStatus()) {
            parseMax(secondHalf);
        }
    }

    public int getHowMany() {
        return howMany;
    }

    public String getMinSpecialStatus() {
        return minSpecial;
    }

    public String getMaxSpecialStatus() {
        return maxSpecial;
    }

    protected abstract void parseMin(String minString);

    protected abstract void parseMax(String maxString);

    public boolean hasSpecialMinStatus() {
        return !minSpecial.equals("NONE");
    }

    public boolean hasSpecialMaxStatus() {
        return !maxSpecial.equals("NONE");
    }

    protected void parseSpecialMax(String secondHalf) {
        for (String thisSpecialValue : AbstractRange.specialValues) {
            if (secondHalf.equals(thisSpecialValue)) {
                maxSpecial = thisSpecialValue;
            }
        }
    }

    protected void parseSpecialMin(String firstHalf) {
        for (String thisSpecialValue : AbstractRange.specialValues) {
            if (firstHalf.equals(thisSpecialValue)) {
                minSpecial = thisSpecialValue;
            }
        }
    }

    public void setMaxInf() {
        maxSpecial = "POSINF";
    }

    public void setMinNegInf() {
        minSpecial = "NEGINF";
    }

    public void setMinUnspecified() {
        minSpecial = "UNSPEC";
    }

    public void setMaxUnspecified() {
        maxSpecial = "UNSPEC";
    }

    public boolean getMinInf() {
        return minSpecial.equals("POSINF");
    }

    public boolean getMinNegInf() {
        return minSpecial.equals("NEGINF");
    }

    public boolean getMinUnspecified() {
        return minSpecial.equals("UNSPEC");
    }

    public boolean getMaxInf() {
        return maxSpecial.equals("POSINF");
    }

    public boolean getMaxNegInf() {
        return maxSpecial.equals("NEGINF");
    }

    public boolean getMaxUnspecified() {
        return maxSpecial.equals("UNSPEC");
    }

    /**
     * Override this is descendant classes and only use super method if min is special.
     * @return The minimum value special status as a string (one of NEGINF, POSINF, UNSPEC, NONE
     */
    public String getMinAsString() {
        return minSpecial;
    }

    /**
     * Override this is descendant classes and only use super method if max is special.
     * @return The maximum value special status as a string (one of NEGINF, POSINF, UNSPEC, NONE
     */
    public String getMaxAsString() {
        return maxSpecial;
    }

    public String toTaskSpec() {
        StringBuilder SB = new StringBuilder();
        SB.append(" (");
        if (getHowMany() > 1) {
            SB.append(getHowMany());
            SB.append(" ");
        }
        SB.append(getMinAsString());
        SB.append(" ");
        SB.append(getMaxAsString());
        SB.append(") ");
        return SB.toString();
    }

    //wRITE A SANITY CEHCK
    public static final String specialValues[] = new String[]{"UNSPEC", "NEGINF", "POSINF"};

    /**
     * Useful if a subclass has its value set after initially not being set.
     */
    protected void setMaxSpecified() {
        maxSpecial = "NONE";
    }

    /**
     * Useful if a subclass has its value set after initially not being set.
     */
    protected void setMinSpecified() {
        minSpecial = "NONE";
    }
}
