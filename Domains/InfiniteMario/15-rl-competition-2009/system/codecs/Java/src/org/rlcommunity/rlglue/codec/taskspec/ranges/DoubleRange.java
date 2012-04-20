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
public class DoubleRange extends AbstractRange {

    //Make sure to add some checks so that if they call getMin() when minSpecial is set, they fail.
    //Also if specials are not set, make sure max>=min
    //Min can't be POSINF
    //Max can't be NEGINF
    private double min;
    private double max;
    
    public DoubleRange(){
        this(1);
    }
    public DoubleRange(int howMany){
        super(howMany);
        super.setMinUnspecified();
        super.setMaxUnspecified();
    }

    public DoubleRange(double min, double max) {
        this(min, max,1);
    }
    public DoubleRange(double min, double max, int howMany) {
        super(howMany);
        this.min=min;
        this.max=max;
    }

    /**
     * Get the range size.  If the range is [0,10] then the range is 10.0. If the
     * range is [.5,1.5], the range is 1.0, etc.  Useful for normalizing.  Behavior
     * is undefined if min or max has a special value.
     * @return The size of the range (max-min) if neither is a special value (NEGINF, UNSPEC, etc)
     */
    public double getRangeSize(){
        return getMax()-getMin();
    }
    public double getMin() {
        if (hasSpecialMinStatus()) {
            System.err.println("This variable has a special state.  The return value of it's getMin method is invalid.");
        }
        return min;
    }

    public double getMax() {
        if (hasSpecialMaxStatus()) {
            System.err.println("This variable has a special state.  The return value of it's getMin method is invalid.");
        }
        return max;
    }

    public DoubleRange(String theRangeString) {
        super(theRangeString);
    }
    
    public void setMax(double newMax){
        this.max=newMax;
        super.setMaxSpecified();
    }
    public void setMin(double newMin){
        this.min=newMin;
        super.setMinSpecified();
    }

    @Override
    protected void parseMin(String minString) {
        min = Double.parseDouble(minString);
    }

    @Override
    protected void parseMax(String maxString) {
        max = Double.parseDouble(maxString);
    }

    @Override
    public String getMinAsString() {
        if (hasSpecialMinStatus()) {
            return super.getMinAsString();
        } else {
            return "" + getMin();
        }
    }

    @Override
    public String getMaxAsString() {
        if (hasSpecialMaxStatus()) {
            return super.getMaxAsString();
        } else {
            return "" + getMax();
        }
    }

    @Override
    public String toString() {
        StringBuilder B = new StringBuilder();
        B.append(" Cardinality: " + super.getHowMany());
        B.append(" Min: ");
        if (hasSpecialMinStatus()) {
            B.append(getMinSpecialStatus());
        } else {
            B.append(getMin());
        }
        B.append(" Max: ");
        if (hasSpecialMaxStatus()) {
            B.append(getMaxSpecialStatus());
        } else {
            B.append(getMax());
        }
        B.append(" ");
        return B.toString();
    }
}
