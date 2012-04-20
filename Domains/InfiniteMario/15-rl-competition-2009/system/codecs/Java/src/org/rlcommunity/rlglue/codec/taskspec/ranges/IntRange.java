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
public class IntRange extends AbstractRange {

    //Make sure to add some checks so that if they call getMin() when minSpecial is set, they fail.
    //Also if specials are not set, make sure max>=min
    //Min can't be POSINF
    //Max can't be NEGINF
    private int min;
    private int max;

    /**
     * Constructs an unknown to unknown IntRange
     */
    public IntRange(){
        this(1);
    }
    /**
     * Constructs a set of howMany unknown to unknown IntRange
     */
    public IntRange(int howMany){
        super(howMany);
        setMinUnspecified();
        setMaxUnspecified();
    }

    /**
     * This should be a 2 or 3 tuple range like "1 2" or "5 1 2"
     * @param thisRange
     */
    public IntRange(String thisRange) {
        super(thisRange);
    }
    
    /**
     * Return the size of the range.  So, if the range was [0,0], the range size
     * is 1.  If it is [0,9], the range size is 10, etc.  Good for determining
     * for big of an array you need to hold all possible values.
     * 
     * Behavior is undefined if min or max has a special value.
     * @return The size of the range (max-min+1) if neither is a special value (NEGINF, UNSPEC, etc)
     */
    public int getRangeSize(){
        return getMax()-getMin()+1;
    }
    
    /**
     * Constructs a min to max IntRange
     */
    public IntRange(int min, int max) {
        this(min,max,1);
    }

    /**
     * Constructs a set of howMany min to max IntRange
     */
    public IntRange(int min, int max, int howMany) {
        super(howMany);
        this.min=min;
        this.max=max;
    }
    
    public int getMin(){
        if(hasSpecialMinStatus()){
            System.err.println("This variable has a special state.  The return value of it's getMin method is invalid.");
        }
        return min;
    }
    public int getMax(){
        if(hasSpecialMaxStatus()){
            System.err.println("This variable has a special state.  The return value of it's getMin method is invalid.");
        }
        return max;
    }
    
        public void setMax(int newMax){
        this.max=newMax;
        super.setMaxSpecified();
    }
    public void setMin(int newMin){
        this.min=newMin;
        super.setMinSpecified();
    }

    protected void parseMin(String lowPart){
            min = Integer.parseInt(lowPart);
    }
    protected void parseMax(String highPart){
            max = Integer.parseInt(highPart);
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
    public String toString(){
        StringBuilder B=new StringBuilder();
        B.append("Cardinality: "+super.getHowMany());
        B.append(" Min: ");
        if(hasSpecialMinStatus())
            B.append(getMinSpecialStatus());
        else
            B.append(getMin());
        B.append(" Max: ");
        if(hasSpecialMaxStatus())
            B.append(getMaxSpecialStatus());
        else
            B.append(getMax());
        
        B.append(" ");
        return B.toString();
    }
           
}
