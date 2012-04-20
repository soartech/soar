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
package org.rlcommunity.rlglue.codec.taskspec;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author btanner
 */
public class TaskSpecVRLGLUE3_BackwardsV2CompatTest {

    public TaskSpecVRLGLUE3_BackwardsV2CompatTest() {
    }

    @Test
    public void OldUNSPECTest() {
        int taskSpecVersion = 2;
        String theTaskSpecString = taskSpecVersion + ":e:10_[f,f,f,f,f,i,i,i,i,i]_[,.6]_[-.07,]_[,]_[]_[ ,  ]_[,6]_[-7,]_[,]_[]_[ ,  ]:10_[f,f,f,f,f,i,i,i,i,i]_[,.6]_[-.07,]_[,]_[]_[ ,  ]_[,6]_[-7,]_[,]_[]_[ ,  ]:[-1,0]";
        TaskSpec theTSO = new TaskSpec(theTaskSpecString);
        //Make sure that if we parse the output of  theTSO and then convert back to a string, we get the same string as theTSO generates
        assertEquals(new TaskSpec(theTSO.getStringRepresentation()).getStringRepresentation(), theTSO.getStringRepresentation());
        assertEquals(theTSO.getNumDiscreteObsDims(), 5);
        assertEquals(theTSO.getNumContinuousObsDims(), 5);

        assertTrue(theTSO.getContinuousObservationRange(0).getMinUnspecified());
        assertEquals(theTSO.getContinuousObservationRange(0).getMax(), .6d);

        assertEquals(theTSO.getContinuousObservationRange(1).getMin(), -.07d);
        assertTrue(theTSO.getContinuousObservationRange(1).getMaxUnspecified());

        assertTrue(theTSO.getContinuousObservationRange(2).getMinUnspecified());
        assertTrue(theTSO.getContinuousObservationRange(2).getMaxUnspecified());

        assertTrue(theTSO.getContinuousObservationRange(3).getMinUnspecified());
        assertTrue(theTSO.getContinuousObservationRange(3).getMaxUnspecified());

        assertTrue(theTSO.getContinuousObservationRange(4).getMinUnspecified());
        assertTrue(theTSO.getContinuousObservationRange(4).getMaxUnspecified());

        assertTrue(theTSO.getDiscreteObservationRange(0).getMinUnspecified());
        assertEquals(theTSO.getDiscreteObservationRange(0).getMax(), 6);

        assertEquals(theTSO.getDiscreteObservationRange(1).getMin(), -7);
        assertTrue(theTSO.getDiscreteObservationRange(1).getMaxUnspecified());

        assertTrue(theTSO.getDiscreteObservationRange(2).getMinUnspecified());
        assertTrue(theTSO.getDiscreteObservationRange(2).getMaxUnspecified());

        assertTrue(theTSO.getDiscreteObservationRange(3).getMinUnspecified());
        assertTrue(theTSO.getDiscreteObservationRange(3).getMaxUnspecified());

        assertTrue(theTSO.getDiscreteObservationRange(4).getMinUnspecified());
        assertTrue(theTSO.getDiscreteObservationRange(4).getMaxUnspecified());


        //ACtions
        assertEquals(theTSO.getNumDiscreteActionDims(), 5);
        assertEquals(theTSO.getNumContinuousActionDims(), 5);

        assertTrue(theTSO.getContinuousActionRange(0).getMinUnspecified());
        assertEquals(theTSO.getContinuousActionRange(0).getMax(), .6d);

        assertEquals(theTSO.getContinuousActionRange(1).getMin(), -.07d);
        assertTrue(theTSO.getContinuousActionRange(1).getMaxUnspecified());

        assertTrue(theTSO.getContinuousActionRange(2).getMinUnspecified());
        assertTrue(theTSO.getContinuousActionRange(2).getMaxUnspecified());

        assertTrue(theTSO.getContinuousActionRange(3).getMinUnspecified());
        assertTrue(theTSO.getContinuousActionRange(3).getMaxUnspecified());

        assertTrue(theTSO.getContinuousActionRange(4).getMinUnspecified());
        assertTrue(theTSO.getContinuousActionRange(4).getMaxUnspecified());

        assertTrue(theTSO.getDiscreteActionRange(0).getMinUnspecified());
        assertEquals(theTSO.getDiscreteActionRange(0).getMax(), 6);

        assertEquals(theTSO.getDiscreteActionRange(1).getMin(), -7);
        assertTrue(theTSO.getDiscreteActionRange(1).getMaxUnspecified());

        assertTrue(theTSO.getDiscreteActionRange(2).getMinUnspecified());
        assertTrue(theTSO.getDiscreteActionRange(2).getMaxUnspecified());

        assertTrue(theTSO.getDiscreteActionRange(3).getMinUnspecified());
        assertTrue(theTSO.getDiscreteActionRange(3).getMaxUnspecified());

        assertTrue(theTSO.getDiscreteActionRange(4).getMinUnspecified());
        assertTrue(theTSO.getDiscreteActionRange(4).getMaxUnspecified());

    }

    @Test
    public void OldInfest() {
        int taskSpecVersion = 2;
        String theTaskSpecString = taskSpecVersion + ":e:5_[f,f,f,f,f,,]_[-inf,.6]_[-.07,inf]_[-inf,]_[,inf]_[-inf,inf]:5_[f,f,f,f,f,,]_[-inf,.6]_[-.07,inf]_[-inf,]_[,inf]_[-inf,inf]:[-1,0]";
        TaskSpec theTSO = new TaskSpec(theTaskSpecString);
        //Make sure that if we parse the output of  theTSO and then convert back to a string, we get the same string as theTSO generates
        assertEquals(new TaskSpec(theTSO.getStringRepresentation()).getStringRepresentation(), theTSO.getStringRepresentation());
        assertEquals(theTSO.getNumDiscreteObsDims(), 0);
        assertEquals(theTSO.getNumContinuousObsDims(), 5);

        assertTrue(theTSO.getContinuousObservationRange(0).getMinNegInf());
        assertEquals(theTSO.getContinuousObservationRange(0).getMax(), .6d);

        assertEquals(theTSO.getContinuousObservationRange(1).getMin(), -.07d);
        assertTrue(theTSO.getContinuousObservationRange(1).getMaxInf());

        assertTrue(theTSO.getContinuousObservationRange(2).getMinNegInf());
        assertTrue(theTSO.getContinuousObservationRange(2).getMaxUnspecified());

        assertTrue(theTSO.getContinuousObservationRange(3).getMinUnspecified());
        assertTrue(theTSO.getContinuousObservationRange(3).getMaxInf());

        assertTrue(theTSO.getContinuousObservationRange(4).getMinNegInf());
        assertTrue(theTSO.getContinuousObservationRange(4).getMaxInf());

        assertEquals(theTSO.getNumDiscreteActionDims(), 0);
        assertEquals(theTSO.getNumContinuousActionDims(), 5);

        assertTrue(theTSO.getContinuousActionRange(0).getMinNegInf());
        assertEquals(theTSO.getContinuousActionRange(0).getMax(), .6d);

        assertEquals(theTSO.getContinuousActionRange(1).getMin(), -.07d);
        assertTrue(theTSO.getContinuousActionRange(1).getMaxInf());

        assertTrue(theTSO.getContinuousActionRange(2).getMinNegInf());
        assertTrue(theTSO.getContinuousActionRange(2).getMaxUnspecified());

        assertTrue(theTSO.getContinuousActionRange(3).getMinUnspecified());
        assertTrue(theTSO.getContinuousActionRange(3).getMaxInf());

        assertTrue(theTSO.getContinuousActionRange(4).getMinNegInf());
        assertTrue(theTSO.getContinuousActionRange(4).getMaxInf());

    }

    @Test
    public void MountainCarV2Spec() {
        int taskSpecVersion = 2;
        String theTaskSpecString = taskSpecVersion + ":e:2_[f,f]_[-1.2,.6]_[-.07,.07]:1_[i]_[0,2]:[-1,0]";
        TaskSpec theTSO = new TaskSpec(theTaskSpecString);
        //Make sure that if we parse the output of  theTSO and then convert back to a string, we get the same string as theTSO generates
        assertEquals(new TaskSpec(theTSO.getStringRepresentation()).getStringRepresentation(), theTSO.getStringRepresentation());
        assertEquals(theTSO.getProblemType(), "episodic");
        assertEquals(theTSO.getNumDiscreteObsDims(), 0);
        assertEquals(theTSO.getNumContinuousObsDims(), 2);

        assertEquals(theTSO.getContinuousObservationRange(0).getMin(), -1.2d);
        assertEquals(theTSO.getContinuousObservationRange(0).getMax(), .6d);
        assertEquals(theTSO.getContinuousObservationRange(1).getMin(), -.07d);
        assertEquals(theTSO.getContinuousObservationRange(1).getMax(), .07d);
        assertEquals(theTSO.getNumContinuousActionDims(), 0);
        assertEquals(theTSO.getNumDiscreteActionDims(), 1);
        assertEquals(theTSO.getDiscreteActionRange(0).getMin(), 0);
        assertEquals(theTSO.getDiscreteActionRange(0).getMax(), 2);
        assertEquals(theTSO.getRewardRange().getMin(), -1d);
        assertEquals(theTSO.getRewardRange().getMax(), 0d);
        assertEquals(theTSO.getExtraString(), "");
    }

    @Test
    public void MountainCarV3Spec() {
        int taskSpecVersion = 3;
        String theTaskSpecString = taskSpecVersion + ":e:2_[f,f]_[-1.2,.6]_[-.07,.07]:1_[i]_[0,2]:[-1,0]:Extra text here";
        TaskSpec theTSO = new TaskSpec(theTaskSpecString);
        //Make sure that if we parse the output of  theTSO and then convert back to a string, we get the same string as theTSO generates
        assertEquals(new TaskSpec(theTSO.getStringRepresentation()).getStringRepresentation(), theTSO.getStringRepresentation());
        assertEquals(theTSO.getProblemType(), "episodic");
        assertEquals(theTSO.getNumDiscreteObsDims(), 0);
        assertEquals(theTSO.getNumContinuousObsDims(), 2);

        assertEquals(theTSO.getContinuousObservationRange(0).getMin(), -1.2d);
        assertEquals(theTSO.getContinuousObservationRange(0).getMax(), .6d);
        assertEquals(theTSO.getContinuousObservationRange(1).getMin(), -.07d);
        assertEquals(theTSO.getContinuousObservationRange(1).getMax(), .07d);
        assertEquals(theTSO.getNumContinuousActionDims(), 0);
        assertEquals(theTSO.getNumDiscreteActionDims(), 1);
        assertEquals(theTSO.getDiscreteActionRange(0).getMin(), 0);
        assertEquals(theTSO.getDiscreteActionRange(0).getMax(), 2);
        assertEquals(theTSO.getRewardRange().getMin(), -1d);
        assertEquals(theTSO.getRewardRange().getMax(), 0d);
        assertEquals(theTSO.getExtraString(), "Extra text here");
    }

}