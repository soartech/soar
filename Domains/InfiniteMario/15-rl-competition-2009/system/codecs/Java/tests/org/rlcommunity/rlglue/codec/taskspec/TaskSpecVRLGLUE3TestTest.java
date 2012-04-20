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

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author btanner
 */
public class TaskSpecVRLGLUE3TestTest {

    public TaskSpecVRLGLUE3TestTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    /**
     * Test of MountainCarVRLGlue3Spec method, of class TaskSpecVRLGLUE3Test.
     */
    @Test
    public void testMountainCarVRLGlue3Spec() {
    }
    
        @Test
    public void MountainCarVRLGlue3Spec() {
        String theTaskSpecString = "VERSION RL-Glue-3.0 PROBLEMTYPE episodic DISCOUNTFACTOR 1 "+
                "OBSERVATIONS DOUBLES (-1.2 0.5) (-.07 .07) ACTIONS INTS (0 2)  REWARDS (-1 0) "+
                "EXTRA Name=Traditional-Mountain-Car Cutoff=None Random-Starts=True";

        TaskSpec theTSO = new TaskSpec(theTaskSpecString);
        //Make sure that if we parse the output of  theTSO and then convert back to a string, we get the same string as theTSO generates
        assertEquals(new TaskSpec(theTSO.getStringRepresentation()).getStringRepresentation(), theTSO.getStringRepresentation());
        assertEquals(theTSO.getProblemType(), "episodic");
        assertEquals(theTSO.getDiscountFactor(),1.0d);
        assertEquals(theTSO.getNumDiscreteObsDims(), 0);
        assertEquals(theTSO.getNumContinuousObsDims(), 2);

        assertEquals(theTSO.getContinuousObservationRange(0).getMin(), -1.2d);
        assertEquals(theTSO.getContinuousObservationRange(0).getMax(), .5d);
        assertEquals(theTSO.getContinuousObservationRange(1).getMin(), -.07d);
        assertEquals(theTSO.getContinuousObservationRange(1).getMax(), .07d);
        assertEquals(theTSO.getNumContinuousActionDims(), 0);
        assertEquals(theTSO.getNumDiscreteActionDims(), 1);
        assertEquals(theTSO.getDiscreteActionRange(0).getMin(), 0);
        assertEquals(theTSO.getDiscreteActionRange(0).getMax(), 2);
        assertEquals(theTSO.getRewardRange().getMin(), -1d);
        assertEquals(theTSO.getRewardRange().getMax(), 0d);
        assertEquals(theTSO.getExtraString(), "Name=Traditional-Mountain-Car Cutoff=None Random-Starts=True");
    }

}