/* Random Agent that works in all domains
* Copyright (C) 2007, Brian Tanner brian@tannerpages.com (http://brian.tannerpages.com/)
* 
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public License
* as published by the Free Software Foundation; either version 2
* of the License, or (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. */
package tetrisexample;

import java.util.Random;

import org.rlcommunity.rlglue.codec.taskspec.TaskSpec;
import org.rlcommunity.rlglue.codec.taskspec.ranges.IntRange;
import org.rlcommunity.rlglue.codec.taskspec.ranges.DoubleRange;
import org.rlcommunity.rlglue.codec.AgentInterface;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;

public class RandomAgent implements AgentInterface {
	private Action action;
	private int numInts =1;
	private int numDoubles =0;
	private Random random = new Random();

        TaskSpec TSO=null;
	

        
        public RandomAgent(){
        }

	public void agent_cleanup() {
	}

	public void agent_end(double arg0) {

	}

	public void agent_freeze() {

	}

	public void agent_init(String taskSpec) {
            TSO = new TaskSpec(taskSpec);

            action = new Action(TSO.getNumDiscreteActionDims(),TSO.getNumContinuousActionDims());	
	}

	public String agent_message(String arg0) {
            return null;
	}

	public Action agent_start(Observation o) {
            randomify(action);
            return action;
	}

	public Action agent_step(double arg0, Observation o) {
            randomify(action);
            return action;
	}

	private void randomify(Action action){
        for (int i = 0; i < TSO.getNumDiscreteActionDims(); i++) {
            IntRange thisActionRange = TSO.getDiscreteActionRange(i);
            action.intArray[i] = random.nextInt(thisActionRange.getRangeSize()) + thisActionRange.getMin();
        }
        for (int i = 0; i < TSO.getNumContinuousActionDims(); i++) {
            DoubleRange thisActionRange = TSO.getContinuousActionRange(i);
            action.doubleArray[i] = random.nextDouble() * (thisActionRange.getRangeSize()) + thisActionRange.getMin();
        }
       	}
}
