package edu.umich;

import org.rlcommunity.rlglue.codec.types.Action;
import java.util.Random;

import sml.Agent;
import sml.Kernel;
import sml.Identifier;

public class SoartoGlue {

	private static Agent agent;

	public SoartoGlue(Agent a) {
		agent = a;
	}

	public static Action getSoarAction() {
		Action act = new Action(3, 0);
		act.intArray[0] = 0;
		act.intArray[0] = 0;
		act.intArray[0] = 0;
		for (int i = 0; i < agent.GetNumberCommands(); ++i) {
			Identifier commandWME = agent.GetCommand(i);
			String commandName = commandWME.GetAttribute();
			if (commandName.equals("move")) {

				String direction = commandWME.GetParameterValue("direction");
				// System.out.println("In move" + "Direction"+direction);
				if (direction.equals("left")) {
					act.intArray[0] = -1;
				} else if (direction.equals("right")) {
					act.intArray[0] = 1;
				} else if (direction.equals("stay")) {
					act.intArray[0] = 0;
				}
				commandWME.AddStatusComplete();
			} else if (commandName.equals("jump")) {

				String jump = commandWME.GetParameterValue("up");
				//System.out.println("In jump" + "UP " + jump);
				if (jump.equals("no")) {
					act.intArray[1] = 0;
				} else if (jump.equals("yes")) {
					act.intArray[1] = 1;
				}
				commandWME.AddStatusComplete();
			} else if (commandName.equals("speed")) {

				String degree = commandWME.GetParameterValue("degree");
				//System.out.println("In speed" + "Degree" + degree);
				if (degree.equals("low")) {
					act.intArray[2] = 0;
				} else if (degree.equals("high")) {
					act.intArray[2] = 1;
				}
				commandWME.AddStatusComplete();
			} else
				System.out.println("Unknown command:" + commandName);

		}
		///System.out.println("0:" + act.intArray[0] + ",1:" + act.intArray[1]
		//		+ ",2:" + act.intArray[2]);
		agent.Commit();
		return act;

	}

}