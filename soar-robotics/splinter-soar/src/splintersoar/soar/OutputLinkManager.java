package splintersoar.soar;

import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import sml.*;
import splintersoar.LogFactory;
import splintersoar.lcmtypes.splinterstate_t;

/**
 * @author voigtjr
 * Soar output-link management. Creates input for splinter and other parts of the system.
 */
public class OutputLinkManager {

	Agent agent;
	Waypoints waypoints;

	SplinterInput splinterInput = new SplinterInput();
	Logger logger;

	public OutputLinkManager(Agent agent, Waypoints waypoints) {
		this.logger = LogFactory.createSimpleLogger("OutputLinkManager", Level.INFO);

		this.agent = agent;
		this.waypoints = waypoints;
	}

	public SplinterInput update(splinterstate_t splinterState) {
		SplinterInput newSplinterInput = null;

		// process output
		for (int i = 0; i < agent.GetNumberCommands(); ++i) {
			Identifier commandwme = agent.GetCommand(i);
			String commandName = commandwme.GetAttribute();

			if (commandName.equals("motor")) {
				if (newSplinterInput != null) {
					// This is a warning
					logger.warning("Motor command received possibly overriding previous orders");
				}

				double[] motorThrottle = { 0, 0 };

				try {
					motorThrottle[0] = Double.parseDouble(commandwme.GetParameterValue("left"));
				} catch (NullPointerException ex) {
					logger.warning("No left on motor command");
					commandwme.AddStatusError();
					continue;
				} catch (NumberFormatException e) {
					logger.warning("Unable to parse left: " + commandwme.GetParameterValue("left"));
					commandwme.AddStatusError();
					continue;
				}

				try {
					motorThrottle[1] = Double.parseDouble(commandwme.GetParameterValue("right"));
				} catch (NullPointerException ex) {
					logger.warning("No right on motor command");
					commandwme.AddStatusError();
					continue;
				} catch (NumberFormatException e) {
					logger.warning("Unable to parse right: " + commandwme.GetParameterValue("right"));
					commandwme.AddStatusError();
					continue;
				}

				motorThrottle[0] = Math.max(motorThrottle[0], -1.0);
				motorThrottle[0] = Math.min(motorThrottle[0], 1.0);

				motorThrottle[1] = Math.max(motorThrottle[1], -1.0);
				motorThrottle[1] = Math.min(motorThrottle[1], 1.0);

				logger.fine(String.format("motor: %10.3f %10.3f", motorThrottle[0], motorThrottle[1]));

				newSplinterInput = new SplinterInput(motorThrottle);

				commandwme.AddStatusComplete();
				continue;
			} else if (commandName.equals("move")) {
				if (newSplinterInput != null) {
					logger.warning("Move command received but motors already have orders");
					commandwme.AddStatusError();
					continue;
				}

				String direction = commandwme.GetParameterValue("direction");
				if (direction == null) {
					logger.warning("No direction on move command");
					commandwme.AddStatusError();
					continue;
				}

				double throttle = 0;
				try {
					throttle = Double.parseDouble(commandwme.GetParameterValue("throttle"));
				} catch (NullPointerException ex) {
					logger.warning("No throttle on move command");
					commandwme.AddStatusError();
					continue;
				} catch (NumberFormatException e) {
					logger.warning("Unable to parse throttle: " + commandwme.GetParameterValue("throttle"));
					commandwme.AddStatusError();
					continue;
				}

				throttle = Math.max(throttle, 0);
				throttle = Math.min(throttle, 1.0);

				logger.fine(String.format("move: %10s %10.3f", direction, throttle));

				if (direction.equals("backward")) {
					newSplinterInput = new SplinterInput(throttle * -1);
				} else if (direction.equals("forward")) {
					newSplinterInput = new SplinterInput(throttle);
				} else if (direction.equals("stop")) {
					newSplinterInput = new SplinterInput(0);
				} else {
					logger.warning("Unknown direction on move command: " + commandwme.GetParameterValue("direction"));
					commandwme.AddStatusError();
					continue;
				}

				commandwme.AddStatusComplete();
				continue;
			} else if (commandName.equals("rotate")) {
				if (newSplinterInput != null) {
					logger.warning("Rotate command received but motors already have orders");
					commandwme.AddStatusError();
					continue;
				}

				String direction = commandwme.GetParameterValue("direction");
				if (direction == null) {
					logger.warning("No direction on rotate command");
					commandwme.AddStatusError();
					continue;
				}

				double throttle = 0;
				try {
					throttle = Double.parseDouble(commandwme.GetParameterValue("throttle"));
				} catch (NullPointerException ex) {
					logger.warning("No throttle on rotate command");
					commandwme.AddStatusError();
					continue;
				} catch (NumberFormatException e) {
					logger.warning("Unable to parse throttle: " + commandwme.GetParameterValue("throttle"));
					commandwme.AddStatusError();
					continue;
				}

				throttle = Math.max(throttle, 0);
				throttle = Math.min(throttle, 1.0);

				logger.fine(String.format("rotate: %10s %10.3f", direction, throttle));

				if (direction.equals("left")) {
					newSplinterInput = new SplinterInput(SplinterInput.Direction.left, throttle);
				} else if (direction.equals("right")) {
					newSplinterInput = new SplinterInput(SplinterInput.Direction.right, throttle);
				} else if (direction.equals("stop")) {
					newSplinterInput = new SplinterInput(0);
				} else {
					logger.warning("Unknown direction on rotate command: " + commandwme.GetParameterValue("direction"));
					commandwme.AddStatusError();
					continue;
				}

				commandwme.AddStatusComplete();
				continue;

			} else if (commandName.equals("rotate-to")) {
				if (newSplinterInput != null) {
					logger.warning("Rotate-to command received but motors already have orders");
					commandwme.AddStatusError();
					continue;
				}

				double yaw = 0;
				try {
					yaw = Double.parseDouble(commandwme.GetParameterValue("yaw"));
				} catch (NullPointerException ex) {
					logger.warning("No yaw on rotate-to command");
					commandwme.AddStatusError();
					continue;
				} catch (NumberFormatException e) {
					logger.warning("Unable to parse yaw: " + commandwme.GetParameterValue("yaw"));
					commandwme.AddStatusError();
					continue;
				}
				yaw = Math.toRadians(yaw);

				double tolerance = 0;
				try {
					tolerance = Double.parseDouble(commandwme.GetParameterValue("tolerance"));
				} catch (NullPointerException ex) {
					logger.warning("No tolerance on rotate-to command");
					commandwme.AddStatusError();
					continue;
				} catch (NumberFormatException e) {
					logger.warning("Unable to parse tolerance: " + commandwme.GetParameterValue("tolerance"));
					commandwme.AddStatusError();
					continue;
				}

				tolerance = Math.toRadians(tolerance);
				tolerance = Math.max(tolerance, 0);
				tolerance = Math.min(tolerance, Math.PI);

				double throttle = 0;
				try {
					throttle = Double.parseDouble(commandwme.GetParameterValue("throttle"));
				} catch (NullPointerException ex) {
					logger.warning("No throttle on rotate-to command");
					commandwme.AddStatusError();
					continue;
				} catch (NumberFormatException e) {
					logger.warning("Unable to parse throttle: " + commandwme.GetParameterValue("throttle"));
					commandwme.AddStatusError();
					continue;
				}

				throttle = Math.max(throttle, 0);
				throttle = Math.min(throttle, 1.0);

				logger.fine(String.format("rotate-to: %10.3f %10.3f %10.3f", yaw, tolerance, throttle));

				newSplinterInput = new SplinterInput(yaw, tolerance, throttle);

				commandwme.AddStatusComplete();
				continue;

			} else if (commandName.equals("stop")) {
				if (newSplinterInput != null) {
					// This is a warning
					logger.warning("Stop command received, possibly overriding previous orders");
				}

				logger.fine("stop:");

				newSplinterInput = new SplinterInput(0);

				commandwme.AddStatusComplete();
				continue;

			} else if (commandName.equals("add-waypoint")) {
				String id = commandwme.GetParameterValue("id");
				if (id == null) {
					logger.warning("No id on add-waypoint command");
					commandwme.AddStatusError();
					continue;
				}

				double[] pos = Arrays.copyOf(splinterState.pose.pos, splinterState.pose.pos.length);
				try {
					pos[0] = Double.parseDouble(commandwme.GetParameterValue("x"));
				} catch (NullPointerException ignored) {
					// no x param is ok, use current
				} catch (NumberFormatException e) {
					logger.warning("Unable to parse x: " + commandwme.GetParameterValue("x"));
					commandwme.AddStatusError();
					continue;
				}

				try {
					pos[1] = Double.parseDouble(commandwme.GetParameterValue("y"));
				} catch (NullPointerException ignored) {
					// no y param is ok, use current
				} catch (NumberFormatException e) {
					logger.warning("Unable to parse y: " + commandwme.GetParameterValue("y"));
					commandwme.AddStatusError();
					continue;
				}

				logger.fine(String.format("add-waypoint: %16s %10.3f %10.3f", id, pos[0], pos[1]));

				waypoints.add(pos, id);

				commandwme.AddStatusComplete();
				continue;
			} else if (commandName.equals("remove-waypoint")) {
				String id = commandwme.GetParameterValue("id");
				if (id == null) {
					logger.warning("No id on remove-waypoint command");
					commandwme.AddStatusError();
					continue;
				}

				logger.fine(String.format("remove-waypoint: %16s", id));

				if (waypoints.remove(id) == false) {
					logger.warning("Unable to remove waypoint " + id + ", no such waypoint");
					commandwme.AddStatusError();
					continue;
				}

				commandwme.AddStatusComplete();
				continue;
			} else if (commandName.equals("enable-waypoint")) {
				String id = commandwme.GetParameterValue("id");
				if (id == null) {
					logger.warning("No id on enable-waypoint command");
					commandwme.AddStatusError();
					continue;
				}

				logger.fine(String.format("enable-waypoint: %16s", id));

				if (waypoints.enable(id) == false) {
					logger.warning("Unable to enable waypoint " + id + ", no such waypoint");
					commandwme.AddStatusError();
					continue;
				}

				commandwme.AddStatusComplete();
				continue;
			} else if (commandName.equals("disable-waypoint")) {
				String id = commandwme.GetParameterValue("id");
				if (id == null) {
					logger.warning("No id on disable-waypoint command");
					commandwme.AddStatusError();
					continue;
				}

				logger.fine(String.format("disable-waypoint: %16s", id));

				if (waypoints.disable(id) == false) {
					logger.warning("Unable to disable waypoint " + id + ", no such waypoint");
					commandwme.AddStatusError();
					continue;
				}

				commandwme.AddStatusComplete();
				continue;
			} else if (commandName.equals("broadcast-message")) {
				logger.warning("broadcast-message command not implemented, ignoring");
				continue;
			}

			logger.warning("Unknown command: " + commandName);
			commandwme.AddStatusError();
		}

		return newSplinterInput;
	}
}
