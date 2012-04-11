package edu.umich.robot.network;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.Set;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;

import edu.umich.robot.Controller;
import edu.umich.robot.Robot;
import edu.umich.robot.TabletLCM;
import edu.umich.robot.events.DropObjectEvent;
import edu.umich.robot.events.ObjectAddedEvent;
import edu.umich.robot.events.PickUpObjectEvent;
import edu.umich.robot.events.RoomLightEvent;
import edu.umich.robot.events.control.DoorCloseEvent;
import edu.umich.robot.events.control.DoorOpenEvent;
import edu.umich.robot.metamap.AbridgedAreaDescription;
import edu.umich.robot.metamap.AbridgedGateway;
import edu.umich.robot.metamap.AbridgedWall;
import edu.umich.robot.metamap.AreaDescription;
import edu.umich.robot.metamap.Door;
import edu.umich.robot.metamap.Door.State;
import edu.umich.robot.metamap.Gateway;
import edu.umich.robot.metamap.RectArea;
import edu.umich.robot.metamap.VirtualObject;
import edu.umich.robot.metamap.VirtualObjectTemplate;
import edu.umich.robot.metamap.Wall;
import edu.umich.robot.radio.RadioHandler;
import edu.umich.robot.radio.RadioMessage;
import edu.umich.robot.splinter.Splinter;
import edu.umich.robot.util.ImmutablePose;
import edu.umich.robot.util.Pose;
import edu.umich.robot.util.events.RobotEvent;
import edu.umich.robot.util.events.RobotEventListener;

/**
 * Listens for commands over the network. Used to talk to the tablet.
 * 
 * @author miller
 * 
 */
public class Server implements RobotEventListener, RadioHandler {

	ServerSocket socket;
	boolean running;
	Controller controller;
	PrintWriter out;
	Thread currentThread;

	TabletLCM lcm;

	public Server(int port) throws IOException {
		socket = new ServerSocket(port);
		running = true;
		out = null;
		currentThread = null;
	}

	public void start() {
		running = true;
		new Thread() {
			public void run() {
				while (running) {
					try {
						System.out.println("About to listen for client");
						final Socket client = socket.accept();
						currentThread = new Thread() {
							@Override
							public void run() {
								System.out.println("Accepted client: "
										+ client.getInetAddress()
												.getCanonicalHostName());
								try {
									Scanner scanner = new Scanner(
											client.getInputStream())
											.useDelimiter("\n");
									boolean scanning = true;
									if (out != null) {
										out.flush();
										out.close();
									}
									out = new PrintWriter(
											client.getOutputStream());
									while (scanning) {
										// Read a command from the client and
										// respond
										// Print something to the command line
										// for debugging
										System.out
												.println("About to listen for line");
										String line = scanner.next();
										System.out
												.println("Got command from client "
														+ client.getInetAddress()
														+ ":"
														+ client.getPort()
														+ ":\n" + line);
										String response = handleCommand(line,
												client);
										System.out
												.println("Returning to client:\n"
														+ response);
										sendMessage(response);
										if (line.trim()
												.equalsIgnoreCase("quit")) {
											System.out.println("Quitting");
											scanning = false;
										}
									}
								} catch (NoSuchElementException e) {
									e.printStackTrace();
								} catch (IOException e) {
									e.printStackTrace();
								} finally {
									try {
										client.close();
									} catch (IOException e) {
										e.printStackTrace();
									}
								}
							}
						};
						currentThread.start();
					} catch (IOException e) {
						e.printStackTrace();
					}
					stopLCM();
				}
				try {
					socket.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
				System.out.println("Server done");
			}
		}.start();
	}

	private synchronized void sendMessage(String message) {
		if (out != null && message != null) {
			out.println(message);
			out.flush();
		}
	}

	private String handleCommand(String command, Socket client) {
		if (controller == null) {
			return "No controller found";
		}

		// map
		// Return map description
		if (command.equalsIgnoreCase("map")) {
			StringBuilder sb = new StringBuilder();
			sb.append("" + controller.getMetersPerPixel() + " ; ");
			int[] origin = controller.getImageOrigin();
			sb.append(origin[0] + " " + origin [1] + " ; ");
			for (AreaDescription ad : controller.getAreaList()) {
				if (ad instanceof RectArea) {
					AbridgedAreaDescription aad = abridgeAreaDescription((RectArea) ad);
					sb.append(aad.toString());
					sb.append(" ; ");
				}
			}
			
			// Also include information on which doors are closed
			// (They are open by default)
			Set<Integer> closedAreas = new HashSet<Integer>();
			for (AreaDescription ad : controller.getAreaList()) {
				int  currentAreaId = ad.getId();
				for (Gateway gateway : ad.getGateways())
				{
					Door door = gateway.getDoor();
					State doorState = door.getState();
					if (doorState != State.CLOSED)
					{
						continue;
					}
					for (int toId : gateway.getTo())
					{
						if (toId == currentAreaId)
						{
							continue;
						}
						closedAreas.add(toId);
					}
				}
			}
			for (AreaDescription ad : controller.getAreaList())
			{
				int thisId = ad.getId();
				if (closedAreas.contains(thisId))
				{
					String type = ad.getProperties().get("type");
					if (type != null && type == "door")
					{
						sb.append("\ndoor-close " + thisId);
					}
				}
			}
			
			// Also include information about which light are off
			// (They are on by default)
			for (AreaDescription ad : controller.getAreaList())
			{
				int thisId = ad.getId();
				if (!controller.getAreaState(thisId).isRoomLightOn())
				{
					sb.append("\nroom-light " + thisId + " " + false);
				}
			}
			for (AreaDescription ad : controller.getAreaList())
			{
				int thisId = ad.getId();
				if (closedAreas.contains(thisId))
				{
					String type = ad.getProperties().get("type");
					if (type != null && type == "door")
					{
						sb.append("\ndoor-close " + thisId);
					}
				}
			}
			
			return "map " + sb.toString();
		}

		// classes
		// Return description of object classes
		if (command.equalsIgnoreCase("classes")) {
			StringBuilder sb = new StringBuilder();
			for (VirtualObjectTemplate vot : controller.getTemplates()) {
				sb.append(vot.getName());
				sb.append(" { ");
				Map<String, String> properties = vot.getProperties();
				for (String key : properties.keySet()) {
					sb.append(key);
					sb.append(" : ");
					sb.append(properties.get(key));
					sb.append(" , ");
				}
				sb.append("};");
			}
			return "classes " + sb.toString();
		}

		// objects
		// Return object instances
		if (command.equalsIgnoreCase("objects")) {
			StringBuilder sb = new StringBuilder();
			for (VirtualObject obj : controller.getPlacedObjects()) {
				sb.append(stringForVirtualObject(obj));
			}
			return "objects " + sb.toString();
		}

		// robots
		// Return robots description
		if (command.equalsIgnoreCase("robots")) {
			StringBuilder sb = new StringBuilder();
			for (Object obj : controller.getAllRobots()) {
				sb.append(stringForRobot((Robot)obj));
			}
			return "robots " + sb.toString();
		}

		// pause
		// Toggle Soar's run state
		if (command.equalsIgnoreCase("pause")) {
			return "text "
					+ (controller.toggleSoarRunState() ? "Soar started"
							: "Soar paused");
		}
		
		if (command.equalsIgnoreCase("emulator")) {
			try {
				startLCM(InetAddress.getByName("127.0.0.1"), 12122);
				return "text Started LCM forwarding to emulator";
			} catch (UnknownHostException e) {
				e.printStackTrace();
				return "text Error forwarding LCM to emulator";
			}
		}

		if (command.equalsIgnoreCase("device")) {
			startLCM(client.getInetAddress(), 12122);
			return "Started LCM forwarding to device";
		}

		String[] tokens = command.split(" ");
		if (tokens[0].equalsIgnoreCase("object") && tokens.length >= 4) {
			controller.addObject(
					tokens[1],
					new double[] { Double.parseDouble(tokens[2]),
							Double.parseDouble(tokens[3]) });
			return "text Created " + tokens[1] + " at (" + tokens[2] + ", "
					+ tokens[3] + ")";
		}
		
		if (tokens[0].equalsIgnoreCase("text") && tokens.length >= 3)
		{
			ArrayList<String> ar = new ArrayList<String>();
			for (int i = 3; i < tokens.length; ++i)
			{
				ar.add(tokens[i]);
			}
			RadioMessage message = new RadioMessage.Builder(tokens[1]).destination(tokens[2]).tokens(ar).build();
			if (message != null)
			{
			    controller.getRadio().postRadioMessage(message);
			    return "text " + message.getDestination() + " received: \"" + message.getConcatenatedTokens(" ") + "\"";
			}
		}

		return "text Invalid command: " + command;
	}

	private static String stringForVirtualObject(VirtualObject obj) {
		Pose p = obj.getPose();
		return obj.getName() + " " + obj.getId() + " " + p.getX() + " " + p.getY() + " "
				+ p.getYaw() + ";";
	}
	
	private static String stringForRobot(Robot robot) {
		if (robot instanceof Splinter) {
			Splinter s = (Splinter) robot;
			Pose p = s.getOutput().getPose();
			return s.getName() + ' ' + p.getX() + ' ' + p.getY() + ' ' + p.getYaw() + ';';
		}
		return null;
	}

	public static AbridgedAreaDescription abridgeAreaDescription(RectArea sa) {
		ImmutablePose p = sa.getPose();
		int[] xywh_ar = sa.getPixelRect();
		/*
		ImmutableList<Double> xywh = new ImmutableList.Builder<Double>().add(
				p.getX(), p.getY(), p.getVX(), -p.getVY()).build();
		*/
		ImmutableList<Integer> xywh = new ImmutableList.Builder<Integer>().add(
				(Integer) xywh_ar[0],
				(Integer) xywh_ar[1],
				(Integer) xywh_ar[2],
				(Integer) xywh_ar[3]).build();

		ImmutableList.Builder<AbridgedGateway> gatewaysBuilder = new ImmutableList.Builder<AbridgedGateway>();
		for (Gateway g : sa.getGateways()) {
			gatewaysBuilder.add(abridgeGateway(g));
		}
		String type = sa.getProperties().get("type");
		if (type == null) {
			type = "none";
		}
		return new AbridgedAreaDescription(sa.getId(), xywh, type,
				gatewaysBuilder.build());
	}

	public static AbridgedGateway abridgeGateway(Gateway g) {
		ImmutableList<Double> xy = new ImmutableList.Builder<Double>().add(
				g.getPose().getX(), g.getPose().getY()).build();
		return new AbridgedGateway(g.getId(), xy);
	}
	
	public static AbridgedWall abridgeWall(Wall w) {
		ImmutableList<Double> xy = new ImmutableList.Builder<Double>().add(
				w.getMidpoint().getX(), w.getMidpoint().getY()).build();
		Builder<Integer> to = new ImmutableList.Builder<Integer>();
		for (Integer i : w.getTo()) {
			to.add(i);
		}

		return new AbridgedWall(xy, w.getDirection(), to.build());
	}

	public void stop() {
		running = false;
		stopLCM();
		try
        {
            socket.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
	}

	public void setController(Controller controller) {
		this.controller = controller;
	}

	private synchronized void startLCM(InetAddress client, int port) {
		if (lcm != null) {
			stopLCM();
		}
		String connectionString = "udp://" + client.getHostAddress() + ":" + port;
		try {
			lcm = new TabletLCM(connectionString);
			System.out.println("Started UDP LCM forwarding to client: " + connectionString);
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private synchronized void stopLCM() {
		if (lcm != null) {
			lcm.close();
			lcm = null;
		}
	}

	public void onEvent(RobotEvent event) {
		if (event instanceof ObjectAddedEvent) {
			ObjectAddedEvent oae = (ObjectAddedEvent) event;
			VirtualObject vo = oae.getObject();
			sendMessage("objects " + stringForVirtualObject(vo));
		} else if (event instanceof PickUpObjectEvent) {
			PickUpObjectEvent ore = (PickUpObjectEvent) event;
			Robot robot = ore.getRobot();
			int id = ore.getID();
			sendMessage("pickup-object " + robot.getName() + " " + id);
		} else if (event instanceof DropObjectEvent) {
			DropObjectEvent ore = (DropObjectEvent) event;
			Robot robot = ore.getRobot();
			sendMessage("drop-object " + robot.getName());
		} else if (event instanceof DoorCloseEvent) {
			DoorCloseEvent dce = (DoorCloseEvent) event;
			int id = dce.getId();
			sendMessage("door-close " + id);
		} else if (event instanceof DoorOpenEvent) {
			DoorOpenEvent dce = (DoorOpenEvent) event;
			int id = dce.getId();
			sendMessage("door-open " + id);
		} else if (event instanceof RoomLightEvent) {
			RoomLightEvent rle = (RoomLightEvent) event;
			int id = rle.getID();
			boolean on = rle.isOn();
			sendMessage("room-light " + id + " " + on);
		}
	}

	@Override
	public void radioMessageReceived(RadioMessage comm) {
		
		/*
		if (comm.getDestination() == null || !comm.getDestination().equalsIgnoreCase("user")) {
			return;
		}
		*/
		
		sendMessage("text " + comm.getFrom() + " to " + comm.getDestination() + ": \"" + comm.getConcatenatedTokens(" ") + "\"");
	}

}
