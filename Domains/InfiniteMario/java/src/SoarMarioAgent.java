package edu.umich;

import org.rlcommunity.rlglue.codec.AgentInterface;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.util.AgentLoader;


import edu.umich.GluetoSoar;
import edu.umich.SoartoGlue;
import sml.*;


import java.io.Console;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

public class SoarMarioAgent implements AgentInterface{
	
	private static Kernel kernel;
	private static Agent agent;
	private EventListener listener;
	private int episode;
	private GluetoSoar inputToSoar;
	private SoartoGlue outputFromSoar;
	private double Reward;
	private static String productions;
	private static String csvFile;
	private static PrintWriter csv;
	
	private static double eligibilityTrace;
	private static double learningRate;
	private static double exploration;
	private static double discountRate;
	private static String learningPolicy;
	private static String explorationPolicy;
	private static String explorationEpsilon;
	private static String explorationTemperature;
	private static double eligibilityTraceDR;
	private static double eligibilityTraceT;
	private static String loadRLRules;
	private static String rlRuleFile;
	private static String saveRLRules;
	private static String reductionPolicy;
	private static double reductionRate;
	private static String autoReduce;
	
	
	public static class EventListener implements Agent.PrintEventInterface,Kernel.SystemEventInterface{
		public void systemEventHandler(int eventID, Object data, Kernel kernel)
		{
			if (eventID == smlSystemEventId.smlEVENT_SYSTEM_START.swigValue()) {
			//	System.out.println("Soar started.") ;
			} else if (eventID == smlSystemEventId.smlEVENT_SYSTEM_STOP.swigValue()) {
				//System.out.println("Soar stopped.") ;
			}
			else
			System.out.println("Received system event in Java") ;
		}
		
		public void printEventHandler(int eventID, Object data, Agent agent, String message)
		{
			System.out.println("Received print event in Java: " + message) ;
		}
		public void outputNotificationHandler(Object data, Agent agent)
		{
			System.out.println("Received an output notification in Java") ;
		}

		public void outputEventHandler(Object data, String agentName, String attributeName, WMElement pWmeAdded)
		{
			System.out.println("Received output event in Java for wme: " + pWmeAdded.GetIdentifierName() + " ^" + pWmeAdded.GetAttribute() + " " + pWmeAdded.GetValueAsString());
			SoarMarioAgent app = (SoarMarioAgent)data ;
			app.printWMEs(pWmeAdded) ;
		}
		
	}
	
	SoarMarioAgent() throws IOException {
	
		episode = 0;
		Reward = 0.0;
		
		csv = new PrintWriter(csvFile);
		
		kernel = Kernel.CreateKernelInNewThread();
		if (kernel.HadError())
			throw new IllegalStateException("Error initializing kernel: " + kernel.GetLastErrorDescription()) ;
		
		String version = kernel.GetSoarKernelVersion() ;
		System.out.println("Soar version " + version) ;
		
		agent = kernel.CreateAgent("mario") ;
		if (kernel.HadError())
			throw new IllegalStateException("Error creating agent: " + kernel.GetLastErrorDescription()) ;
		
		setRLParams();

		boolean load = agent.LoadProductions(productions);
		if (!load || agent.HadError())
			throw new IllegalStateException("Error loading productions: " + agent.GetLastErrorDescription()) ;
		
		kernel.SetAutoCommit(false);
		
		inputToSoar = new GluetoSoar(agent);
		outputFromSoar = new SoartoGlue(agent);
		EventListener listener = new EventListener();
		
		//agent.RegisterForPrintEvent(smlPrintEventId.smlEVENT_PRINT, listener,null) ;				
		kernel.RegisterForSystemEvent(smlSystemEventId.smlEVENT_SYSTEM_START, listener, null);
		kernel.RegisterForSystemEvent(smlSystemEventId.smlEVENT_SYSTEM_STOP, listener, null);
	}

	public void agent_init(String task) {
		inputToSoar.reset();
	}
	
	public void agent_cleanup() {
	}
	public Action agent_start(Observation o) {
		Reward = 0.0;
	/*	for (int i = 0;i <16; i++){
			for(int j = 0; j < 21; j++){
				System.out.print(o.charArray[22*i+j]);
			}
			System.out.println();
		}*/
		inputToSoar.writeToSoar(o,0.0);
		agent.RunSelfTilOutput();
		return (outputFromSoar.getSoarAction());
		
	}
	public Action agent_step (double r, Observation o) {
		
	/*	for (int i = 0;i <16; i++){
			for(int j = 0; j < 21; j++){
				System.out.print(o.charArray[22*i+j]);
			}
			System.out.println();
		}*/
		
		Reward += r;
		inputToSoar.writeToSoar(o,r);
		agent.RunSelfTilOutput();
		return (outputFromSoar.getSoarAction());
	}
	public void agent_end(double r) {
		
		Reward += r;
		inputToSoar.writeToSoar(r);
		agent.RunSelfTilOutput();
		agent.InitSoar();
		inputToSoar.reset();
		System.out.println("Soar Inititalized. Episode : " +episode +" Reward :" + Reward);
		csv.write(Reward+"\n");
		episode++;
	}
	public String agent_message(String msg) {
		return null;
	}
	public void printWMEs(WMElement pRoot)
	{
		if (pRoot.GetParent() == null)
			System.out.println("Top Identifier " + pRoot.GetValueAsString()) ;
		else
		{
			System.out.println("(" + pRoot.GetParent().GetIdentifierSymbol() + " ^" + pRoot.GetAttribute() + " " + pRoot.GetValueAsString() + ")") ;
		}

		if (pRoot.IsIdentifier())
		{
			Identifier pID = pRoot.ConvertToIdentifier() ;
			int size = pID.GetNumberChildren();
			for (int i = 0 ; i < size ; i++)
			{
				WMElement pWME = pID.GetChild(i) ;

				printWMEs(pWME) ;
			}
		}
	}
	
	public void setRLParams(){
		
		//turn rl on
		String name = agent.GetAgentName();
		String cmd = "rl --set learning on";
		kernel.ExecuteCommandLine(cmd, name);
		
		cmd = "rl --set learning-policy " + learningPolicy;
		kernel.ExecuteCommandLine(cmd, name);
		
		cmd = "rl --set learning-rate " + learningRate;
		kernel.ExecuteCommandLine(cmd, name);
		
		cmd = "rl --set discount-rate " + discountRate;
		kernel.ExecuteCommandLine(cmd, name);
		
		cmd = "rl --set eligibility-trace-decay-rate " + eligibilityTraceDR;
		kernel.ExecuteCommandLine(cmd, name);
		
		cmd = "rl --set eligibility-trace-tolerance " + eligibilityTraceT;
		kernel.ExecuteCommandLine(cmd, name);
		
		cmd = "indifferent-selection  --" + explorationPolicy;
		kernel.ExecuteCommandLine(cmd, name);
		
		cmd = "indifferent-selection  --auto-reduce " + autoReduce;
		kernel.ExecuteCommandLine(cmd, name);
		
		cmd = "gp-max  2000000";
		kernel.ExecuteCommandLine(cmd, name);
		
		//cmd = "gp-max";
		String temp = kernel.ExecuteCommandLine(cmd, name);
		System.out.println (temp);
		
		
		
		if(explorationPolicy.equals("epsilon-greedy")){
			cmd = "indifferent-selection  --epsilon " + explorationEpsilon;
			kernel.ExecuteCommandLine(cmd, name);	
			if (autoReduce.equals("on")){
				cmd = "indifferent-selection  --reduction-policy epsilon " + reductionPolicy;
				kernel.ExecuteCommandLine(cmd, name);
				cmd = "indifferent-selection  --reduction-rate epsilon " + reductionPolicy +" " + reductionRate;
				kernel.ExecuteCommandLine(cmd, name);
			}
		}
		
		if(explorationPolicy.equals("boltzmann")){
			cmd = "indifferent-selection  --temprature " + explorationTemperature;
			kernel.ExecuteCommandLine(cmd, name);
			if(autoReduce.equals("on")){
				cmd = "indifferent-selection  --reduction-policy temprature " + reductionPolicy;
				kernel.ExecuteCommandLine(cmd, name);
				cmd = "indifferent-selection  --reduction-rate temprature " + reductionPolicy +" " + reductionRate;
				kernel.ExecuteCommandLine(cmd, name);
			}
		}
		
		if(loadRLRules.equals("yes")){
			cmd = "source " + rlRuleFile;
			kernel.ExecuteCommandLine(cmd, name);
			System.out.println(cmd);
		}
		
	}
	public static void main(String[] args) throws IOException {
		
		String prod,cmd, RLrules;
		
		PrintWriter rlFile;
		
		if(args.length < 1){
			System.out.println("You forgot the config file maybe.");
			System.exit(0);
		}
		
		
		Properties configFile = new Properties();
		FileInputStream in = new FileInputStream(args[0]);
		configFile.load(in);
		
		prod = configFile.getProperty("productions");
		
		productions = prod;
		
		learningPolicy = configFile.getProperty("learning-policy", "sarsa");
		
		
		learningRate = Double.parseDouble(configFile.getProperty("learning-rate", "0.3"));
		
			
		discountRate = Double.parseDouble(configFile.getProperty("discount-rate", "0.9"));
		
		
		eligibilityTraceDR = Double.parseDouble(configFile.getProperty("eligibility-trace-decay-rate", "0"));
				
		eligibilityTraceT = Double.parseDouble(configFile.getProperty("eligibility-trace-tolerance", "0.001"));
		
		
		explorationPolicy = configFile.getProperty("exploration", "epsilon-greedy");
		
		
		explorationEpsilon = configFile.getProperty("epsilon", "0.1");
		
		
		explorationTemperature = configFile.getProperty("temprature", "25");
		
		
		autoReduce = configFile.getProperty("auto-reduce", "off");
		
		
		reductionPolicy = configFile.getProperty("reduction-policy", "exponential");
		
		
		reductionRate = Double.parseDouble(configFile.getProperty("reduction-rate", "0.9"));
		
		
		loadRLRules = configFile.getProperty("load-rl-rules", "no");
		rlRuleFile = configFile.getProperty("rl-rules-file");
		saveRLRules = configFile.getProperty("save-rl-rules", "no");
		
		csvFile = configFile.getProperty("data-file");
		
		
		new AgentLoader(new SoarMarioAgent()).run();
		
		if(saveRLRules.equals("yes")){
			System.out.println("printing");
			rlFile= new PrintWriter(rlRuleFile);
			cmd = "print --rl";
			RLrules = kernel.ExecuteCommandLine(cmd, agent.GetAgentName());
		//	System.out.println(RLrules);
			rlFile.write(RLrules);
			rlFile.close();
		}
		
		
		kernel.Shutdown();
		kernel.delete();
		csv.close();
	}
}
