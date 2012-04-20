package edu.umich.dice3;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import org.ini4j.InvalidFileFormatException;
import org.ini4j.Wini;

import sml.Agent;
import sml.Agent.OutputNotificationInterface;
import sml.Agent.PrintEventInterface;
import sml.Agent.ProductionEventInterface;
import sml.ClientAnalyzedXML;
import sml.Identifier;
import sml.IntElement;
import sml.Kernel;
import sml.Kernel.RhsFunctionInterface;
import sml.WMElement;
import sml.smlPrintEventId;
import sml.smlProductionEventId;
import sml.smlRunState;
import sml.sml_Names;
import edu.umich.dice3.args.ArgsParser;
import edu.umich.dice3.gamestate.Bid;
import edu.umich.dice3.gamestate.DiceGameState;
import edu.umich.dice3.gamestate.Player;
import edu.umich.dice3.gamestate.exceptions.DiceActionException;
import edu.umich.dice3.ui.AgentChooser;
import edu.umich.dice3.ui.AgentChooser.RunType;
import edu.umich.dice3.ui.DiceFrame;
import edu.umich.dice3.ui.UIAction;
import edu.umich.soar.SoarProperties;

public class SoarMatch
{
    public static final String USER_STRING = "***USER***";

    // For RL runs, how many bins should be made.
    public static int RL_BINS = 100;

    public static boolean debuggingEnabled = true;
    private static File debugFile;

    private static boolean handleBid(DiceGameState game, int playerId, int multiplier, int face) throws IOException, InterruptedException
    {
        try
        {
            game.handleBidAction(playerId, new Bid(playerId, multiplier, face));
        }
        catch (DiceActionException e)
        {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    private static boolean handleChallenge(DiceGameState game, int playerId, int target) throws IOException, InterruptedException
    {
        try
        {
            game.handleChallangeAction(playerId, target);
        }
        catch (DiceActionException e)
        {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    private static boolean handleExact(DiceGameState game, int playerId) throws IOException, InterruptedException
    {
        try
        {
            game.handleExactAction(playerId);
        }
        catch (DiceActionException e)
        {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    private static boolean handlePass(DiceGameState game, int playerId) throws IOException, InterruptedException
    {
        try
        {
            game.handlePassAction(playerId);
        }
        catch (DiceActionException e)
        {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    private static boolean handlePush(DiceGameState game, int playerId, int[] faces) throws IOException, InterruptedException
    {
        try
        {
            game.handlePushAction(playerId, faces);
        }
        catch (DiceActionException e)
        {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    private static boolean handleAccept(DiceGameState game, int playerId) throws IOException, InterruptedException
    {
        try
        {
            game.handleAcceptAction(playerId);
        }
        catch (DiceActionException e)
        {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    private static List<String> agentsFromServer(String server, String process)
    {
        if (process.startsWith("recover-"))
        {
            List<String> ret = new ArrayList<String>();
            if (process.equals("recover-gp"))
            {
                ret.add("dice-gp-norm");
                ret.add("comp-pmh-num");
                return ret;
            }
            else if (process.equals("recover-p-0"))
            {
                ret.add("dice-p-0-norm");
                ret.add("comp-pmh-num");
                return ret;
            }
            else if (process.equals("recover-p-num"))
            {
                ret.add("dice-p-num-norm");
                ret.add("comp-pmh-num");
                return ret;
            }
            throw new IllegalStateException("Bad argument to --process: " + process);
        }
        try
        {
            URL url = new URL(server + "/task/" + process);
            Scanner scanner = new Scanner(url.openStream());
            String line = scanner.nextLine();
            debug("Got agent names: " + line);
            if (!line.contains(":")) System.exit(1);
            List<String> ret = new ArrayList<String>();
            for (String agent : line.split(":"))
            {
                ret.add(agent);
            }
            return ret;
        }
        catch (MalformedURLException e)
        {
            e.printStackTrace();
            System.exit(1);
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.exit(1);
        }
        return null;
    }

    public static void main(String[] args)
    {
        Map<String, String> parsedArgs = ArgsParser.parseArgs(args);
        try
        {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        }
        catch (ClassNotFoundException e)
        {
            e.printStackTrace();
        }
        catch (InstantiationException e)
        {
            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            e.printStackTrace();
        }
        catch (UnsupportedLookAndFeelException e)
        {
            e.printStackTrace();
        }
        String configPath = parsedArgs.get("config");
        if (configPath == null)
        {
            configPath = System.getenv("INI_CONFIG_FILE");
        }
        File configFile = null;
        if (configPath != null)
        {
            configFile = new File(configPath);
        }

        if (configFile == null)
        {
            JFileChooser chooser = new JFileChooser(System.getProperty("user.dir"));
            chooser.setDialogTitle("Select .ini configuration file");
            int result = chooser.showOpenDialog(null);
            if (result != JFileChooser.APPROVE_OPTION)
            {
                System.exit(1);
            }

            configFile = chooser.getSelectedFile();
        }

        Wini wini = null;
        try
        {
            wini = new Wini(configFile);
        }
        catch (InvalidFileFormatException e)
        {
            exit("Bad .ini file: " + configFile.getName());
        }
        catch (IOException e)
        {
            exit(e);
        }
        String configDir = configFile.getParent();

        // Get configuration
        List<String> agentNames = null;
        int numGames = -1;

        // For RL-mode
        int numTestingGames = -1;

        SoarMatchConfig matchConfig = new SoarMatchConfig();
        
        if (parsedArgs.containsKey("rl-bins")) {
            try {
                int rlBins = Integer.parseInt(parsedArgs.get("rl-bins"));
                RL_BINS = rlBins;
                debug("Setting number of rl bins from command line args: " + rlBins);
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        
        if (parsedArgs.containsKey("optimized-kernel") && parsedArgs.get("optimized-kernel").equals("on"))
        {
            System.out.println("Using optimized kernel");
            matchConfig.optimizedKernel = true;
        }
        
        if (parsedArgs.containsKey("temperature")) {
            try {
                double temperature = Double.parseDouble(parsedArgs.get("temperature"));
                matchConfig.temperature = temperature;
                debug("Setting temperature based on args to: " + temperature);
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        
        if (parsedArgs.containsKey("meta-learning-rate")) {
            try {
                double metaLearningRate = Double.parseDouble(parsedArgs.get("meta-learning-rate"));
                matchConfig.metaLearningRate = metaLearningRate;
                debug("Setting meta-learning-rate based on args to: " + metaLearningRate);
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }

        RunType runType = null;
        matchConfig.useGui = true;
        matchConfig.collectMetadata = true;
        String argsLog = null;

        // Maybe don't cellect metadata
        if (parsedArgs.containsKey("metadata") && parsedArgs.get("metadata").equals("off"))
        {
            debug("Setting metadata to off from command line args.");
            matchConfig.collectMetadata = false;
        }
        else
        {
            debug("No metadata command line switch found; leaving it on");
        }

        // Get server and process number information
        String server = null;
        String process = null;
        String beginTime = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(Calendar.getInstance().getTime());
        boolean recoveryMode = false;
        matchConfig.writeOverride = null;
        if (parsedArgs.containsKey("server") && parsedArgs.containsKey("process"))
        {
            server = parsedArgs.get("server");
            process = parsedArgs.get("process");
            agentNames = agentsFromServer(server, process);
            if (process.startsWith("recover-"))
            {
                recoveryMode = true;
                matchConfig.writeOverride = "recover-write/recover_" + agentNames.get(0) + ".soar";
            }
            if (agentNames == null)
            {
                debug("Error getting agent names from server.");
                System.exit(1);
            }

            numGames = 1000;

            runType = RunType.RL;
            matchConfig.useGui = false;
            String logPrefix = configDir == null ? "" : configDir + System.getProperty("file.separator");

            StringBuffer vsAgents = new StringBuffer();
            for (int i = 0; i < agentNames.size(); ++i)
            {
                vsAgents.append(agentNames.get(i));
                if (i + 1 < agentNames.size())
                {
                    vsAgents.append("_vs_");
                }
            }
            argsLog = logPrefix + vsAgents.toString() + "_" + beginTime;
        }
        else if (parsedArgs.containsKey("agents"))
        {
            agentNames = new ArrayList<String>();
            for (String agentName : parsedArgs.get("agents").split(":"))
            {
                agentNames.add(agentName);
            }

            numGames = 1000;

            // TODO this is shorter, for debugging
            // numGames = 10;

            runType = RunType.RL;
            matchConfig.useGui = false;
            String time = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(Calendar.getInstance().getTime());
            String logPrefix = configDir == null ? "" : configDir + System.getProperty("file.separator");
            String vsAgents = parsedArgs.get("agents").replace(":", "_vs_");
            argsLog = logPrefix + vsAgents + "_" + time;
        }
        else
        {
            AgentChooser agentChooser = new AgentChooser(wini, USER_STRING);
            agentChooser.setVisible(true);
            agentNames = agentChooser.getSelectedAgents();
            int numAgents = agentNames.size();
            if (numAgents < 2)
            {
                exit("Must pick at least 2 agents.");
                System.exit(1);
            }

            numGames = agentChooser.getNumGames();
            runType = agentChooser.getSelectedRunType();
            matchConfig.useGui = !agentChooser.getNoGui();
            matchConfig.collectMetadata = !agentChooser.getNoMeta();
        }

        if (parsedArgs.containsKey("num-games"))
        {
            numGames = Integer.parseInt(parsedArgs.get("num-games"));
        }

        if (numGames == -1)
        {
            exit("Invalid number of games.");
            System.exit(1);
        }

        if (parsedArgs.containsKey("num-test-games"))
        {
            numTestingGames = Integer.parseInt(parsedArgs.get("num-test-games"));
            if (numTestingGames == -1)
            {
                exit("Invalid number of test games.");
                System.exit(1);
            }
        }
        
        if (parsedArgs.containsKey("decay-mode")) {
            matchConfig.decayMode = parsedArgs.get("decay-mode");
        }

        /* Apoptosis stuff */

        // boolean apoptosis = parsedArgs.containsKey("apoptosis") &&
        // parsedArgs.get("apoptosis").equals("on");
        Double apoptosis = null;
        if (parsedArgs.containsKey("apoptosis"))
        {
            apoptosis = Double.parseDouble(parsedArgs.get("apoptosis"));

            if (parsedArgs.containsKey("apoptosis-small") && parsedArgs.get("apoptosis-small").equals("on"))
            {
                apoptosis *= -1;
            }
        }

        Integer rngSeed = null;
        if (parsedArgs.containsKey("rng-seed"))
        {
            rngSeed = Integer.parseInt(parsedArgs.get("rng-seed"));
            debug("Setting random seed from command line args to " + rngSeed);
        }
        Random random;
        if (rngSeed == null) 
        {
            random = new Random();
        }
        else
        {
            random = new Random(rngSeed);
        }

        boolean optimizedKernel = parsedArgs.containsKey("kernel") && parsedArgs.get("kernel").equals("optimized");
        boolean silence = parsedArgs.containsKey("silence") && parsedArgs.get("silence").equals("on");
        //setDebuggingEnabled(!silence);
        setDebuggingEnabled(true);

        boolean maxTime = parsedArgs.containsKey("maxtime") && parsedArgs.get("maxtime").equals("on");

        /* End apoptosis stuff */

        try
        {
            if (runType == RunType.Single_Game)
            {
                File logDir = chooseLogDir(argsLog);
                File logFile = null;
                if (logDir != null)
                {
                    logFile = new File(logDir, "log.txt");
                    File debugFile = new File(logDir, "debug.txt");
                    if (!logFile.exists()) logFile.createNewFile();
                    setDebugFile(debugFile);
                }
                matchConfig.spawnDebugger = true;
                matchConfig.firstGames = true;
                matchConfig.learningOn = true;
                runGames(wini, agentNames, logFile, beginTime, numGames, matchConfig, apoptosis, random, silence, false, null, "single");
            }
            else if (runType == RunType.RL)
            {
                // If no number of testing games is specified,
                // use the number of games as a default.
                if (numTestingGames == -1)
                {
                    numTestingGames = numGames;
                }

                File logDir = chooseLogDir(argsLog);
                if (logDir == null)
                {
                    debug("Null log directory");
                    System.exit(1);
                }
                File allLog = new File(logDir, "all.txt");
                // File debugFile = new File(logDir, "debug.txt");
                setDebugFile(new File(logDir, "debug.txt"));
                if (!allLog.exists()) allLog.createNewFile();

                // Don't do the first games if this is in recovery mode
                for (int bin = (recoveryMode ? 1 : 0); bin < RL_BINS; ++bin)
                {
                    // Run once with learning off.
                    File logFile = new File(logDir, "bin_" + bin + "_learning_off.txt");
                    setDebugFile(new File(logDir, "debug_" + bin + "_learning_off.txt"));
                    File times = null;
                    if (maxTime)
                    {
                        times = new File(logDir, "time_" + bin + "_learning_off.txt");
                    }
                    matchConfig.spawnDebugger = true;
                    matchConfig.firstGames = bin == 0;
                    matchConfig.learningOn = false;
                    DiceFrame frame = runGames(wini, agentNames, logFile, beginTime, numTestingGames, matchConfig, apoptosis, random, silence, false, times, "bin_" + bin + "_learning_off");
                    int[] wins = frame.getWins();
                    RulesData[] rulesData = frame.getRulesData();
                    FileWriter fw = new FileWriter(allLog, true);
                    fw.write("Bin " + bin + ", learning off:\n");
                    for (int i = 0; i < agentNames.size(); ++i)
                    {
                        fw.write(agentNames.get(i) + '\t' + wins[i]);
                        if (i + 1 < agentNames.size())
                        {
                            fw.write('\t');
                        }
                        else
                        {
                            fw.write("\n");
                        }
                    }
                    // Print rules metadata
                    for (int i = 0; i < agentNames.size(); ++i)
                    {
                        fw.write(agentNames.get(i) + '\t' + rulesData[i]);
                        if (i + 1 < agentNames.size())
                        {
                            fw.write('\t');
                        }
                        else
                        {
                            fw.write("\n\n");
                        }
                    }
                    fw.close();
                    frame.setVisible(false);

                    // Report the results to the server, and stop the process if
                    // the server tells us to.
                    // NOTE: this assumes that we're dealing with two agents.
                    if (server != null)
                    {
                        RulesData rd = rulesData[0];
                        String nowTime = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(Calendar.getInstance().getTime());

                        // Create a string to represent the baseline's wins
                        StringBuilder winsSb = new StringBuilder();
                        StringBuilder namesSb = new StringBuilder();
                        for (int i = 1; i < wins.length; ++i)
                        {
                            winsSb.append(wins[i]);
                            namesSb.append(agentNames.get(i));
                            if (i + 1 < wins.length)
                            {
                                winsSb.append(",");
                                namesSb.append(",");
                            }
                        }
                        String baselineWins = winsSb.toString();
                        String baselineNames = namesSb.toString();

                        URL url = new URL(server + "/insert/" + process + "/" + agentNames.get(0) + "/" + baselineNames + "/" + beginTime + "/"
                                + (bin * numGames) + "/" + numTestingGames + "/" + wins[0] + "/" + baselineWins + "/" + nowTime + "/" + rd.rlRules + "/"
                                + rd.nonRlRules + "/" + rd.averageFiringCount() + "/" + rd.averageRl());
                        Scanner scanner = new Scanner(url.openStream());
                        String line = scanner.nextLine();
                        debug("Got response from server: " + line);
                        if (line.equals("end"))
                        {
                            debug("Server told us to end");
                            System.exit(0);
                        }
                    }

                    // Run once with learning on.
                    logFile = new File(logDir, "bin_" + bin + "_learning_on.txt");
                    setDebugFile(new File(logDir, "debug_" + bin + "_learning_on.txt"));
                    times = null;
                    if (maxTime)
                    {
                        times = new File(logDir, "time_" + bin + "_learning_on.txt");
                    }
                    matchConfig.spawnDebugger = true;
                    matchConfig.firstGames = bin == 0;
                    matchConfig.learningOn = true;
                    frame = runGames(wini, agentNames, logFile, beginTime, numGames, matchConfig, apoptosis, random, silence, bin == 1, times, "bin_" + bin + "_learning_on");
                    wins = frame.getWins();
                    rulesData = frame.getRulesData();
                    fw = new FileWriter(allLog, true);
                    fw.write("Bin " + bin + ", learning on:\n");
                    for (int i = 0; i < agentNames.size(); ++i)
                    {
                        fw.write(agentNames.get(i) + '\t' + wins[i]);
                        if (i + 1 < agentNames.size())
                        {
                            fw.write('\t');
                        }
                        else
                        {
                            fw.write("\n");
                        }
                    }
                    // Print rules metadata
                    for (int i = 0; i < agentNames.size(); ++i)
                    {
                        fw.write(agentNames.get(i) + '\t' + rulesData[i]);
                        if (i + 1 < agentNames.size())
                        {
                            fw.write('\t');
                        }
                        else
                        {
                            fw.write("\n\n");
                        }
                    }

                    fw.close();
                    frame.setVisible(false);
                }

                // Finally, run one more time with learning off.
                File logFile = new File(logDir, "bin_" + RL_BINS + "_learning_off.txt");
                setDebugFile(new File(logDir, "debug_" + RL_BINS + "_learning_off.txt"));
                File times = null;
                if (maxTime)
                {
                    times = new File(logDir, "time_" + RL_BINS + "_learning_off.txt");
                }
                matchConfig.spawnDebugger = true;
                matchConfig.firstGames = false;
                matchConfig.learningOn = false;
                DiceFrame frame = runGames(wini, agentNames, logFile, beginTime, numTestingGames, matchConfig, apoptosis, random, silence, false, times, "bin_" + RL_BINS + "_learning_off");
                int[] wins = frame.getWins();
                FileWriter fw = new FileWriter(allLog, true);
                fw.write("Bin " + RL_BINS + ", learning off:\n");
                for (int i = 0; i < agentNames.size(); ++i)
                {
                    fw.write(agentNames.get(i) + '\t' + wins[i]);
                    if (i + 1 < agentNames.size())
                    {
                        fw.write('\t');
                    }
                    else
                    {
                        fw.write("\n\n");
                    }
                }
                fw.close();
            }
            else if (runType == RunType.First_vs_Each)
            {
                File logDir = chooseLogDir(argsLog);
                if (logDir == null)
                {
                    debug("null log directory");
                    System.exit(1);
                }
                File allLog = new File(logDir, "all.txt");
                File debugFile = new File(logDir, "debug.txt");
                setDebugFile(debugFile);
                if (!allLog.exists()) allLog.createNewFile();
                for (int i = 1; i < agentNames.size(); ++i)
                {
                    ArrayList<String> pair = new ArrayList<String>();
                    pair.add(agentNames.get(0));
                    pair.add(agentNames.get(i));
                    File logFile = new File(logDir, pair.get(0) + "_vs_" + pair.get(1) + ".txt");
                    if (!logFile.exists()) logFile.createNewFile();
                    matchConfig.spawnDebugger = false;
                    matchConfig.learningOn = true;
                    matchConfig.firstGames = true;
                    DiceFrame frame = runGames(wini, pair, logFile, beginTime, numGames, matchConfig, apoptosis, random, silence, false, null, "first_vs_each");
                    int[] wins = frame.getWins();
                    FileWriter fw = new FileWriter(allLog, true);
                    fw.write(pair.get(0) + '\t' + wins[0] + '\t' + pair.get(1) + '\t' + wins[1] + '\n');
                    fw.close();
                    frame.setVisible(false);
                }
            }
            else if (runType == RunType.All_Permutations)
            {
                File logDir = chooseLogDir(argsLog);
                if (logDir == null)
                {
                    System.out.println("null log directory");
                    System.exit(1);
                }
                File allLog = new File(logDir, "all.txt");
                File debugFile = new File(logDir, "debug.txt");
                setDebugFile(debugFile);
                if (!allLog.exists()) allLog.createNewFile();
                List<ArrayList<String>> namePermutations = permutateNames(agentNames);
                for (ArrayList<String> pair : namePermutations)
                {
                    File logFile = new File(logDir, pair.get(0) + "_vs_" + pair.get(1) + ".txt");
                    if (!logFile.exists()) logFile.createNewFile();
                    matchConfig.spawnDebugger = true;
                    matchConfig.learningOn = true;
                    matchConfig.firstGames = true;
                    DiceFrame frame = runGames(wini, pair, logFile, beginTime, numGames, matchConfig, apoptosis, random, silence, false, null, "all_permutations");
                    int[] wins = frame.getWins();
                    FileWriter fw = new FileWriter(allLog, true);
                    fw.write(pair.get(0) + '\t' + wins[0] + '\t' + pair.get(1) + '\t' + wins[1] + '\n');
                    fw.close();
                    frame.setVisible(false);
                }
            }
        }
        catch (Exception e)
        {
            exit(e);
        }
    }

    /**
     * 
     * @param names
     * @return All pairs of two strings from names.
     */
    public static List<ArrayList<String>> permutateNames(List<String> names)
    {
        List<ArrayList<String>> ret = new ArrayList<ArrayList<String>>();
        for (int i = 0; i < names.size(); ++i)
        {
            for (int j = i + 1; j < names.size(); ++j)
            {
                ArrayList<String> pair = new ArrayList<String>();
                pair.add(names.get(i));
                pair.add(names.get(j));
                ret.add(pair);
            }
        }
        return ret;
    }

    /*
     * private static File chooseLogFile(String argsLog) { if (argsLog != null)
     * { File logFile = new File(argsLog + ".txt"); if (logFile.exists()) {
     * logFile.delete(); } return logFile; } JFileChooser chooser = new
     * JFileChooser(System.getProperty("user.dir"));
     * chooser.setDialogTitle("Choose log file"); int result =
     * chooser.showSaveDialog(null); File logFile = null; if (result ==
     * JFileChooser.APPROVE_OPTION) { logFile = chooser.getSelectedFile(); if
     * (logFile.exists()) { result = JOptionPane.showOptionDialog(null,
     * "The file " + logFile.getName() +
     * " already exists. Overwrite, Append, or Cancel?", "The File " +
     * logFile.getName() + " Already Exists", 0, JOptionPane.QUESTION_MESSAGE,
     * null, new String[] { "Overwrite", "Append", "Cancel" }, 0); if (result ==
     * 0) { logFile.delete(); } else if (result == 1) { } else { return null; }
     * } } return logFile; }
     */

    private static File chooseLogDir(String argsLog)
    {
        if (argsLog != null)
        {
            File file = new File(argsLog);
            boolean made = file.mkdir();
            return file;
        }
        JFileChooser chooser = new JFileChooser(System.getProperty("user.dir"));
        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        chooser.setDialogTitle("Choose log directory");
        int result = chooser.showSaveDialog(null);
        File logFile = null;
        if (result == JFileChooser.APPROVE_OPTION)
        {
            logFile = chooser.getSelectedFile();
        }
        return logFile;
    }

    private static void exit(String string)
    {
        debug("Error: " + string);
        System.exit(1);
    }

    private static void exit(Exception e)
    {
        e.printStackTrace();
        exit(e.getMessage());
    }

    /**
     * 
     * @param wini
     * @param agentNames
     * @param configDir
     * @param logFile
     * @param numGames
     * @param spawnDebugger
     * @param useGui
     * @param learningOn
     *            For RL agents, is learning enabled?
     * @return
     * @throws Exception
     */
    private static DiceFrame runGames(Wini wini, List<String> agentNames, File logFile, String beginTime, int numGames, SoarMatchConfig matchConfig,
            Double apoptosis, Random random, boolean silence, boolean watchBT, File maxTime, String runTag) throws Exception
    {
        debug("Running games, matchConfig: " + matchConfig);

        // Unpack config variables.
        boolean spawnDebugger = matchConfig.spawnDebugger;
        boolean useGui = matchConfig.useGui;
        boolean learningOn = matchConfig.learningOn;
        boolean firstGames = matchConfig.firstGames;
        String writeOverride = matchConfig.writeOverride;
        boolean collectMetadata = matchConfig.collectMetadata;

        debug("Running games, metadata on: " + collectMetadata);

        if (!useGui) spawnDebugger = false;
        int numAgents = agentNames.size();

        // make sure config is good
        FreeDiceAgentConfiguration config[] = new FreeDiceAgentConfiguration[numAgents];
        {
            try
            {
                for (int i = 0; i < numAgents; i++)
                {
                    String agentName = agentNames.get(i);
                    if (!agentName.equals(USER_STRING))
                    {
                        config[i] = FreeDiceClient.attemptConfig(wini, agentName, beginTime, i == 0 ? writeOverride : null);
                        debug("Made config #" + i + ":\nconfig.sourceFile: " + config[i].sourceFile + "\nconfig.writeFile: " + config[i].writeFile
                                + "\nconfig.qnaFile: " + config[i].qnaFile + "\nconfig.gpFile: " + config[i].gpFile);
                    }
                    else
                    {
                        config[i] = null;
                    }
                }
            }
            catch (Exception e)
            {
                e.printStackTrace();
                System.exit(1);
            }
        }

        SoarProperties p = new SoarProperties();
        final String soarPrefix = p.getPrefix();

        final DiceFrame frame = new DiceFrame(-1, numAgents, useGui);
        // frame.setDoStuff(false);
        // DiceFrame frame = null;

        // make kernel/agents
        final Kernel kernel;
        Agent agents[] = new Agent[numAgents];

        String[] vars = { "LD_LIBRARY_PATH", "CLASSPATH", "PATH", "SOAR_HOME" };
        for (String var : vars)
        {
            System.out.println("getenv(" + var + "): " + System.getenv(var));
        }

        String libraryName = Kernel.GetDefaultLibraryName();
        debug("got library name: " + libraryName);
        if (matchConfig.optimizedKernel)
        {
            kernel = Kernel.CreateKernelInCurrentThread(libraryName, true, 0);
        }
        else
        {
            kernel = Kernel.CreateKernelInNewThread(libraryName, Kernel.kUseAnyPort);
        }

        if (kernel.HadError())
        {
            throw new IllegalStateException(kernel.GetLastErrorDescription());
        }

        final int kernelPort = kernel.GetListenerPort();
        debug("Started kernel on port " + kernelPort);

        // Only spawn one debugger
        boolean multipleDebuggers = false;
        final boolean[] debuggerSpawned = { !spawnDebugger };
        final boolean[] realDebuggerSpawned = { false };
        final boolean[] rhsErrorCalled = { false };

        // Register listener for dice-error RHS function
        kernel.AddRhsFunction("dice-error", new RhsFunctionInterface()
        {
            @Override
            public String rhsFunctionHandler(int eventID, Object data, String agentName, String functionName, String argument)
            {
                Agent agent = kernel.GetAgent(agentName);
                debug("dice-error called by agent: " + agentName);
                agent.StopSelf();
                /*
                 * executeCommand(agent, "p -s"); executeCommand(agent,
                 * "p -d 10 s1"); executeCommand(agent, "p -d 10 <s>");
                 * executeCommand(agent, "pref -n s1");
                 */
                while (true)
                {
                    String command = (String) JOptionPane.showInputDialog("Enter Soar Command:");
                    if (command == null) continue;
                    if (command.equals("done")) break;
                    executeCommand(agent, command);
                }

                agent.SpawnDebugger(kernelPort, soarPrefix);
                realDebuggerSpawned[0] = true;
                rhsErrorCalled[0] = true;
                return "";
            }
        }, null);

        // Create structures to keep track of which RL rules have been fired.
        final ArrayList<HashSet<String>> rlRulesFired = new ArrayList<HashSet<String>>();

        // e.g. signedUpdateSums.get(agentId).get(ruleName) == signed sum for
        // that rule for that agent.
        final ArrayList<HashMap<String, Double>> signedUpdateSums = new ArrayList<HashMap<String, Double>>();
        final ArrayList<HashMap<String, Double>> unsignedUpdateSums = new ArrayList<HashMap<String, Double>>();

        for (int i = 0; i < numAgents; ++i)
        {
            if (config[i] != null)
            {
                String agentName = "agent-" + i;
                agents[i] = kernel.CreateAgent(agentName);
                final int index = i;
                rlRulesFired.add(new HashSet<String>());
                signedUpdateSums.add(new HashMap<String, Double>());
                unsignedUpdateSums.add(new HashMap<String, Double>());

                if (collectMetadata)
                {
                    debug("Setting metadata on for agent " + i);
                    executeCommand(agents[i], "rl --set meta on");
                    debug("Setting metadata on for agent " + i);
                }
                else
                {
                    debug("Leaving metadata off for agent " + i);
                }

                /*
                if (collectMetadata)
                {
                    agents[i].RegisterForProductionEvent(smlProductionEventId.smlEVENT_AFTER_PRODUCTION_FIRED, new ProductionEventInterface()
                    {
                        @Override
                        public void productionEventHandler(int eventID, Object data, Agent agent, String prodName, String instantiation)
                        {
                            String shortRule = executeCommand(agent, "p -n " + prodName, false).trim();
                            String[] tokens = shortRule.split("\\s+");
                            if (tokens.length < 3) return; // This isn't an RL
                                                           // rule.
                            rlRulesFired.get(index).add(prodName);
                            debug("RL rule fired: " + prodName);
                        }
                    }, null);
                }
                */

                if (collectMetadata)
                {
                    // Trying to do this in the kernel now
                    String rlUpdateLogPath = config[i].getEscapedRlUpdateLogPathWithAppending("-" + runTag);
                    if (rlUpdateLogPath != null) {
                        System.out.println("Setting rl update log path: \"" + rlUpdateLogPath + "\"");
                        executeCommand(agents[i], "rl --set update-log-path \"" + rlUpdateLogPath + "\"");
                    }
                    /*
                    agents[i].RegisterForPrintEvent(smlPrintEventId.smlEVENT_PRINT, new PrintEventInterface()
                    {
                        @Override
                        public void printEventHandler(int eventID, Object data, Agent agent, String message)
                        {
                            int adjustingIndex = message.indexOf("Adjusting");
                            if (adjustingIndex >= 0)
                            {
                                debug(message.substring(adjustingIndex));
                            }
                            int updateIndex = message.indexOf("RL update");
                            if (updateIndex < 0) return;
                            message = message.substring(updateIndex);
                            String[] tokens = message.split("\\s+");
                            if (tokens.length < 10)
                            {
                                debug("ERROR: RL update line has " + tokens.length + ", expected at least 10: " + message);
                                return;
                            }
                            String ruleName = tokens[2];
                            // String oldEcrStr = tokens[3]; // expected current
                            // reward
                            // String oldEfrStr = tokens[4]; // expected future
                            // reward
                            String oldTotalStr = tokens[5]; // ecr + efr
                            // String newEcrStr = tokens[7];
                            // String newEfrStr = tokens[8];
                            String newTotalStr = tokens[9];

                            double oldTotal = Double.parseDouble(oldTotalStr);
                            double newTotal = Double.parseDouble(newTotalStr);
                            double difference = newTotal - oldTotal;
                            HashMap<String, Double> agentSignedSums = signedUpdateSums.get(index);
                            HashMap<String, Double> agentUnsignedSums = unsignedUpdateSums.get(index);
                            Double oldSignedSum = agentSignedSums.get(ruleName);
                            Double oldUnsignedSum = agentUnsignedSums.get(ruleName);
                            if (oldSignedSum == null) oldSignedSum = 0.0;
                            if (oldUnsignedSum == null) oldUnsignedSum = 0.0;
                            oldSignedSum += difference;
                            oldUnsignedSum += Math.abs(difference);
                            agentSignedSums.put(ruleName, oldSignedSum);
                            agentUnsignedSums.put(ruleName, oldUnsignedSum);

                            debug("Set new values for RL rule " + ruleName + ": " + oldSignedSum + ", " + oldUnsignedSum);
                        }
                    }, null);
                    */
                }

                if (agents[i] == null)
                {
                    throw new IllegalStateException(kernel.GetLastErrorDescription());
                }

                if (maxTime == null)
                {
                    executeCommand(agents[i], "timers -d");
                }
                agents[i].SetOutputLinkChangeTracking(true);
                if (i == 0)
                {
                    agents[i].RegisterForPrintEvent(smlPrintEventId.smlEVENT_PRINT, new PrintEventInterface()
                    {

                        @Override
                        public void printEventHandler(int eventID, Object data, Agent agent, String message)
                        {
                            debug("Output from " + agent.GetAgentName() + ": " + message);
                        }
                    }, null);
                }
            }

            else
            {
                agents[i] = null; // Player-controller
            }
        }

        // load source
        for (int i = 0; i < numAgents; ++i)
        {
            Agent agent = agents[i];
            if (config[i] != null)
            {
                sourceAgent(agents[i], config, i, firstGames, learningOn, apoptosis, silence, matchConfig);
                if (watchBT)
                {
                    // agents[i].ExecuteCommandLine("watch 5");
                }
                // For tracking rl values
                // executeCommand(agents[i], "watch --rl");

                synchronized (debuggerSpawned)
                {
                    if (!debuggerSpawned[0] || (spawnDebugger && multipleDebuggers))
                    {
                        debuggerSpawned[0] = true;
                        realDebuggerSpawned[0] = true;
                        int port = kernel.GetListenerPort();
                        agent.SpawnDebugger(port, soarPrefix);
                        Thread.sleep(2000);
                    }
                }
            }
        }

        // QnA
        /*
         * DataSourceManager qnaManager[] = new DataSourceManager[numAgents];
         * QnASMLModule qna[] = new QnASMLModule[numAgents]; CountDownLatch
         * doneSignal[] = new CountDownLatch[numAgents]; for (int i = 0; i <
         * numAgents; i++) { if (config[i] != null && config[i].qnaFile != null)
         * { qnaManager[i] = new DataSourceManager(); doneSignal[i] = new
         * CountDownLatch(1);
         * 
         * String qnaFile = config[i].qnaFile; if (qnaFile != null) { try {
         * qnaManager[i].addDataSource("dice", qnaFile); } catch (Exception e) {
         * e.printStackTrace(); System.exit(1); } }
         * 
         * qna[i] = new QnASMLModule(kernel, agents[i], qnaManager[i],
         * doneSignal[i]); } }
         */

        Object lock = new Object();

        int numTurns[] = new int[numAgents];
        int dcCounter[] = new int[numAgents];

        for (int gameIndex = 0; gameIndex < numGames; ++gameIndex)
        {
            String gameName = "game-" + gameIndex;
            debug("Beginning game " + gameName);

            log("Beginning " + gameName, logFile);

            DiceGameState game = new DiceGameState(agentNames, 5);
            if (frame != null)
            {
                frame.setGame(game);
            }
            boolean gameInProgress = true;
            boolean oneMore = false;

            for (int i = 0; i < agents.length; ++i)
            {
                Agent agent = agents[i];
                int seed = random.nextInt();

                // TODO
                // for debugging
                // seed = i == 0 ? 445759192 : -2091699139;

                // If the agent is null, this is a human player.
                if (agent != null)
                {
                    executeCommand(agent, "srand " + seed);
                    numTurns[i] = 0;
                    {
                        ClientAnalyzedXML response = new ClientAnalyzedXML();
                        agent.ExecuteCommandLineXML("stats", response);

                        dcCounter[i] = response.GetArgInt(sml_Names.getKParamStatsCycleCountDecision(), 0);
                    }
                }
            }

            while (gameInProgress || !oneMore)
            {
                if (!gameInProgress)
                {
                    oneMore = true;
                }

                // First, all players whose turn it is not get a chance to go.
                // Finally, the player whose turn it is gets to go.
                int[] turnIds = new int[numAgents];
                int currentTurnIdIndex = 0;
                int currentPlayerId = game.getCurrentPlayerId();
                for (int playerId = 0; playerId < numAgents; ++playerId)
                {
                    if (currentPlayerId != playerId)
                    {
                        turnIds[currentTurnIdIndex] = playerId;
                        ++currentTurnIdIndex;
                    }
                }
                turnIds[numAgents - 1] = currentPlayerId;

                for (int playerId : turnIds)
                // for (int playerId = 0; playerId < numAgents; ++playerId)
                {
                    /*
                     * if (game.getPlayer(playerId).getLost()) { continue; }
                     */

                    debug("Player " + playerId);
                    Agent agent = agents[playerId];

                    boolean agentHalted = false;
                    boolean agentSlept = false;
                    boolean needsRefresh = true;

                    DiceSMLData newData = null;

                    do
                    {
                        if (needsRefresh)
                        {
                            if (newData != null)
                            {
                                newData.idState.DestroyWME();
                                newData.idPlayers.DestroyWME();
                                newData.idAffordances.DestroyWME();
                                newData.idHistory.DestroyWME();
                                newData.idRounds.DestroyWME();
                                newData = null;
                            }

                            if (agent != null)
                            {
                                newData = DiceSMLModule.GameStateToWM(game, agent.GetInputLink(), playerId);
                            }

                            if (frame != null)
                            {
                                frame.refreshText();
                                frame.refreshHistory();
                            }

                            needsRefresh = false;
                        }

                        if (agent != null)
                        {
                            if ((useGui && frame.getRunSoarManually()) || rhsErrorCalled[0])
                            {
                                long notificationId = agent.RegisterForOutputNotification(new OutputNotificationInterface()
                                {

                                    @Override
                                    public void outputNotificationHandler(Object data, Agent agent)
                                    {
                                        synchronized (data)
                                        {
                                            data.notifyAll();
                                        }
                                        agent.StopSelf();
                                    }
                                }, lock);

                                do
                                {
                                    synchronized (lock)
                                    {
                                        lock.wait();
                                    }
                                    smlRunState agentState = agent.GetRunState();
                                    agentHalted = (agentState == smlRunState.sml_RUNSTATE_HALTED);
                                    // debug("Thread slept, agent state is: " +
                                    // agentState + ", num commands: " +
                                    // agent.GetNumberCommands());
                                } while (!agentHalted && (agent.GetNumberCommands() == 0));

                                agent.UnregisterForOutputNotification(notificationId);

                            }
                            else
                            {
                                do
                                {
                                    agent.RunSelfTilOutput();
                                    smlRunState agentState = agent.GetRunState();
                                    agentHalted = (agentState == smlRunState.sml_RUNSTATE_HALTED || agentState == smlRunState.sml_RUNSTATE_INTERRUPTED);
                                } while (!agentHalted && (agent.GetNumberCommands() == 0));
                            }
                            if (agent.GetNumberCommands() != 0)
                            {
                                // Pack needsRefresh in array to use as output
                                // variable.
                                boolean[] refreshAr = { needsRefresh };
                                agentSlept = handleAgentCommands(agent, game, playerId, refreshAr);
                                needsRefresh = refreshAr[0];
                                if (!agentSlept)
                                {
                                    numTurns[playerId]++;
                                }
                            }
                        }
                        else if (frame != null)
                        {
                            UIAction action = frame.getAction(playerId);
                            switch (action.getType())
                            {
                            case BID:
                                Bid bid = action.getBid();
                                handleBid(game, playerId, bid.getCount(), bid.getRank());
                                int[] push = bid.getPush();
                                if (push.length > 0)
                                {
                                    handlePush(game, playerId, push);
                                }
                                needsRefresh = true;
                                break;
                            case CHALLENGE_BID:
                                handleChallenge(game, playerId, game.getPreviousBid().getPlayerId());
                                break;
                            case EXACT:
                                handleExact(game, playerId);
                                break;
                            case PASS:
                                handlePass(game, playerId);
                                break;
                            case SLEEP:
                                break;
                            }
                            agentSlept = true;
                        }
                        else
                        {
                            throw new Exception("Null angent and no GUI frame.");
                        }
                    } while (!agentHalted && !agentSlept);

                    // about to take the agent down - allow for halt
                    /*
                     * if (agent != null) { WMElement halter = null; if
                     * (!agentHalted) { halter =
                     * agent.GetInputLink().CreateStringWME("halt", "now");
                     * agent.RunSelfTilOutput(); }
                     * 
                     * // reinitialize agent
                     * System.out.println(agent.ExecuteCommandLine
                     * ("fc apply*add-reward")); agent.InitSoar(); if (halter !=
                     * null) { halter.DestroyWME(); } }
                     */
                    if (newData != null)
                    {
                        newData.idState.DestroyWME();
                        newData.idPlayers.DestroyWME();
                        newData.idAffordances.DestroyWME();
                        newData.idHistory.DestroyWME();
                        newData.idRounds.DestroyWME();
                        newData = null;
                    }

                    // game state
                    if (!game.inProgress())
                    {
                        gameInProgress = false;
                    }
                    /*
                     * else if (agentHalted) { System.out.println("Player " +
                     * playerId + " halted but the game is still in progress.");
                     * System.exit(1); }
                     */
                }
            }

            // output log
            Player winner = game.getWinner();
            debug("Winner: " + winner);
            frame.getWins()[winner.getId()] += 1;
            debug("game " + gameName + " complete: " + new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(Calendar.getInstance().getTime()));
            /*
             * for (int i = 0; i < numAgents; i++) { if (hasRL(config, i)) {
             * debug(agents[i].ExecuteCommandLine("print --rl")); } }
             */

            // Append to log file
            if (logFile != null)
            {
                FileOutputStream os = new FileOutputStream(logFile, true);
                DiceFrame.writeHistoryToStream(frame.getHistory(), os);
                String message = "End of " + gameName + ", winner: Player #" + winner.getId() + "\n\n\n";
                os.write(message.getBytes());
                os.close();
            }

            if (maxTime != null)
            {
                FileOutputStream os = new FileOutputStream(maxTime, true);

                // get pid
                Process proc = Runtime.getRuntime().exec("resources/pid.sh");
                proc.waitFor();
                int pid = Integer.parseInt(new BufferedReader(new InputStreamReader(proc.getInputStream())).readLine());

                proc = Runtime.getRuntime().exec("resources/report.php" + " " + pid);
                proc.waitFor();
                String result = new BufferedReader(new InputStreamReader(proc.getInputStream())).readLine();

                os.write(("RSS=" + result + "\n").getBytes());

                for (int i = 0; i < numAgents; ++i)
                {
                    Agent agent = agents[i];

                    if (agent != null)
                    {
                        String message = (agent.GetAgentName() + ", " + gameName + "\n" + agent.ExecuteCommandLine("stats --max"));
                        os.write(message.getBytes());
                        ClientAnalyzedXML response = new ClientAnalyzedXML();
                        agent.ExecuteCommandLineXML("stats", response);

                        message = ("\n" + numTurns[i] + " " + (response.GetArgInt(sml_Names.getKParamStatsCycleCountDecision(), 0) - dcCounter[i]) + "\n");
                        os.write(message.getBytes());
                    }
                }

                os.close();
            }

            // Re-init all agents
            for (Agent agent : agents)
            {
                if (agent != null)
                {
                    agent.InitSoar();
                }
            }
        }

        // finish rl
        // if (learningOn)
        // {
        for (int i = 0; i < numAgents; i++)
        {
            RulesData rulesData = writeBack(agents, config, i, learningOn);
            frame.setRulesData(rulesData, i);
        }
        // }

        // finish qna
        /*
         * for (int i = 0; i < numAgents; i++) { if (qna[i] != null) {
         * qna[i].close(); qnaManager[i].close(); } }
         */

        // Wins record
        debug("Wins:");
        for (int i = 0; i < numAgents; ++i)
        {
            debug("" + i + ": " + frame.getWins()[i]);
        }

        log("Final win record:\n" + frame.getWinsString(), logFile);

        // Also log rule metadata for each agent.
        if (collectMetadata)
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < numAgents; ++i)
            {
                HashMap<String, Double> signedSums = signedUpdateSums.get(i);
                HashMap<String, Double> unsignedSums = unsignedUpdateSums.get(i);

                // String[] allRuleNames = executeCommand(agents[i], "p",
                // false).split("\n");

                sb.append("Rule metadata for agent #" + i + ":\n\n");
                sb.append("Rule Name,Update Count,Delta Bar Delta Beta,Delta Bar Delta H,RL Value,Total signed update,Total unsigned update,\n");
                for (String ruleName : rlRulesFired.get(i))
                {
                    debug("Writing metadata for RL rule: " + ruleName);
                    String shortRule = executeCommand(agents[i], "p -n " + ruleName, false);
                    String[] shortRuleTokens = shortRule.split("\\s+");
                    String ruleText = executeCommand(agents[i], "p " + ruleName, false);
                    // DiceRuleMetadata metadata = new
                    // DiceRuleMetadata(ruleText);

                    sb.append(ruleName + ",");
                    sb.append(shortRuleTokens[1] + ",");
                    sb.append(shortRuleTokens[2] + ",");
                    sb.append(shortRuleTokens[3] + ",");
                    sb.append(shortRuleTokens[4] + ",");
                    Double signedUpdate = signedSums.get(ruleName);
                    if (signedUpdate != null)
                    {
                        sb.append(signedUpdate + ",");
                    }
                    else
                    {
                        sb.append(",");
                    }
                    Double unsignedUpdate = unsignedSums.get(ruleName);
                    if (unsignedUpdate != null)
                    {
                        sb.append(unsignedUpdate + ",");
                    }
                    else
                    {
                        sb.append(",");
                    }
                    sb.append("\n");
                    // sb.append("Metadata:\n" + metadata.toString() + "\n\n");
                }
            }
            log(sb.toString() + "\n\n", logFile);
        }

        kernel.Shutdown();

        return frame;
    }

    private static void sourceAgent(Agent agent, FreeDiceAgentConfiguration[] config, int i, boolean firstGames, boolean learningOn, Double apoptosis,
            boolean silence, SoarMatchConfig matchConfig) throws IOException
    {
        boolean loadedRete = false;
        executeCommand(agent, "rl --set learning on");
        executeCommand(agent, "rl --set learning off");
        if (!firstGames)
        {
            String reteFile = config[i].getEscapedReteFile();
            if (reteFile != null)
            {
                System.out.println("Sourcing rete file: " + reteFile);
                executeCommand(agent, "rete-net -l " + reteFile);
                doFirstLoad(agent);

                if (agent.HadError())
                {
                    debug("Error: " + agent.GetLastErrorDescription());
                    System.exit(1);
                }
                loadedRete = true;

                // TODO debugging
                // String chunkNames = executeCommand(agent, "print --chunks",
                // false);
                // System.out.println("LOADED CHUNKS: \n" + chunkNames);
            }
        }
        if (!loadedRete)
        {
            String sourceCommand = "source \"" + config[i].getEscapedSourceFile() + "\"";
            System.out.println("Sourcing agent file: " + sourceCommand);
            executeCommand(agent, sourceCommand);

            if (isGP(config, i) && firstGames)
            {
                System.out.println("Also sourcing gp file: " + config[i].getEscapedGpFile());
                executeCommand(agent, "source \"" + config[i].getEscapedGpFile() + "\"");
            }

            if (agent.HadError())
            {
                debug(agent.GetLastErrorDescription());
                System.exit(1);
            }
        }

        if (silence) executeCommand(agent, "watch 0");

        if (hasRL(config, i))
        {
            executeCommand(agent, "learn --only");
            executeCommand(agent, "watch --rl");
            
            // For debugging chunking
            // executeCommand(agent, "watch --learning print");

            // Create the rl file if it doesn't exist.
            File rlFile = new File(config[i].writeFile);
            if (!rlFile.exists())
            {
                System.out.println("Tring to create write file: " + config[i].writeFile);
                debug("Write file doesn't exist, creating: " + rlFile.getAbsolutePath());
                rlFile.createNewFile();
            }

            // Shouldn't have to do this once we're over to rete-net
            /*
             * System.out.println("Sourcing write file: " +
             * config[i].getEscapedWriteFile()); executeCommand(agent,
             * "source \"" + config[i].getEscapedWriteFile() + "\"");
             */

            if (learningOn)
            {
                debug("Enabling learning");
                executeCommand(agent, "rl --set learning on");
                executeCommand(agent, "rl --set chunk-stop on");
                executeCommand(agent, "rl --set hrl-discount off");
                if (apoptosis != null)
                {
                    executeCommand(agent, "rl --set apoptosis-decay " + Math.abs(apoptosis));
                    executeCommand(agent, "rl --set apoptosis-thresh 2");
                    executeCommand(agent, "rl --set apoptosis chunks");
                    if (apoptosis < 0)
                    {
                        executeCommand(agent, "rl --set apoptosis-rl off");
                    }
                }
            }
        }
        
        // If a temperature has been set from the command-line args,
        // use that to override any other temperature.
        if (matchConfig.temperature >= 0.0) {
            executeCommand(agent, "indifferent-selection -t " + matchConfig.temperature);
        }
        
        if (matchConfig.metaLearningRate >= 0.0) {
            executeCommand(agent, "rl --set meta-learning-rate " + matchConfig.metaLearningRate);
        }

        // Finally, disable chunking if this is a testing run.
        if (!learningOn)
        {
            debug("Disabling learning");
            executeCommand(agent, "learn --disable");
        }
    }

    /**
     * Takes care of non-rule stuff that happens in _firstload.soar To be used
     * when sourcing an agent from a rete-net instead of from .soar source.
     * 
     * @param agent
     */
    private static void doFirstLoad(Agent agent)
    {
        String[] commands = { "multi-attributes dice-counts 5", "multi-attributes player 5", "multi-attributes operator 9", "multi-attributes die 4",
                "multi-attributes next-bid 25", "multi-attributes action 10", "multi-attributes evaluation 10", "gp-max 10000000", "max-chunks 1000",
                // Boltzman with low temperature
                "indifferent-selection -b", "indifferent-selection -t .08", };
        for (String command : commands)
        {
            executeCommand(agent, command);
        }
    }

    private static RulesData writeBack(Agent[] agents, FreeDiceAgentConfiguration[] config, int i, boolean learningOn)
    {
        if (!hasRL(config, i)) return null;

        // For now a bunch of this stuff isn't going to be used;
        // We're trying to just use the rete net.

        // Get the lie of all chunk rules
        String chunkNames = executeCommand(agents[i], "print --chunks", false);

        // Also get the names of rl rules, if this is a GP agent
        String rlNames = isGP(config, i) ? executeCommand(agents[i], "print --rl", false) : "";

        // TODO debugging
        // System.out.println("CHUNKS: \n" + chunkNames);

        // Make a list of all the names.
        List<String> allNames = new ArrayList<String>();
        for (String chunkName : chunkNames.split("\n"))
        {
            if (!chunkName.isEmpty())
            {
                allNames.add(chunkName);
            }
        }

        for (String rlName : rlNames.split("\n"))
        {
            if (!rlName.isEmpty())
            {
                allNames.add(rlName);
            }
        }

        int rlRules = 0;
        int nonRlRules = 0;
        int totalFiringCount = 0;
        // double totalRl = 0.0;

        // Write all the rules to the file specified in the configuration.
        // File file = learningOn ? new File(config[i].getEscapedWriteFile()) :
        // null;
        // try
        // {
        // FileWriter fw = file == null ? null : new FileWriter(file, false);
        for (String wholeRule : allNames)
        {
            String[] tokens = wholeRule.split("\\s+");
            List<String> tokensList = new ArrayList<String>();
            for (String token : tokens)
            {
                String trimmed = token.trim();
                if (!trimmed.isEmpty()) tokensList.add(trimmed);
            }
            String ruleName = tokensList.get(0);
            if (tokensList.size() > 1)
            {
                String firingString = tokensList.get(1);
                firingString = firingString.substring(0, firingString.length() - 1);
                int firingCount = Integer.parseInt(firingString);
                // double rl = Double.parseDouble(tokensList.get(2));
                ++rlRules;
                totalFiringCount += firingCount;
                // totalRl += rl;
                // if (fw != null) fw.write("# RL Rule, fc: " + firingString +
                // "\n");
            }
            else
            {
                // if (fw != null) fw.write("# Non-RL rule\n");
                debug("Rule not rl: " + ruleName);
                ++nonRlRules;
            }
            String ruleBody = executeCommand(agents[i], "p " + ruleName, false);
            /*
             * if (fw != null) { fw.write(ruleBody + "\n\n"); }
             */
        }
        // if (fw != null) fw.close();
        // }
        /*
         * catch (IOException e) { debug(e.toString()); debug(e.getMessage());
         * exit(e); }
         */

        // Also write the rete net to disk
        if (learningOn)
        {
            String retePath = config[i].getEscapedReteFile();
            String retePathWithTime = config[i].getEscapedRetePathWithAppending("_" + nowTime());
            if (retePath != null)
            {
                executeCommand(agents[i], "rete-net -s " + retePath);

                // Also write rete-path for this particular time.
                // executeCommand(agents[i], "rete-net -s " + retePathWithTime);

                if (agents[i].HadError())
                {
                    debug("Error: " + agents[i].GetLastErrorDescription());
                }
            }
        }

        // Writing back all the learned RL rules
        // Shouldn't have to do this once we're over to rete-net
        // For debugging for now.
        /*
         * String command = "command-to-file \"" +
         * config[i].getEscapedWritePathWithAppending("_" + nowTime()) +
         * "\" print --full --chunks"; executeCommand(agents[i], command); if
         * (isGP(config, i)) { // TODO for debugging //
         * executeCommand(agents[i], "print --full --rl"); command =
         * "command-to-file --append \"" +
         * config[i].getEscapedWritePathWithAppending("_" + nowTime()) +
         * "\" print --full --rl"; executeCommand(agents[i], command); }
         */

        return new RulesData(rlRules, nonRlRules, totalFiringCount);
    }

    private static String nowTime()
    {
        return new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(Calendar.getInstance().getTime());
    }

    private static String executeCommand(Agent agent, String command)
    {
        debug("Executing on " + agent.GetAgentName() + ": " + command);
        String output = agent.ExecuteCommandLine(command);
        debug("Output: " + output);
        // System.out.println(command + '\n' + output);
        if (agent.HadError())
        {
            debug("Error with command: " + agent.GetLastErrorDescription());
            System.exit(1);
        }
        return output;
    }

    private static String executeCommand(Agent agent, String command, boolean debug)
    {
        if (debug) debug("Executing on " + agent.GetAgentName() + ": " + command);

        String output = agent.ExecuteCommandLine(command);

        if (debug) debug("Output: " + output);

        if (agent.HadError())
        {
            debug("Error with command: " + agent.GetLastErrorDescription());
            System.exit(1);
        }
        return output;
    }

    private static boolean isGP(FreeDiceAgentConfiguration[] config, int i)
    {
        return config[i].gpFile != null;
    }

    private static boolean hasRL(FreeDiceAgentConfiguration[] configs, int i)
    {
        return configs[i] != null && configs[i].writeFile != null;
    }

    private static void log(String message, File logFile)
    {
        if (logFile != null)
        {
            FileOutputStream os = null;
            try
            {
                os = new FileOutputStream(logFile, true);
                os.write((message + '\n').getBytes());
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
            finally
            {
                if (os != null)
                {
                    try
                    {
                        os.close();
                    }
                    catch (IOException e)
                    {
                        e.printStackTrace();
                    }
                }
            }
        }
        else
        {
            System.out.println(message);
        }
    }

    /**
     * 
     * @param agent
     * @return True if the agent sleeps, false otherwise.
     * @throws InterruptedException
     * @throws IOException
     */
    private static boolean handleAgentCommands(Agent agent, DiceGameState game, int playerId, boolean[] needsRefresh) throws IOException, InterruptedException
    {
        boolean agentSlept = false;
        for (int j = 0; j < agent.GetNumberCommands(); ++j)
        {
            Identifier id = agent.GetCommand(j);
            boolean justAdded = id.IsJustAdded();
            String attrName = id.GetAttribute();
            String commandStatus = id.GetParameterValue("status");
            if (commandStatus != null && commandStatus.equals("complete"))
            {
                continue;
            }
            debug("" + agent.GetAgentName() + " used command " + attrName);
            if (attrName.compareTo("qna-query") != 0)
            {
                boolean status = false;
                if (attrName.equals("bid"))
                {
                    status = handleBid(game, playerId, Integer.parseInt(id.GetParameterValue("multiplier")), Integer.parseInt(id.GetParameterValue("face")));
                    needsRefresh[0] = true;
                }
                else if (attrName.equals("exact"))
                {
                    status = handleExact(game, playerId);
                    needsRefresh[0] = true;
                }
                else if (attrName.equals("accept"))
                {
                    status = handleAccept(game, playerId);
                    needsRefresh[0] = true;
                }
                else if (attrName.equals("push"))
                {
                    boolean goodCommand = true;
                    int[] faces = new int[id.GetNumberChildren()];

                    for (int k = 0; k < id.GetNumberChildren(); k++)
                    {
                        if (goodCommand)
                        {
                            WMElement child = id.GetChild(k);
                            String attr = child.GetAttribute();
                            if (!attr.equals("die"))
                            {
                                goodCommand = false;
                                continue;
                            }
                            Identifier childId = child.ConvertToIdentifier();
                            if (childId == null)
                            {
                                goodCommand = false;
                                continue;
                            }
                            if (childId.GetNumberChildren() != 1)
                            {
                                goodCommand = false;
                                continue;
                            }
                            WMElement face = childId.GetChild(0);
                            if (!face.GetAttribute().equals("face"))
                            {
                                goodCommand = false;
                                continue;
                            }
                            IntElement intFace = face.ConvertToIntElement();
                            if (intFace == null)
                            {
                                goodCommand = false;
                                continue;
                            }
                            if (goodCommand)
                            {
                                faces[k] = (int) id.GetChild(k).ConvertToIdentifier().GetChild(0).ConvertToIntElement().GetValue();
                            }
                        }
                    }

                    if (goodCommand)
                    {
                        status = handlePush(game, playerId, faces);
                        needsRefresh[0] = true;
                    }
                }
                else if (attrName.equals("challenge"))
                {
                    Long target = null;

                    if (id.GetNumberChildren() == 1)
                    {
                        WMElement targetWme = id.FindByAttribute("target", 0);

                        if ((targetWme != null) && (targetWme.ConvertToIntElement() != null))
                        {
                            target = targetWme.ConvertToIntElement().GetValue();
                        }
                    }

                    if (target != null)
                    {
                        status = handleChallenge(game, playerId, target.intValue());
                        needsRefresh[0] = true;
                    }
                }
                else if (attrName.equals("pass"))
                {
                    status = handlePass(game, playerId);
                    needsRefresh[0] = true;
                }
                else if (attrName.equals("sleep"))
                {
                    status = true;
                    agentSlept = true;
                }
                else
                {
                    debug("wanted to " + attrName);
                    System.exit(1);
                }

                if (status)
                {
                    id.AddStatusComplete();
                }
                else
                {
                    id.AddStatusError();
                    // Don't keep playing this round.
                    // Let this agent lose this round for making a bad command.
                    // Otherwise we get stuck in cycles where the match won't
                    // progress.
                    // Also make sure to log this.
                    game.playerMadeIllegalMove(playerId);
                }
            }
        }
        return agentSlept;
    }

    public static void debug(String message)
    {
        if (!debuggingEnabled) return;
        log(message, debugFile);
    }

    public static void setDebugFile(File file)
    {
        if (!file.exists())
        {
            try
            {
                file.createNewFile();
            }
            catch (IOException e)
            {
                e.printStackTrace();
            }
        }
        debugFile = file;
    }

    public static void setDebuggingEnabled(boolean enabled)
    {
        debuggingEnabled = enabled;
    }

}
