/*
 * Copyright (c) 2011, Regents of the University of Michigan
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package edu.umich.robot.soar;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Scanner;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import sml.ClientAnalyzedXML;
import april.config.Config;

import com.google.common.base.Joiner;
import com.google.common.util.concurrent.MoreExecutors;

import edu.umich.robot.util.WallClock;
import edu.umich.robot.util.properties.DefaultPropertyProvider;
import edu.umich.robot.util.properties.PropertyKey;
import edu.umich.robot.util.properties.PropertyManager;

/**
 * @author voigtjr@gmail.com
 */
public class SoarDataCollector
{
    private static final Log logger = LogFactory.getLog(SoarDataCollector.class);
    private int count = 0;

    public enum DataCollectionMode
    {
        DECISION_CYCLES,
        ELAPSED_TIME,
    }
    
    private final DefaultPropertyProvider<DataCollectionMode> mode = new DefaultPropertyProvider<DataCollectionMode>(SoarProperties.DATA_COLLECTION_MODE);
    private final DefaultPropertyProvider<Integer> periodCycles = new DefaultPropertyProvider<Integer>(SoarProperties.PERIOD_CYCLES);
    private final DefaultPropertyProvider<Integer> periodMillis = new DefaultPropertyProvider<Integer>(SoarProperties.PERIOD_MILLIS);
    private final DefaultPropertyProvider<File> file = new DefaultPropertyProvider<File>(SoarProperties.DATA_FILE);
    
    private PrintWriter pout;
    
    private final WallClock clock;
    
    private ScheduledExecutorService schexec = MoreExecutors.getExitingScheduledExecutorService(new ScheduledThreadPoolExecutor(1));;
    
    private int flushPeriod = 30;
    private AtomicBoolean flush = new AtomicBoolean(false);
    private boolean printedSettings = false;
    private double lastSmemTime = 0;
    private double lastEpmemTime = 0;
    private long lastDc = 0;
    private long lastTimeMillis = 0;
    
    public SoarDataCollector(WallClock clock, PropertyManager soarProperties, Config propc)
    {
        this.clock = clock;
        
        if (propc != null)
            setDefaults(propc.getChild("properties"));

        soarProperties.setProvider(SoarProperties.DATA_COLLECTION_MODE, mode);
        soarProperties.setProvider(SoarProperties.PERIOD_CYCLES, periodCycles);
        soarProperties.setProvider(SoarProperties.PERIOD_MILLIS, periodMillis);
        soarProperties.setProvider(SoarProperties.DATA_FILE, file);
        
        schexec.scheduleAtFixedRate(new Runnable() {
            public void run()
            {
                flush.set(true);
            }
        }, flushPeriod, flushPeriod, TimeUnit.SECONDS);
    }
    
    private void setDefaults(Config propc)
    {
        if (propc.hasKey(SoarProperties.DATA_COLLECTION_MODE.getName()))
            mode.set(DataCollectionMode.valueOf(propc.requireString(SoarProperties.DATA_COLLECTION_MODE.getName())));
        if (propc.hasKey(SoarProperties.PERIOD_CYCLES.getName()))
            periodCycles.set(Integer.valueOf(propc.requireString(SoarProperties.PERIOD_CYCLES.getName())));
        if (propc.hasKey(SoarProperties.PERIOD_MILLIS.getName()))
            periodMillis.set(Integer.valueOf(propc.requireString(SoarProperties.PERIOD_MILLIS.getName())));
        if (propc.hasKey(SoarProperties.DATA_FILE.getName()))
            file.set(new File(propc.requireString(SoarProperties.DATA_FILE.getName())));
    }
    
    public void incrementDecisionCycle()
    {
        ++count;
    }
    
    public boolean shouldCollectData()
    {
        if (!isEnabled())
            return false;

        switch (mode.get())
        {
        case DECISION_CYCLES:
            return count % periodCycles.get() == 0;
                
        case ELAPSED_TIME:
            long now = System.currentTimeMillis();
            if (lastTimeMillis == 0)
            {
                lastTimeMillis = now;
                return false;
            }
            long deltaMillis = now - lastTimeMillis;
            if (deltaMillis >= periodMillis.get())
            {
                lastTimeMillis = now;
                return true;
            }
            return false;
        }
        
        // unreachable unless more modes added
        return false;
    }

    public File getFile()
    {
        return file.get();
    }

    public void setFile(File file)
    {
        if (this.file.get() != null && this.file.get().equals(file))
            return;
        this.file.set(file);
        this.pout = null;
        this.printedSettings = false;
    }

    public void collect(SoarAgent agent)
    {
        if (!isEnabled())
            return;
        
        logger.debug("Collecting data.");
        try {
            StringBuilder headerBuilder = new StringBuilder();
            StringBuilder formatBuilder = new StringBuilder();
            
            addStat(headerBuilder, "agent", formatBuilder, "%s");
            addStat(headerBuilder, "wall clock", formatBuilder, "%f");
            addStat(headerBuilder, "dc num", formatBuilder, "%d");
            addStat(headerBuilder, "kernel sec", formatBuilder, "%f");
            addStat(headerBuilder, "avg msec/dc", formatBuilder, "%f");
            addStat(headerBuilder, "cpu sec", formatBuilder, "%f");
            addStat(headerBuilder, "pf total", formatBuilder, "%d");
            addStat(headerBuilder, "average msec/pf", formatBuilder, "%f");
            addStat(headerBuilder, "wm current", formatBuilder, "%d");
            addStat(headerBuilder, "wm mean", formatBuilder, "%f");
            addStat(headerBuilder, "wm max", formatBuilder, "%d");
            addStat(headerBuilder, "wm additions", formatBuilder, "%d");
            addStat(headerBuilder, "wm removals", formatBuilder, "%d");
            addStat(headerBuilder, "max dc time cycle", formatBuilder, "%d");
            addStat(headerBuilder, "max dc time value", formatBuilder, "%d");
            addStat(headerBuilder, "max dc changes cycle", formatBuilder, "%d");
            addStat(headerBuilder, "max dc changes value", formatBuilder, "%d");
            addStat(headerBuilder, "max dc pf cycle", formatBuilder, "%d");
            addStat(headerBuilder, "max dc pf value", formatBuilder, "%d");
            addStat(headerBuilder, "epmem time", formatBuilder, "%f");
            addStat(headerBuilder, "epmem max time cycle", formatBuilder, "%d");
            addStat(headerBuilder, "epmem max time value", formatBuilder, "%f");
            addStat(headerBuilder, "epmem bytes", formatBuilder, "%d");
            addStat(headerBuilder, "epmem stores", formatBuilder, "%d");
            addStat(headerBuilder, "epmem time per dc", formatBuilder, "%f");
            addStat(headerBuilder, "smem time", formatBuilder, "%f");
            addStat(headerBuilder, "smem max time cycle", formatBuilder, "%d");
            addStat(headerBuilder, "smem max time value", formatBuilder, "%f");
            addStat(headerBuilder, "smem bytes", formatBuilder, "%d");
            addStat(headerBuilder, "smem retrieves", formatBuilder, "%d");
            addStat(headerBuilder, "smem queries", formatBuilder, "%d");
            addStat(headerBuilder, "smem stores", formatBuilder, "%d");
            addStat(headerBuilder, "smem time per dc", formatBuilder, "%f");
            
            headerBuilder.append("settings");
            
            final String header = headerBuilder.toString();
            final String format = formatBuilder.toString();
            
            if (pout == null)
            {
                try
                {
                    FileOutputStream fos = new FileOutputStream(file.get());
                    OutputStreamWriter out = new OutputStreamWriter(fos);
                    pout = new PrintWriter(out);
                }
                catch (IOException e)
                {
                    e.printStackTrace();
                    file.set(null);
                    return;
                }
                
                pout.println(header);
            }
    
            ClientAnalyzedXML response = new ClientAnalyzedXML();
            agent.getSoarAgent().ExecuteCommandLineXML("stats", response);
            
            long dc = response.GetArgInt(sml.sml_Names.getKParamStatsCycleCountDecision(), 0L);
            long deltaDc = dc - lastDc;
            lastDc = dc;
            if (dc < 1)
                return;
            double ksec = response.GetArgFloat(sml.sml_Names.getKParamStatsKernelCPUTime(), 0);
            double tsec = response.GetArgFloat(sml.sml_Names.getKParamStatsTotalCPUTime(), 0);
            long pf = response.GetArgInt(sml.sml_Names.getKParamStatsProductionFiringCount(), 0L);
            long wmcount = response.GetArgInt(sml.sml_Names.getKParamStatsWmeCount(), 0L);
            double wmmean = response.GetArgFloat(sml.sml_Names.getKParamStatsWmeCountAverage(), 0);
            long wmmax = response.GetArgInt(sml.sml_Names.getKParamStatsWmeCountMax(), 0L);
            
            long wmadd = response.GetArgInt(sml.sml_Names.getKParamStatsWmeCountAddition(), 0L);
            long wmrem = response.GetArgInt(sml.sml_Names.getKParamStatsWmeCountRemoval(), 0L);
            
            long maxdctimec = response.GetArgInt(sml.sml_Names.getKParamStatsMaxDecisionCycleTimeCycle(), 0L);
            long maxdctimev = response.GetArgInt(sml.sml_Names.getKParamStatsMaxDecisionCycleTimeValueUSec(), 0L);
            long maxdcwmcc = response.GetArgInt(sml.sml_Names.getKParamStatsMaxDecisionCycleWMChangesCycle(), 0L);
            long maxdcwmcv = response.GetArgInt(sml.sml_Names.getKParamStatsMaxDecisionCycleWMChangesValue(), 0L);
            long maxdcpfcc = response.GetArgInt(sml.sml_Names.getKParamStatsMaxDecisionCycleFireCountCycle(), 0L);
            long maxdcpfcv = response.GetArgInt(sml.sml_Names.getKParamStatsMaxDecisionCycleFireCountValue(), 0L);
            
            long epmemMaxTimeCycle = response.GetArgInt(sml.sml_Names.getKParamStatsMaxDecisionCycleEpMemTimeCycle(), 0);
            double epmemMaxTimeValue = response.GetArgFloat(sml.sml_Names.getKParamStatsMaxDecisionCycleEpMemTimeValueSec(), 0);
            long smemMaxTimeCycle = response.GetArgInt(sml.sml_Names.getKParamStatsMaxDecisionCycleSMemTimeCycle(), 0);
            double smemMaxTimeValue = response.GetArgFloat(sml.sml_Names.getKParamStatsMaxDecisionCycleSMemTimeValueSec(), 0);
            
            Scanner epmemTimeScanner = new Scanner(agent.getSoarAgent().ExecuteCommandLine("epmem -t"));
            double epmemTime = epmemTimeScanner.skip(".+: ").nextDouble();
            
            Scanner epmemStatsScanner = new Scanner(agent.getSoarAgent().ExecuteCommandLine("epmem -S"));
            long epmemStores = epmemStatsScanner.skip(".+: ").nextLong(); // Time == Stores (more or less)
            epmemStatsScanner.nextLine();
            epmemStatsScanner.nextLine(); // SQLite version
            long epmemBytes = epmemStatsScanner.skip(".+: ").nextLong(); 
            
            Scanner smemTimeScanner = new Scanner(agent.getSoarAgent().ExecuteCommandLine("smem -t"));
            double smemTime = smemTimeScanner.skip(".+: ").nextDouble(); 
    
            Scanner smemStatsScanner = new Scanner(agent.getSoarAgent().ExecuteCommandLine("smem -S"));
            smemStatsScanner.nextLine(); // SQLite version
            long smemBytes = smemStatsScanner.skip(".+: ").nextLong();
            smemStatsScanner.nextLine();
            smemStatsScanner.nextLine(); // Memory Highwater
            long smemRetrieves = smemStatsScanner.skip(".+: ").nextLong();
            smemStatsScanner.nextLine();
            long smemQueries = smemStatsScanner.skip(".+: ").nextLong();
            smemStatsScanner.nextLine();
            long smemStores = smemStatsScanner.skip(".+: ").nextLong();
            
            double wallClock = clock.getMillis() / 1000.0;
            
            double deltaSmemTime = smemTime - lastSmemTime;
            lastSmemTime = smemTime;
            double smemTotalTimePerDc = deltaDc > 0 ? deltaSmemTime / deltaDc : 0;

            double deltaEpmemTime = epmemTime - lastEpmemTime;
            lastEpmemTime = epmemTime;
            double epmemTotalTimePerDc = deltaDc > 0 ? deltaEpmemTime / deltaDc : 0;
            
            String out = String.format(format, agent.getSoarAgent().GetAgentName(), wallClock, dc, ksec, ((ksec * 1000.0) / dc), 
                    tsec, pf, ((ksec * 1000.0) / pf), wmcount, wmmean, wmmax, wmadd, wmrem,
                    maxdctimec, maxdctimev, maxdcwmcc, maxdcwmcv, maxdcpfcc, maxdcpfcv,
                    epmemTime, epmemMaxTimeCycle, epmemMaxTimeValue, epmemBytes, epmemStores, epmemTotalTimePerDc, 
                    smemTime, smemMaxTimeCycle, smemMaxTimeValue, smemBytes, smemRetrieves, smemQueries, smemStores, smemTotalTimePerDc);
            pout.print(out);
            //System.out.println(out);
            if (!printedSettings)
            {
                pout.print(getSettingsString(agent));
                printedSettings = true;
            }
            pout.println();
            if (flush.getAndSet(false))
            {
                if (logger.isDebugEnabled())
                    logger.debug("Flushing data to " + file.get());
                pout.flush();
            }
            
            agent.getSoarAgent().ExecuteCommandLine("stats -R"); // reset max stats
        } 
        catch (Throwable e)
        {
            e.printStackTrace();
        }
    }
    
    private void addStat(StringBuilder headerBuilder, String header, StringBuilder formatBuilder, String format)
    {
        headerBuilder.append(header);
        headerBuilder.append(",");
        
        formatBuilder.append(format);
        formatBuilder.append(",");
    }
    
    private String getSettingsString(SoarAgent agent)
    {
        StringBuilder sb = new StringBuilder("\"");

        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sb.append(dateFormat.format(new Date()));
        appendProperty(agent, sb, AgentProperties.LEARN);
        appendProperty(agent, sb, AgentProperties.EPMEM_LEARNING);
        appendProperty(agent, sb, AgentProperties.SMEM_LEARNING);
        appendProperty(agent, sb, AgentProperties.EPMEM_EXCLUSIONS);
        appendProperty(agent, sb, AgentProperties.DEFAULT_STORAGE_AREA_ID);
        appendProperty(agent, sb, AgentProperties.AREAS_HELD_IN);
        appendProperty(agent, sb, AgentProperties.OBJECTS_HELD_IN);
        appendProperty(agent, sb, AgentProperties.LOOK_AHEAD_PLANNING);
        appendProperty(agent, sb, AgentProperties.SEARCH_CONTROL_GO_TO_GATEWAY);
        appendProperty(agent, sb, AgentProperties.DELETE_OLD_AREAS);
        appendProperty(agent, sb, AgentProperties.MISC_COMMANDS);
        appendProperty(agent, sb, AgentProperties.MISSION);
        
        sb.append("\"");
        return sb.toString();
    }
    
    private void appendProperty(SoarAgent agent, StringBuilder sb, PropertyKey<?> key)
    {
        sb.append(",").append(key.toString()).append(":");
        Object o = agent.getProperties().get(key);
        if (o instanceof String[])
        {
            Joiner joiner = Joiner.on(";");
            sb.append(joiner.join((String[])o));
        } 
        else
            sb.append(o.toString());
    }

    public void shutdown()
    {
        schexec.shutdown();
        if (pout != null)
            pout.close();
        pout = null;
    }

    public boolean isEnabled()
    {
        return file.get() != null;
    }

    public void reset()
    {
        if (pout != null)
            pout.flush();
        count = 0;
        lastSmemTime = 0;
        lastEpmemTime = 0;
        lastDc = 0;
        lastTimeMillis = 0;
    }
    
}
