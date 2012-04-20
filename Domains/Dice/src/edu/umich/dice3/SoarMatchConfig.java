package edu.umich.dice3;

import java.util.Map;

public class SoarMatchConfig
{
    public boolean spawnDebugger;
    public boolean useGui;
    public boolean learningOn;
    public boolean firstGames;
    public String writeOverride;
    public boolean collectMetadata;
    public double metaLearningRate = -1.0;
    public String decayMode = null;
    
    // Newer options
    public boolean optimizedKernel = false;
    public double temperature = -1.0;

    @Override
    public String toString()
    {
        return "SoarMatchConfig, "
                + "spawnDebugger: " + spawnDebugger + ", "
                + "useGui: " + useGui + ", "
                + "learningOn: " + learningOn + ", "
                + "firstGames: " + firstGames + ", "
                + "writeOverride: " + writeOverride + ", "
                + "collectMetadata: " + collectMetadata + ", "
                + "optimizedKernel: " + optimizedKernel + ", "
                + "temperature: " + temperature + "."
                + "metaLearningRate: " + metaLearningRate + "."
                + "decayMode: " + decayMode + ".";
        }
}
