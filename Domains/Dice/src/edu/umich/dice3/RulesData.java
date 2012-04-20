package edu.umich.dice3;

public class RulesData
{
    public int rlRules;
    public int nonRlRules;
    public int totalFiringCount;
    public double totalRl;
    
    public RulesData(int rlRules, int nonRlRules, int totalFiringCount)
    {
        this.rlRules = rlRules;
        this.nonRlRules = nonRlRules;
        this.totalFiringCount = totalFiringCount;
        totalRl = 0.0;
    }
    
    public double averageFiringCount()
    {
        return totalFiringCount / (double) rlRules;
    }
    
    public double averageRl()
    {
        return 0.0;
    }
    
    @Override
    public String toString()
    {
        return "RL rules: " + rlRules
                + ", non-RL rules: " + nonRlRules
                + ", fc: " + averageFiringCount();
    }
}
