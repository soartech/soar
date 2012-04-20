/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tetrisexample;

/**
 *
 * @author szityu
 */

import tetrisexample.util.consoleTrainerHelper;
import tetrisexample.util.RLGlueThread;
import tetrisexample.util.AgentThread;
import tetrisexample.util.EnvThread;
import org.rlcommunity.rlglue.codec.RLGlue; 

public class ConsoleTrainer {

    
    public static void main(String []args)
    {



        Thread rlgluethread = new RLGlueThread();
        rlgluethread.start();

        EnvThread envthread = new EnvThread();
        envthread.start();

        AgentThread agentthread = new AgentThread();
        agentthread.start();


        int whichTrainingMDP = 0; // select the MDP to load
        for (int it=0; it<20; it++)
        {
            whichTrainingMDP = it;
            consoleTrainerHelper.loadTetris(whichTrainingMDP); //whichTrainingMDP should be in [0,19]
            RLGlue.RL_init();


            int stepsRemaining = 50000;
            int totalEpisodes = 0;
            double returnThisMDP=0.0d;

            while (stepsRemaining > 0) 
            {
                RLGlue.RL_episode(stepsRemaining);

                int thisStepCount = RLGlue.RL_num_steps();
                stepsRemaining -= thisStepCount;

                returnThisMDP += RLGlue.RL_return();
                totalEpisodes++;
            }
            System.out.println("MDP " + it + " completed with " + totalEpisodes + " episodes, got " + returnThisMDP + " reward");         

            //clean up the environment and end the program
            RLGlue.RL_cleanup();
        }
        System.exit(0);

    }     
    

}
