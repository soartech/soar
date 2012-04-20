/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tetrisexample;

/**
 *
 * @author szityu
 */

import java.util.logging.Level;
import java.util.logging.Logger;
import org.rlcommunity.rlglue.codec.RLGlue; 
import tetrisexample.util.AgentThread;
import tetrisexample.util.EnvThread;
import tetrisexample.util.RLGlueThread;
import tetrisexample.util.VizThread;

public class GuiTrainer {

    
    public static void main(String []args)
    {

        Thread rlgluethread = new RLGlueThread();
        rlgluethread.start();

        EnvThread envthread = new EnvThread();
        envthread.start();

        VizThread vizthread = new VizThread();
        vizthread.start();

        AgentThread agentthread = new AgentThread();
        agentthread.start();


    }     
    

}
