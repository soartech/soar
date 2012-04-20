/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tetrisexample.util;

//import org.rlcommunity.environments.competition2009.Tetrlais.StandardTetrisAgent;
import tetrisexample.*;
import org.rlcommunity.rlglue.codec.util.AgentLoader;

/**
 *
 * @author szityu
 */
public class AgentThread extends Thread {

    @Override
    public void run() 
    {
        SampleTetrisAgent agent = new SampleTetrisAgent();
        //RandomAgent agent = new RandomAgent();
        AgentLoader agentloader = new AgentLoader(agent);
        agentloader.run();
   }
}
