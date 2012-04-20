/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tetrisexample.util;

import org.rlcommunity.rlglue.codec.util.EnvironmentLoader;
import org.rlcommunity.rlviz.environmentshell.EnvironmentShell;
import org.rlcommunity.rlviz.settings.RLVizSettings;
import rlVizLib.general.ParameterHolder;

/**
 *
 * @author szityu
 */
public class EnvThread extends Thread {

    @Override
    public void run() 
    {
        
        String[] args = {};
        ParameterHolder parameterholder = new ParameterHolder();
        parameterholder.addStringParam("environment-jar-path", 
                "../../../rl-competition/system/libraries/envJars");
        
        RLVizSettings.initializeSettings(args);
        RLVizSettings.addNewParameters(parameterholder);
        EnvironmentLoader environmentloader = new EnvironmentLoader(new EnvironmentShell());
        environmentloader.run();
//         
    }
}
