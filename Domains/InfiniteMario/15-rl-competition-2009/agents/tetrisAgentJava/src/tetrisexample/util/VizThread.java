/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tetrisexample.util;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.rlcommunity.rlglue.codec.util.EnvironmentLoader;
import org.rlcommunity.rlviz.app.RLVizApp;
import org.rlcommunity.rlviz.environmentshell.EnvironmentShell;
import org.rlcommunity.rlviz.settings.RLVizSettings;
import rlVizLib.general.ParameterHolder;

/**
 *
 * @author szityu
 */
public class VizThread extends Thread {

    @Override
    public void run() 
    {
        try {
            String[] viz_args = {"list-environments=true", "env-viz=true"};
            RLVizApp.main(viz_args);
        } catch (IOException ex) {
            Logger.getLogger(VizThread.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
