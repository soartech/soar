/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package tetrisexample.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author szityu
 */
public class RLGlueThread extends Thread {

    @Override
    public void run() 
    {
        //// for non-Windows systems
        // String command = "../../system/bin/rl_glue";
        //// for Windows
        String command = "..\\..\\system\\bin\\rl_glue.exe";
        try {
            Process process = new ProcessBuilder(command).start();
            InputStream is = process.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);
            String line;

            while ((line = br.readLine()) != null) {
                System.out.println("RLG> "+line);
            }

        } catch (IOException ex) {
            System.err.printf("Error starting RLGlue. Most likely reason: executable is not on the given path. \n" +
                    "Currently looking at: %s \n", command);
        }
         
    }
}
