/*
Copyright 2007 Brian Tanner
http://rl-glue-ext.googlecode.com/
brian@tannerpages.com
http://brian.tannerpages.com
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
* 
*  $Revision: 490 $
*  $Date: 2009-01-31 18:37:26 -0500 (Sat, 31 Jan 2009) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/RLGlueCore.java $
* 
*/

package org.rlcommunity.rlglue.codec;

import org.rlcommunity.rlglue.codec.installer.Installer;
import org.rlcommunity.rlglue.codec.util.AgentLoader;
import org.rlcommunity.rlglue.codec.util.EnvironmentLoader;

/**
 * This is the executable class from the JavaRLGlueCodec JAR.
 * <p>It can be used to load agents and environments (although that approach is
 * no longer the best way :: see Agent/Environment Loading in the Java Codec Manual).
 * <p>It will also tell you exactly what version and build of the codec you are using if you
 * do<br>java -jar JavaRLGlueCodec --version
 * @author Brian Tanner
 */
public class RLGlueCore {

    private static void printDiagnostics(){
        System.out.println("--------------------------------");
        System.out.println("RL-Glue Java Codec");
        System.out.println("--------------------------------");
        System.out.println("\t\tInterface Version: "+getSpecVersion());
        System.out.println("\t\tBuild Version: "+getImplementationVersion());

        boolean isInstalled=Installer.isInstalled();

        if(isInstalled){
            System.out.println("Installed in: "+Installer.getInstallDir());
        }else{
            System.out.println("NOT Installed: free-floating at: "+Installer.getFloatDir());
        }

    }
    private static void printHelp() {
        System.out.println("--------------------------------");
        System.out.println("RL-Glue.jar main diagnostic program");
        System.out.println("--------------------------------");
        System.out.println("-v or --version will print out the interface version");
        System.out.println("-b or --buildversion will print out the build version");
        System.out.println("--install install the RL-Glue Java Extension into a system extensions directory");
        System.out.println("--uninstall remove the RL-Glue Java Extension from the system extensions directory");
    }

    private static void printVersion() {
        System.out.println(getSpecVersion());
    }
    private static void printBuildVersion() {
        System.out.println(getImplementationVersion());
    }
    /*
     * Print out the current interface version of RLGlue.  We'll make it do more interesting things with commandline parameters later
     */

    public static void main(String[] args) {
        if (args.length == 0) {
            printDiagnostics();
            printHelp();
            return;
        }
        if (args[0].equalsIgnoreCase("-v") || args[0].equalsIgnoreCase("--version")) {
            printVersion();
            return;
        }
        if (args[0].equalsIgnoreCase("-b") || args[0].equalsIgnoreCase("--buildversion")) {
            printBuildVersion();
            return;
        }

        if (args[0].equalsIgnoreCase("--install")) {
            Installer.install();
        }
        if (args[0].equalsIgnoreCase("--uninstall")) {
            Installer.uninstall();
        }

    }

    /**
     * Get the Specification (Interface) version of RLGlue as set in the Manifest file.  
     * @return String representation of current RLGlue Interface version.
     * @since 2.1
     * 
     */
    public static String getSpecVersion() {
        String specAsString = RLGlueCore.class.getPackage().getSpecificationVersion();
        return specAsString;
    }

    /**
     * Get the Implementation (Build) version of RLGlue as set in the Manifest file.  
     * @return String representation of current RLGlue Build version.
     * @since 2.1
     */
    public static String getImplementationVersion() {
        return RLGlueCore.class.getPackage().getImplementationVersion();
    }
    
    
}
