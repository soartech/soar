/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.rlcommunity.rlglue.codec.installer;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.rlcommunity.rlglue.codec.RLGlueCore;

/**
 * This class can be used to check the installation status of the RL-Glue
 * Java Extension,  It can install or uninstall that codec also.
 * @author btanner
 */
public class Installer {

    public static String getFloatDir() {
        assert (!isInstalled());
        File theJar = getThisJar();
        if (theJar == null) {
            return "unknown";
        } else {
            return theJar.getAbsolutePath();
        }
    }

    public static String getInstallDir() {
        assert (isInstalled());
        return getThisJar().getAbsolutePath();
    }

    public static void install() {
        File theJarFile = getThisJar();

        if (isInstalled()) {
            System.err.println(" -- PROBLEM --");
            System.err.println("You are presumably trying to install the RL-Glue" +
                    "Extension from a JAR file that you have downloaded.  You already " +
                    "have a version of this extension installed on this computer.\n");

            System.err.println("The current version is: " + RLGlueCore.getSpecVersion() + " :: " + RLGlueCore.getImplementationVersion());
            System.err.println("Because of the way that Java handles class-loading order of extensions, you will have to " +
                    "remove the old one before installing the presumably newer one.  I (the one writing this) am the old one.\n");

            System.err.println("To remove me, try:\n java org.rlcommunity.rlglue.codec.RLGlueCore --uninstall");
           System.exit(1);
        } 

        if (theJarFile == null) {
            System.err.println("Could not get the JAR that you are " +
                    "executing from, aborting.");
           System.exit(1);
        }

        System.out.println("This program will install the RL-Glue Java " +
                "extension " +
                "into one of your system extension directories.  This may " +
                "require superuser or root access to do.  Also, depending on the " +
                "location you choose, you may need to " +
                "reinstall if you update your VM.\n\nYou should be aware that there are some "+
				"disadvantages to installing the codec instead of just manually putting it into "+
				"you java class path.  These are explained in the manual.");




        System.out.println();
        System.out.println("Possible extension directories to install to:");
        boolean anyWriteable = false;
        boolean anyNotWriteable = false;

        Set<File> allDirs = getExtensionDirs();

        Set<File> writeableChoices = new TreeSet<File>();
        for (File thisDirFile : allDirs) {

            if (thisDirFile.canWrite()) {
                System.out.print(" WRITEABLE     :: ");
                writeableChoices.add(thisDirFile);
            } else {
                System.out.print(" NOT WRITEABLE :: ");
            }

            System.out.println(thisDirFile.getAbsolutePath());
            anyWriteable |= thisDirFile.canWrite();
            anyNotWriteable |= !thisDirFile.canWrite();
        }

        if (!anyWriteable) {
            System.out.println("None of the choices were writeable. " +
                    "You will have to execute as root or with sudo to install.");
        } else {
            if (anyNotWriteable) {
                System.out.println("Some of the choices were not writeable. " +
                        "You will have to execute as root or with sudo to install there.");
            }
        }

        File theChoice = null;
        if (anyWriteable) {
            try {
                Map<Integer, File> choiceMap = new TreeMap<Integer, File>();
                System.out.println();
                System.out.println("If you would like to proceed, select a location to install:");
                int counter = 1;
                System.out.println("0. Cancel Installation (Quit)");
                for (File file : writeableChoices) {
                    choiceMap.put(counter, file);
                    System.out.println(counter + ".  " + file.getAbsolutePath());
                }
                System.out.print("Your selection: ");

                int theIntChoice = 0;

                try {
                    theIntChoice = ConsoleReader.readInt();
                } catch (Exception e) {
                    //Errors will fall through and cause us to quit.
                }
                System.out.println();
                if (choiceMap.containsKey(theIntChoice)) {
                    theChoice = choiceMap.get(theIntChoice);
                } else {
                    System.err.println("You did not enter a valid selection or chose to quit, cancelling installation.");
                   System.exit(1);
                }


                File DestinationFile = new File(theChoice, theJarFile.getName());
                System.out.println();
                System.out.println(theJarFile.getAbsolutePath() + " ==> " + DestinationFile.getAbsolutePath());
                if (DestinationFile.exists()) {
                    System.out.println("Removing existing version first...");
                    DestinationFile.delete();
                }
                copyFileTo(theJarFile, DestinationFile);
                System.out.println("Installation Complete!");
                System.out.println("Test it by typing:");
                System.out.println(">$ java org.rlcommunity.rlglue.codec.RLGlueCore --version");
            } catch (IOException ex) {
                Logger.getLogger(Installer.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    /**
     * Returns true of this class was loaded from an installed location.
     * @return
     */
    public static boolean isInstalled() {
        Set<File> allExtensionDirs = getExtensionDirs();

        File thisJarFile = getThisJar();

        if (thisJarFile == null) {
            return false;
        }

        return allExtensionDirs.contains(thisJarFile.getParentFile());
    }

    /**
     * This will make sure that the codec is installed somewhere in the
     * extensions path,and if so, it will remove the jar.
     */
    public static void uninstall() {
        if (!isInstalled()) {
            System.out.println("You cannot uninstall the RL-Glue Java Extension if " +
                    "it has not been formally \"installed\" on your system.  From " +
                    "what I can detect, you loaded the RL-Glue codec from: \n" + getThisJar());
           System.exit(1);
        }
        File theJarFile = getThisJar();
        if (!theJarFile.getName().equals("JavaRLGlueCodec.jar")) {
            System.out.println("I will not uninstall because the jar that RL-Glue" +
                    "is in appears to be part of a redistribution.  Deleting" +
                    "a jar that is not called \"JavaRLGlueCodec.jar\" might" +
                    "make you mad.  The jar that I was loaded from is: \n" + getThisJar());
           System.exit(1);
        }
        System.out.println("Removing RL-Glue Java Extension from:\n"+getThisJar().getAbsolutePath());
        getThisJar().delete();
        if(getThisJar().exists()){
            System.err.println("Could not delete the file.  Try again with sudo or root access.");
        }else{
            System.out.println("Uninstallation Complete!");
        }
    }

    private static Set<File> getExtensionDirs() {
        String extensionDirs = System.getProperty("java.ext.dirs");
        String[] allDirsArray = extensionDirs.split(File.pathSeparator);
        Set<File> allDirSet = new TreeSet<File>();

        for (String thisFileName : allDirsArray) {
            allDirSet.add(new File(thisFileName));
        }
        return allDirSet;
    }

    private static File getThisJar() {
        File theJarFile = null;
        URL codeBase = Installer.class.getProtectionDomain().getCodeSource().getLocation();
        if (codeBase.getPath().endsWith(".jar")) {
            try {
                theJarFile = new File(codeBase.toURI());
            } catch (URISyntaxException ex) {
                Logger.getLogger(Installer.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return theJarFile;
    }

    /**
     * Copy the source file to the target file.  If the destination file
     * does not exist, it is created.
     * @param source
     * @param target
     * @throws java.io.IOException
     */
    public static void copyFileTo(File source, File target) throws IOException {
        assert (source != null);
        assert (target != null);
        assert source.exists() : "File we are copying from (" + source.getAbsolutePath() + ") does not exist in copyFileTo";
        assert (source != target);

        InputStream in = new BufferedInputStream(new FileInputStream(source));
        OutputStream out = new BufferedOutputStream(new FileOutputStream(target));

        // Copy the bits from instream to outstream
        byte[] buf = new byte[1024];
        int len;

        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }

        in.close();
        out.close();
    }
}
