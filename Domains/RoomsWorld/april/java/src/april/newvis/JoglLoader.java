package april.newvis;

import com.sun.opengl.util.*;
import java.util.*;
import java.util.jar.*;
import java.util.zip.*;
import java.io.*;

/** Custom native library loader for finding JOGL libraries in a more
 * fool-proof (hopefully!) way. **/
public class JoglLoader implements com.sun.opengl.impl.NativeLibLoader.LoaderAction
{
    static boolean initialized = false;

    public static void initialize()
    {
        if (initialized)
            return;

        com.sun.gluegen.runtime.NativeLibLoader.disableLoading();
        JoglLoader jl = new JoglLoader();
        com.sun.opengl.impl.NativeLibLoader.setLoadingAction(jl);

        jl.loadLibrary("gluegen-rt", null, false, true);
        //	jl.loadLibraryFromClassPathJars("gluegen-rt");
        initialized = true;
    }

    static String unsplit(String toks[])
    {
        String s = "";

        if (toks==null)
            return s;

        for (String tok : toks)
            s = s + tok + " ";

        return s;
    }

    public void loadLibrary(String libname, String[] preload, boolean doPreload, boolean ignoreError)
    {
        ByteArrayOutputStream bouts = new ByteArrayOutputStream();
        PrintStream outs = new PrintStream(bouts);

        boolean verbose = false;
        boolean okay = true;

        outs.println("JoglLoader: libname:    " + libname);
        outs.println("JoglLoader: preload:    " + unsplit(preload));
        outs.println("JoglLoader: doPreload:  " + doPreload);
        outs.println("JoglLoader: ignore err: " + ignoreError);
        outs.println("JoglLoader: name:       " + System.getProperty("os.name"));
        outs.println("JoglLoader: arch:       " + System.getProperty("os.arch"));

        if (doPreload) {
            try {
                for (String lib : preload)
                    okay &= loadLibraryFromSystemDirs(lib, outs);
            } catch (UnsatisfiedLinkError e) {
                if (!ignoreError && e.getMessage().indexOf("already loaded") < 0) {
                    throw e;
                }
            }
        }

        okay &= loadLibraryFromSystemDirs(libname, outs);
        if (verbose || !okay)
            System.out.println(bouts.toString());
    }

    // e.g. "jogl" --> "libjogl.so"
    static String makeLibraryFileName(String libname)
    {
        // Construct the name of the library file
        String arch = System.getProperty("os.arch").toLowerCase();
        String os = System.getProperty("os.name").toLowerCase();

        if (os.contains("windows"))
            return libname+".dll";
        if (os.contains("linux"))
            return "lib"+libname+".so";

        // MacOS X. This is totally untested.
        if (os.contains("mac"))
            return "lib"+libname+".jnilib";

        return "lib"+libname+".so";
    }

    // add each path in a list of paths to the array list. E.g.,
    // "/usr/lib:/usr/local/lib" will add "/usr/lib" and
    // "/usr/local/lib" to dest.
    static void addPaths(ArrayList<String> dest, String paths)
    {
        if (paths == null)
            return;

        String parts[] = paths.split(System.getProperty("path.separator"));

        for (String s : parts)
            dest.add(s);
    }

    static boolean loadLibraryFromSystemDirs(String libname, PrintStream diagouts)
    {
        try {
            System.loadLibrary(libname);
            return true;
        } catch (UnsatisfiedLinkError ex) {
        } catch (SecurityException ex) {
        }

        ArrayList<String> dirs = new ArrayList<String>();
        addPaths(dirs, System.getProperty("java.library.path"));
        addPaths(dirs, System.getProperty("jogl.library.path"));
        dirs.add("/usr/lib/jni");
        dirs.add("/usr/lib/jvm/java-6-openjdk/jre/lib/amd64/");
        dirs.add("c:\\jogl\\");

        String libfile = makeLibraryFileName(libname);

        for (String dir : dirs) {
            if (dir==null)
                continue;

            try {
                String libpath = dir + System.getProperty("file.separator") + libfile;

                diagouts.println("JoglLoader: Trying library at "+libpath);
                System.load(libpath);
                return true;
            } catch (SecurityException ex) {
                diagouts.println("JoglLoader: "+ex);
            } catch (UnsatisfiedLinkError ex) {
                diagouts.println("JoglLoader: "+ex);
            }
        }

        return false;
    }

    public static void main(String args[])
    {
        System.out.println("JoglLoader: name:       " + System.getProperty("os.name"));
        System.out.println("JoglLoader: arch:       " + System.getProperty("os.arch"));
    }
}
