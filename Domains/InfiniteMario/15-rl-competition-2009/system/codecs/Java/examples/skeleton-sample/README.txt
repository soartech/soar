=================================
RL-Glue Java SKELETON-SAMPLE README
=================================
----------------------------
Introduction
----------------------------
This is a sample experiment that has a simple Skeleton environment, agent, and experiment.  These don't do much, they are more like scaffolding for your own projects.

This project is distributed with the RL-Glue Java Codec.

----------------------------
Compiling
----------------------------
Depending on whether you have the Java codec installed to your Java path or not, you will either need to do:

#Java Codec Installed
>$ javac *.java

#Java Codec In Free-Float mode (not installed)
>$ javac -classpath /path/to/JavaRLGlueCodec.jar:. *.java

Note, if you are on windows you need to change the classpath separator from : to \;

----------------------------
Running
----------------------------
Compile First! (above)

- These instructions assume JavaRLGlueCodec.jar is in your classpath (installed).  If not, you will need to add -classpath /path/to/JavaRLGlueCodec.jar in each line that calls 'java'.

- They also assume that you have rl_glue (or rl_glue.exe) installed on your path so that you don't have to type the full path to it.


-- Using Separate Processes and Communicating over Sockets --
This is the most general option, because you could use any combination of agent/environment/experiment in Java or any other language.

Do the following in different console/terminal windows:
#If you want to do them in the same terminal window, append an ampersand & to each line
$> java SkeletonAgent
$> java SkeletonEnvironment
$> java SkeletonExperiment
$> rl_glue #(maybe rl_glue.exe)

-- Using Separate Threads and Communicating over Sockets--
This might be useful if at least two of your components are in Java.  You can stick your code together to save some command-line manipulation.

Do the following in different console/terminal windows:
#If you want to do them in the same terminal window, append an ampersand & to each line

$> java RunAllSkeleton
$> rl_glue #(maybe rl_glue.exe)

-- Using Separate Threads and Communicating WITHOUT Sockets (Local Glue!)--
This is useful is fall three of your components are in Java.  You can stick your code together to save some command-line manipulation, and it will run MUCH faster.

$> java RunAllSkeletonNoSockets
#No rl_glue necessary.


----------------------------
More Information
----------------------------
Please see the Java Codec Manual, JavaDocs, and FAQ if you are looking for more information:
http://glue.rl-community.org/Home/Extensions/java-codec


-- 
Brian Tanner
btanner@rl-community.org

