=================================
RL-Glue C Direct-Compile MINES-SARSA-SAMPLE README
=================================
----------------------------
Introduction
----------------------------
This is a sample experiment that has the "Mines" environment and a simple tabular Sarsa agent.  This project lives in RL-Library, but is also distributed with the RL-Glue Core Source.

This example requires that the RL-Glue Core has been installed.  The C/C++ Codec is not necessary.
----------------------------
Compiling
----------------------------
Depending on whether you have RL-Glue Core in your include and library paths, you may have to update the Makefile or not.
If the installation was to the default location, this Makefile may not need to be edited.
If you have to update the Makefile, set the -I and -L for CFLAGS and LDFLAGS to point to where you installed the headers and libs.  

>$ make

----------------------------
Running
----------------------------
Compile First! (above)

$> ./SampleExperiment

----------------------------
More Information
----------------------------
Please see the RL-Glue Core Manual and FAQ if you are looking for more information:
http://glue.rl-community.org/Home/rl-glue

-- 
Brian Tanner
btanner@rl-community.org

