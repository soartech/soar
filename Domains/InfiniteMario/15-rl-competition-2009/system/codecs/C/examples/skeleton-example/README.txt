=================================
RL-Glue C/C++SKELETON-SAMPLE README
=================================
----------------------------
Introduction
----------------------------
This is a sample experiment that has a "Skeleton" agent, environment, and experiment that might provide good scaffolding for your own future projects.
----------------------------
Compiling
----------------------------
Depending on whether you have RL-Glue Core and the C/C++ codec installed in your include and library paths, you may have to update the Makefile or not.
If the installation was to the default location, this Makefile may not need to be edited.
If you have to update the Makefile, set the -I and -L for EXTRA_CFLAGS and EXTRA_LDFLAGS to point to where you installed the headers and libs.  

>$ make

----------------------------
Running
----------------------------
Compile First! (above)

- These instructions assume that you have rl_glue (or rl_glue.exe) installed on your path so that you don't have to type the full path to it.

Do the following in different console/terminal windows:
#If you want to do them in the same terminal window, append an ampersand & to each line
$> ./SkeletonAgent
$> ./SkeletonEnvironment
$> ./SkeletonExperiment
$> rl_glue #(maybe rl_glue.exe)

----------------------------
More Information
----------------------------
Please see the C/C++ Codec Manual and FAQ if you are looking for more information:
http://glue.rl-community.org/Home/Extensions/c-c-codec

-- 
Brian Tanner
btanner@rl-community.org

