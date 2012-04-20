=================================
RL-Glue Matlab SKELETON-SAMPLE README
=================================
----------------------------
Introduction
----------------------------
This is a sample experiment that has the "Skeleton" environment, agent, and experiment.  Hopefully this will provide useful scaffolding for your own projects.

This example requires the Matlab Codec:
http://glue.rl-community.org/Home/Extensions/matlab-codec

----------------------------
Running
----------------------------
- These instructions assume that you have installed the Matlab codec.  If not, you will need to add 'src' and all of its subdirectories to your Matlab path, and also you need the JavaRLGlueCodec.jar in your Matlab Java path.


-- Run All Together, Communicating over Sockets --
This is the easiest option, it will run the agent, environment, and experiment all together.

>> !/usr/local/bin/rl_glue & %(maybe !rl_glue.exe &)
>> runAllTogether()


-- Running just one, or a subset in Matlab --
Please refer to the Matlab Codec Manual for more details.

----------------------------
More Information
----------------------------
Please see the Matlab Codec Manual and FAQ if you are looking for more information:
http://glue.rl-community.org/Home/Extensions/matlab-codec


-- 
Brian Tanner
btanner@rl-community.org

