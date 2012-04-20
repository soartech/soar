===========================================
RL-Competition 2009 Training Distribution
===========================================

http://2009.rl-competition.org/

Thank you for downloading the training distribution!

==========
Contents:

1: Hyper Quick Start
2: Installation Instructions
3: Information about what is in this package
4: Running your first graphical experiment
5: Agents
6: Trainers
7: Running your first console experiment
8: Where to go for more info
9: Credits
==========


--------------------------
1 :: HYPER QUICK START
--------------------------

If you just want to see something happen without learning anything
about what's in this package, do:

$> bash install.bash
$> bash runDemo.bash

***
*  Note: This is a graphical demo and will only work if you can display
*  a graphical Java window!
***

This will run the graphical trainer for all domains with a random
agent.  Please read the rest of this document to learn what that means
and what other things you can do!

--------------------------
2 :: INSTALLATION INSTRUCTIONS
--------------------------

To install the core RL-Glue code, as well as all of the sample agents,
environments, and trainers, type:

$> bash install.bash

All of the competition domains (Octopus, Mario, Tetris, Helicopter
Control, Acrobot, Polyathlon) are implemented in Java and are already
packaged and ready to use.

--------------------------
3 :: THIS PACKAGE
--------------------------

The purpose of this package is to provide the software foundation for
the reinforcement learning competition.  This package contains all of
the resources required to create agents and experiments and to test
them on the testing versions of the competition problems.

Every experiment that is run using this software consists of four components:
	- An experiment program (a trainer)
	- An agent program
	- An environment program
	- The RL_glue communication software

We have taken care of RL_glue and the environments, so the important
components for you are the agents and the trainers.


----------------------------------------------------
4 :: RUNNING YOUR FIRST GRAPHICAL EXPERIMENT
----------------------------------------------------

Running your first experiment is as simple as choosing a trainer and
an agent and running the run.bash script in each of their directories.
Details about trainers and agents are explained later in this file.

For now, to run a random agent on any of the Java domains, open two
terminal windows:

Terminal 1:
$>cd trainers/guiTrainerJava
$>bash run.bash

Terminal 2:
$>cd agents/randomAgentJava
$>bash run.bash

This will bring up the rlVizApp visualization window.  Choose the
problem you want to use (GeneralizedHelicopter, GeneralizedTetris,
GlueMario, Octopus, TrainingPolyathlon or GeneralizedAcrobot), press
"Load Experiment", and press "Start".  Voila!  You can select the
speed of the experiment using the slider bar, or proceed one time-step
at a time by pressing "stop" and then "step" repeatedly.

Since you are running the random agent, the experiment is probably not
going very well.  To see better results, try running one of the
specialized sample agents and matching the environment appropriately.
Notice that if you mismatch the agents and environments (run the
Acrobot agent on Tetris), the experiment may crash.


--------------------------
5 :: AGENTS
--------------------------

Agents are located in the /agents directory.

We have provided several sample agents, implemented in C/C++, Python,
and Java.  There is at least one sample agent specifically created for
each domain.

There is also one general purpose agent that can work on any problem
(in each of the 3 languages):

/agents/randomAgentJava
/agents/randomAgentCPP
/agents/randomAgentPython

To run any agent, go into its directory and type:
$>make
$>bash run.bash 

*****
* Note 1: running an agent is only half of what is necessary to run
* an experiment -- you also need a trainer!  See the next section.
*****

The source of each agent is in the /agents/<someAgent>/src directory

To rebuild an agent, in that agent's directory type:
$>make clean;make

----------------------------------------------------
6 :: TRAINERS
----------------------------------------------------

Trainers are programs that put an agent into an environment and
control the experiment.  Sample trainers are located in the /trainers
directory.

There are two types of trainers: console trainers, and graphical
trainers.

Console trainers are well suited to running a proper experiment,
trying parameters, etc.  The graphical trainer gives a visual
representation of the problem and can be very handy for debugging and
visually evaluating your agent's performance.

We have provided several sample trainers, implemented in C/C++,
Python, and Java.  They are all identical in function, and are located
in:

/trainers/consoleTrainerJava
/trainers/consoleTrainerCPP
/trainers/consoleTrainerPython

These console trainers will run any of the Java domains.  Check the
source code in /trainers/<someTrainer>/src/ for an idea how to select
which problem the trainer will select.

To run any trainer, go into its directory and type:
$>make
$>bash run.bash

*****
* Note 2: running a trainer is only half of what is necessary to run
* an experiment -- you also need an agent!  See the previous section!
*****

To rebuild a trainer, in that trainer's directory type:
$>make clean;make

The graphical trainer that will run for any of the Java domains is in:
/trainers/guiTrainerJava

To run this trainer, go into its directory and type:
$>bash run.bash

The source code for the GUI trainer is not provided.  This trainer is
provided on an AS-IS basis.

----------------------------------------------------
7 :: RUNNING YOUR FIRST CONSOLE EXPERIMENT
----------------------------------------------------

Running a console experiment is very similar to running a graphical
experiment, you need to choose a trainer and an agent.  By default,
the trainers are set to run 10 episodes of Acrobot.  In order to run
Tetris or Helicopter go into the src/ directory and uncomment the
appropriate lines in consoleTrainer.cpp, and then type "make" back in
the trainer's home directory.

To run the default Acrobot experiment, open two terminal windows:

Terminal 1:
$>cd trainers/consoleTrainerJava
$>bash run.bash

Terminal 2:
$>cd agents/acrobotAgentPython
$>bash run.bash

You should see an experiment unfold!

----------------------------------------------------
8 :: Where to go for more info
----------------------------------------------------

To get more information about how to use this package, how to create
custom experiments, new agents, and trouble shooting, please visit:

http://2009.rl-competition.org /

----------------------------------------------------
9 :: CREDITS (2009 Tech Committee)
----------------------------------------------------
Brian Tanner
Adam White
John Asmuth
Chris Rayner
Monica Dinculescu
Istvan Szita
John Asmuth
Jose Antonio Martin H. 
Carlos Diuk


