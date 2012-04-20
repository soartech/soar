This Java agent is meant to select random actions for any RL-Glue domain whose actions are properly characterized by a valid task spec.  It can handle multi-dimensional discrete and continuous actions. This agent does not learn.


To recompile just the random python agent:
>> ant clean
>> ant build

in octopusAgentJava directory.

>>run.bash:
- Starts the octopusAgentJava process

You need to start an experiment separately.

Note: This agent CAN be dynamically loaded by RL-Viz experiments.
