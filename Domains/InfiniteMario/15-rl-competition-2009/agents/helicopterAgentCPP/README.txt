This java agent for the helicopter domain.  This has been specifically implemented for the helicopter domain and will not work with any other competition domains. This is a weak baseline controller. It is _simpler_ to fly in the helicopter domain(simulator) than the real helicopter.  The real helicopter is more difficult due to (i) observation noise, (ii) latency.


To recompile just the helicopter java agent:
>> make clean
>> make

in helicopterAgentCPP directory.

run.bash:
- Starts the weak_baseline_controller process

You need to start a trainer separately.