#If RL-Glue Core or the C/C++ Codecs are not installed to default locations
#You may need to use set the CFLAGS and LDFLAGS. 

#Current values of these two vars is supposed to be illustrative, not correct for you.
#This is what you would use if you installed the RL-Glue Core with:
# ./configure --prefix=/Users/btanner/rl-glue
# LDFLAGS := $(LDFLAGS) -L/Users/btanner/rl-glue/lib
# CFLAGS := $(CFLAGS) -I/Users/btanner/rl-glue/include/rlglue

all: SampleExperiment

SampleExperiment: SampleExperiment.o SampleSarsaAgent.o SampleMinesEnvironment.o
	$(CC)   SampleExperiment.o SampleSarsaAgent.o SampleMinesEnvironment.o  $(LDFLAGS) -lm -lrlglue -lrlutils -o SampleExperiment      

SampleExperiment.o: SampleExperiment.c
	$(CC)  $(CFLAGS) -c SampleExperiment.c -o SampleExperiment.o 

SampleSarsaAgent.o: SampleSarsaAgent.c
	$(CC)  $(CFLAGS) -c SampleSarsaAgent.c -o SampleSarsaAgent.o 

SampleMinesEnvironment.o: SampleMinesEnvironment.c
	$(CC)  $(CFLAGS) -c SampleMinesEnvironment.c -o SampleMinesEnvironment.o 

clean:
	rm -rf SampleExperiment SampleExperiment.exe *.o









