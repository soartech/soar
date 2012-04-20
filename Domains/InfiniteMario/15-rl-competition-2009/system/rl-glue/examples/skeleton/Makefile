#If RL-Glue Core or the C/C++ Codecs are not installed to default locations
#You may need to use set the CFLAGS and LDFLAGS. 

#Current values of these two vars is supposed to be illustrative, not correct for you.
#This is what you would use if you installed the RL-Glue Core with:
# ./configure --prefix=/Users/btanner/rl-glue
# LDFLAGS := $(LDFLAGS) -L/Users/btanner/rl-glue/lib
# CFLAGS := $(CFLAGS) -I/Users/btanner/rl-glue/include/rlglue


all: SkeletonExperiment

SkeletonExperiment: SkeletonExperiment.o SkeletonAgent.o SkeletonEnvironment.o
	$(CC)  $(LDFLAGS) SkeletonExperiment.o SkeletonAgent.o SkeletonEnvironment.o -lrlglue -lrlutils -o SkeletonExperiment      

SkeletonExperiment.o: SkeletonExperiment.c
	$(CC)  $(CFLAGS) -c SkeletonExperiment.c -o SkeletonExperiment.o 

SkeletonAgent.o: SkeletonAgent.c
	$(CC)  $(CFLAGS) -c SkeletonAgent.c -o SkeletonAgent.o 

SkeletonEnvironment.o: SkeletonEnvironment.c
	$(CC)  $(CFLAGS) -c SkeletonEnvironment.c -o SkeletonEnvironment.o 

clean:
	rm -rf SkeletonExperiment SkeletonExperiment.exe *.o









