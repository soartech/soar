#If RL-Glue Core or the C/C++ Codecs are not installed to default locations
#You may need to use set the EXTRA_CFLAGS and EXTRA_LDFLAGS. 

#Current values of these two vars is supposed to be illustrative, not correct for you.
#This is what you would use if you installed the C/C++ Codec with:
# ./configure --prefix=/Users/btanner/rl-glue
#EXTRA_CFLAGS = -I/Users/btanner/rl-glue/include/rlglue
#EXTRA_LDFLAGS =-L/Users/btanner/rl-glue/lib 

all: 	
	EXTRA_CFLAGS=$(EXTRA_CFLAGS) EXTRA_LDFLAGS=$(EXTRA_LDFLAGS) make -f Makefile.agent 
	EXTRA_CFLAGS=$(EXTRA_CFLAGS) EXTRA_LDFLAGS=$(EXTRA_LDFLAGS) make -f Makefile.environment 
	EXTRA_CFLAGS=$(EXTRA_CFLAGS) EXTRA_LDFLAGS=$(EXTRA_LDFLAGS) make -f Makefile.experiment

clean:
	make -f Makefile.agent  clean
	make -f Makefile.environment clean
	make -f Makefile.experiment clean
