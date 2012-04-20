LDFLAGS := $(LDFLAGS)
CFLAGS := $(CFLAGS) -Wall -pedantic

all: SampleCodecEnvironment

SampleCodecEnvironment: Custom_Integrated_Env_Codec.o TheGame.o
	$(CC)  $(LDFLAGS) Custom_Integrated_Env_Codec.o TheGame.o -lrlutils -lrlgluenetdev -o SampleCodecEnvironment      

Custom_Integrated_Env_Codec.o: src/Custom_Integrated_Env_Codec.c
	$(CC)  $(CFLAGS) -c src/Custom_Integrated_Env_Codec.c -o Custom_Integrated_Env_Codec.o 

TheGame.o: src/TheGame.c
	$(CC)	$(CFLAGS) -c src/TheGame.c -o TheGame.o
clean:
	rm -rf SampleCodecEnvironment SampleCodecEnvironment.exe *.o









