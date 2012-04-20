RLVizPath = ../../system/libraries/rl-viz
OS = $(shell uname -o)

all:
ifneq ($(strip $(shell gcc -v 2>&1 |grep "cygwin")),)
	@echo Cygwin
	javac -source 5 -d bin -cp $(RLVizPath)/RLVizLib.jar\;$(RLVizPath)/RLVizApp.jar src/tetrisexample/*.java src/tetrisexample/util/*.java
else
	@echo Other
	javac -source 5 -d bin -cp $(RLVizPath)/RLVizLib.jar:$(RLVizPath)/RLVizApp.jar src/tetrisexample/*.java src/tetrisexample/util/*.java
endif

clean:
	rm -Rf bin/*