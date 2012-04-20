systemPath=../../system
#Source a script that sets all important functions and variables
#source $systemPath/rl-competition-includes.sh

GLUEINCLUDES = $(systemPath)/build/include
GLUELIBS = $(systemPath)/build/lib
PARAMHOLDERSRC = $(systemPath)/includes
BIN = bin
SRC = src

CXX = g++
CXXFLAGS = -ansi -pedantic -Wall -I$(GLUEINCLUDES) -L$(GLUELIBS) -I$(PARAMHOLDERSRC) 
LDXFLAGS = -L$(GLUELIBS) -lrlexperiment -lrlutils -lrlgluenetdev

consoleTrainer: consoleTrainer.o ParameterHolder.o consoleTrainerHelper.o
	$(CXX) $(BIN)/consoleTrainer.o $(BIN)/consoleTrainerHelper.o $(BIN)/ParameterHolder.o -o $(BIN)/$@ $(LDXFLAGS)
	rm -fr $(BIN)/*.o

consoleTrainer.o: $(SRC)/consoleTrainer.cpp  $(SRC)/consoleTrainer.h
	$(CXX) -c $(CXXFLAGS) $< -o $(BIN)/$@

consoleTrainerHelper.o : $(SRC)/consoleTrainerHelper.cpp $(SRC)/consoleTrainerHelper.h
	$(CXX) -c $(CXXFLAGS) $< -o $(BIN)/$@

ParameterHolder.o: $(PARAMHOLDERSRC)/ParameterHolder.cpp $(PARAMHOLDERSRC)/ParameterHolder.h
	$(CXX) -c $(CXXFLAGS) $< -o $(BIN)/$@

clean:
	rm -fr $(BIN)/*
