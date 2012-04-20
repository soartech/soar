AGENT = RandomAgent.cpp
AGENT_OBJECTS = $(basename $(AGENT)).o

AGENT_SRC_PATH = ./src
BUILD_PATH = ./bin
BIN_PATH = ./bin
RL_GLUE_BASE = ../../system/build
RL_GLUE_PATH = $(RL_GLUE_BASE)/RL-Glue
UTILS_PATH = $(RL_GLUE_BASE)/Utils

CFLAGS = -I$(RL_GLUE_BASE)/include -I$(AGENT_SRC_PATH) -g
LDFLAGS= -L$(RL_GLUE_BASE)/lib -lrlagent -lrlgluenetdev -lrlutils
ifeq ($(suffix $(AGENT)),.cpp)
	CC = $(CXX)
endif

all: $(BIN_PATH)/RL_agent


$(BIN_PATH)/RL_agent: $(addprefix $(BUILD_PATH)/, $(AGENT_OBJECTS))
	$(CC) -o $(BIN_PATH)/RL_agent $^ $(LDFLAGS) -lm

$(BUILD_PATH)/$(basename $(AGENT)).o: $(AGENT_SRC_PATH)/$(AGENT)
	$(CC) -c $(CFLAGS) $^ -o $@

clean:
	rm -rf RL_agent $(BUILD_PATH)/*
