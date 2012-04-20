# ------------------------------------------------------------------
# Change these values to use different source files and directories 
# ------------------------------------------------------------------
AGENT = weak_baseline_controller.cpp
AGENT_SRC_PATH = src
BUILD_PATH = bin

RL_GLUE_BASE = ../../system/build
CODEC_BASE   = $(RL_GLUE_BASE)
# ------------------------------------------------------------------

AGENT_OBJECTS = $(basename $(AGENT)).o

CFLAGS  = -I$(AGENT_SRC_PATH)   -I$(RL_GLUE_BASE)/include
LDFLAGS= -L$(RL_GLUE_BASE)/lib -lrlagent -lrlgluenetdev -lrlutils

ifeq ($(suffix $(AGENT)),.cpp)
	CC = $(CXX)
endif

all: RL_agent

RL_agent: $(addprefix $(BUILD_PATH)/, $(AGENT_OBJECTS))
	$(CC) -o RL_agent $^  $(LDFLAGS) -lm

$(BUILD_PATH)/$(basename $(AGENT)).o: $(AGENT_SRC_PATH)/$(AGENT)
	$(CC) -c $(CFLAGS) $^ -o $@

clean:
	rm -rf RL_agent RL_agent.exe $(BUILD_PATH)/*

