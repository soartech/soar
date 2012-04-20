.PHONY: RL_experiment
# Experiment Source Files
EXPERIMENT = consoleTrainer
EXPERIMENT_PATH = ./src

# Path to RL-Glue to find RL_interface and related files.
PYTHON_CODEC_PATH = ../../system/codecs/Python/src/
PARAMETER_HOLDER_PATH = ../../system/includes

all: RL_experiment

RL_experiment:
	echo -e "#! /bin/bash\n  PYTHONPATH=$(PYTHON_CODEC_PATH):$(PARAMETER_HOLDER_PATH):$(EXPERIMENT_PATH) python $(EXPERIMENT_PATH)/$(EXPERIMENT).py " | cat > RL_experiment
	chmod u+x RL_experiment

clean:
	rm -f RL_experiment *.pyc $(EXPERIMENT_PATH)/*.pyc