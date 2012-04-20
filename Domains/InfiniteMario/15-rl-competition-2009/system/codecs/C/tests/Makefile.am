TESTS = test_sanity.sh test_1.sh test_message.sh test_rl_episode.sh test_init_cleanup.sh test_empty.sh test_speed.sh

check_SCRIPTS = test_sanity.sh test_1.sh test_message.sh test_rl_episode.sh test_init_cleanup.sh test_empty.sh test_speed.sh
check_PROGRAMS = test_sanity_experiment test_1_agent test_1_environment test_1_experiment test_message_agent test_message_environment test_message_experiment  test_rl_episode_experiment test_init_cleanup_experiment test_empty_agent test_empty_environment test_empty_experiment test_speed_environment test_speed_experiment

AM_CFLAGS=-g -Wall -pedantic -std=c99

EXTRA_LIBS=-lrlgluenetdev -lrlutils
test_sanity_experiment_SOURCES = test_sanity_experiment.c useful_functions.c useful_functions.h
test_sanity_experiment_LDADD = ../src/librlexperiment.la  $(EXTRA_LIBS)

test_1_agent_SOURCES = test_1_agent.c useful_functions.c useful_functions.h
test_1_agent_LDADD = ../src/librlagent.la  $(EXTRA_LIBS)

test_1_environment_SOURCES = test_1_environment.c useful_functions.c useful_functions.h
test_1_environment_LDADD = ../src/librlenvironment.la  $(EXTRA_LIBS)

test_1_experiment_SOURCES = test_1_experiment.c useful_functions.c useful_functions.h
test_1_experiment_LDADD = ../src/librlexperiment.la  $(EXTRA_LIBS)


test_message_agent_SOURCES = test_message_agent.c useful_functions.c useful_functions.h
test_message_agent_LDADD = ../src/librlagent.la  $(EXTRA_LIBS)

test_message_environment_SOURCES = test_message_environment.c useful_functions.c useful_functions.h
test_message_environment_LDADD = ../src/librlenvironment.la  $(EXTRA_LIBS)

test_message_experiment_SOURCES = test_message_experiment.c useful_functions.c useful_functions.h
test_message_experiment_LDADD = ../src/librlexperiment.la  $(EXTRA_LIBS)

test_rl_episode_experiment_SOURCES = test_rl_episode_experiment.c useful_functions.c useful_functions.h
test_rl_episode_experiment_LDADD = ../src/librlexperiment.la  $(EXTRA_LIBS)

test_init_cleanup_experiment_SOURCES = test_init_cleanup_experiment.c useful_functions.c useful_functions.h
test_init_cleanup_experiment_LDADD = ../src/librlexperiment.la  $(EXTRA_LIBS)

test_empty_agent_SOURCES = test_empty_agent.c useful_functions.c useful_functions.h
test_empty_agent_LDADD = ../src/librlagent.la  $(EXTRA_LIBS)

test_empty_environment_SOURCES = test_empty_environment.c useful_functions.c useful_functions.h
test_empty_environment_LDADD = ../src/librlenvironment.la  $(EXTRA_LIBS)

test_empty_experiment_SOURCES = test_empty_experiment.c useful_functions.c useful_functions.h
test_empty_experiment_LDADD = ../src/librlexperiment.la  $(EXTRA_LIBS)

test_speed_environment_SOURCES = test_speed_environment.c useful_functions.c useful_functions.h
test_speed_environment_LDADD = ../src/librlenvironment.la  $(EXTRA_LIBS)

test_speed_experiment_SOURCES = test_speed_experiment.c useful_functions.c useful_functions.h
test_speed_experiment_LDADD = ../src/librlexperiment.la  $(EXTRA_LIBS)

INCLUDES = -I$(top_srcdir)/src

EXTRA_DIST=$(check_SCRIPTS)