CODEC_RELEASE=1:0:0
CODEC_VERSION=1:0:0

lib_LTLIBRARIES = librlagent.la librlenvironment.la librlexperiment.la

EXTRA_C_FLAGS=-O2

noinst_HEADERS = RL_codec_common.h

librlagent_la_SOURCES = RL_client_agent.c RL_debug.c
librlagent_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
librlagent_la_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99 $(EXTRA_C_FLAGS)

librlenvironment_la_SOURCES = RL_client_environment.c RL_debug.c
librlenvironment_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
librlenvironment_la_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99 $(EXTRA_C_FLAGS)

librlexperiment_la_SOURCES = RL_client_experiment.c RL_debug.c
librlexperiment_la_LDFLAGS= -version-info $(CODEC_VERSION) -release $(CODEC_RELEASE)
librlexperiment_la_CFLAGS = $(AM_CFLAGS) -Wall -pedantic -std=c99 $(EXTRA_C_FLAGS)