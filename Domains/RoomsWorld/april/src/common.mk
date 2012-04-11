############################################################
# 
# This file should only contain CFLAGS_XXX and LDFLAGS_XXX directives.
# CFLAGS and LDFLAGS themselves should NOT be set: that is the job
# for the actual Makefiles (which will combine the flags given here)
#
# *** DO NOT SET CFLAGS or LDFLAGS  ***
#
# Our recommended flags for all projects. Note -pthread specifies reentrancy

# -Wno-format-zero-length: permit printf("");
# -Wno-unused-parameter: permit a function to ignore an argument
CFLAGS_STD   := -std=gnu99 -g -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -D_REENTRANT -Wall -Wno-unused-parameter -Wno-format-zero-length -pthread
LDFLAGS_STD  := -lm

ROOT_PATH    := $(shell pwd)/../..
SRC_PATH     := $(ROOT_PATH)/src
BIN_PATH     := $(ROOT_PATH)/bin
LIB_PATH     := $(ROOT_PATH)/lib
CONFIG_DIR   := $(shell pwd)/../../config

CC           := gcc
LD           := gcc

# dynamic libraries
ifeq "$(shell uname -s)" "Darwin"
	LDSH := -dynamic
	SHEXT := .dylib
	WHOLE_ARCHIVE_START := -all_load
else
	LD := gcc
	LDSH := -shared
	SHEXT := .so
	WHOLE_ARCHIVE_START := -Wl,-whole-archive
	WHOLE_ARCHIVE_STOP := -Wl,-no-whole-archive
endif

############################################################
#
# External libraries
#
# List these in roughly the order of dependency; those with fewest
# dependencies first. Within each LDFLAGS, list the dependencies in in
# decreasing order (e.g., end with LDFLAGS_GLIB)
#
############################################################

# common library
CFLAGS_COMMON  := -I$(SRC_PATH) -DCONFIG_DIR='"$(CONFIG_DIR)"'
LDFLAGS_COMMON := $(LIB_PATH)/libcommon.a

# glib
CFLAGS_GLIB  := `pkg-config --cflags glib-2.0 gmodule-2.0`
LDFLAGS_GLIB := `pkg-config --libs glib-2.0 gmodule-2.0 gthread-2.0 gobject-2.0`

# gsl (GNU scientific libraries)
CFLAGS_GSL   := -DHAVE_INLINE `gsl-config --cflags`
LDFLAGS_GSL  := `gsl-config --libs`

# jpeg
ifeq "$(shell test -f /usr/lib/libjpeg-ipp.so -o -f /usr/lib64/libjpeg-ipp.so && echo ipp)" "ipp"
	LDFLAGS_JPEG := -ljpeg-ipp
else
	LDFLAGS_JPEG := -ljpeg
endif

# gtk
CFLAGS_GTK   :=`pkg-config --cflags gtk+-2.0`
LDFLAGS_GTK  :=`pkg-config --libs gtk+-2.0 gthread-2.0`

CFLAGS_GTK_UTIL  := -I$(SRC_PATH) $(CFLAGS_GTK)
LDFLAGS_GTK_UTIL := $(LIB_PATH)/libgtk_util.a $(LDFLAGS_GTK)

# glade target path should be relative to one directory deep from common.mk
GLADE_TARGET_PATH:=$(BIN_PATH)/glade
CFLAGS_GLADE:=-DGLADE_TARGET_PATH='"$(GLADE_TARGET_PATH)"' \
	`pkg-config --cflags libglade-2.0`
LDFLAGS_GLADE:=`pkg-config --libs libglade-2.0` -rdynamic

# lcm
CFLAGS_LCM  := `pkg-config --cflags lcm`
LDFLAGS_LCM := `pkg-config --libs lcm`

# lcmtypes
CFLAGS_LCMTYPES  := -I$(SRC_PATH)
LDFLAGS_LCMTYPES := $(LIB_PATH)/liblcmtypes.a

# Open GL
CFLAGS_GL    := 
LDFLAGS_GL   := -lGLU -lGLU -lglut

# our gl util library
LDFLAGS_GLUTIL := $(LIB_PATH)/libglutil.a

CFLAGS_VIEWER  := $(CFLAGS_GTK_UTIL) $(CFLAGS_GTK) $(CFLAGS_GLIB) $(CFLAGS_GL)
LDFLAGS_VIEWER := $(LIB_PATH)/libviewer.a $(LDFLAGS_GTK_UTIL) $(LDFLAGS_GL) $(LDFLAGS_COMMON) $(LDFLAGS_GLIB)

# dgc library
#CFLAGS_DGC  := -I$(SRC_PATH)
#LDFLAGS_DGC := $(LIB_PATH)/libdgc.a $(LDFLAGS_LCMTYPES) $(LDFLAGS_LC)

# mesh models
MESH_MODEL_PATH:=$(ROOT_PATH)/../../meshmodels
CFLAGS_MESHMODELS:=-DMESH_MODEL_PATH='"$(MESH_MODEL_PATH)"'

LDFLAGS_VISION := $(LIB_PATH)/libvision.a

# dc1394
ENABLE_DC1394:=0

ifeq ($(ENABLE_DC1394),1)
ifeq "$(shell uname -s)" "Darwin"
LDFLAGS_DC1394:=-ldc1394 -L/usr/local/lib
else
LDFLAGS_DC1394:=-ldc1394 
endif
endif

# Intel Integrated Performance Primitives
IPPA:=
IPP_LIBS:=-lguide -lippcore -lippi -lippcc -lippcv
ifeq "$(shell uname -s)" "Darwin"
    IPP_BASE:=/Library/Frameworks/Intel_IPP.framework
    CFLAGS_IPP:=-I$(IPP_BASE)/Headers
    LDFLAGS_IPP:=-L$(IPP_BASE)/Libraries $(IPP_LIBS)
else
    ifeq "$(shell uname -m)" "x86_64"
        IPP_SEARCH := /usr/local/intel/ipp/5.1/em64t \
                      /opt/intel/ipp/5.2/em64t       \
		      /opt/intel/ipp/5.3/em64t       \
		      /opt/intel/ipp/5.3.1.062/em64t \
	              /opt/intel/ipp/5.3.2.068/em64t
        test_dir = $(shell [ -e $(dir) ] && echo $(dir))
        IPP_SEARCH := $(foreach dir, $(IPP_SEARCH), $(test_dir))
        IPP_BASE := $(firstword $(IPP_SEARCH))
        IPP_LIBS:=-lguide -lippcoreem64t -lippiem64t -lippjem64t -lippccem64t -lippcvem64t -lpthread -lippsem64t
        IPPA:=em64t
    else
        IPP_BASE:=/usr/local/intel/ipp/5.1/ia32
    endif
    CFLAGS_IPP:=-I$(IPP_BASE)/include
    LDFLAGS_IPP:=-L$(IPP_BASE)/sharedlib -Wl,-R$(IPP_BASE)/sharedlib $(IPP_LIBS)
endif

# libcam
LDFLAGS_LIBCAM:=$(LIB_PATH)/libcam.a \
				$(LDFLAGS_DGC) $(LDFLAGS_LCMTYPES) $(LDFLAGS_COMMON) \
				$(LDFLAGS_GLIB) $(LDFLAGS_GTK) $(LDFLAGS_GL) \
				$(LDFLAGS_JPEG) $(LDFLAGS_DC1394)

%.o: %.c %.h
	@echo "    [$@]"
	$(CC) $(CFLAGS) -c $< 

%.o: %.c
	@echo "    [$@]"
	$(CC) $(CFLAGS) -c $< 

%.o: %.cpp
	@echo "    [$@]"
	g++ -c -o $@ $< $(CFLAGS_CXX)


