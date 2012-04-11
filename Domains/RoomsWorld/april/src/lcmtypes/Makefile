include ../common.mk

SUFFIXES += .java .lcm .c .h .o

LCMTYPES_DIR  = ../../lcmtypes/
CFLAGS  = $(CFLAGS_STD) $(CFLAGS_LC) -I.. -O4 -fPIC
LDFLAGS = $(LDFLAGS_STD) $(LDFLAGS_LC)

LCMTYPES_LCM = $(shell ls $(LCMTYPES_DIR)/*.lcm)
LCMTYPES     = $(notdir $(LCMTYPES_LCM))
LCMTYPES_C   = $(LCMTYPES:%.lcm=%.c)
LCMTYPES_O   = $(LCMTYPES:%.lcm=%.o)

LCM = lcm-gen
LIBLCMTYPES = $(LIB_PATH)/liblcmtypes.a

all: $(LIBLCMTYPES)


$(LIBLCMTYPES): $(LCMTYPES_C) $(LCMTYPES_O)
	ar rc $@ $(LCMTYPES_O)


#	@echo "    [$@]"
%.c %.h: $(LCMTYPES_DIR)%.lcm 
	$(LCM) $< -c

clean:
	rm -f *.c *.h *.o *.a *~
