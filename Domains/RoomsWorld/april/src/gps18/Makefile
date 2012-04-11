include ../common.mk

CFLAGS = $(CFLAGS_STD) $(CFLAGS_COMMON) $(CFLAGS_DGC) $(CFLAGS_GLIB) $(CFLAGS_LCM) $(CFLAGS_LCMTYPES) -D_GNU_SOURCE
LDFLAGS = $(LDFLAGS_DGC) $(LDFLAGS_COMMON) $(LDFLAGS_STD) $(LDFLAGS_GLIB) $(LDFLAGS_LCM) $(LDFLAGS_LCMTYPES)

# BELOW FROM gps, above from sick

LDFLAGS += -lpthread -lm
#LDFLAGS +=   `pkg-config --libs glib-2.0 gthread-2.0`

gpsd:=$(BIN_PATH)/gpsd

gpsd_obj:=gps.o main.o 

.PHONY: all test clean tidy

all: $(gpsd)

$(gpsd): $(gpsd_obj)
	@echo "    [$@]"
	$(CC) -o $@ $(gpsd_obj) $(LDFLAGS)

%.o: %.c
	@echo "    [$@]"
	$(CC) -c -o $@ $< $(CFLAGS)

test:
	@echo gps18 : nothing to test

clean: tidy
	rm -f $(gpsd) $(gps_lcm_listen)

tidy:
	rm -f $(gpsd_obj) $(gps_lcm_listen_obj)
