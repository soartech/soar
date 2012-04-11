include ../common.mk

CFLAGS = $(CFLAGS_STD) $(CFLAGS_COMMON) $(CFLAGS_GLIB) -msse2 -fPIC -O4
LDFLAGS = $(LDFLAGS_STD) $(LDFLAGS_COMMON) $(LDFLAGS_GLIB)

COMMON_OBJS = getopt.o gps_linearize.o ioutils.o math_util.o moxa.o serial_wrapper.o timespec.o timestamp.o serial.o ssocket.o varray.o vhash.o

LIBCOMMON=$(LIB_PATH)/libcommon.a

all: $(LIBCOMMON)


$(LIBCOMMON): $(COMMON_OBJS)
	ar rc $@ $(COMMON_OBJS)

pixels.o: pixels.c
	@echo "    [$@]"
	$(CC) $(CFLAGS) -msse3 -c $<

gridmap.o: gridmap.c
	@echo "    [$@]"
	$(CC) $(CFLAGS) -msse -c $<

clean:
	rm -f *.o *~ *.a
	rm -f gu_minheap_tester
