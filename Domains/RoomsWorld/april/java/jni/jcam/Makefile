GCC = gcc -Wall -g -std=gnu99 -D_REENTRANT -fPIC -shared -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -Wno-unused-parameter -Wno-format-zero-length
LIBIMAGESOURCE_OBJS = image_source.o image_source_v4l2.o image_source_jni.o image_source_dc1394.o url_parser.o image_source_islog.o

LIB=libimagesource.so
DESTLIB=../../../lib/$(LIB)

all: $(DESTLIB) istest


istest: main.o $(LIBIMAGESOURCE_OBJS)
	gcc -o $@ main.o $(LIBIMAGESOURCE_OBJS) -ldc1394


$(DESTLIB): $(LIB)
	cp $(LIB) $(DESTLIB)

libimagesource.so: april_jcam_ImageSourceNative.h $(LIBIMAGESOURCE_OBJS)
	ld --shared $(LIBIMAGESOURCE_OBJS) -o libimagesource.so -ldc1394

april_jcam_ImageSourceNative.h:
	echo "Rebuilding JNI headers. Ensure java file has been recently built."
	javah -classpath ../../april.jar  april.jcam.ImageSourceNative

clean:
	rm -f *~ april_jcam_ImageSourceNative.h april_jcam_ImageSourceFormat.h $(LIB) $(DESTLIB) $(LIBIMAGESOURCE_OBJS)

# If you get an error message saying that you're missing jni.h or
# jni_md.h, locate those files and add the directories using the -I
# flag. See example below

JNI_INCLUDES = -I/usr/lib/jvm/java-6-sun/include/ -I/usr/lib/jvm/java-6-sun/include/linux -I/usr/lib/jvm/java-6-openjdk/include/
DC1394_INCLUDES = -I/usr/local/include/

%.o: %.c
	$(GCC) -shared -O2 -c -fPIC -fno-omit-frame-pointer -fno-stack-protector -D_REENTRANT $< $(JNI_INCLUDES) $(DC1394_INCLUDES)
