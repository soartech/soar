#! /bin/sh
libtoolize --automake -f -c
aclocal -I config
autoheader
autoconf
automake --foreign --add-missing --copy
