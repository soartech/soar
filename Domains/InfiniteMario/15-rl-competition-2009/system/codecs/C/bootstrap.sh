#! /bin/sh
libtoolize --automake -c -f
aclocal -I config
autoheader
autoconf
automake --foreign --add-missing --copy
