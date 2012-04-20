#!/bin/bash
THISDIR=`pwd`
BASEDIR=$THISDIR/system
BUILDDIR=$BASEDIR/build
rm -Rf $BUILDDIR
mkdir $BUILDDIR
cd $BASEDIR/rl-glue
./configure --prefix=$BUILDDIR
make clean
make
make install
cd ../codecs/C
./configure --prefix=$BUILDDIR --with-rl-glue=$BUILDDIR
make clean
make
make install


cd $THISDIR
make clean
make all