#!/bin/bash
BASEDIR=`pwd`
rm -Rf build
mkdir build
cd rl-glue
make clean
./configure --prefix=$BASEDIR/build
make clean
make
make install
cd ../codecs/C
./configure --prefix=$BASEDIR/build --with-rl-glue=$BASEDIR/build
make clean
make
make install



