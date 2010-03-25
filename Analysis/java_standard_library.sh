#!/bin/bash
#
# Script to download and install classpath.  Works for me.  YMMV.
# Generates java_standard_library.jar.  

VER=classpath-0.98
if [ ! -a $VER.tar.gz ]; then
    wget ftp://ftp.gnu.org/gnu/classpath/$VER.tar.gz
fi
tar zxf $VER.tar.gz
cd $VER
env JAVAC=javac ./configure --enable-jni --disable-plugin --disable-gconf-peer --disable-examples --disable-gtk-peer --disable-gjdoc
make
if [ "$?" == 0 ]; then
    cp lib/glibj.zip ../java_standard_library.jar
    cd ..
    rm -rf $VER
else
    echo "make failed!"
fi
