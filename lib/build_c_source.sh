#!/bin/sh

prefix=$1alien-matrix
CC=`which gcc`
COPTS="-O3"

rm $prefix/source.o 2> /dev/null
$CC $COPTS -pipe -fPIC -c $prefix/source.c -o $prefix/source.o
echo "; Source compiled"
$CC -shared -o $prefix/libsbmath.so $prefix/source.o
mv $prefix/libsbmath.so $1lib/
rm $prefix/source.o
echo "; Shared library installed"
