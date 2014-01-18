#!/bin/sh

MSGPACK_VERSION=0.5.8
MSGPACK_TARBALL=msgpack-$MSGPACK_VERSION.tar.gz

if ! test -f c_src/$MSGPACK_TARBALL ; then
  wget https://github.com/msgpack/msgpack-c/releases/download/cpp-0.5.8/msgpack-0.5.8.tar.gz -P c_src
fi

if ! test -f c_src/$MSGPACK_TARBALL/src/.libs/libmsgpack.a ; then
  tar xzf c_src/$MSGPACK_TARBALL -C c_src
  cd c_src/msgpack-$MSGPACK_VERSION
  ./configure
  make
fi
