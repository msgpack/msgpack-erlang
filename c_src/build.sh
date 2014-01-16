#!/bin/sh

if ! test -f c_src/msgpack-0.5.8/src/.libs/libmsgpack.a ; then
  tar xzf c_src/msgpack-0.5.8.tar.gz -C c_src
  cd c_src/msgpack-0.5.8
  ./configure
  make
fi
