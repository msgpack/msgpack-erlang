#!/bin/sh

tar xzf c_src/msgpack-0.5.7.tar.gz -C c_src
cd c_src/msgpack-0.5.7
./configure
make