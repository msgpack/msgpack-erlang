##################
MessagePack Erlang
##################

.. image:: https://secure.travis-ci.org/msgpack/msgpack-erlang.png

prequisites for runtime
-----------------------

Erlang runtime system ( http://erlang.org/ ), >= R15B -- otherwise rebar won't work.

rebar.config
------------

::

   {deps, [
     {msgpack, ".*",
       {git, "git://github.com/msgpack/msgpack-erlang.git", "master"}}
   ]}.

Simple deserialization

::

   Ham = msgpack:pack(Spam),
   {ok, Spam} = msgpack:unpack(Ham).

Stream deserialization

::

   {Term0, Rest0} = msgpack:unpack_stream(Binary),
   {Term1, Rest1} = msgpack:unpack_stream(Rest0),
   ...

experimental feature: NIF (de)serializer
----------------------------------------

since 0.1.1 - only tested in MacOS

::

  src/msgpack.erl:343:<0.131.0>:   serialize: 0.405 s
  src/msgpack.erl:344:<0.131.0>: deserialize: 0.470 s
  src/msgpack.erl:345:<0.131.0>: for 1884 KB test data(msgpack).
  src/msgpack.erl:349:<0.131.0>:   serialize: 0.019 s
  src/msgpack.erl:350:<0.131.0>: deserialize: 0.036 s
  src/msgpack.erl:351:<0.131.0>: for 1884 KB test data.(msgpack_nif).
  src/msgpack.erl:355:<0.131.0>:   serialize: 0.043 s
  src/msgpack.erl:356:<0.131.0>: deserialize: 0.027 s
  src/msgpack.erl:357:<0.131.0>: for 3828 KB test data.(t2b/b2t).


License
-------

Apache License 2.0
