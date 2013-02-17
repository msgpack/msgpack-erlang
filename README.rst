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

  src/msgpack.erl:343:<0.127.0>:   serialize: 0.865 s
  src/msgpack.erl:344:<0.127.0>: deserialize: 0.784 s
  src/msgpack.erl:345:<0.127.0>: for 1884 KB test data(msgpack).
  src/msgpack.erl:349:<0.127.0>:   serialize: 0.027 s
  src/msgpack.erl:350:<0.127.0>: deserialize: 0.071 s
  src/msgpack.erl:351:<0.127.0>: for 1884 KB test data.(msgpack_nif).


License
-------

Apache License 2.0
