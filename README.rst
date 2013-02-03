##################
MessagePack Erlang
##################

.. image:: https://secure.travis-ci.org/msgpack/msgpack-erlang.png

prequisites for runtime
-----------------------

Erlang runtime system ( http://erlang.org/ ), >= R15B -- rebar won't work.

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


License
-------

Apache License 2.0
