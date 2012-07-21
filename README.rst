##################
MessagePack Erlang
##################

.. image:: https://secure.travis-ci.org/msgpack/msgpack-erlang.png

prequisites for runtime
-----------------------

Erlang runtime system (http://erlang.org/)

rebar.config
------------

::

   {deps, [
     {msgpack, ".*",
       {git, "git://github.com/msgpack/msgpack-erlang.git", "master"}}
   ]}.

::

   Ham = msgpack:pack(Spam),
   {Spam, <<>>} = msgpack:unpack(Ham).

License
-------

Apache License 2.0
