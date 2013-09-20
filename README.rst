##################
MessagePack Erlang
##################

.. image:: https://secure.travis-ci.org/msgpack/msgpack-erlang.png

.. image:: https://drone.io/github.com/msgpack/msgpack-erlang/status.png

prequisites for runtime
-----------------------

`Erlang runtime system <http://erlang.org/>`_ , >= R15B -- otherwise rebar won't work.
Based on `the new msgpack spec 232a0d <https://github.com/msgpack/msgpack/blob/232a0d14c6057000cc4a478f0dfbb5942ac54e9e/spec.md>`_ .

Now this supports string type.

::

  1> {ok, "埼玉"} = msgpack:unpack(msgpack:pack("埼玉")).
  {ok,[22524,29577]}


There are several options for `msgpack:pack/2` and `msgpack:unpack/2` .
See `msgpack_list_options()` in `msgpack.hrl`.


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

Compatibility mode
------------------

To use as same with `old spec <https://github.com/msgpack/msgpack/blob/master/spec-old.md>`_ ::

   OldHam = msgpack:pack(Spam, [{enable_str,false}]),
   {ok, Spam} = msgpack:unpack(OldHam, [{enable_str,false}]).


experimental feature: NIF (de)serializer
----------------------------------------

**Currently NIF is unavailable on both new and old spec.**

since 0.1.1 - only tested in MacOS, Linux

::

  test/bench_tests.erl:36:<0.125.0>:   serialize: 0.543 s
  test/bench_tests.erl:37:<0.125.0>: deserialize: 0.653 s
  test/bench_tests.erl:38:<0.125.0>: for 2041 KB test data(jiffy).
  test/bench_tests.erl:42:<0.125.0>:   serialize: 0.508 s
  test/bench_tests.erl:43:<0.125.0>: deserialize: 0.630 s
  test/bench_tests.erl:44:<0.125.0>: for 2041 KB test data(jsx).
  test/bench_tests.erl:54:<0.125.0>:   serialize: 0.063 s
  test/bench_tests.erl:55:<0.125.0>: deserialize: 0.053 s
  test/bench_tests.erl:56:<0.125.0>: for 3828 KB test data(t2b/b2t).
  test/bench_tests.erl:75:<0.125.0>:    serialize: 1.332 s
  test/bench_tests.erl:87:<0.125.0>:  deserialize: 1.601 s
  test/bench_tests.erl:88:<0.125.0>: for 2041 KB test data(jiffy x 5).
  test/bench_tests.erl:75:<0.125.0>:    serialize: 1.243 s
  test/bench_tests.erl:87:<0.125.0>:  deserialize: 3.233 s
  test/bench_tests.erl:88:<0.125.0>: for 2041 KB test data(jsx x 5).
  test/bench_tests.erl:75:<0.125.0>:    serialize: 0.076 s
  test/bench_tests.erl:87:<0.125.0>:  deserialize: 0.061 s
  test/bench_tests.erl:88:<0.125.0>: for 3828 KB test data(t2b/b2t x 5).


License
-------

Apache License 2.0
