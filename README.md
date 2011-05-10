MessagePack-RPC Erlang
======================

This code is in alpha-release. Synchronous RPC seems working.

# prequisites for runtime
Erlang runtime system (http://erlang.org/)

# prequisites for build and test
GNU Make, Erlang, (optional)MessagePack-RPC/C++

# client

## usage

## supervision tree

# server

## usage

## supervision tree

# install

## TODO

- session TIMEOUTs for client and server
- where do I make <<"nil">> atom??
- error handling 
-- what if happens when badarg/noproc/bad_clause, and exceptions.
-- check Msgpack-RPC spec of error handling
- crosslang test
-- Ruby: msgpack-rpc 0.4.5 can't access while 0.4.4 works
-- Python: make msgpackrpc on PyPI
- coverage 100%
- UDP transport
- UDS transport
- SCTP/SSL/zip and more...
- rewrite tutorial and README
- release handling (/release/*.appup)
- full-spec type/spec notation
- longrun test
