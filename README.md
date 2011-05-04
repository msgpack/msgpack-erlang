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

- notification protocol
- where do I make <<"nil">> atom??
- error handling 
-- what if happens when badarg/noproc/bad_clause, and exceptions.
-- check Msgpack-RPC spec of error handling
- crosslang test
- coverage 100%
- UDP transport
- UDS transport
- SCTP/SSL/zip and more...
- rewrite tutorial and README
- release handling (/release/*.appup)
- full-spec type/spec notation
- longrun test
