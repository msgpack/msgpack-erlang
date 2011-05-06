require 'rubygems'
require 'msgpack/rpc'

c = MessagePack::RPC::Client.new("localhost", 9199)
c.timeout = 100
p c.call(:hello)
p c.call(:add, 21, 32) 
