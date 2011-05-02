require 'rubygems'
require 'msgpack/rpc'

MessagePack::client.new("127.0.0.1", 9199) do |c|
  futures = []
  futures << c.call_async(:add, 23, 3)
  futures << c.call_async(:add, 23, 3)
  futures << c.call_async(:add, 23, 3)
  futures << c.call_async(:add, 23, 3)
  
  futures.each do |f|
    p f.get
  end
end

