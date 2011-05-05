require 'test/unit'

require 'rubygems'
require 'msgpack/rpc'

class TestHoge < Test::Unit::TestCase
  def setup
    @c = MessagePack::RPC::Client.new("localhost", 9199)
  end

  # def teardown
  # end

  def test_foo
    p @c.call(:add, 1, 2)
    p @c.call(:hello)
    assert(true)
  end
end
