require 'test/unit'

require 'rubygems'
require 'msgpack/rpc'

class TestSample < Test::Unit::TestCase
  def setup
    @c = MessagePack::RPC::Client.new("localhost", 9199)
    @c.timeout = 10
  end

  def teardown
    @c.close
  end

  def test_foo
    assert_equal(@c.call(:add, 1, 2), 3)
  end

  def test_call

    result = @c.call(:hello)
    assert_equal(result, "hello, msgpack!")
    
    result = @c.call(:add, 1, 2)
    assert_equal(result, 3)
    
  end

  def test_send
    req1 = @c.call_async(:hello)
    req2 = @c.call_async(:add, 1, 2)
    
    req1.join
    req2.join
    
    assert_equal(req1.result, "hello, msgpack!")
    assert_nil(req1.error)
    assert_equal(req2.result, 3)
    assert_nil(req2.error)
  end
  

  def test_hidden
    count = 0
    
    rejected = false
    begin
      @c.call(:hidden)
    rescue MessagePack::RPC::RemoteError
      rejected = true
    end
    
    assert_equal(rejected, true)

  end

  def test_exception
    raised = false
    begin
      @c.call(:exception)
    rescue MessagePack::RPC::RemoteError
      assert_equal($!.message, "no such method: exception")
      raised = true
    end
    
    assert_equal(raised, true)
  end

  def test_pool
    sp = MessagePack::RPC::SessionPool.new
    s = sp.get_session('127.0.0.1', @c.port)
    
    result = s.call(:hello)
    assert_equal(result, "hello, msgpack!")
    
    result = s.call(:add, 1, 2)
    assert_equal(result, 3)
    
    sp.close
    @c.close
  end


  def test_loop
    port = 9199
    loop = MessagePack::RPC::Loop.new

    @c = MessagePack::RPC::Client.new("127.0.0.1", port, loop)
    @c.timeout = 10
    
    count = 0
    
    @c.callback(:hello) do |error, result|
      assert_equal(result, "hello, msgpack!")
      assert_nil(error)
      count += 1
    end
    
    @c.callback(:add, 1, 2) do |error, result|
      assert_equal(result, 3)
      assert_nil(error)
      count += 1
    end
    
    while count < 2
      loop.run_once
    end
    
  end

  def test_timeout
  end

  def test_address
    addr = MessagePack::RPC::Address.new('172.16.0.11', 18900)
    raw = addr.to_msgpack
    msg = MessagePack.unpack(raw)
    addr2 = MessagePack::RPC::Address.load(msg)
    assert_equal(addr, addr2)
  end


end
