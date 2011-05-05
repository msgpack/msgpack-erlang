crosslang:
	@erl -make
	@ruby -e "require 'rubygems'; require 'msgpack/rpc'"
	@./samplectl start
	@sleep 3
	@ruby crosslang.rb --runner=console
	@./samplectl stop
