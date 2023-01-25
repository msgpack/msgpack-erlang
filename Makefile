.PHONY: compile clean check-all test

REBAR=rebar3

all: compile

compile:
	@$(REBAR) compile

test:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer

check-all: test dialyzer
