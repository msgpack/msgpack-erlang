.PHONY: compile xref eunit clean check-all make deps test

REBAR=./rebar3

all: compile

# for busy typos
m: all
ma: all
mak: all
make: all

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref

test:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer

check-all:
	@$(REBAR)  eunit xref dialyzer

crosslang:
	@echo "do ERL_LIBS=../ before you make crosslang or fail"
	cd test && make crosslang
