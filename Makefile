.PHONY: compile xref eunit clean doc check make deps

REBAR=./rebar

all: compile

# for busy typos
m: all
ma: all
mak: all
make: all

console: compile
	@erl -pa ebin
deps:
	@$(REBAR) update-deps get-deps

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref

eunit: compile
	@$(REBAR) skip_deps=true eunit

test: eunit

clean:
	@$(REBAR) clean

doc:
	@$(REBAR) doc

bench: compile
	@$(REBAR) euni skip_deps=true suites=bench_tests

check: compile xref
#	@echo "you need $(REBAR) build-plt before make check"
#	@$(REBAR) build-plt
	@$(REBAR) check-plt
	@$(REBAR) dialyze

crosslang:
	@echo "do ERL_LIBS=../ before you make crosslang or fail"
	cd test && make crosslang
