.PHONY: compile xref eunit clean doc check make deps test

all: compile

# for busy typos
m: all
ma: all
mak: all
make: all

deps:
	@./rebar update-deps get-deps

compile:
	@./rebar compile

xref:
	@./rebar xref

test: eunit

eunit: compile
	@./rebar skip_deps=true eunit

clean:
	@./rebar clean

doc:
	@./rebar doc

bench: compile
	@./rebar euni skip_deps=true suites=bench_tests

check: compile xref
#	@echo "you need ./rebar build-plt before make check"
#	@./rebar build-plt
	@./rebar check-plt
	@./rebar dialyze

crosslang:
	@echo "do ERL_LIBS=../ before you make crosslang or fail"
	cd test && make crosslang
