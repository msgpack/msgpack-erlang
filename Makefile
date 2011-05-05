.PHONY: compile xref eunit clean doc check make

all: compile xref eunit

# for busy typos
m: all
ma: all
mak: all
make: all

compile:
	@./rebar compile

xref:
	@./rebar xref

eunit:
	@./rebar eunit

clean:
	@./rebar clean

doc:
	@./rebar doc

check:
	@./rebar build_plt
	@./rebar check_plt
	@./rebar analyze


crosslang:
	export ERL_LIBS=../
	cd test && make crosslang
