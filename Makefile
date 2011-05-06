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
	@./rebar build-plt
	@./rebar check-plt
	@./rebar analyze

crosslang:
	@echo "do ERL_LIBS=../ before you make crosslang or fail"
	cd test && make crosslang
