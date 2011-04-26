all: compile xref eunit

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
#	@./rebar build_plt
	@./rebar check_plt
	@./rebar analyze