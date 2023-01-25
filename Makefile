.PHONY: all clean

REBAR=rebar3

all:
	@$(REBAR) do compile, eunit, dialyzer, xref
clean:
	@$(REBAR) clean
