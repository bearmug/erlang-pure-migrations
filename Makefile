REBAR = ./rebar3

all: clean code-checks test

clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

test: compile
	$(REBAR) do ct -v

code-checks: compile
	$(REBAR) dialyzer
	$(REBAR) as lint lint
