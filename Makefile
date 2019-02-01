REBAR = ./rebar3

all: clean code-checks test cover

travis: all coveralls

local: format all

clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

code-checks: compile
	$(REBAR) dialyzer
	$(REBAR) as lint lint

test: compile
	$(REBAR) as test do ct -v

cover:
	$(REBAR) as test cover --min_coverage=100 -v

coveralls:
	$(REBAR) as test coveralls send

format:
	$(REBAR) fmt
