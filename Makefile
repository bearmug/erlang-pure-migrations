REBAR = ./rebar3

all: clean code-checks test

clean:
	$(REBAR) clean

compile:
	$(REBAR) compile

cover:
	$(REBAR) as test cover --min_coverage=100 -v
	$(REBAR) as test coveralls send

test: compile
	$(REBAR) as test do ct -v

code-checks: compile
	$(REBAR) dialyzer
	$(REBAR) as lint lint
