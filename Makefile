REBAR = ./rebar3
DOCKER = docker
CONTAINER_NAME = postgres-migration-test-container

all: clean code-checks test cover

travis: all coveralls

local: format postgres-bounce all postgres-down

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

postgres-up:
	$(DOCKER) run --name $(CONTAINER_NAME) \
	-p 5432:5432 \
	-e POSTGRES_PASSWORD=migration \
	-e POSTGRES_USER=migration \
	-e POSTGRES_DB=migration \
	-d postgres:9.6-alpine

postgres-down:
	-$(DOCKER) rm -f $(CONTAINER_NAME)

postgres-bounce: postgres-down postgres-up