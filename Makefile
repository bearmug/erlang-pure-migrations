REBAR = ./rebar3
DOCKER = docker
CONTAINER_POSTGRES = postgres-migration-test-container
CONTAINER_MYSQL = mysql-migration-test-container

all: clean code-checks test cover

travis: all coveralls

local: format db-bounce all db-down

clean:
	$(REBAR) clean
	rm -rf ./_build/default/lib/pure_migrations
	rm -rf ./_build/test/lib/pure_migrations

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
	$(DOCKER) run --name $(CONTAINER_POSTGRES) \
	-p 5432:5432 \
	-e POSTGRES_PASSWORD=migration \
	-e POSTGRES_USER=migration \
	-e POSTGRES_DB=migration \
	-d postgres:9.6-alpine

postgres-down:
	-$(DOCKER) rm -f $(CONTAINER_POSTGRES)

mysql-up:
	$(DOCKER) run --name $(CONTAINER_MYSQL) \
	-p 3306:3306 \
	-e MYSQL_ROOT_PASSWORD=puremigration \
	-e MYSQL_USER=puremigration \
	-e MYSQL_PASSWORD=puremigration \
	-e MYSQL_DATABASE=puremigration \
	-d mysql:5.7

mysql-down:
	-$(DOCKER) rm -f $(CONTAINER_MYSQL)

db-bounce: postgres-down mysql-down postgres-up mysql-up

db-down: postgres-down mysql-down
