REBAR = ./rebar3
DOCKER = docker
CONTAINER_POSTGRES = postgres-migration-test-container
CONTAINER_MYSQL = mysql-migration-test-container
IMAGE_POSTGRES = postgres:9.6-alpine
IMAGE_MYSQL = mysql:5.7
USERNAME := $(shell whoami)

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
    ifeq ($(USERNAME), gitpod)
		sudo /etc/init.d/postgresql start
    else
		$(DOCKER) run --name $(CONTAINER_POSTGRES) \
		-p 5432:5432 \
		-e POSTGRES_PASSWORD=puremigration \
		-e POSTGRES_USER=puremigration \
		-e POSTGRES_DB=puremigration \
		-d $(IMAGE_POSTGRES)
    endif

postgres-down:
    ifeq ($(USERNAME), gitpod)
		sudo /etc/init.d/postgresql stop
    else
	    -$(DOCKER) rm -f $(CONTAINER_POSTGRES)
    endif

mysql-up:
	$(DOCKER) run --name $(CONTAINER_MYSQL) \
	-p 3306:3306 \
	-e MYSQL_ALLOW_EMPTY_PASSWORD=true \
	-e MYSQL_USER=puremigration \
	-e MYSQL_PASSWORD=puremigration \
	-e MYSQL_DATABASE=puremigration \
	-d $(IMAGE_MYSQL)

mysql-wait:
	while ! docker exec -it mysql-migration-test-container mysqladmin ping --silent; do \
        echo "mysql image starting, wait for 1 second..."; \
        sleep 1; \
    done; done

mysql-down:
	-$(DOCKER) rm -f $(CONTAINER_MYSQL)

db-bounce: postgres-down mysql-down postgres-up mysql-up mysql-wait

db-down: postgres-down mysql-down
