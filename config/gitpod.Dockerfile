FROM gitpod/workspace-full

USER root
# ========================================================================
# erlang setup
# ========================================================================
RUN sh -c 'echo "deb https://packages.erlang-solutions.com/ubuntu xenial  contrib" >> /etc/apt/sources.list.d/erlang.list' && \
    wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc && \
    apt-key add erlang_solutions.asc && \
    apt-get update && \
    apt-get install -y erlang emacs

# ========================================================================
# postgresql setup with unsigned repo
# ========================================================================
RUN add-apt-repository "deb http://apt.postgresql.org/pub/repos/apt/ $(lsb_release -sc)-pgdg main"; exit 0
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add - && \
    apt-get update --allow-unauthenticated && \
    apt-get install -y postgresql-9.6 postgresql-contrib

USER postgres
RUN /etc/init.d/postgresql start && \
    psql --command "create database puremigration;" && \
    psql --command "create user puremigration with encrypted password 'puremigration';" && \
    psql --command "grant all privileges on database puremigration to puremigration;"

user gitpod
