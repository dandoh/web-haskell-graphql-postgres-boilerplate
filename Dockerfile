FROM haskell:8.6.5

### Install PostgreSQL
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ stretch-pgdg main" | tee /etc/apt/sources.list.d/pgdg.list
RUN curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN apt-get update
RUN apt-get -y install postgresql-11
RUN apt-get install libpq-dev

ARG pg_password
ARG pg_user
ARG pg_db
USER postgres
RUN /etc/init.d/postgresql start && psql --command "CREATE USER $pg_user WITH SUPERUSER PASSWORD '$pg_password';" && createdb -O $pg_user $pg_db
USER root

### Install dbmate
RUN curl -fsSL -o /usr/local/bin/dbmate https://github.com/amacneil/dbmate/releases/download/v1.7.0/dbmate-linux-amd64
RUN chmod +x /usr/local/bin/dbmate

WORKDIR /root/

COPY ./entrypoint.sh /opt/entrypoint.sh
RUN chmod +x /opt/entrypoint.sh
ENTRYPOINT ["/opt/entrypoint.sh"]

