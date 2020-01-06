-- migrate:up
create table users
(
    id            serial,
    email         varchar not null unique,
    password_hash varchar not null,
    name          varchar not null
);

create unique index users_id on users (id);

-- migrate:down
drop table users;

