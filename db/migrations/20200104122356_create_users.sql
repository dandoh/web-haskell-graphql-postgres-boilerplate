-- migrate:up
create table users (
    id integer not null,
    email varchar not null,
    password_hash varchar not null
);

-- migrate:down
drop table users;

