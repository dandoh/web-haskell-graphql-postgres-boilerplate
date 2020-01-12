-- migrate:up
create table users
(
    id            serial,
    email         varchar not null unique,
    password_hash varchar not null,
    name          varchar not null,
    created_at   timestamptz not null default now(),
    updated_at    timestamptz not null default now()
);

create unique index users_id on users (id);
create unique index users_email on users (email);

CREATE TRIGGER users_updated_at
    BEFORE UPDATE ON users
    FOR EACH ROW
EXECUTE PROCEDURE update_timestamp();

-- migrate:down
drop table users;

