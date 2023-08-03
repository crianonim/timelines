create table
eras (
id bigint primary key generated always as identity,
name text,
erastart text,
eraend text,
created_at timestamptz default now()
);