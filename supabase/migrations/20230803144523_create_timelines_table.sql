create table
timelines (
id bigint primary key generated always as identity,
name text,
period text,
created_at timestamptz default now()
);