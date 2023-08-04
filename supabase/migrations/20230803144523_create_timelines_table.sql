create table
timelines (
id bigint primary key generated always as identity,
name text,
period text,
tags text[],
created_at timestamptz default now()
);