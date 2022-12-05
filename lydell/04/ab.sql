create schema aoc;
create table aoc.day04 (a int4range not null, b int4range not null);

-- Run this (needs ripgrep installed):
-- rg '(\d+)-(\d+),(\d+)-(\d+)' input.txt -r '[$1,$2];[$3,$4]' > input.csv
-- Then import input.csv into `aoc.day04` as semicolon separated CSV.

-- Part 1:
select count(*) from aoc.day04 where a @> b or a <@ b;

-- Part 2:
select count(*) from aoc.day04 where a && b;
