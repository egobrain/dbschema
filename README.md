[![Build Status](https://travis-ci.org/egobrain/dbschema.png?branch=master)](https://travis-ci.org/egobrain/dbschema.png?branch=master)
[![Coveralls](https://img.shields.io/coveralls/egobrain/dbschema.svg)](https://coveralls.io/github/egobrain/dbschema)
[![GitHub tag](https://img.shields.io/github/tag/egobrain/dbschema.svg)](https://github.com/egobrain/dbschema)
[![Hex.pm](https://img.shields.io/hexpm/v/dbschema.svg)](https://hex.pm/packages/dbschema)

# Erlang posrgresql migrations utils
----------------------------------------------------

## Description ##

Project is based on epgpool and epgsql.
It supports sql and erl migration description.

## Usage ##

1. Create migrations folder (for example, `./migrations`)  
2. Create migration file with numeric prefix and `.erl` or `.sql` extension.  
The prefix must be unique monotonically increasing integer (1,2,3...).  
No dupplicates allowed!  
  * In case of `erl` you need to implement `dbschema_migration` behaviour.  
  * In case of `sql` you need to write your sql commands directly.  
    And specify `-- up` and `-- down` comments to separate "up" migration part from "down"
3. Run `dbschema:up(MigrationDir)` in you `_app.erl` start function.
4. To downgrade call `dbscheam:down(MigrationDir, PrefixNum)`

## Sql migration example

./migrations/1-init.sql

```sql
-- Up

create table test(
    id serial primary key,
    name text
);

-- Down

drop table test;
```

