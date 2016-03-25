-- UP

create table test(
   id int primary key,
   name text not null
);

insert into test("id", "name") values (1, 'test1'), (2, 'test2');

-- DOWN

drop table test;
