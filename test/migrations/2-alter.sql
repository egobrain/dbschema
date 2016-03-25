-- UP

alter table test add column ts timestamp default now();

-- DOWN

alter table test drop column ts;
