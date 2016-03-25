-module(dbschema_migration).

-callback perform(epgpool:connection()) -> ok | {error, Reason :: any()}.
