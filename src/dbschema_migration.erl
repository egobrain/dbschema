-module(dbschema_migration).

-callback up(epgpool:connection()) -> ok | {error, Reason :: any()}.
-callback down(epgpool:connection()) -> ok | {error, Reason :: any()}.
