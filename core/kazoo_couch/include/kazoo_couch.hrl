-ifndef(KAZOO_COUCH_HRL).

%% Throttle how many docs we bulk insert to BigCouch
-define(COUCH_MAX_BULK_INSERT, 2000).

%% How many docs we can fetch in one request
-define(COUCH_MAX_BULK_READ, 2000).

-define(KAZOO_COUCH_HRL, 'true').
-endif.
