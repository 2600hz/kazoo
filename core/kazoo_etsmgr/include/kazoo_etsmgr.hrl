-ifndef(KAZOO_ETSMGR).

-define(TABLE_READY(Tbl), {'ETS-TRANSFER', Tbl, _EtsMgr, _Data}).
-define(TABLE_READY(Tbl, Data), {'ETS-TRANSFER', Tbl, _EtsMgr, Data}).

-define(KAZOO_ETSMGR, 'true').
-endif.
