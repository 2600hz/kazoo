-ifndef(KAZOO_IPS_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(AVAILABLE, <<"available">>).
-define(ASSIGNED, <<"assigned">>).
-define(PVT_TYPE, <<"dedicated_ip">>).

-define(CONFIG_CAT, <<"ips">>).

-define(KAZOO_IPS_HRL, 'true').
-endif.
