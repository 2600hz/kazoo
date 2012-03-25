-ifndef(LINEMAN_INCLUDED).

-define(DEFAULT_LOG_DIR, wh_util:to_binary(code:lib_dir(lineman, log))).
-define(WHAPPS_VM_ARGS, "/opt/whistle/whistle/whistle_apps/conf/vm.args").
-define(ECALL_VM_ARGS, "/opt/whistle/whistle/ecallmgr/conf/vm.args").

-define(NE_BINARY, <<_:8,_/binary>>).
-type ne_binary() :: <<_:8,_:_*8>>.

-type proplist_bool() :: [{boolean(), term()} | boolean(),...] | [].
-type proplist_key() :: nonempty_string() | ne_binary() | atom() | number().
-type proplist() :: [{proplist_key(), term()} | atom(),...] | [].

-define(LINEMAN_INCLUDED, true).
-endif.
