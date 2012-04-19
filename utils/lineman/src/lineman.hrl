-ifndef(LINEMAN_INCLUDED).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("whistle/include/wh_types.hrl").

-define(DEFAULT_LOG_DIR, wh_util:to_binary(code:lib_dir(lineman, log))).
-define(WHAPPS_VM_ARGS, "/opt/whistle/whistle/whistle_apps/conf/vm.args").
-define(ECALL_VM_ARGS, "/opt/whistle/whistle/ecallmgr/conf/vm.args").

-type text() :: atom() | string() | binary().
-type xml() :: term().

-define(LINEMAN_INCLUDED, true).
-endif.
