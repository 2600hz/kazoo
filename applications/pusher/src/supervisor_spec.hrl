-ifndef(supervisor_spec_hrl).
-define(supervisor_spec_hrl, included).

-type restart_strategy() :: one_for_all | one_for_one | rest_for_one | simple_one_for_one.

-type supervisor_restart() :: permanent | transient | temporary.
-type supervisor_shutdown() :: brutal_kill | non_neg_integer().
-type supervisor_child_type() :: worker | supervisor.
-type supervisor_child_id() :: any().
-type supervisor_mfargs() :: {M::module(), F::atom(), A::[any()] | undefined}.

-type supervisor_child_spec() :: {
    Id :: supervisor_child_id(),
    StartFunc :: supervisor_mfargs(),
    Restart :: supervisor_restart(),
    Shutdown :: supervisor_shutdown(),
    Type :: supervisor_child_type(),
    Modules :: [atom()]
}.

-type supervisor_restart_policy() :: {restart_strategy(), non_neg_integer(), pos_integer()}.

-spec init(any()) -> {ok, {supervisor_restart_policy(), [supervisor_child_spec()]}} | ignore.

-endif.
