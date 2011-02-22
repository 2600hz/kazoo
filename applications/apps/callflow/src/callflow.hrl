-include("../../../utils/src/whistle_types.hrl").

-define ( CALLFLOW_DB, "callflow" ).

-record ( cf_call, {
   call_id = <<>> :: binary(),
   ctrl_q = <<>> :: binary(),
   amqp_q = <<>> :: binary(),
   cf_pid = undefined :: pid() | undefined
} ).
