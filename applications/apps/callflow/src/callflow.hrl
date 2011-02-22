-include("../../../utils/src/whistle_types.hrl").

-define ( CALLFLOW_DB, "callflow" ).

-record ( cf_call, {
   module = "" : string(),
   data = [] : proplist(),
   call_id = <<>> : binary(),
   ctrl_q = <<>> : binary(),
   amqp_q = <<>> : binary()
} ).
