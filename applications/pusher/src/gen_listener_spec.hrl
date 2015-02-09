-ifndef(gen_listener_spec_hrl).
-define(gen_listener_spec_hrl, included).

-include("gen_server_spec.hrl").

-type gen_listener_handle_event_result() ::
        {reply, wh_proplist()}
    |   ignore.
-spec handle_event(wh_json:object(), gen_srv_state()) -> gen_listener_handle_event_result().


-endif.
