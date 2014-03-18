-module(bh_call).

-export([handle_event/2]).

-include("../blackhole.hrl").

-spec handle_event(bh_context:context(), wh_json:object()) -> any().
handle_event(Context, EventJObj) ->
    'true' = wapi_call:event_v(EventJObj) andalso is_account_event(Context, EventJObj),
    blackhole_data_emitter:emit(bh_context:session_pid(Context), event_name(EventJObj), EventJObj).

is_account_event(Context, EventJObj) ->
    wh_json:get_first_defined([<<"Account-ID">>
                               ,[<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                              ], EventJObj
                             ) =:=
        bh_context:account_id(Context).

event_name(JObj) ->
    wh_json:get_value(<<"Event-Name">>, JObj).
