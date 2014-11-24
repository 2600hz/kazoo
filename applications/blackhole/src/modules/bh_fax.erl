%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(bh_fax).

-export([handle_event/2
         ,add_amqp_binding/2, rm_amqp_binding/2
        ]).

-include("../blackhole.hrl").

-spec handle_event(bh_context:context(), wh_json:object()) -> any().
handle_event(Context, EventJObj) ->
    wh_util:put_callid(EventJObj),
    lager:debug("handle_event fired for ~s ~s", [bh_context:account_id(Context), bh_context:websocket_session_id(Context)]),
    'true' = wapi_fax:status_v(EventJObj) andalso is_account_event(Context, EventJObj),
    lager:debug("valid event and emitting to ~p: ~s", [bh_context:websocket_pid(Context), event_name(EventJObj)]),
    J = wh_json:normalize_jobj(EventJObj),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context), event_name(EventJObj), J).

-spec is_account_event(bh_context:context(), wh_json:object()) -> any().
is_account_event(Context, EventJObj) ->
    wh_json:get_first_defined([<<"Account-ID">>
                               ,[<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                              ], EventJObj
                             )
        =:= bh_context:account_id(Context).

-spec event_name(wh_json:object()) -> ne_binary().
event_name(_JObj) -> <<"fax.status">>.

-spec add_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
add_amqp_binding(<<"fax.status.", FaxId/binary>>, Context) ->
    blackhole_listener:add_binding('fax', [{'restrict_to', ['status']}
                                           ,{'account_id', bh_context:account_id(Context)}
                                           ,{'fax_id', FaxId}
                                          ]);
add_amqp_binding(_Binding, _Context) ->
    lager:debug("unmatched binding ~s", [_Binding]).

-spec rm_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
rm_amqp_binding(<<"fax.status.", FaxId/binary>>, Context) ->
    blackhole_listener:remove_binding('fax', [{'restrict_to', ['status']}
                                              ,{'account_id', bh_context:account_id(Context)}
                                              ,{'fax_id', FaxId}
                                             ]);
rm_amqp_binding(_Binding, _Context) ->
    lager:debug("unmatched binding ~s", [_Binding]).
