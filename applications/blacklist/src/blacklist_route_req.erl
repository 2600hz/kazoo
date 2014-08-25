%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(blacklist_route_req).

-export([handle_req/2]).


-define(ACCOUNT_CACHE_KEY(AccountId, CallerId), {AccountId, CallerId}).

-include("blacklist.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put('callid', CallId),
    Call = whapps_call:from_route_req(JObj),
    case get_action(Call) of
        {'error', _M} ->
            lager:debug("blacklist does not know what to do with this: ~p", [_M]);
        {'ok', Action} -> excute_action(Action, JObj, Call, Props)
    end.

get_action(Call) ->
    CallerId = whapps_call:caller_id_number(Call),
    case whapps_call:custom_channel_var(<<"Account-IDs">>, Call) of
        'undefined' -> bl_utils:get_global_action(CallerId);
        AccountId -> bl_utils:get_account_action(CallerId, AccountId)
    end.

excute_action(<<"hangup">>, JObj, _Call, Props) ->
    ControllerQ = props:get_value('queue', Props),
    send_route_response(ControllerQ, JObj, <<"park">>);
excute_action(<<"error">>, JObj, _Call, Props) ->
    ControllerQ = props:get_value('queue', Props),
    send_route_response(ControllerQ, JObj, <<"error">>);
excute_action(Action, _, _, _) ->
    lager:error("unknown action ~p", [Action]).


send_route_response(ControllerQ, JObj, Method) ->
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, Method}
                                   | wh_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("blacklist knows how to route the call! sent ~s response", [Method]).

