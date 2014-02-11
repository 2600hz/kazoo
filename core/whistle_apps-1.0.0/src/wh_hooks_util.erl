%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_hooks_util).

-export([register/0, register/1, register/2
         ,register_rr/0, register_rr/1, register_rr/2
         ,handle_call_event/2
        ]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-define(HOOK_REG
        ,{'p', 'l', 'wh_hook'}).
-define(HOOK_REG(AccountId)
        ,{'p', 'l', {'wh_hook', AccountId}}).
-define(HOOK_REG(AccountId, EventName)
        ,{'p', 'l', {'wh_hook', AccountId, EventName}}).

-define(HOOK_REG_RR
        ,{'p', 'l', 'wh_hook_rr'}).
-define(HOOK_REG_RR(AccountId)
        ,{'p', 'l', {'wh_hook_rr', AccountId}}).
-define(HOOK_REG_RR(AccountId, EventName)
        ,{'p', 'l', {'wh_hook_rr', AccountId, EventName}}).

-spec register() -> 'true'.
-spec register(ne_binary()) -> 'true'.
-spec register(ne_binary(), ne_binary()) -> 'true'.
register() ->
    'true' = gproc:reg(?HOOK_REG).
register(AccountId) ->
    'true' = gproc:reg(?HOOK_REG(AccountId)).
register(AccountId, EventName) ->
    'true' = gproc:reg(?HOOK_REG(AccountId, EventName)).

-spec register_rr() -> 'true'.
-spec register_rr(ne_binary()) -> 'true'.
-spec register_rr(ne_binary(), ne_binary()) -> 'true'.
register_rr() ->
    'true' = gproc:reg(?HOOK_REG_RR).
register_rr(AccountId) ->
    'true' = gproc:reg(?HOOK_REG_RR(AccountId)).
register_rr(AccountId, EventName) ->
    'true' = gproc:reg(?HOOK_REG_RR(AccountId, EventName)).

-spec handle_call_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    'true' = wapi_call:event_v(JObj) orelse wapi_route:req_v(JObj),
    HookEvent = wh_json:get_value(<<"Event-Name">>, JObj),
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>
                                   ,<<"Account-ID">>
                                  ], JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    wh_util:put_callid(CallId),
    handle_call_event(JObj, AccountId, HookEvent, CallId, props:get_is_true('rr', Props)).

-spec handle_call_event(wh_json:object(), api_binary(), ne_binary(), ne_binary(), boolean()) ->
                               'ok'.
handle_call_event(JObj, 'undefined', <<"CHANNEL_CREATE">>, CallId, RR) ->
    lager:debug("event 'channel_create' had no account id, caching"),
    maybe_cache_call_event(JObj, CallId, RR);
handle_call_event(_JObj, 'undefined', _HookEvent, _CallId, _RR) ->
    lager:debug("event '~s' had no account id, ignoring", [_HookEvent]);
handle_call_event(_JObj, AccountId, <<"route_req">>, CallId, _RR) ->
    lager:debug("recv route_req with account id ~s, looking for events to relay"
                ,[AccountId]
               ),
    maybe_relay_new_event(AccountId, CallId);
handle_call_event(JObj, AccountId, HookEvent, _CallId, 'false') ->
    Evt = ?HOOK_EVT(AccountId, HookEvent, JObj),
    gproc:send(?HOOK_REG, Evt),
    gproc:send(?HOOK_REG(AccountId), Evt),
    gproc:send(?HOOK_REG(AccountId, HookEvent), Evt);
handle_call_event(JObj, AccountId, HookEvent, _CallId, 'true') ->
    Evt = ?HOOK_EVT(AccountId, HookEvent, JObj),
    gproc:send(?HOOK_REG_RR, Evt),
    gproc:send(?HOOK_REG_RR(AccountId), Evt),
    gproc:send(?HOOK_REG_RR(AccountId, HookEvent), Evt).

-spec maybe_cache_call_event(wh_json:object(), ne_binary(), boolean()) -> 'ok'.
maybe_cache_call_event(JObj, CallId, RR) ->
    case wh_cache:peek_local(?HOOKS_CACHE_NAME, CallId) of
        {'error', 'not_found'} ->
            wh_cache:store_local(?HOOKS_CACHE_NAME, CallId, {'CHANNEL_CREATE', JObj, RR}, [{'expires', 5000}]);
        {'ok', {'account_id', AccountId}} when RR ->
            %% relay channel create to round_robin and non-round_robin registrants
            EvtName = wh_json:get_value(<<"Event-Name">>, JObj),
            handle_call_event(JObj, AccountId, EvtName, CallId, 'true'),
            handle_call_event(JObj, AccountId, EvtName, CallId, 'false');
        {'ok', {'account_id', AccountId}} ->
            %% only relay channel create to non-round_robin registrants
            EvtName = wh_json:get_value(<<"Event-Name">>, JObj),
            handle_call_event(JObj, AccountId, EvtName, CallId, 'false')
    end.

-spec maybe_relay_new_event(ne_binary(), ne_binary()) -> 'ok'.
maybe_relay_new_event(AccountId, CallId) ->
    case wh_cache:peek_local(?HOOKS_CACHE_NAME, CallId) of
        {'error', 'not_found'} ->
            wh_cache:store_local(?HOOKS_CACHE_NAME, CallId, {'account_id', AccountId}, [{'expires', 5000}]);
        {'ok', {'CHANNEL_CREATE', JObj, 'true'}} ->
            EvtName = wh_json:get_value(<<"Event-Name">>, JObj),
            handle_call_event(JObj, AccountId, EvtName, CallId, 'true'),
            handle_call_event(JObj, AccountId, EvtName, CallId, 'false');
        {'ok', {'CHANNEL_CREATE', JObj, 'false'}} ->
            EvtName = wh_json:get_value(<<"Event-Name">>, JObj),
            handle_call_event(JObj, AccountId, EvtName, CallId, 'false')
    end.
