%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_hooks_util).

-export([register/0
         ,register/1
         ,register/2
        ]).
-export([register_rr/0
         ,register_rr/1
         ,register_rr/2
        ]).
-export([lookup_account_id/1]).
-export([handle_call_event/2]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
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
    'true' = wapi_call:event_v(JObj),
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
    lager:debug("event 'channel_create' had no account id"),
    case lookup_account_id(JObj) of
        {'error', _R} -> 
            lager:debug("failed to determine account id for 'channel_create'", []);
        {'ok', AccountId} ->
            lager:debug("determined account id for 'channel_create' is ~s", [AccountId]),
            J = wh_json:set_value([<<"Custom-Channel-Vars">>
                                   ,<<"Account-ID">>
                                  ], AccountId, JObj),
            handle_call_event(J, AccountId, <<"CHANNEL_CREATE">>, CallId, RR)
    end;
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

-spec lookup_account_id(wh_json:object()) -> {'ok', ne_binary()} | {'error', _}.
lookup_account_id(JObj) ->
    Number = get_inbound_destination(JObj),
    case wh_cache:peek_local(?HOOKS_CACHE_NAME, cache_key_number(Number)) of
        {'ok', AccountId} -> {'ok', AccountId};
        {'error', 'not_found'} -> fetch_account_id(Number)
    end.

-spec fetch_account_id(ne_binary()) -> {'ok', ne_binary()} | {'error', _}.
fetch_account_id(Number) ->
    case wh_number_manager:lookup_account_by_number(Number) of
        {'ok', AccountId, _} ->
            CacheProps = [{'origin', {'db', wnm_util:number_to_db_name(Number), Number}}],
            wh_cache:store_local(?HOOKS_CACHE_NAME, cache_key_number(Number), AccountId, CacheProps),
            {'ok', AccountId};
        {'error', _}=Error -> Error
    end.

-spec cache_key_number(ne_binary()) -> {'wh_hooks', ne_binary()}.
cache_key_number(Number) -> {'wh_hooks', Number}.

-spec get_inbound_destination(wh_json:object()) -> ne_binary().
get_inbound_destination(JObj) ->
    {Number, _} = whapps_util:get_destination(JObj, <<"stepswitch">>, <<"inbound_user_field">>),
    case whapps_config:get_is_true(<<"stepswitch">>, <<"assume_inbound_e164">>, 'false') of
        'true' -> assume_e164(Number);
        'false' -> wnm_util:to_e164(Number)
    end.

-spec assume_e164(ne_binary()) -> ne_binary().
assume_e164(<<$+, _/binary>> = Number) -> Number;
assume_e164(Number) -> <<$+, Number/binary>>.
