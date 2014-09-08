%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_hooks_util).

-export([register/0, register/1, register/2
         ,registered/0, registered/1, registered/2
         ,all_registered/0
        ]).
-export([register_rr/0, register_rr/1, register_rr/2
         ,registered_rr/0, registered_rr/1, registered_rr/2
         ,all_registered_rr/0
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
    maybe_add_hook(?HOOK_REG).
register(AccountId) ->
    maybe_add_hook(?HOOK_REG(AccountId)).
register(AccountId, EventName) ->
    maybe_add_hook(?HOOK_REG(AccountId, EventName)).

-spec registered() -> pids().
-spec registered(ne_binary()) -> pids().
-spec registered(ne_binary(), ne_binary()) -> pids().
registered() ->
    gproc:lookup_pids(?HOOK_REG).
registered(AccountId) ->
    gproc:lookup_pids(?HOOK_REG(AccountId)).
registered(AccountId, EventName) ->
    gproc:lookup_pids(?HOOK_REG(AccountId, EventName)).

-spec all_registered() -> pids().
all_registered() ->
    lists:usort(
      lists:foldl(fun all_registered_fold/2
                  ,[]
                  ,[?HOOK_REG
                    ,?HOOK_REG('_')
                    ,?HOOK_REG('_', '_')
                   ])
     ).

all_registered_fold(HookPattern, Acc) ->
    Match = [{{HookPattern, '$1', '_'}
              ,[]
              ,['$1']
             }],
    gproc:select(Match) ++ Acc.

-spec register_rr() -> 'true'.
-spec register_rr(ne_binary()) -> 'true'.
-spec register_rr(ne_binary(), ne_binary()) -> 'true'.
register_rr() ->
    maybe_add_hook(?HOOK_REG_RR).
register_rr(AccountId) ->
    maybe_add_hook(?HOOK_REG_RR(AccountId)).
register_rr(AccountId, EventName) ->
    maybe_add_hook(?HOOK_REG_RR(AccountId, EventName)).

-spec registered_rr() -> pids().
-spec registered_rr(ne_binary()) -> pids().
-spec registered_rr(ne_binary(), ne_binary()) -> pids().
registered_rr() ->
    gproc:lookup_pids(?HOOK_REG_RR).
registered_rr(AccountId) ->
    gproc:lookup_pids(?HOOK_REG_RR(AccountId)).
registered_rr(AccountId, EventName) ->
    gproc:lookup_pids(?HOOK_REG_RR(AccountId, EventName)).

-spec all_registered_rr() -> pids().
all_registered_rr() ->
    lists:usort(
      lists:foldl(fun all_registered_fold/2
                  ,[]
                  ,[?HOOK_REG_RR
                    ,?HOOK_REG_RR('_')
                    ,?HOOK_REG_RR('_', '_')
                   ])
     ).

-spec maybe_add_hook(tuple()) -> 'true'.
-spec maybe_add_hook(tuple(), pids()) -> 'true'.
maybe_add_hook(Hook) ->
    case gproc:lookup_pids(Hook) of
        [] -> hook_it_up(Hook);
        Pids -> maybe_add_hook(Hook, Pids)
    end.

maybe_add_hook(Hook, Pids) ->
    case lists:member(self(), Pids) of
        'true' -> 'true';
        'false' -> hook_it_up(Hook)
    end.

-spec hook_it_up(tuple()) -> 'true'.
hook_it_up(Hook) ->
    maybe_add_binding(Hook),
    'true' = gproc:reg(Hook).

-spec maybe_add_binding(tuple()) -> 'ok'.
maybe_add_binding(?HOOK_REG) ->
    maybe_add_binding_to_listener('wh_hooks_listener');
maybe_add_binding(?HOOK_REG(_AccountId)) ->
    maybe_add_binding_to_listener('wh_hooks_listener');
maybe_add_binding(?HOOK_REG(_AccountId, EventName)) ->
    maybe_add_binding_to_listener('wh_hooks_listener', EventName);
maybe_add_binding(?HOOK_REG_RR) ->
    maybe_add_binding_to_listener('wh_hooks_shared_listener');
maybe_add_binding(?HOOK_REG_RR(_AccountId)) ->
    maybe_add_binding_to_listener('wh_hooks_shared_listener');
maybe_add_binding(?HOOK_REG_RR(_AccountId, EventName)) ->
    maybe_add_binding_to_listener('wh_hooks_shared_listener', EventName).

-spec maybe_add_binding_to_listener(atom()) -> 'ok'.
-spec maybe_add_binding_to_listener(atom(), ne_binary() | 'all') -> 'ok'.
maybe_add_binding_to_listener(ServerName) ->
    maybe_add_binding_to_listener(ServerName, 'all').
maybe_add_binding_to_listener(ServerName, EventName) ->
    gen_listener:cast(ServerName, {'maybe_add_binding', EventName}).

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
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        'undefined' ->
            Number = get_inbound_destination(JObj),
            case wh_cache:peek_local(?HOOKS_CACHE_NAME, cache_key_number(Number)) of
                {'ok', AccountId} -> {'ok', AccountId};
                {'error', 'not_found'} -> fetch_account_id(Number)
            end;
        Id -> {'ok', Id}
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
