%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_channel_util).

-export([handle_event/2]).
-export([maybe_handle_channel_event/3]).

-ifdef(TEST).
-export([is_fireable_hook/2]).
-endif.

-include("webhooks.hrl").

-spec handle_event(kz_call_event:doc(), kz_term:proplist()) -> 'ok'.
handle_event(CallEvent, _Props) ->
    HookEvent = hook_event_name(kz_api:event_name(CallEvent)),
    case kz_hooks_util:lookup_account_id(CallEvent) of
        {'error', _R} ->
            lager:debug("failed to determine account id for ~s", [HookEvent]);
        {'ok', AccountId} ->
            CallEvent1 = kz_json:set_value([<<"Custom-Channel-Vars">>
                                           ,<<"Account-ID">>
                                           ]
                                          ,AccountId
                                          ,CallEvent
                                          ),
            maybe_handle_channel_event(AccountId, HookEvent, CallEvent1)
    end.

-spec maybe_handle_channel_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_call_event:doc()) -> 'ok'.
maybe_handle_channel_event(AccountId, HookEvent, CallEvent) ->
    lager:debug("evt ~s for ~s", [HookEvent, AccountId]),
    case webhooks_util:find_webhooks(HookEvent, AccountId) of
        [] -> lager:debug("no hooks to handle ~s for ~s", [HookEvent, AccountId]);
        Hooks ->
            maybe_fire_event(AccountId, HookEvent, CallEvent, Hooks)
    end.

-spec maybe_fire_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_call_event:doc(), webhooks()) -> 'ok'.
maybe_fire_event(AccountId, HookEvent, CallEvent, Hooks) ->
    FireAbleHooks = fireable_hooks(CallEvent, Hooks),
    webhooks_util:fire_hooks(format_event(CallEvent, AccountId, HookEvent), FireAbleHooks).

-spec fireable_hooks(kz_call_event:doc(), webhooks()) -> webhooks().
fireable_hooks(CallEvent, Hooks) ->
    [Hook || #webhook{}=Hook <- Hooks,
             is_fireable_hook(CallEvent, Hook)
    ].

-spec is_fireable_hook(kz_call_event:doc(), webhook()) -> boolean().
is_fireable_hook(_CallEvent, #webhook{include_loopback='true'}) -> 'true';
is_fireable_hook(CallEvent, #webhook{include_loopback='false'
                                    ,id=_Id
                                    }) ->
    case is_loopback_channel_name(kz_call_event:channel_name(CallEvent))
        orelse kz_json:is_true(<<"Channel-Is-Loopback">>, CallEvent, 'false')
    of
        'false' -> 'true';
        'true' ->
            lager:debug("channel is loopback, filtering hook ~s", [_Id]),
            'false'
    end.

-spec is_loopback_channel_name(kz_term:api_ne_binary()) -> boolean().
is_loopback_channel_name(<<"loopback/", _/binary>>=_N) -> 'true';
is_loopback_channel_name(_N) -> 'false'.

-spec hook_event_name(kz_term:ne_binary()) -> kz_term:ne_binary().
hook_event_name(<<"CHANNEL_DISCONNECTED">>) -> <<"CHANNEL_DESTROY">>;
hook_event_name(Event) -> Event.

-spec format_event(kz_call_event:doc(), kz_term:api_binary(), kz_term:ne_binary()) ->
          kz_json:object().
format_event(CallEvent, AccountId, <<"CHANNEL_CREATE">>) ->
    kz_json:set_value(<<"hook_event">>
                     ,<<"channel_create">>
                     ,base_hook_event(CallEvent, AccountId)
                     );
format_event(CallEvent, AccountId, <<"CHANNEL_ANSWER">>) ->
    kz_json:set_value(<<"hook_event">>
                     ,<<"channel_answer">>
                     ,base_hook_event(CallEvent, AccountId)
                     );
format_event(CallEvent, AccountId, <<"CHANNEL_BRIDGE">>) ->
    base_hook_event(CallEvent
                   ,AccountId
                   ,[{<<"hook_event">>, <<"channel_bridge">>}
                    ,{<<"original_number">>, ccv(CallEvent, <<"Original-Number">>)}
                    ,{<<"other_leg_destination_number">>, kz_call_event:other_leg_destination_number(CallEvent)}
                    ]
                   );
format_event(CallEvent, AccountId, <<"CHANNEL_DESTROY">>) ->
    base_hook_event(CallEvent
                   ,AccountId
                   ,[{<<"hook_event">>, <<"channel_destroy">>}
                    ,{<<"hangup_cause">>, kz_call_event:hangup_cause(CallEvent)}
                    ,{<<"hangup_code">>, kz_call_event:hangup_code(CallEvent)}
                    ,{<<"duration_seconds">>, kz_call_event:duration_seconds(CallEvent)}
                    ,{<<"ringing_seconds">>, kz_call_event:ringing_seconds(CallEvent)}
                    ,{<<"billing_seconds">>, kz_call_event:billing_seconds(CallEvent)}
                    ]
                   ).

-spec base_hook_event(kz_call_event:doc(), kz_term:api_binary()) -> kz_json:object().
base_hook_event(CallEvent, AccountId) ->
    base_hook_event(CallEvent, AccountId, []).

-spec base_hook_event(kz_call_event:doc(), kz_term:api_binary(), kz_term:proplist()) -> kz_json:object().
base_hook_event(CallEvent, AccountId, Acc) ->
    WasGlobal = kz_term:is_true(ccv(CallEvent, <<"Global-Resource">>)),

    kz_json:from_list(
      [{<<"account_id">>, ccv(CallEvent, <<"Account-ID">>, AccountId)}
      ,{<<"authorizing_id">>, kz_call_event:authorizing_id(CallEvent)}
      ,{<<"authorizing_type">>, kz_call_event:authorizing_type(CallEvent)}
      ,{<<"call_direction">>, kz_call_event:call_direction(CallEvent)}
      ,{<<"call_forwarded">>, kz_call_event:is_call_forwarded(CallEvent)}
      ,{<<"call_id">>, kz_call_event:call_id(CallEvent)}
      ,{<<"callee_id_name">>, kz_call_event:callee_id_name(CallEvent)}
      ,{<<"callee_id_number">>, kz_call_event:callee_id_number(CallEvent)}
      ,{<<"caller_id_name">>, kz_call_event:caller_id_name(CallEvent)}
      ,{<<"caller_id_number">>, kz_call_event:caller_id_number(CallEvent)}
      ,{<<"custom_channel_vars">>, non_reserved_ccvs(CallEvent)}
      ,{<<"custom_application_vars">>, cavs(CallEvent)}
      ,{<<"emergency_resource_used">>, kz_term:is_true(ccv(CallEvent, <<"Emergency-Resource">>))}
      ,{<<"from">>, kz_json:get_value(<<"From">>, CallEvent)}
      ,{<<"inception">>, kz_json:get_value(<<"Inception">>, CallEvent)}
      ,{<<"local_resource_id">>, resource_used(WasGlobal, CallEvent)}
      ,{<<"local_resource_used">>, (not WasGlobal)}
      ,{<<"is_internal_leg">>, kz_json:is_true(<<"Channel-Is-Loopback">>, CallEvent)}
      ,{<<"other_leg_call_id">>, kz_call_event:other_leg_call_id(CallEvent)}
      ,{<<"owner_id">>, kz_call_event:owner_id(CallEvent)}
      ,{<<"request">>, kz_call_event:request(CallEvent)}
      ,{<<"reseller_id">>, kz_services_reseller:get_id(AccountId)}
      ,{<<"timestamp">>, kz_call_event:timestamp(CallEvent)}
      ,{<<"to">>, kz_json:get_value(<<"To">>, CallEvent)}
       | Acc
      ]).

-spec resource_used(boolean(), kz_call_event:doc()) -> kz_term:api_binary().
resource_used('true', _CallEvent) -> 'undefined';
resource_used('false', CallEvent) -> ccv(CallEvent, <<"Resource-ID">>).

-spec ccv(kz_call_event:doc(), kz_json:key()) ->
          kz_term:api_ne_binary().
ccv(CallEvent, Key) ->
    ccv(CallEvent, Key, 'undefined').

-spec ccv(kz_call_event:doc(), kz_json:key(), Default) ->
          kz_term:ne_binary() | Default.
ccv(CallEvent, Key, Default) ->
    kz_call_event:custom_channel_var(CallEvent, Key, Default).

-spec non_reserved_ccvs(kz_call_event:doc()) -> kz_term:api_object().
non_reserved_ccvs(CallEvent) ->
    CCVs = kz_call_event:custom_channel_vars(CallEvent, kz_json:new()),
    non_reserved_ccvs(CCVs, kapps_config:get_ne_binaries(<<"call_command">>, <<"reserved_ccv_keys">>)).

-spec non_reserved_ccvs(kz_json:object(), kz_term:api_ne_binaries()) -> kz_term:api_object().
non_reserved_ccvs(_CCVs, 'undefined') -> 'undefined';
non_reserved_ccvs(CCVs, Keys) ->
    kz_json:filter(fun({K, _}) -> not lists:member(K, Keys) end, CCVs).

-spec cavs(kz_call_event:doc()) -> kz_term:api_object().
cavs(CallEvent) -> kz_call_event:custom_application_vars(CallEvent).
