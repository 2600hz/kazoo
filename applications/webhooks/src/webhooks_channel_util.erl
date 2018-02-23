%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
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

-spec handle_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_event(JObj, _Props) ->
    HookEvent = hook_event_name(kz_json:get_value(<<"Event-Name">>, JObj)),
    case kz_hooks_util:lookup_account_id(JObj) of
        {'error', _R} ->
            lager:debug("failed to determine account id for ~s", [HookEvent]);
        {'ok', AccountId} ->
            J = kz_json:set_value([<<"Custom-Channel-Vars">>
                                  ,<<"Account-ID">>
                                  ], AccountId, JObj),
            maybe_handle_channel_event(AccountId, HookEvent, J)
    end.

-spec maybe_handle_channel_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_handle_channel_event(AccountId, HookEvent, JObj) ->
    lager:debug("evt ~s for ~s", [HookEvent, AccountId]),
    case webhooks_util:find_webhooks(HookEvent, AccountId) of
        [] -> lager:debug("no hooks to handle ~s for ~s", [HookEvent, AccountId]);
        Hooks ->
            maybe_fire_event(AccountId, HookEvent, JObj, Hooks)
    end.

-spec maybe_fire_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), webhooks()) -> 'ok'.
maybe_fire_event(AccountId, HookEvent, JObj, Hooks) ->
    FireAbleHooks = fireable_hooks(JObj, Hooks),
    webhooks_util:fire_hooks(format_event(JObj, AccountId, HookEvent), FireAbleHooks).

-spec fireable_hooks(kz_json:object(), webhooks()) -> webhooks().
fireable_hooks(JObj, Hooks) ->
    [Hook || #webhook{}=Hook <- Hooks,
             is_fireable_hook(JObj, Hook)
    ].

-spec is_fireable_hook(kz_json:object(), webhook()) -> boolean().
is_fireable_hook(_JObj, #webhook{include_loopback='true'}) -> 'true';
is_fireable_hook(JObj, #webhook{include_loopback='false'
                               ,id=_Id
                               }) ->
    case is_loopback_channel_name(kz_call_event:channel_name(JObj))
        orelse kz_json:is_true(<<"Channel-Is-Loopback">>, JObj, 'false')
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

-spec format_event(kz_json:object(), kz_term:api_binary(), kz_term:ne_binary()) ->
                          kz_json:object().
format_event(JObj, AccountId, <<"CHANNEL_CREATE">>) ->
    kz_json:set_value(<<"hook_event">>, <<"channel_create">>
                     ,base_hook_event(JObj, AccountId)
                     );
format_event(JObj, AccountId, <<"CHANNEL_ANSWER">>) ->
    kz_json:set_value(<<"hook_event">>, <<"channel_answer">>
                     ,base_hook_event(JObj, AccountId)
                     );
format_event(JObj, AccountId, <<"CHANNEL_BRIDGE">>) ->
    base_hook_event(JObj
                   ,AccountId
                   ,[{<<"hook_event">>, <<"channel_bridge">>}
                    ,{<<"original_number">>, ccv(JObj, <<"Original-Number">>)}
                    ,{<<"other_leg_destination_number">>, kz_call_event:other_leg_destination_number(JObj)}
                    ]
                   );
format_event(JObj, AccountId, <<"CHANNEL_DESTROY">>) ->
    base_hook_event(JObj
                   ,AccountId
                   ,[{<<"hook_event">>, <<"channel_destroy">>}
                    ,{<<"hangup_cause">>, kz_call_event:hangup_cause(JObj)}
                    ,{<<"hangup_code">>, kz_call_event:hangup_code(JObj)}
                    ,{<<"duration_seconds">>, kz_call_event:duration_seconds(JObj)}
                    ,{<<"ringing_seconds">>, kz_call_event:ringing_seconds(JObj)}
                    ,{<<"billing_seconds">>, kz_call_event:billing_seconds(JObj)}
                    ]
                   ).

-spec base_hook_event(kz_json:object(), kz_term:api_binary()) -> kz_json:object().
base_hook_event(JObj, AccountId) ->
    base_hook_event(JObj, AccountId, []).

-spec base_hook_event(kz_json:object(), kz_term:api_binary(), kz_term:proplist()) -> kz_json:object().
base_hook_event(JObj, AccountId, Acc) ->
    WasGlobal = kz_term:is_true(ccv(JObj, <<"Global-Resource">>)),

    kz_json:from_list(
      [{<<"account_id">>, ccv(JObj, <<"Account-ID">>, AccountId)}
      ,{<<"authorizing_id">>, kz_call_event:authorizing_id(JObj)}
      ,{<<"authorizing_type">>, kz_call_event:authorizing_type(JObj)}
      ,{<<"call_direction">>, kz_json:get_value(<<"Call-Direction">>, JObj)}
      ,{<<"call_forwarded">>, kz_call_event:is_call_forwarded(JObj)}
      ,{<<"call_id">>, kz_call_event:call_id(JObj)}
      ,{<<"callee_id_name">>, kz_json:get_value(<<"Callee-ID-Name">>, JObj)}
      ,{<<"callee_id_number">>, kz_json:get_value(<<"Callee-ID-Number">>, JObj)}
      ,{<<"caller_id_name">>, kz_json:get_value(<<"Caller-ID-Name">>, JObj)}
      ,{<<"caller_id_number">>, kz_json:get_value(<<"Caller-ID-Number">>, JObj)}
      ,{<<"custom_channel_vars">>, non_reserved_ccvs(JObj)}
      ,{<<"custom_application_vars">>, cavs(JObj)}
      ,{<<"emergency_resource_used">>, kz_term:is_true(ccv(JObj, <<"Emergency-Resource">>))}
      ,{<<"from">>, kz_json:get_value(<<"From">>, JObj)}
      ,{<<"inception">>, kz_json:get_value(<<"Inception">>, JObj)}
      ,{<<"local_resource_id">>, resource_used(WasGlobal, JObj)}
      ,{<<"local_resource_used">>, (not WasGlobal)}
      ,{<<"is_internal_leg">>, kz_json:is_true(<<"Channel-Is-Loopback">>, JObj)}
      ,{<<"other_leg_call_id">>, kz_call_event:other_leg_call_id(JObj)}
      ,{<<"owner_id">>, kz_call_event:owner_id(JObj)}
      ,{<<"request">>, kz_json:get_value(<<"Request">>, JObj)}
      ,{<<"reseller_id">>, kz_services:find_reseller_id(AccountId)}
      ,{<<"timestamp">>, kz_call_event:timestamp(JObj)}
      ,{<<"to">>, kz_json:get_value(<<"To">>, JObj)}
       | Acc
      ]).

-spec resource_used(boolean(), kz_json:object()) -> kz_term:api_binary().
resource_used('true', _JObj) -> 'undefined';
resource_used('false', JObj) -> ccv(JObj, <<"Resource-ID">>).

-spec ccv(kz_json:object(), kz_json:path()) ->
                 kz_term:api_binary().
ccv(JObj, Key) ->
    ccv(JObj, Key, 'undefined').

-spec ccv(kz_json:object(), kz_json:path(), Default) ->
                 kz_term:ne_binary() | Default.
ccv(JObj, Key, Default) ->
    kz_call_event:custom_channel_var(JObj, Key, Default).

-spec non_reserved_ccvs(kz_call_event:doc()) -> kz_term:api_object().
non_reserved_ccvs(JObj) ->
    CCVs = kz_call_event:custom_channel_vars(JObj, kz_json:new()),
    non_reserved_ccvs(CCVs, kapps_config:get_ne_binaries(<<"call_command">>, <<"reserved_ccv_keys">>)).

-spec non_reserved_ccvs(kz_json:object(), kz_term:api_ne_binaries()) -> kz_term:api_object().
non_reserved_ccvs(_CCVs, 'undefined') -> 'undefined';
non_reserved_ccvs(CCVs, Keys) ->
    kz_json:filter(fun({K, _}) -> not lists:member(K, Keys) end, CCVs).

-spec cavs(kz_json:object()) ->
                  kz_term:api_object().
cavs(JObj) ->
    kz_call_event:custom_application_vars(JObj).
