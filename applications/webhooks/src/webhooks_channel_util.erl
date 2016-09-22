%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------
-module(webhooks_channel_util).

-export([handle_event/2]).
-export([maybe_handle_channel_event/3]).

-include("webhooks.hrl").

-spec handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_event(JObj, Props) ->
    case kz_api:is_federated_event(JObj) of
        'false' ->
            continue_handle_event(JObj, Props);
        'true' ->
            ShouldHandle = props:get_is_true(<<"should_handle_federated">>, Props),
            maybe_handle_federated_event(JObj, Props, ShouldHandle)
    end.

-spec maybe_handle_federated_event(kz_json:object(), kz_proplist(), boolean()) -> 'ok'.
maybe_handle_federated_event(JObj, _Props, 'false') ->
    HookEvent = hook_event_name(kz_json:get_value(<<"Event-Name">>, JObj)),
    lager:debug("event ~s would be handled in other webhooks zones", [HookEvent]);
maybe_handle_federated_event(JObj, Props, _) ->
    continue_handle_event(JObj, Props).

-spec continue_handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
continue_handle_event(JObj, _Props) ->
    HookEvent = hook_event_name(kz_json:get_value(<<"Event-Name">>, JObj)),
    case kz_hooks_util:lookup_account_id(JObj) of
        {'error', _R} ->
            lager:debug("failed to determine account id for ~s", [HookEvent]);
        {'ok', AccountId} ->
            lager:debug("determined account id for ~s is ~s", [HookEvent, AccountId]),
            J = kz_json:set_value([<<"Custom-Channel-Vars">>
                                  ,<<"Account-ID">>
                                  ], AccountId, JObj),
            maybe_handle_channel_event(AccountId, HookEvent, J)
    end.

%% @public
-spec maybe_handle_channel_event(ne_binary(), ne_binary(), kz_json:object()) -> 'ok'.
maybe_handle_channel_event(AccountId, HookEvent, JObj) ->
    lager:debug("evt ~s for ~s", [HookEvent, AccountId]),
    case webhooks_util:find_webhooks(HookEvent, AccountId) of
        [] -> lager:debug("no hooks to handle ~s for ~s", [HookEvent, AccountId]);
        Hooks -> webhooks_util:fire_hooks(format_event(JObj, AccountId, HookEvent), Hooks)
    end.

-spec hook_event_name(ne_binary()) -> ne_binary().
hook_event_name(<<"CHANNEL_DISCONNECTED">>) -> <<"CHANNEL_DESTROY">>;
hook_event_name(Event) -> Event.

-spec format_event(kz_json:object(), api_binary(), ne_binary()) ->
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

-spec base_hook_event(kz_json:object(), api_binary()) -> kz_json:object().
-spec base_hook_event(kz_json:object(), api_binary(), kz_proplist()) -> kz_json:object().
base_hook_event(JObj, AccountId) ->
    base_hook_event(JObj, AccountId, []).
base_hook_event(JObj, AccountId, Acc) ->
    WasGlobal = kz_util:is_true(ccv(JObj, <<"Global-Resource">>)),

    kz_json:from_list(
      props:filter_undefined(
        [{<<"call_direction">>, kz_json:get_value(<<"Call-Direction">>, JObj)}
        ,{<<"timestamp">>, kz_call_event:timestamp(JObj)}
        ,{<<"account_id">>, ccv(JObj, <<"Account-ID">>, AccountId)}
        ,{<<"request">>, kz_json:get_value(<<"Request">>, JObj)}
        ,{<<"to">>, kz_json:get_value(<<"To">>, JObj)}
        ,{<<"from">>, kz_json:get_value(<<"From">>, JObj)}
        ,{<<"inception">>, kz_json:get_value(<<"Inception">>, JObj)}
        ,{<<"call_id">>, kz_call_event:call_id(JObj)}
        ,{<<"other_leg_call_id">>, kz_call_event:other_leg_call_id(JObj)}
        ,{<<"caller_id_name">>, kz_json:get_value(<<"Caller-ID-Name">>, JObj)}
        ,{<<"caller_id_number">>, kz_json:get_value(<<"Caller-ID-Number">>, JObj)}
        ,{<<"callee_id_name">>, kz_json:get_value(<<"Callee-ID-Name">>, JObj)}
        ,{<<"callee_id_number">>, kz_json:get_value(<<"Callee-ID-Number">>, JObj)}
        ,{<<"owner_id">>, kz_call_event:owner_id(JObj)}
        ,{<<"reseller_id">>, kz_services:find_reseller_id(AccountId)}
        ,{<<"authorizing_id">>, kz_call_event:authorizing_id(JObj)}
        ,{<<"authorizing_type">>, kz_call_event:authorizing_type(JObj)}
        ,{<<"local_resource_used">>, (not WasGlobal)}
        ,{<<"local_resource_id">>, resource_used(WasGlobal, JObj)}
        ,{<<"emergency_resource_used">>, kz_util:is_true(ccv(JObj, <<"Emergency-Resource">>))}
        ,{<<"call_forwarded">>, kz_call_event:is_call_forwarded(JObj)}
         | Acc
        ])).

-spec resource_used(boolean(), kz_json:object()) -> api_binary().
resource_used('true', _JObj) -> 'undefined';
resource_used('false', JObj) -> ccv(JObj, <<"Resource-ID">>).

-spec ccv(kz_json:object(), kz_json:path()) ->
                 api_binary().
-spec ccv(kz_json:object(), kz_json:path(), Default) ->
                 ne_binary() | Default.
ccv(JObj, Key) ->
    ccv(JObj, Key, 'undefined').
ccv(JObj, Key, Default) ->
    kz_call_event:custom_channel_var(JObj, Key, Default).
