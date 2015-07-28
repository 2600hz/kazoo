%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

-module(webhooks_channel_util).

-export([handle_event/2]).

-include("webhooks.hrl").

-spec handle_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_event(JObj, _Props) ->
    HookEvent = hook_event_name(wh_json:get_value(<<"Event-Name">>, JObj)),
    case wh_hooks_util:lookup_account_id(JObj) of
        {'error', _R} ->
            lager:debug("failed to determine account id for ~s", [HookEvent]);
        {'ok', AccountId} ->
            lager:debug("determined account id for ~s is ~s", [HookEvent, AccountId]),
            J = wh_json:set_value([<<"Custom-Channel-Vars">>
                                   ,<<"Account-ID">>
                                  ], AccountId, JObj),
            maybe_handle_channel_event(AccountId, HookEvent, J)
    end.

-spec maybe_handle_channel_event(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_handle_channel_event(AccountId, HookEvent, JObj) ->
    lager:debug("evt ~s for ~s", [HookEvent, AccountId]),
    case webhooks_util:find_webhooks(HookEvent, AccountId) of
        [] -> lager:debug("no hooks to handle ~s for ~s", [HookEvent, AccountId]);
        Hooks -> webhooks_util:fire_hooks(format_event(JObj, AccountId, HookEvent), Hooks)
    end.

-spec hook_event_name(ne_binary()) -> ne_binary().
hook_event_name(<<"CHANNEL_DISCONNECTED">>) -> <<"CHANNEL_DESTROY">>;
hook_event_name(Event) -> Event.

-spec format_event(wh_json:object(), api_binary(), ne_binary()) ->
                          wh_json:object().
format_event(JObj, AccountId, <<"CHANNEL_CREATE">>) ->
    wh_json:set_value(<<"hook_event">>, <<"channel_create">>
                      ,base_hook_event(JObj, AccountId)
                     );
format_event(JObj, AccountId, <<"CHANNEL_ANSWER">>) ->
    wh_json:set_value(<<"hook_event">>, <<"channel_answer">>
                      ,base_hook_event(JObj, AccountId)
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

-spec base_hook_event(wh_json:object(), api_binary()) -> wh_json:object().
-spec base_hook_event(wh_json:object(), api_binary(), wh_proplist()) -> wh_json:object().
base_hook_event(JObj, AccountId) ->
    base_hook_event(JObj, AccountId, []).
base_hook_event(JObj, AccountId, Acc) ->
    WasGlobal = wh_util:is_true(ccv(JObj, <<"Global-Resource">>)),

    wh_json:from_list(
      props:filter_undefined(
        [{<<"call_direction">>, wh_json:get_value(<<"Call-Direction">>, JObj)}
         ,{<<"timestamp">>, kz_call_event:timestamp(JObj)}
         ,{<<"account_id">>, ccv(JObj, <<"Account-ID">>, AccountId)}
         ,{<<"request">>, wh_json:get_value(<<"Request">>, JObj)}
         ,{<<"to">>, wh_json:get_value(<<"To">>, JObj)}
         ,{<<"from">>, wh_json:get_value(<<"From">>, JObj)}
         ,{<<"inception">>, wh_json:get_value(<<"Inception">>, JObj)}
         ,{<<"call_id">>, kz_call_event:call_id(JObj)}
         ,{<<"other_leg_call_id">>, kz_call_event:other_leg_call_id(JObj)}
         ,{<<"caller_id_name">>, wh_json:get_value(<<"Caller-ID-Name">>, JObj)}
         ,{<<"caller_id_number">>, wh_json:get_value(<<"Caller-ID-Number">>, JObj)}
         ,{<<"callee_id_name">>, wh_json:get_value(<<"Callee-ID-Name">>, JObj)}
         ,{<<"callee_id_number">>, wh_json:get_value(<<"Callee-ID-Number">>, JObj)}
         ,{<<"owner_id">>, kz_call_event:owner_id(JObj)}
         ,{<<"reseller_id">>, wh_services:find_reseller_id(AccountId)}
         ,{<<"authorizing_id">>, kz_call_event:authorizing_id(JObj)}
         ,{<<"authorizing_type">>, kz_call_event:authorizing_type(JObj)}
         ,{<<"local_resource_used">>, (not WasGlobal)}
         ,{<<"local_resource_id">>, resource_used(WasGlobal, JObj)}
         ,{<<"emergency_resource_used">>, wh_util:is_true(ccv(JObj, <<"Emergency-Resource">>))}
         ,{<<"call_forwarded">>, kz_call_event:is_call_forwarded(JObj)}
         | Acc
        ])).

-spec resource_used(boolean(), wh_json:object()) -> api_binary().
resource_used('true', _JObj) -> 'undefined';
resource_used('false', JObj) -> ccv(JObj, <<"Resource-ID">>).

-spec ccv(wh_json:object(), wh_json:key()) ->
                 api_binary().
-spec ccv(wh_json:object(), wh_json:key(), Default) ->
                 ne_binary() | Default.
ccv(JObj, Key) ->
    ccv(JObj, Key, 'undefined').
ccv(JObj, Key, Default) ->
    kz_call_event:custom_channel_var(JObj, Key, Default).
