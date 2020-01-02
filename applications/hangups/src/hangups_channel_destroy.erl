%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hangups_channel_destroy).

-include("hangups.hrl").

-export([handle_req/2]).
-export([start_meters/1
        ,start_meters/2
        ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_call_event:doc(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_call:event_v(JObj),
    HangupCause = kz_call_event:hangup_cause(JObj, <<"unknown">>),
    case lists:member(HangupCause, hangups_config:ignored_hangup_causes()) of
        'true' -> 'ok';
        'false' -> alert_about_hangup(HangupCause, JObj)
    end.

-spec alert_about_hangup(kz_term:ne_binary(), kz_call_event:doc()) -> 'ok'.
alert_about_hangup(HangupCause, JObj) ->
    lager:debug("abnormal call termination: ~s", [HangupCause]),
    AccountId = kz_call_event:account_id(JObj, <<"unknown">>),
    kz_notify:detailed_alert("~s ~s to ~s (~s) on ~s(~s)"
                            ,[kz_term:to_lower_binary(HangupCause)
                             ,find_source(JObj)
                             ,find_destination(JObj)
                             ,find_direction(JObj)
                             ,find_realm(JObj, AccountId)
                             ,AccountId
                             ]
                            ,maybe_add_hangup_specific(HangupCause, JObj)
                            ),
    add_to_meters(AccountId, HangupCause).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_hangup_specific(kz_term:ne_binary(), kz_call_event:doc()) -> kz_term:proplist().
maybe_add_hangup_specific(<<"UNALLOCATED_NUMBER">>, JObj) ->
    maybe_add_number_info(JObj);
maybe_add_hangup_specific(<<"NO_ROUTE_DESTINATION">>, JObj) ->
    maybe_add_number_info(JObj);
maybe_add_hangup_specific(_HangupCause, JObj) ->
    kz_json:recursive_to_proplist(JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_number_info(kz_call_event:doc()) -> kz_term:proplist().
maybe_add_number_info(JObj) ->
    Destination = find_destination(JObj),
    Props = kz_json:recursive_to_proplist(JObj),
    try knm_number:lookup_account(Destination) of
        {'ok', AccountId, _Props} ->
            Tree = build_account_tree(AccountId),
            props:set_value(<<"Account-Tree">>, Tree, Props);
        {'error', _} ->
            Msg = <<"Destination was not found in numbers DBs">>,
            props:set_value(<<"Hangups-Message">>, Msg, Props)
    catch
        _:_ -> Props
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec build_account_tree(kz_term:ne_binary()) -> kz_term:proplist().
build_account_tree(AccountId) ->
    {'ok', AccountDoc} = kzd_accounts:fetch(AccountId),
    [{AncestorId, ?NE_BINARY=kzd_accounts:fetch_name(AncestorId)}
     || AncestorId <- kzd_accounts:tree(AccountDoc)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_realm(kz_call_event:doc(), kz_term:ne_binary()) -> kz_term:ne_binary().
find_realm(JObj, <<_/binary>> = AccountId) ->
    case kz_call_event:account_id(JObj) of
        undefined ->
            case kzd_accounts:fetch_realm(AccountId) of
                undefined -> <<"unknown">>;
                Realm -> Realm
            end;
        Realm -> Realm
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_destination(kz_call_event:doc()) -> kz_term:ne_binary().
find_destination(JObj) ->
    case catch binary:split(kz_json:get_value(<<"Request">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> use_to_as_destination(JObj)
    end.

-spec use_to_as_destination(kz_call_event:doc()) -> kz_term:ne_binary().
use_to_as_destination(JObj) ->
    AccountId = kz_call_event:account_id(JObj),
    case catch binary:split(kz_json:get_value(<<"To-Uri">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> kz_json:get_value(<<"Callee-ID-Number">>, JObj,  kz_privacy:anonymous_caller_id_number(AccountId))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_source(kz_call_event:doc()) -> kz_term:ne_binary().
find_source(JObj) ->
    AccountId = kz_call_event:account_id(JObj),
    case catch binary:split(kz_json:get_value(<<"From-Uri">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> kz_json:get_value(<<"Caller-ID-Number">>, JObj,  kz_privacy:anonymous_caller_id_number(AccountId))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_direction(kz_call_event:doc()) -> kz_term:ne_binary().
find_direction(JObj) ->
    kz_call_event:call_direction(JObj, <<"unknown">>).


-spec start_meters(kz_term:ne_binary()) -> 'ok'.
start_meters(HangupCause) ->
    folsom_metrics:new_meter(hangups_util:meter_name(HangupCause)).

-spec start_meters(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
start_meters(AccountId, HangupCause) ->
    folsom_metrics:new_meter(hangups_util:meter_name(HangupCause, AccountId)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_to_meters(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
add_to_meters(AccountId, HangupCause) ->
    lager:debug("add to meter ~s/~s", [AccountId, HangupCause]),

    start_meters(HangupCause),
    start_meters(AccountId, HangupCause),

    notify_meters(HangupCause),
    notify_meters(AccountId, HangupCause),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec notify_meters(kz_term:ne_binary()) -> any().
notify_meters(HangupCause) ->
    folsom_metrics_meter:mark(hangups_util:meter_name(HangupCause)).

-spec notify_meters(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
notify_meters(AccountId, HangupCause) ->
    folsom_metrics_meter:mark(hangups_util:meter_name(HangupCause, AccountId)).
