%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(hangups_channel_destroy).

-include("hangups.hrl").

-export([handle_req/2]).
-export([start_meters/1
        ,start_meters/2
        ]).

-define(IGNORE, kapps_config:get(?APP_NAME
                                ,<<"ignore_hangup_causes">>
                                ,[<<"NO_ANSWER">>
                                 ,<<"USER_BUSY">>
                                 ,<<"NO_USER_RESPONSE">>
                                 ,<<"LOSE_RACE">>
                                 ,<<"ATTENDED_TRANSFER">>
                                 ,<<"ORIGINATOR_CANCEL">>
                                 ,<<"NORMAL_CLEARING">>
                                 ,<<"ALLOTTED_TIMEOUT">>
                                 ])).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_call:event_v(JObj),
    HangupCause = kz_json:get_value(<<"Hangup-Cause">>, JObj, <<"unknown">>),
    case lists:member(HangupCause, ?IGNORE) of
        'true' -> 'ok';
        'false' -> alert_about_hangup(HangupCause, JObj)
    end.

-spec alert_about_hangup(ne_binary(), kz_json:object()) -> 'ok'.
alert_about_hangup(HangupCause, JObj) ->
    lager:debug("abnormal call termination: ~s", [HangupCause]),
    AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj, <<"unknown">>),
    kz_notify:detailed_alert("~s ~s to ~s (~s) on ~s(~s)"
                            ,[kz_util:to_lower_binary(HangupCause)
                             ,find_source(JObj)
                             ,find_destination(JObj)
                             ,find_direction(JObj)
                             ,find_realm(JObj, AccountId)
                             ,AccountId
                             ]
                            ,maybe_add_hangup_specific(HangupCause, JObj)
                            ),
    add_to_meters(AccountId, HangupCause).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_hangup_specific(ne_binary(), kz_json:object()) -> kz_proplist().
maybe_add_hangup_specific(<<"UNALLOCATED_NUMBER">>, JObj) ->
    maybe_add_number_info(JObj);
maybe_add_hangup_specific(<<"NO_ROUTE_DESTINATION">>, JObj) ->
    maybe_add_number_info(JObj);
maybe_add_hangup_specific(_HangupCause, JObj) ->
    kz_json:to_proplist(JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_number_info(kz_json:object()) -> kz_proplist().
maybe_add_number_info(JObj) ->
    Destination = find_destination(JObj),
    %% TODO: decouple from stepswitch_util!
    try stepswitch_util:lookup_number(Destination) of
        {'ok', AccountId, _Props} ->
            [{<<"Account-Tree">>, build_account_tree(AccountId)}
             | kz_json:to_proplist(JObj)
            ];
        {'error', _} ->
            [{<<"Hangups-Message">>, <<"Destination was not found in numbers DBs">>}
             | kz_json:to_proplist(JObj)
            ]
    catch
        _:_ -> kz_json:to_proplist(JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_account_tree(ne_binary()) -> kz_json:object().
build_account_tree(AccountId) ->
    {'ok', AccountDoc} = kz_datamgr:open_cache_doc(?KZ_ACCOUNTS_DB, AccountId),
    Tree = kz_account:tree(AccountDoc),
    build_account_tree(Tree, []).

-spec build_account_tree(ne_binaries(), kz_proplist()) -> kz_json:object().
build_account_tree([], Map) -> kz_json:from_list(Map);
build_account_tree([AccountId|Tree], Map) ->
    {'ok', AccountDoc} = kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, AccountId),
    build_account_tree(Tree, [{AccountId, kz_json:get_value(<<"name">>, AccountDoc)} | Map]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_realm(kz_json:object(), ne_binary()) -> ne_binary().
find_realm(JObj, AccountId) ->
    case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        'undefined' -> get_account_realm(AccountId);
        Realm -> Realm
    end.

-spec get_account_realm(api_binary()) -> ne_binary().
get_account_realm('undefined') -> <<"unknown">>;
get_account_realm(AccountId) ->
    case kz_datamgr:open_cache_doc(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} -> kz_json:get_value(<<"realm">>, JObj, <<"unknown">>);
        {'error', _} -> <<"unknown">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_destination(kz_json:object()) -> ne_binary().
find_destination(JObj) ->
    case catch binary:split(kz_json:get_value(<<"Request">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> use_to_as_destination(JObj)
    end.

-spec use_to_as_destination(kz_json:object()) -> ne_binary().
use_to_as_destination(JObj) ->
    case catch binary:split(kz_json:get_value(<<"To-Uri">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> kz_json:get_value(<<"Callee-ID-Number">>, JObj,  kz_util:anonymous_caller_id_number())
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_source(kz_json:object()) -> ne_binary().
find_source(JObj) ->
    case catch binary:split(kz_json:get_value(<<"From-Uri">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> kz_json:get_value(<<"Caller-ID-Number">>, JObj,  kz_util:anonymous_caller_id_number())
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_direction(kz_json:object()) -> ne_binary().
find_direction(JObj) ->
    kz_json:get_value(<<"Call-Direction">>, JObj, <<"unknown">>).

%% @public
-spec start_meters(ne_binary()) -> 'ok'.
-spec start_meters(ne_binary(), ne_binary()) -> 'ok'.
start_meters(HangupCause) ->
    folsom_metrics:new_meter(hangups_util:meter_name(HangupCause)).
start_meters(AccountId, HangupCause) ->
    folsom_metrics:new_meter(hangups_util:meter_name(HangupCause, AccountId)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_to_meters(ne_binary(), ne_binary()) -> 'ok'.
add_to_meters(AccountId, HangupCause) ->
    lager:debug("add to meter ~s/~s", [AccountId, HangupCause]),

    start_meters(HangupCause),
    start_meters(AccountId, HangupCause),

    notify_meters(HangupCause),
    notify_meters(AccountId, HangupCause),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec notify_meters(ne_binary()) -> any().
-spec notify_meters(ne_binary(), ne_binary()) -> any().
notify_meters(HangupCause) ->
    folsom_metrics_meter:mark(hangups_util:meter_name(HangupCause)).

notify_meters(AccountId, HangupCause) ->
    folsom_metrics_meter:mark(hangups_util:meter_name(HangupCause, AccountId)).
