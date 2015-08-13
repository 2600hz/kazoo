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

-define(IGNORE, whapps_config:get(?APP_NAME
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
-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    HangupCause = wh_json:get_value(<<"Hangup-Cause">>, JObj, <<"unknown">>),
    case lists:member(HangupCause, ?IGNORE) of
        'true' -> 'ok';
        'false' -> alert_about_hangup(HangupCause, JObj)
    end.

-spec alert_about_hangup(ne_binary(), wh_json:object()) -> 'ok'.
alert_about_hangup(HangupCause, JObj) ->
    lager:debug("abnormal call termination: ~s", [HangupCause]),
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj, <<"unknown">>),
    wh_notify:detailed_alert("~s ~s to ~s (~s) on ~s(~s)"
                             ,[wh_util:to_lower_binary(HangupCause)
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
-spec maybe_add_hangup_specific(ne_binary(), wh_json:object()) -> wh_proplist().
maybe_add_hangup_specific(<<"UNALLOCATED_NUMBER">>, JObj) ->
    maybe_add_number_info(JObj);
maybe_add_hangup_specific(<<"NO_ROUTE_DESTINATION">>, JObj) ->
    maybe_add_number_info(JObj);
maybe_add_hangup_specific(_HangupCause, JObj) ->
    wh_json:to_proplist(JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_number_info(wh_json:object()) -> wh_proplist().
maybe_add_number_info(JObj) ->
    Destination = find_destination(JObj),
    %% TODO: decouple from stepswitch_util!
    try stepswitch_util:lookup_number(Destination) of
        {'ok', AccountId, _Props} ->
            [{<<"Account-Tree">>, build_account_tree(AccountId)}
             | wh_json:to_proplist(JObj)
            ];
        {'error', _} ->
            [{<<"Hangups-Message">>, <<"Destination was not found in numbers DBs">>}
             | wh_json:to_proplist(JObj)
            ]
    catch
        _:_ -> wh_json:to_proplist(JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec build_account_tree(ne_binary()) -> wh_json:object().
build_account_tree(AccountId) ->
    {'ok', AccountDoc} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId),
    Tree = kz_account:tree(AccountDoc),
    build_account_tree(Tree, []).

-spec build_account_tree(ne_binaries(), wh_proplist()) -> wh_json:object().
build_account_tree([], Map) -> wh_json:from_list(Map);
build_account_tree([AccountId|Tree], Map) ->
    {'ok', AccountDoc} = couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId),
    build_account_tree(Tree, [{AccountId, wh_json:get_value(<<"name">>, AccountDoc)} | Map]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_realm(wh_json:object(), ne_binary()) -> ne_binary().
find_realm(JObj, AccountId) ->
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        'undefined' -> get_account_realm(AccountId);
        Realm -> Realm
    end.

-spec get_account_realm(api_binary()) -> ne_binary().
get_account_realm('undefined') -> <<"unknown">>;
get_account_realm(AccountId) ->
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} -> wh_json:get_value(<<"realm">>, JObj, <<"unknown">>);
        {'error', _} -> <<"unknown">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_destination(wh_json:object()) -> ne_binary().
find_destination(JObj) ->
    case catch binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> use_to_as_destination(JObj)
    end.

-spec use_to_as_destination(wh_json:object()) -> ne_binary().
use_to_as_destination(JObj) ->
    case catch binary:split(wh_json:get_value(<<"To-Uri">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> wh_json:get_value(<<"Callee-ID-Number">>, JObj,  wh_util:anonymous_caller_id_number())
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_source(wh_json:object()) -> ne_binary().
find_source(JObj) ->
    case catch binary:split(wh_json:get_value(<<"From-Uri">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> wh_json:get_value(<<"Caller-ID-Number">>, JObj,  wh_util:anonymous_caller_id_number())
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_direction(wh_json:object()) -> ne_binary().
find_direction(JObj) ->
    wh_json:get_value(<<"Call-Direction">>, JObj, <<"unknown">>).

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
