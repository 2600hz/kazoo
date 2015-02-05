%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz INC
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
        'false' ->
            alert_about_hangup(HangupCause, JObj)
    end.


-spec alert_about_hangup(ne_binary(), wh_json:object()) -> 'ok'.
alert_about_hangup(HangupCause, JObj) ->
    lager:debug("abnormal call termination: ~s", [HangupCause]),
    UseEmail = whapps_config:get_is_true(?APP_NAME, <<"enable_email_alerts">>, 'true'),
    SUBUrl   = whapps_config:get_string(?APP_NAME, <<"subscriber_url">>),
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    DataDetails = maybe_add_hangup_specific(HangupCause, JObj),
    Data = [{<<"hangup_cause">>, wh_util:to_lower_binary(HangupCause)}
           ,{<<"source">>,       find_source(JObj)}
           ,{<<"destination">>,  find_destination(JObj)}
           ,{<<"direction">>,    find_direction(JObj)}
           ,{<<"realm">>,        find_realm(JObj, AccountId)}
           ,{<<"account_id">>,   AccountId}
           ],
    alert_about_hangup__email(UseEmail, Data, DataDetails),
    alert_about_hangup__POST(SUBUrl,    Data, DataDetails, UseEmail),
    add_to_meters(AccountId, HangupCause).

-type ne_binary_proplist() :: [{ne_binary(), ne_binary()}].

-spec alert_about_hangup__POST(string() | 'undefined', ne_binary_proplist(), wh_proplist(), boolean()) -> 'ok'.
alert_about_hangup__POST(SUBUrl, Data, Details0, EmailUsed) ->
    Details = {<<"details">>, {Details0}},
    Headers = [{"Content-Type", "application/json"}],
    Encoded = wh_json:encode({[Details] ++ Data}),
    case ibrowse:send_req(SUBUrl, Headers, 'post', Encoded) of
        {'ok', "200", _ResponseHeaders, _ResponseBody} ->
            lager:debug("hangup JSON data successfully POSTed to ~p", [SUBUrl]);
        Error ->
            lager:debug("failed to POST hangup JSON data to ~p for reason: ~p", [SUBUrl,Error]),
            alert_about_hangup__email(not EmailUsed, Data, Details0)
    end.

-spec alert_about_hangup__email(boolean(), ne_binary_proplist(), wh_proplist()) -> 'ok'.
alert_about_hangup__email('false', _Data, _Details) -> 'ok';
alert_about_hangup__email('true', Data, Details) ->
    {_Lhs, DataRhs} = lists:unzip(Data),
    wh_notify:system_alert("~s ~s to ~s (~s) on ~s(~s)", DataRhs, Details),
    lager:debug("hangup JSON data successfully emailed").


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
    Tree = wh_json:get_value(<<"pvt_tree">>, AccountDoc, []),
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
        _ -> wh_json:get_value(<<"Callee-ID-Number">>, JObj, <<"unknown">>)
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
        _ -> wh_json:get_value(<<"Caller-ID-Number">>, JObj, <<"unknown">>)
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec start_meters(ne_binary()) -> 'ok'.
-spec start_meters(api_binary(), api_binary()) -> 'ok'.
start_meters(HangupCause) ->
    folsom_metrics:new_meter(hangups_util:meter_name(HangupCause)).

start_meters('undefined', _) -> 'ok';
start_meters(_, 'undefined') -> 'ok';
start_meters(AccountId, HangupCause) ->
    folsom_metrics:new_meter(hangups_util:meter_name(HangupCause, AccountId)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_to_meters(api_binary(), api_binary()) -> 'ok'.
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
-spec notify_meters(api_binary(), api_binary()) -> any().
notify_meters(HangupCause) ->
    folsom_metrics_meter:mark(hangups_util:meter_name(HangupCause)).

notify_meters('undefined', _) -> 'ok';
notify_meters(_, 'undefined') -> 'ok';
notify_meters(AccountId, HangupCause) ->
    folsom_metrics_meter:mark(hangups_util:meter_name(HangupCause, AccountId)).
