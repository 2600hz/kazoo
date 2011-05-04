%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_ring_group).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [b_bridge/5, wait_for_bridge/1, wait_for_unbridge/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> tuple(stop | continue)).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->
    Endpoints = lists:foldr(fun(Endpoint, Acc) ->
                                    case get_endpoint(Endpoint, Call) of
                                        {ok, E} -> [E|Acc];
                                        _ -> Acc
                                    end
                            end, [], wh_json:get_value([<<"endpoints">>], Data, [])),
    Timeout = wh_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    Strategy = wh_json:get_value(<<"strategy">>, Data, <<"simultaneous">>),
    case b_bridge(Endpoints, Timeout, <<"internal">>, Strategy, Call) of
        {ok, _} ->
            _ = wait_for_unbridge(),
            CFPid ! { stop };
        {error, _} ->
            CFPid ! { continue }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches a endpoint defintion from the database and returns the
%% whistle_api endpoint json
%% @end
%%--------------------------------------------------------------------
-spec(get_endpoint/2 :: (Data :: json_object(), Call :: #cf_call{}) -> tuple(ok, json_object()) | tuple(error, atom())).
get_endpoint(Data, #cf_call{account_db=Db}) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            Endpoint = [
                         {<<"Invite-Format">>, wh_json:get_value([<<"sip">>, <<"invite_format">>], JObj)}
                        ,{<<"To-User">>, wh_json:get_value([<<"sip">>, <<"username">>], JObj)}
                        ,{<<"To-Realm">>, wh_json:get_value([<<"sip">>, <<"realm">>], JObj)}
                        ,{<<"To-DID">>, wh_json:get_value([<<"sip">>, <<"number">>], JObj)}
                        ,{<<"Route">>, wh_json:get_value([<<"sip">>, <<"url">>], JObj)}
                        ,{<<"Ignore-Early-Media">>, wh_json:get_value([<<"media">>, <<"ignore_early_media">>], JObj)}
                        ,{<<"Bypass-Media">>, wh_json:get_value([<<"media">>, <<"bypass_media">>], JObj)}
                        ,{<<"Endpoint-Timeout">>, wh_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT)}
                        ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_value(<<"progress_timeout">>, Data, <<"6">>)}
                        ,{<<"Endpoint-Delay">>, wh_json:get_value(<<"delay">>, Data)}
                        ,{<<"Codecs">>, wh_json:get_value([<<"media">>, <<"codecs">>], JObj)}
                    ],
            {ok, {struct, lists:filter(fun({_, undefined}) -> false; (_) -> true end, Endpoint)}};
        {error, _}=E ->
            logger:format_log(error, "CF_DEVICES(~p): Could not locate endpoint ~p in ~p (~p)~n", [self(), Id, Db, E]),
            E
    end.
