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

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).
-import(cf_call_command, [b_bridge/4, wait_for_bridge/1, wait_for_unbridge/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or 
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->    
    Endpoints = lists:foldr(fun(Endpoint, Acc) ->
                                    case get_endpoint(Endpoint, Call) of
                                        {ok, E} -> [E|Acc];
                                        _ -> Acc
                                    end
                            end, [], whapps_json:get_value([<<"endpoints">>], Data, [])),
    Timeout = whapps_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    Strategy = whapps_json:get_value(<<"strategy">>, Data, <<"simultaneous">>),
    case b_bridge(Endpoints, Timeout, Strategy, Call) of
        {ok, _} ->
            wait_for_unbridge(),
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
get_endpoint({struct, Props}, #cf_call{account_db=Db}) ->
    Id = get_value(<<"id">>, Props),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            Endpoint = [
                         {<<"Invite-Format">>, whapps_json:get_value([<<"sip">>, <<"invite-format">>], JObj)}
                        ,{<<"To-User">>, whapps_json:get_value([<<"sip">>, <<"username">>], JObj)}
                        ,{<<"To-Realm">>, whapps_json:get_value([<<"sip">>, <<"realm">>], JObj)}
                        ,{<<"To-DID">>, whapps_json:get_value([<<"sip">>, <<"number">>], JObj)}
                        ,{<<"Route">>, whapps_json:get_value([<<"sip">>, <<"url">>], JObj)}
                        ,{<<"Ignore-Early-Media">>, whapps_json:get_value([<<"media">>, <<"ignore-early-media">>], JObj)}
                        ,{<<"Bypass-Media">>, whapps_json:get_value([<<"media">>, <<"bypass-media">>], JObj)}
                        ,{<<"Endpoint-Timeout">>, get_value(<<"timeout">>, Props, ?DEFAULT_TIMEOUT)}
                        ,{<<"Endpoint-Progress-Timeout">>, get_value(<<"progress-timeout">>, Props, <<"6">>)}
                        ,{<<"Endpoint-Delay">>, get_value(<<"delay">>, Props)}
                        ,{<<"Codecs">>, whapps_json:get_value([<<"media">>, <<"codecs">>], JObj)}
                    ],
            {ok, {struct, lists:filter(fun({_, undefined}) -> false; (_) -> true end, Endpoint)}};
        {error, _}=E ->
            format_log(error, "CF_DEVICES(~p): Could not locate endpoint ~p in ~p (~p)~n", [self(), Id, Db, E]),
            E
    end.
