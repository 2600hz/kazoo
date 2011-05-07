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
    Endpoints = lists:foldr(fun(Member, Acc) ->
                                    case cf_endpoint:build(wh_json:get_value(<<"id">>, Member), Call) of
                                        {ok, {struct, Props}=E} -> 
                                            case wh_json:get_value(<<"delay">>, Member) of
                                                undefined -> [E|Acc];
                                                Delay -> [{struct, [{<<"Endpoint-Delay">>, Delay}|Props]}|Acc]
                                            end;
                                        _=E -> logger:format_log(info, "GOT SOMETHING ELSE: ~p", [E]), Acc
                                    end
                            end, [], wh_json:get_value([<<"endpoints">>], Data, [])),
    Timeout = wh_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    Strategy = wh_json:get_value(<<"strategy">>, Data, <<"simultaneous">>),
    case b_bridge(Endpoints, Timeout, {undefined, undefined}, Strategy, Call) of
        {ok, _} ->
            _ = wait_for_unbridge(),
            CFPid ! { stop };
        {error, _} ->
            CFPid ! { continue }
    end.
