%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_ring_group).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [b_bridge/5, wait_for_unbridge/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    Endpoints = get_endpoints(Data, Call),
    Timeout = wh_json:get_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    Strategy = wh_json:get_value(<<"strategy">>, Data, <<"simultaneous">>),
    ?LOG("attempting ring group of ~b members with strategy ~s", [length(Endpoints), Strategy]),
    case b_bridge(Endpoints, Timeout, Strategy, <<"true">>, Call) of
        {ok, _} ->
            ?LOG("bridged to ring group"),
            _ = wait_for_unbridge(),
            ?LOG("ring group completed"),
            CFPid ! { stop };
        {fail, Reason} ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            ?LOG("failed to bridge to ring group ~s:~s", [Code, Cause]),
            CFPid ! { continue };
        {error, R} ->
            ?LOG("failed to bridge to ring group ~w", [R]),
            CFPid ! { continue }
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over the provided endpoints for the callflow and build the
%% json object used in the bridge API
%% @end
%%--------------------------------------------------------------------
-spec(get_endpoints/2 :: (Data :: json_object(), Call :: #cf_call{}) -> json_objects()).
get_endpoints(Data, Call) ->
    lists:foldr(fun(Member, Acc) ->
                        try
                            {ok, {struct, Props}=E} = cf_endpoint:build(wh_json:get_value(<<"id">>, Member), Call),
                            case wh_json:get_value(<<"delay">>, Member) of
                                undefined -> [E|Acc];
                                Delay -> [{struct, [{<<"Endpoint-Delay">>, Delay}|Props]}|Acc]
                            end
                        catch
                            _:_ -> Acc
                        end
                end, [], wh_json:get_value([<<"endpoints">>], Data, [])).
