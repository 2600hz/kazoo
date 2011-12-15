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

-import(cf_call_command, [b_bridge/5]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (Data, Call) -> {'stop' | 'continue'} when
      Data :: json_object(),
      Call :: #cf_call{}.
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId, account_id=AccountId}=Call) ->
    put(callid, CallId),
    Endpoints = get_endpoints(Data, Call),
    Timeout = wh_json:get_binary_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    Strategy = wh_json:get_binary_value(<<"strategy">>, Data, <<"simultaneous">>),
    ?LOG("attempting ring group of ~b members with strategy ~s", [length(Endpoints), Strategy]),
    case b_bridge(Endpoints, Timeout, Strategy, <<"true">>, Call) of
        {ok, _} ->
            ?LOG("completed successful bridge to the ring group"),
            CFPid ! { stop };
        {fail, Reason} ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            Level = wh_util:hangup_cause_to_alert_level(Cause),
            whapps_util:alert(Level, ["Source: ~s(~p)~n"
                                      ,"Alert: failed to bridge to device~n"
                                      ,"Fault: ~p~n"
                                      ,"~n~s"]
                              ,[?MODULE, ?LINE, Reason, cf_util:call_info_to_string(Call)], AccountId),
            ?LOG("failed to bridge to ring group ~s:~s", [Code, Cause]),
            CFPid ! { continue };
        {error, R} ->
            ?LOG("failed to bridge to ring group ~p", [R]),
            whapps_util:alert(<<"error">>, ["Source: ~s(~p)~n"
                                            ,"Alert: error bridging to ring group~n"
                                            ,"Fault: ~p~n"
                                            ,"~n~s"]
                              ,[?MODULE, ?LINE, R, cf_util:call_info_to_string(Call)], AccountId),
            CFPid ! { continue }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over the provided endpoints for the callflow and build the
%% json object used in the bridge API
%% @end
%%--------------------------------------------------------------------
-spec get_endpoints/2 :: (Data, Call) -> json_objects() when
      Data :: json_object(),
      Call :: #cf_call{}.
get_endpoints(Data, Call) ->
    lists:foldr(fun(Member, Acc) ->
                        EndpointId = wh_json:get_value(<<"id">>, Member),
                        case cf_endpoint:build(EndpointId, Member, Call) of
                            {ok, Endpoint} -> Endpoint ++ Acc;
                            {error, _} -> Acc
                        end
                end, [], wh_json:get_value([<<"endpoints">>], Data, [])).
