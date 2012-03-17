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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> ok.
handle(Data, Call) ->
    Endpoints = get_endpoints(wh_json:get_value(<<"endpoints">>, Data, []), Call),
    io:format("~p~n", [Endpoints]),
    Timeout = wh_json:get_binary_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    Strategy = wh_json:get_binary_value(<<"strategy">>, Data, <<"simultaneous">>),
    Ringback = wh_json:get_value(<<"ringback">>, Data),
    lager:debug("attempting ring group of ~b members with strategy ~s", [length(Endpoints), Strategy]),
    case length(Endpoints) > 0 andalso whapps_call_command:b_bridge(Endpoints, Timeout, Strategy, <<"true">>, Ringback, Call) of
        false ->
            lager:notice("ring group has no endpoints"),
            cf_exe:continue(Call);
        {ok, _} ->
            lager:debug("completed successful bridge to the ring group"),
            cf_exe:stop(Call);
        {fail, _}=F ->
            cf_util:handle_bridge_failure(F, Call);
        {error, _R} ->
            lager:debug("error bridging to ring group: ~p", [_R]),
            cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over the provided endpoints for the callflow and build the
%% json object used in the bridge API
%% @end
%%--------------------------------------------------------------------
-spec get_endpoints/2 :: (wh_json:json_objects(), whapps_call:call()) -> wh_json:json_objects().
get_endpoints([], _) ->
    [];
get_endpoints(Members, Call) when length(Members) < 2 ->
    lists:foldr(fun(Member, Acc) ->
                        EndpointId = wh_json:get_value(<<"id">>, Member),
                        Properties = wh_json:set_value(<<"source">>, ?MODULE, Member),
                        case cf_endpoint:build(EndpointId, Properties, Call) of
                            {ok, Endpoint} -> 
                                Endpoint ++ Acc;
                            {error, _} -> 
                                Acc
                        end
                end, [], Members);
get_endpoints(Members, Call) ->
    S = self(),
    Builders = [spawn(fun() ->
                              EndpointId = wh_json:get_value(<<"id">>, Member),
                              Properties = wh_json:set_value(<<"source">>, ?MODULE, Member),
                              S ! {self(), (catch cf_endpoint:build(EndpointId, Properties, Call))}
                      end)
                || Member <- Members
               ],
    io:format("~p~n", [Builders]),
    Endpoints = [JObj || {Status, JObj} <- [receive {Pid, Endpoint} -> Endpoint end
                                            || Pid <- Builders
                                           ], Status =:= ok
                ],
    lists:flatten(Endpoints).
