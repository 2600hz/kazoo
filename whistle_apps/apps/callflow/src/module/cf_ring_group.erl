%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
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

    Timeout = wh_json:get_binary_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    Strategy = wh_json:get_binary_value(<<"strategy">>, Data, <<"simultaneous">>),
    Ringback = wh_json:get_value(<<"ringback">>, Data),

    lager:info("attempting ring group of ~b members with strategy ~s", [length(Endpoints), Strategy]),

    case length(Endpoints) > 0 andalso whapps_call_command:b_bridge(Endpoints, Timeout, Strategy, <<"true">>, Ringback, Call) of
        false ->
            lager:notice("ring group has no endpoints"),
            cf_exe:continue(Call);
        {ok, _} ->
            lager:info("completed successful bridge to the ring group"),
            cf_exe:stop(Call);
        {fail, _}=F ->
            cf_util:handle_bridge_failure(F, Call);
        {error, _R} ->
            lager:info("error bridging to ring group: ~p", [_R]),
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
get_endpoints([_]=Members, Call) ->
    get_endpoints_direct(Members, Call);
get_endpoints([_,_]=Members, Call) ->
    get_endpoints_direct(Members, Call);
get_endpoints(Members, Call) ->
    S = self(),
    Builders = [spawn(fun() ->
                              put(callid, whapps_call:call_id(Call)),
                              EPIds = fetch_endpoints(wh_json:get_value(<<"id">>, Member), wh_json:get_value(<<"endpoint_type">>, Member), Call),

                              Properties = wh_json:set_value(<<"source">>, ?MODULE, Member),
                              S ! {self(), [(catch cf_endpoint:build(EndpointId, Properties, Call)) || EndpointId <- EPIds]}
                      end)
                || Member <- Members
               ],

    lists:foldl(fun(Pid, Acc) ->
                        BRs = receive {Pid, BuildResults} -> BuildResults end,
                        lists:foldl(fun fold_eps/2, Acc, BRs)
                end, [], lists:reverse(Builders)).

get_endpoints_direct(Members, Call) ->
    lists:foldr(fun(Member, Acc) ->
                        EPIds = fetch_endpoints(wh_json:get_value(<<"id">>, Member), wh_json:get_value(<<"endpoint_type">>, Member), Call),

                        Properties = wh_json:set_value(<<"source">>, ?MODULE, Member),
                        lists:foldl(fun fold_eps/2, Acc, [(catch cf_endpoint:build(EndpointId, Properties, Call)) || EndpointId <- EPIds])
                end, [], Members).

fold_eps({ok, EPs}, Acc0) when is_list(EPs) -> EPs ++ Acc0;
fold_eps({ok, EP}, Acc0) -> [EP | Acc0];
fold_eps(_, Acc0) -> Acc0.

fetch_endpoints(MemberId, <<"user">>, Call) ->
    lager:info("member ~s is a user, get all the user's endpoints", [MemberId]),
    cf_attributes:fetch_owned_by(MemberId, device, Call);
fetch_endpoints(MemberId, _, _) ->
    lager:info("member ~s is not a user, assuming a device", [MemberId]),
    [MemberId].
