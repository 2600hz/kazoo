%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(acdc_util).

-export([get_endpoints/2, log_agent_activity/3
         ,get_agents/2, get_agent_status/2
         ,find_queue/2, fetch_owned_by/3
        ]).

-include("acdc.hrl").

%% TODO - remove need for callflows to be present in VM
get_endpoints(Call, EPs) ->
    Properties = wh_json:from_list([{<<"source">>, ?MODULE}]),
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, Properties, Call) of
                            {ok, Endpoint} -> Endpoint ++ Acc;
                            {error, _E} -> Acc
                        end
                end, [], EPs).

log_agent_activity(Db, Action, AgentId) when is_binary(Db) ->
    lager:debug("setting action for agent ~s to ~s", [AgentId, Action]),
    Doc = wh_json:from_list([{<<"agent_id">>, AgentId}
                             ,{<<"action">>, Action}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                             ,{<<"pvt_created">>, wh_util:current_tstamp()}
                            ]),
    couch_mgr:save_doc(Db, Doc);
log_agent_activity(Call, Action, AgentId) ->
    lager:debug("setting action for agent ~s to ~s", [AgentId, Action]),
    Doc = wh_json:from_list([{<<"call_id">>, whapps_call:call_id(Call)}
                             ,{<<"agent_id">>, AgentId}
                             ,{<<"action">>, Action}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                             ,{<<"pvt_created">>, wh_util:current_tstamp()}
                            ]),
    couch_mgr:save_doc(whapps_call:account_db(Call), Doc).

get_agent_status(AcctDb, AgentId) ->
    case couch_mgr:get_results(AcctDb, <<"agents/agent_status">>, [{<<"startkey">>, [AgentId, wh_json:new()]}
                                                                   ,{<<"endkey">>, [AgentId, 0]}
                                                                   ,{<<"descending">>, true}
                                                                   ,{<<"limit">>, 1}
                                                                   ,{<<"reduce">>, false}
                                                                   ,{<<"include_docs">>, true}
                                                                  ]) of
        {ok, []} ->
            false;
        {ok, [StatusJObj]} ->
            wh_json:get_value([<<"doc">>, <<"action">>], StatusJObj);
        {error, _Reason} ->
            lager:debug("unable to determine the status of the agent: ~p", [_Reason]),
            false
    end.

get_agents(AcctDb, QueueId) ->
    case couch_mgr:get_results(AcctDb, <<"agents/agent_listing">>, [{<<"key">>, QueueId}]) of
        {ok, []}=OK ->
            lager:debug("no agents assigned to ~s/~s", [AcctDb, QueueId]),
            OK;
        {ok, Agents} ->
            {ok, [wh_json:get_value(<<"id">>, Agent) || Agent <- Agents]};
        {error, _E}=Err ->
            lager:debug("error getting agents for ~s/~s", [AcctDb, QueueId]),
            Err
    end.

find_queue(AcctDb, QueueId) ->
    {ok, Cache} = acdc_sup:cache_proc(),
    find_queue(AcctDb, QueueId, Cache).
find_queue(AcctDb, QueueId, Cache) ->
    case wh_cache:fetch_local(Cache, queue_cache_key(AcctDb, QueueId)) of
        {ok, _QueueJObj}=OK -> OK;
        {error, not_found} ->
            case couch_mgr:open_doc(AcctDb, QueueId) of
                {ok, JObj}=OK ->
                    wh_cache:store_local(Cache, queue_cache_key(AcctDb, QueueId), JObj),
                    OK;
                E -> E
            end
    end.

queue_cache_key(AcctDb, QueueId) ->
    {?MODULE, AcctDb, QueueId}.

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec owned_by/2 :: ('undefined' | ne_binary(), ne_binary()) -> list().
-spec owned_by/3 :: ('undefined' | ne_binary(), atom() | string() | ne_binary(), ne_binary()) -> list().

-spec fetch_owned_by/3 :: ('undefined' | ne_binary(), atom() | string() | ne_binary(), ne_binary()) -> list().

owned_by(undefined, _) ->
    [];
owned_by(OwnerId, AcctDb) ->
    Attributes = fetch_attributes(owned, AcctDb),
    [V || {[I, _], V} <- Attributes, I =:= OwnerId].

owned_by(undefined, _, _) ->
    [];
owned_by(OwnerId, false, AcctDb) ->
    owned_by(OwnerId, AcctDb);
owned_by(OwnerId, Attribute, AcctDb) when not is_binary(Attribute) ->
    owned_by(OwnerId, wh_util:to_binary(Attribute), AcctDb);
owned_by(OwnerId, Attribute, AcctDb) ->
    Attributes = fetch_attributes(owned, AcctDb),
    [V || {[I, T], V} <- Attributes, I =:= OwnerId, T =:= Attribute].

fetch_owned_by(OwnerId, Attribute, AcctDb) ->
    owned_by(OwnerId, Attribute, AcctDb).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec fetch_attributes/2 :: (atom(), ne_binary()) -> proplist().
fetch_attributes(Attribute, AcctDb) ->
    case wh_cache:peek({?MODULE, AcctDb, Attribute}) of
        {ok, Attributes} ->
            Attributes;
        {error, not_found} ->
            case couch_mgr:get_all_results(AcctDb, <<"cf_attributes/", (wh_util:to_binary(Attribute))/binary>>) of
                {ok, JObjs} ->
                    [{wh_json:get_value(<<"key">>, JObj), wh_json:get_value(<<"value">>, JObj)}
                     || JObj <- JObjs];
                {error, R} ->
                    lager:debug("unable to fetch attribute ~s: ~p", [Attribute, R]),
                    []
            end
    end.
