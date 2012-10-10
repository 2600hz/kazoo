%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_util).

-export([get_endpoints/2
         ,bind_to_call_events/1
         ,unbind_from_call_events/1
         ,agents_in_queue/2
         ,agent_status/2
         ,proc_id/0, proc_id/1, proc_id/2
        ]).

-include("acdc.hrl").

%% Returns the list of agents configured for the queue
-spec agents_in_queue/2 :: (ne_binary(), ne_binary()) -> wh_json:json_strings().
agents_in_queue(AcctDb, QueueId) ->
    case couch_mgr:get_results(AcctDb, <<"queues/agents_listing">>, [{key, QueueId}]) of
        {ok, []} -> [];
        {error, _E} -> lager:debug("failed to lookup agents for ~s: ~p", [QueueId, _E]), [];
        {ok, As} -> [wh_json:get_value(<<"value">>, A) || A <- As]
    end.

-spec get_endpoints/2 :: (whapps_call:call(), ne_binary() | couch_mgr:get_results_return()) ->
                                 wh_json:json_objects().
get_endpoints(Call, ?NE_BINARY = AgentId) ->
    AcctDb = whapps_call:account_db(Call),
    get_endpoints(Call
                  ,couch_mgr:get_results(AcctDb
                                         ,<<"cf_attributes/owned">>
                                         ,[{key, [AgentId, <<"device">>]}]
                                        )
                 );
get_endpoints(_Call, {ok, []}) -> [];
get_endpoints(_Call, {error, _E}) -> [];
get_endpoints(Call, {ok, Devices}) ->
    {ok, AcctDoc} = couch_mgr:open_cache_doc(whapps_call:account_db(Call), whapps_call:account_id(Call)),
    AcctRealm = wh_json:get_value(<<"realm">>, AcctDoc),

    EPDocs = [EPDoc
              || Device <- Devices,
                 (EPDoc = get_endpoint(Call, wh_json:get_value(<<"id">>, Device))) =/= undefined,
                 wh_json:is_true(<<"enabled">>, EPDoc, false),
                 is_endpoint_registered(EPDoc, AcctRealm)
             ],

    lists:foldl(fun(EPDoc, Acc) ->
                        case cf_endpoint:build(EPDoc, Call) of
                            {ok, EP} -> EP ++ Acc;
                            {error, _} -> Acc
                        end
                end, [], EPDocs).

is_endpoint_registered(EPDoc, AcctRealm) ->
    Query = [{<<"Realm">>, AcctRealm}
             ,{<<"Username">>, wh_json:get_value([<<"sip">>, <<"username">>], EPDoc)}
             ,{<<"Fields">>, [<<"Contact">>]}
            ],
    case whapps_util:amqp_pool_request(Query
                                       ,fun wapi_registration:publish_query_req/1
                                       ,fun wapi_registration:query_resp_v/1
                                      ) of
        {ok, _Resp} -> true;
        {error, _E} -> false
    end.

-spec get_endpoint/2 :: (whapps_call:call(), ne_binary()) -> wh_json:json_object() | 'undefined'.
get_endpoint(Call, ?NE_BINARY = EndpointId) ->
    case couch_mgr:open_doc(whapps_call:account_db(Call), EndpointId) of
        {ok, JObj} -> JObj;
        {error, _R} -> undefined
    end.

%% Handles subscribing/unsubscribing from call events
-spec bind_to_call_events/1 :: (ne_binary() | whapps_call:call()) -> 'ok'.
-spec unbind_from_call_events/1 :: (ne_binary() | whapps_call:call()) -> 'ok'.
bind_to_call_events(?NE_BINARY = CallId) ->
    gen_listener:add_binding(self(), call, [{callid, CallId}
                                            ,{restrict_to, [events, error]}
                                           ]);
bind_to_call_events(Call) ->
    bind_to_call_events(whapps_call:call_id(Call)).

unbind_from_call_events(?NE_BINARY = CallId) ->
    gen_listener:rm_binding(self(), call, [{callid, CallId}
                                           ,{restrict_to, [events, error]}
                                          ]);
unbind_from_call_events(Call) ->
    unbind_from_call_events(whapps_call:call_id(Call)).

-spec agent_status/2 :: (ne_binary(), ne_binary()) -> ne_binary().
agent_status(?NE_BINARY = AcctDb, AgentId) ->
    Opts = [{endkey, [AgentId, 0]}
            ,{startkey, [AgentId, wh_json:new()]}
            ,{limit, 1}
            ,descending
           ],
    case couch_mgr:get_results(AcctDb, <<"agents/agent_status">>, Opts) of
        {ok, []} -> <<"logout">>;
        {error, _E} -> <<"logout">>;
        {ok, [StatusJObj|_]} -> wh_json:get_value(<<"value">>, StatusJObj)
    end.

-spec proc_id/0 :: () -> ne_binary().
-spec proc_id/1 :: (pid()) -> ne_binary().
-spec proc_id/2 :: (pid(), atom() | ne_binary()) -> ne_binary().
proc_id() ->
    proc_id(self()).
proc_id(Pid) ->
    proc_id(Pid, node()).
proc_id(Pid, Node) ->
    list_to_binary([wh_util:to_binary(Node), "-", pid_to_list(Pid)]).
