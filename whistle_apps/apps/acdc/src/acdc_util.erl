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
        ]).

-include("acdc.hrl").

-spec get_endpoints/2 :: (ne_binary(), ne_binary() | couch_mgr:get_results_return()) ->
                                 wh_json:json_objects().
get_endpoints(?NE_BINARY = AcctDb, ?NE_BINARY = AgentId) ->
    get_endpoints(AcctDb
                  ,couch_mgr:get_results(AcctDb
                                         ,<<"cf_attributes/owned">>
                                         ,[{key, [AgentId, <<"device">>]}]
                                        )
                 );
get_endpoints(_AcctDb, {ok, []}) -> [];
get_endpoints(_AcctDb, {error, _E}) -> [];
get_endpoints(AcctDb, {ok, Devices}) ->
    Call = whapps_call:new(),
    [begin
         {ok, EP} = cf_endpoint:build(EPDoc, Call),
         EP
     end
     || Device <- Devices,
        (EPDoc = get_endpoint(AcctDb, wh_json:get_value(<<"id">>, Device))) =/= undefined
    ].

-spec get_endpoint/2 :: (ne_binary(), ne_binary()) -> wh_json:json_object() | 'undefined'.
get_endpoint(?NE_BINARY = AcctDb, ?NE_BINARY = EndpointId) ->
    case couch_mgr:open_doc(AcctDb, EndpointId) of
        {ok, JObj} -> JObj;
        {error, _R} -> undefined
    end.

%% Handles subscribing/unsubscribing from call events
-spec bind_to_call_events/1 :: (ne_binary() | whapps_call:call()) -> 'ok'.
-spec unbind_from_call_events/1 :: (ne_binary() | whapps_call:call()) -> 'ok'.
bind_to_call_events(?NE_BINARY = CallId) ->
    gen_listener:add_binding(self(), call, [{callid, CallId}
                                            ,{restrict_to, [events]}
                                           ]);
bind_to_call_events(Call) ->
    bind_to_call_events(whapps_call:call_id(Call)).

unbind_from_call_events(?NE_BINARY = CallId) ->
    gen_listener:rm_binding(self(), call, [{callid, CallId}
                                           ,{restrict_to, [events]}
                                          ]);
unbind_from_call_events(Call) ->
    unbind_from_call_events(whapps_call:call_id(Call)).
