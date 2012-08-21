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
    EPDocs = [EPDoc
              || Device <- Devices,
                 (EPDoc = get_endpoint(Call, wh_json:get_value(<<"id">>, Device))) =/= undefined,
                 wh_json:is_true(<<"enabled">>, EPDoc, false)
             ],
    lists:foldl(fun(EPDoc, Acc) ->
                        case cf_endpoint:build(EPDoc, Call) of
                            {ok, EP} -> EP ++ Acc;
                            {error, _} -> Acc
                        end
                end, [], EPDocs).

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
