%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_util).

-export([get_endpoint/2
         ,get_endpoints/2
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
    [EP || Device <- Devices,
           (EP = get_endpoint(AcctDb, wh_json:get_value(<<"id">>, Device))) =/= undefined
    ].

-spec get_endpoint/2 :: (ne_binary(), ne_binary()) -> wh_json:json_object() | 'undefined'.
get_endpoint(?NE_BINARY = AcctDb, ?NE_BINARY = EndpointId) ->
    case couch_mgr:open_doc(AcctDb, EndpointId) of
        {ok, JObj} -> JObj;
        {error, _R} -> undefined
    end.
