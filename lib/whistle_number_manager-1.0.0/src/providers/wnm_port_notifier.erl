%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% Handle publishing notification events for new port requests
%%%
%%% @end
%%% Created : 27 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wnm_port_notifier).

-export([save/4]).
-export([delete/4]).

-include("../wh_number_manager.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will 
%% produce notifications if the porting object changes
%% @end
%%--------------------------------------------------------------------
-spec save/4 :: (wh_json:json_object(), wh_json:json_object(), ne_binary(), ne_binary()) 
                -> {ok, wh_json:json_object()}.
save(JObj, PriorJObj, Number, <<"port_in">>) ->
    EmptyJObj = wh_json:new(),
    Port = wh_json:get_value(<<"port">>, JObj, EmptyJObj),
    case Port =/= EmptyJObj andalso Port =/= wh_json:get_value(<<"port">>, PriorJObj) of
        false -> {ok, JObj};
        true ->
            lager:debug("port information has been updated"),
            Notify = [{<<"Account-ID">>, wh_json:get_value(<<"pvt_reserved_for">>, JObj)}
                      ,{<<"Number-State">>, wh_json:get_value(<<"pvt_number_state">>, JObj)}
                      ,{<<"Local-Number">>, wh_json:get_value(<<"pvt_module_name">>, JObj) =:= <<"wnm_local">>}
                      ,{<<"Number">>, Number}
                      ,{<<"Acquired-For">>, wh_json:get_value([<<"pvt_module_data">>, <<"acquire_for">>], JObj)}
                      ,{<<"Port">>, Port}
                      | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
                     ],
            wapi_notifications:publish_port_request(Notify),
            {ok, JObj}
    end;
save(JObj, _, _, _) -> {ok, JObj}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete/4 :: (wh_json:json_object(), wh_json:json_object(), ne_binary(), ne_binary()) 
                  -> {ok, wh_json:json_object()}.
delete(JObj, _, _, _) ->
    {ok, JObj}.

