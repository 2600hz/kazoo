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

-export([save/1]).
-export([delete/1]).

-include_lib("src/wnm.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will 
%% produce notifications if the porting object changes
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (wnm_number()) -> wnm_number().
save(#number{current_state = <<"port_in">>
                 ,state = <<"in_service">>
                 ,number_doc=JObj} = Number) ->
    Port = wh_json:get_ne_value(<<"port">>, JObj, wh_json:new()),
    _ = publish_ported(Port, Number),
    Number;
save(#number{state = <<"port_in">>} = Number) ->
    maybe_publish_port(Number);
save(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (wnm_number()) -> wnm_number().
delete(#number{features=Features}=N) -> 
    N#number{features=sets:del_element(<<"port">>, Features)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_publish_port/1 :: (wnm_number()) -> wnm_number().
maybe_publish_port(#number{current_number_doc=CurrentJObj, number_doc=JObj
                           ,features=Features}=N) ->
    CurrentPort = wh_json:get_ne_value(<<"port">>, CurrentJObj),
    Port = wh_json:get_ne_value(<<"port">>, JObj),
    NotChanged = wnm_util:are_jobjs_identical(CurrentPort, Port),
    case wh_util:is_empty(Port) of
        true -> N#number{features=sets:del_element(<<"port">>, Features)};
        false when NotChanged -> N#number{features=sets:add_element(<<"port">>, Features)};
        false ->
            lager:debug("port information has been changed: ~s", [wh_json:encode(Port)]),
            N1 = wnm_number:activate_feature(<<"port">>, N),
            publish_port_update(Port, N),
            N1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_port_update/2 :: (wh_json:json_object(), wnm_number()) -> 'ok'.
publish_port_update(Port, #number{number=Number, state=State, assigned_to=AssignedTo
                                  ,module_name=ModuleName, auth_by=AuthBy}) ->
    Notify = [{<<"Account-ID">>, AssignedTo}
              ,{<<"Number-State">>, State}
              ,{<<"Local-Number">>, ModuleName =:= wnm_local}
              ,{<<"Number">>, Number} 
              ,{<<"Authorized-By">>, AuthBy}
              ,{<<"Port">>, Port}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_port_request(Notify).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_ported/2 :: (wh_json:json_object(), wnm_number()) -> 'ok'.
publish_ported(Port, #number{number=Number, state=State, assigned_to=AssignedTo
                             ,module_name=ModuleName, auth_by=AuthBy}) ->
    Notify = [{<<"Account-ID">>, AssignedTo}
              ,{<<"Number-State">>, State}
              ,{<<"Local-Number">>, ModuleName =:= wnm_local}
              ,{<<"Number">>, Number}
              ,{<<"Authorized-By">>, AuthBy}
              ,{<<"Port">>, Port}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_ported(Notify).
