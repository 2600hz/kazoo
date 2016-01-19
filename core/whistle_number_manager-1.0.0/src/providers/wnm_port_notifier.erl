%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% Handle publishing notification events for new port requests
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_port_notifier).

-export([save/1]).
-export([delete/1]).

-include("wnm.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the porting object changes
%% @end
%%--------------------------------------------------------------------
-spec save(wnm_number()) -> wnm_number().
save(#number{current_state = ?NUMBER_STATE_PORT_IN
             ,state = ?NUMBER_STATE_IN_SERVICE
             ,number_doc=JObj
            }=Number) ->
    Port = wh_json:get_ne_value(<<"port">>, JObj, wh_json:new()),
    _ = publish_ported(Port, Number),
    Number;
save(#number{state = ?NUMBER_STATE_PORT_IN}=Number) ->
    Routines = [fun maybe_port_feature/1
                ,fun maybe_activate_feature/1
               ],
    lists:foldl(fun(F, N) -> F(N) end, Number, Routines);
save(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(wnm_number()) -> wnm_number().
delete(#number{features=Features}=N) ->
    N#number{features=sets:del_element(<<"port">>, Features)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_port_feature(wnm_number()) -> wnm_number().
maybe_port_feature(#number{number_doc=JObj
                           ,features=Features
                          }=Number) ->
    case wh_json:get_ne_value(<<"port">>, JObj) of
        'undefined' ->
            F = sets:del_element(<<"port">>, Features),
            Number#number{features=F};
        Port ->
            F = sets:add_element(<<"port">>, Features),
            maybe_port_changed(Port, Number#number{features=F})
    end.

-spec maybe_port_changed(wh_json:object(), wnm_number()) -> wnm_number().
maybe_port_changed(_, #number{dry_run='true'}=Number) -> Number;
maybe_port_changed(Port, #number{current_number_doc=CurrentJObj}=Number) ->
    CurrentPort = wh_json:get_ne_value(<<"port">>, CurrentJObj),
    case wnm_util:are_jobjs_identical(CurrentPort, Port) of
        'true' -> Number;
        'false' ->
            lager:debug("port information has been changed: ~s"
                        ,[wh_json:encode(Port)]),
            publish_port_update(Port, Number),
            Number
    end.

-spec maybe_activate_feature(wnm_number()) -> wnm_number().
maybe_activate_feature(#number{is_new='false'}=Number) -> Number;
maybe_activate_feature(#number{is_new='true'}=Number) ->
    wnm_number:activate_feature(<<"port">>, Number).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_port_update(wh_json:object(), wnm_number()) -> 'ok'.
publish_port_update(Port, #number{number=Number
                                  ,state=State
                                  ,assigned_to=AssignedTo
                                  ,module_name=ModuleName
                                  ,auth_by=AuthBy
                                 }) ->
    Notify = [{<<"Account-ID">>, AssignedTo}
              ,{<<"Number-State">>, State}
              ,{<<"Local-Number">>, ModuleName =:= 'wnm_local'}
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
-spec publish_ported(wh_json:object(), wnm_number()) -> 'ok'.
publish_ported(Port, #number{number=Number
                             ,state=State
                             ,assigned_to=AssignedTo
                             ,module_name=ModuleName
                             ,auth_by=AuthBy
                            }) ->
    Notify = [{<<"Account-ID">>, AssignedTo}
              ,{<<"Number-State">>, State}
              ,{<<"Local-Number">>, ModuleName =:= 'wnm_local'}
              ,{<<"Number">>, Number}
              ,{<<"Authorized-By">>, AuthBy}
              ,{<<"Port">>, Port}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_ported(Notify).
