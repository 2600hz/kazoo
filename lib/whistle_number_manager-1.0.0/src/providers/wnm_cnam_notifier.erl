%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% Handle publishing notification events for new port requests
%%%
%%% @end
%%% Created : 27 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wnm_cnam_notifier).

-export([save/1]).
-export([delete/1]).

-include_lib("whistle_number_manager/src/wh_number_manager.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will 
%% produce notifications if the cnam object changes
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (wnm_number()) -> wnm_number().
save(#number{state = <<"reserved">>} = Number) ->
    maybe_publish_cnam(Number);
save(#number{state = <<"in_service">>} = Number) ->
    maybe_publish_cnam(Number);
save(#number{state = <<"port_in">>} = Number) ->
    maybe_publish_cnam(Number);
save(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (wnm_number()) -> wnm_number().
delete(#number{features=Features}=N) -> 
    N#number{features=sets:del_element(<<"cnam">>, Features)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_publish_cnam/1 :: (wnm_number()) -> wnm_number().
maybe_publish_cnam(#number{current_number_doc=CurrentJObj, number_doc=JObj
                           ,features=Features}=N) ->
    CurrentCnam = wh_json:get_ne_value(<<"cnam">>, CurrentJObj),
    Cnam = wh_json:get_ne_value(<<"cnam">>, JObj),
    NotChanged = wnm_util:are_jobjs_identical(CurrentCnam, Cnam),
    case wh_util:is_empty(Cnam) of
        true -> N#number{features=sets:del_element(<<"cnam">>, Features)};
        false when NotChanged -> N#number{features=sets:add_element(<<"cnam">>, Features)};
        false ->
            lager:debug("cnam information has been changed: ~s", [wh_json:encode(Cnam)]),
            N1 = wnm_number:activate_feature(<<"cnam">>, N),
            publish_cnam_update(Cnam, N),
            N1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_cnam_update/2 :: (wh_json:json_object(), wnm_number()) -> 'ok'.
publish_cnam_update(Cnam, #number{number=Number, state=State, assigned_to=AssignedTo
                                  ,module_name=ModuleName, auth_by=AuthBy}) ->
    Notify = [{<<"Account-ID">>, AssignedTo}
              ,{<<"Number-State">>, State}
              ,{<<"Local-Number">>, ModuleName =:= wnm_local}
              ,{<<"Number">>, Number}
              ,{<<"Acquired-For">>, AuthBy}
              ,{<<"Cnam">>, Cnam}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_cnam_request(Notify). 
