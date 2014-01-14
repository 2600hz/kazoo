%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_cnam_notifier).

-export([save/1]).
-export([delete/1]).

-include("../wnm.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%--------------------------------------------------------------------
-spec save(wnm_number()) -> wnm_number().
save(#number{state = <<"reserved">>} = Number) ->
    update_cnam_features(Number);
save(#number{state = <<"in_service">>} = Number) ->
    update_cnam_features(Number);
save(#number{state = <<"port_in">>} = Number) ->
    update_cnam_features(Number);
save(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(wnm_number()) -> wnm_number().
delete(#number{features=Features}=N) ->
    N#number{features=remove_all_cnam_features(Features)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_cnam_features(wnm_number()) -> wnm_number().
update_cnam_features(#number{}=N) ->
    handle_outbound_cnam(N).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_outbound_cnam(wnm_number()) -> wnm_number().
handle_outbound_cnam(#number{current_number_doc=CurrentJObj
                             ,number_doc=JObj
                             ,features=Features
                            }=N) ->
    CurrentCNAM = wh_json:get_ne_value([<<"cnam">>, <<"display_name">>], CurrentJObj),
    case wh_json:get_ne_value([<<"cnam">>, <<"display_name">>], JObj) of
        'undefined' ->
            handle_inbound_cnam(N#number{features=sets:del_element(<<"outbound_cnam">>, Features)});
        CurrentCNAM ->
            handle_inbound_cnam(N#number{features=sets:add_element(<<"outbound_cnam">>, Features)});
        _Else ->
            N1 = wnm_number:activate_feature(<<"outbound_cnam">>, N),
            _ = publish_cnam_update(N1),
            handle_inbound_cnam(N1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_inbound_cnam(wnm_number()) -> wnm_number().
handle_inbound_cnam(#number{number_doc=JObj
                            ,features=Features
                           }=N) ->
    N1 = case wh_json:is_true([<<"cnam">>, <<"inbound_lookup">>], JObj) of
             'false' -> N#number{features=sets:del_element(<<"inbound_cnam">>, Features)};
             'true' ->  wnm_number:activate_feature(<<"inbound_cnam">>, N)
         end,
    support_depreciated_cnam(N1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec support_depreciated_cnam(wnm_number()) -> wnm_number().
support_depreciated_cnam(#number{features=Features}=N) ->
    N#number{features=sets:del_element(<<"cnam">>, Features)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_all_cnam_features(set()) -> set().
remove_all_cnam_features(Features) ->
    Routines = [fun(F) -> sets:del_element(<<"inbound_cnam">>, F) end
                ,fun(F) -> sets:del_element(<<"outbound_cnam">>, F) end
                ,fun(F) -> sets:del_element(<<"cnam">>, F) end
               ],
    lists:foldl(fun(F, Feature) -> F(Feature) end, Features, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_cnam_update(wnm_number()) -> 'ok'.
publish_cnam_update(#number{number=Number
                            ,state=State
                            ,assigned_to=AssignedTo
                            ,module_name=ModuleName
                            ,auth_by=AuthBy
                            ,number_doc=JObj
                           }) ->
    Notify = [{<<"Account-ID">>, AssignedTo}
              ,{<<"Number-State">>, State}
              ,{<<"Local-Number">>, ModuleName =:= 'wnm_local'}
              ,{<<"Number">>, Number}
              ,{<<"Acquired-For">>, AuthBy}
              ,{<<"Cnam">>, wh_json:get_value(<<"cnam">>, JObj, wh_json:new())}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_cnam_request(Notify).
