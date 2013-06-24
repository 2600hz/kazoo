%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handle failover provisioning
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wnm_failover).

-export([save/1
         ,delete/1
        ]).

-include("../wnm.hrl").

-define(FAILOVER_KEY, <<"failover">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% add the failover route (for in service numbers only)
%% @end
%%--------------------------------------------------------------------
-spec save/1 :: (wnm_number()) -> wnm_number().
save(#number{state = <<"in_service">>} = Number) ->
    maybe_update_failover(Number);
save(Number) ->
    delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% remove the failover route
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (wnm_number()) -> wnm_number().
delete(#number{features=Features
               ,current_number_doc=CurrentDoc
               ,number_doc=Doc
              }=Number) ->
    case wh_json:get_ne_value(?FAILOVER_KEY, CurrentDoc) of
        undefined -> Number;
        _Else ->
            Number#number{features=sets:del_element(?FAILOVER_KEY, Features)
                          ,number_doc=wh_json:delete_key(?FAILOVER_KEY, Doc)
                         }
    end.

-spec maybe_update_failover/1 :: (wnm_number()) -> wnm_number().
maybe_update_failover(#number{current_number_doc=CurrentJObj
                              ,number_doc=JObj
                              ,features=Features
                             }=N) ->
    CurrentFailover = wh_json:get_ne_value(?FAILOVER_KEY, CurrentJObj),
    Failover = wh_json:get_ne_value(?FAILOVER_KEY, JObj),
    NotChanged = wnm_util:are_jobjs_identical(CurrentFailover, Failover),

    case wh_util:is_empty(Failover) of
        true ->
            N#number{features=sets:del_element(?FAILOVER_KEY, Features)};
        false when NotChanged  -> N#number{features=sets:add_element(?FAILOVER_KEY, Features)};
        false ->
            wnm_number:activate_feature(?FAILOVER_KEY, N)
    end.
