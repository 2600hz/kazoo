%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600hz INC
%%% @doc
%%% Handle prepend feature
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(wnm_prepend).

-export([save/1
         ,delete/1
        ]).

-include("wnm.hrl").

-define(PREPEND_KEY, <<"prepend">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%--------------------------------------------------------------------
-spec save(wnm_number()) -> wnm_number().
save(#number{state = ?NUMBER_STATE_IN_SERVICE} = Number) ->
    maybe_update_prepend(Number);
save(Number) ->
    delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% remove the prepend route
%% @end
%%--------------------------------------------------------------------
-spec delete(wnm_number()) -> wnm_number().
delete(#number{features=Features
               ,current_number_doc=CurrentDoc
               ,number_doc=Doc
              }=Number) ->
    case wh_json:get_ne_value(?PREPEND_KEY, CurrentDoc) of
        'undefined' -> Number;
        _Else ->
            Number#number{features=sets:del_element(?PREPEND_KEY, Features)
                          ,number_doc=wh_json:delete_key(?PREPEND_KEY, Doc)
                         }
    end.

-spec maybe_update_prepend(wnm_number()) -> wnm_number().
maybe_update_prepend(#number{current_number_doc=CurrentJObj
                              ,number_doc=JObj
                              ,features=Features
                             }=N) ->
    CurrentPrepend = wh_json:get_ne_value(?PREPEND_KEY, CurrentJObj),
    Prepend = wh_json:get_ne_value(?PREPEND_KEY, JObj),
    NotChanged = wnm_util:are_jobjs_identical(CurrentPrepend, Prepend),

    case wh_util:is_empty(Prepend) of
        'true' ->
            N#number{features=sets:del_element(?PREPEND_KEY, Features)};
        'false' when NotChanged  ->
            N#number{features=sets:add_element(?PREPEND_KEY, Features)};
        'false' ->
            case wh_json:is_true(<<"enabled">>, Prepend) of
                'false' ->
                     N#number{features=sets:del_element(?PREPEND_KEY, Features)};
                'true' ->
                    wnm_number:activate_feature(?PREPEND_KEY, N)
            end
    end.
