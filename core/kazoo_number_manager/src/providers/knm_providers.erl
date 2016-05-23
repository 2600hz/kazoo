%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600hz INC
%%% @doc
%%% Handle prepend feature
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_providers).

-include_lib("kazoo_number_manager/src/knm.hrl").

-export([save/1]).
-export([delete/1]).
-export([has_emergency_services/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) ->
                  knm_number:knm_number().
save(Number) ->
    exec(Number, 'save').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) ->
                    knm_number:knm_number().
delete(Number) ->
    exec(Number, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc Return whether a number has emergency services enabled
%%--------------------------------------------------------------------
-spec has_emergency_services(knm_number:number()) -> boolean().
has_emergency_services(Number) ->
    F = fun (Provider) -> apply_action(Number, 'has_emergency_services', Provider) end,
    lists:foldl(fun erlang:'or'/2, 'false', lists:filtermap(F, provider_modules())).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-type exec_action() :: 'save' | 'delete' | 'has_emergency_services'.
-spec exec(knm_number:knm_number(), exec_action()) ->
                  knm_number:knm_number().
-spec exec(knm_number:knm_number(), exec_action(), ne_binaries()) ->
                  knm_number:knm_number().

-ifdef(TEST).
provider_modules() ->
    ?DEFAULT_PROVIDER_MODULES.
-else.
provider_modules() ->
    kapps_config:get(?KNM_CONFIG_CAT, <<"providers">>, ?DEFAULT_PROVIDER_MODULES).
-endif.

exec(Number, Action) ->
    exec(Number, Action, provider_modules()).

exec(Num, Action, Providers) ->
    lists:foldl(
      fun (Provider, Number) ->
              case apply_action(Number, Action, Provider) of
                  {'true', Ret} -> Ret;
                  'false' -> Number
              end
      end
      ,Num
      ,Providers
     ).

-spec apply_action(knm_number:knm_number(), exec_action(), ne_binary()) ->
                          {'true', any()} | 'false'.
apply_action(Number, Action, Provider) ->
    case kz_util:try_load_module(<<"knm_", Provider/binary>>) of
        'false' ->
            lager:debug("provider ~s is unknown, skipping", [Provider]),
            'false';
        Module ->
            Ret = erlang:apply(Module, Action, [Number]),
            lager:debug("successfully attempted ~s:~s/1", [Module, Action]),
            {'true', Ret}
    end.
