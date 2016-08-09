%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Handle prepend feature
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_providers).

-include("knm.hrl").

-export([save/1]).
-export([delete/1]).
-export([has_emergency_services/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(Number) ->
    exec(Number, 'save').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    exec(Number, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc Return whether a number has emergency services enabled
%%--------------------------------------------------------------------
-spec has_emergency_services(knm_number:number()) -> boolean().
has_emergency_services(Number) ->
    Providers = provider_modules(Number),
    F = fun (Provider) -> apply_action(Number, 'has_emergency_services', Provider) end,
    lists:foldl(fun erlang:'or'/2
               ,'false'
               ,lists:filtermap(F, Providers)
               ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec provider_modules(knm_number:knm_number()) -> ne_binaries().
provider_modules(Number) ->
    Allowed = kapps_config:get(?KNM_CONFIG_CAT, <<"allowed_features">>, ?DEFAULT_ALLOWED_FEATURES),
    Possible = kz_json:get_keys(knm_phone_number:doc(knm_number:phone_number(Number))),
    [provider_module(Feature)
     || Feature <- Possible,
        lists:member(Feature, Allowed)
    ].

-spec provider_module(ne_binary()) -> ne_binary().
provider_module(?FEATURE_CNAM) ->
    <<"knm_cnam_notifier">>;
provider_module(?DASH_KEY) ->
    <<"knm_dash_e911">>;
provider_module(?VITELITY_KEY) ->
    <<"knm_vitelity_e911">>;
provider_module(<<"prepend">>) ->
    <<"knm_prepend">>;
provider_module(<<"port">>) ->
    <<"knm_port_notifier">>;
provider_module(<<"failover">>) ->
    <<"knm_failover">>;
provider_module(Other) ->
    lager:debug("unmatched feature provider '~s', allowing", [Other]),
    Other.

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

exec(Number, Action) ->
    exec(Number, Action, provider_modules(Number)).

exec(Number, Action, Providers) ->
    lists:foldl(fun (Provider, N) ->
                        case apply_action(N, Action, Provider) of
                            {'true', NewN} -> NewN;
                            'false' -> N
                        end
                end
               ,Number
               ,Providers
               ).

-spec apply_action(knm_number:knm_number(), exec_action(), ne_binary()) ->
                          {'true', any()} | 'false'.
apply_action(Number, Action, Provider) ->
    case kz_util:try_load_module(Provider) of
        'false' ->
            lager:debug("provider ~s is unknown, skipping", [Provider]),
            'false';
        Module ->
            lager:debug("attempting ~s:~s/1", [Module, Action]),
            Ret = erlang:apply(Module, Action, [Number]),
            {'true', Ret}
    end.
