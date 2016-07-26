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
-ifndef(TEST).
-spec provider_module(ne_binary()) -> ne_binary().
provider_module(<<"inbound_cnam">>) ->
    <<"knm_cnam_notifier">>;
provider_module(<<"outbound_cnam">>) ->
    <<"knm_cnam_notifier">>;
provider_module(<<"dash_e911">>) ->
    <<"knm_dash_e911">>;
provider_module(<<"prepend">>) ->
    <<"knm_prepend">>;
provider_module(<<"port">>) ->
    <<"knm_port_notifier">>;
provider_module(<<"failover">>) ->
    <<"knm_failover">>;
provider_module(Other) ->
    lager:debug("unmatched feature provider '~s', allowing", [Other]),
    Other.
-endif.

-spec provider_modules(knm_number:knm_number()) -> ne_binaries().
-ifdef(TEST).
provider_modules(_Number) ->
    ?DEFAULT_PROVIDER_MODULES.
-else.
provider_modules(Number) ->
    Features = knm_phone_number:features_list(knm_number:phone_number(Number)),
    Providers = kapps_config:get(?KNM_CONFIG_CAT
                                ,<<"providers">>
                                ,?DEFAULT_PROVIDER_MODULES
                                ),
    [provider_module(Provider)
     || Provider <- Providers,
        lists:member(Provider, Features)
    ].
-endif.

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

exec(Num, Action, Providers) ->
    lists:foldl(fun (Provider, Number) ->
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
    case kz_util:try_load_module(Provider) of
        'false' ->
            lager:debug("provider ~s is unknown, skipping", [Provider]),
            'false';
        Module ->
            Ret = erlang:apply(Module, Action, [Number]),
            lager:debug("successfully attempted ~s:~s/1", [Module, Action]),
            {'true', Ret}
    end.
