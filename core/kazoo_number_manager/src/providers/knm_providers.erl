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

-define(MOD_CNAM_NOTIFIER, <<"knm_cnam_notifier">>).
-define(DEFAULT_CNAM_PROVIDER, ?MOD_CNAM_NOTIFIER).
-define(DEFAULT_E911_FEATURE, ?DASH_KEY).
-define(DEFAULT_ALLOWED_FEATURES, [?FEATURE_CNAM
                                  ,?FEATURE_E911
                                  ,<<"failover">>
                                  ,<<"port">>
                                  ,<<"prepend">>
                                  ]).

-define(CNAM_PROVIDER(ResellerId),
        kapps_account_config:get(ResellerId, ?KNM_CONFIG_CAT, <<"cnam_provider">>, ?DEFAULT_CNAM_PROVIDER)).
-define(E911_FEATURE(ResellerId),
        kapps_account_config:get(ResellerId, ?KNM_CONFIG_CAT, <<"e911_feature">>, ?DEFAULT_E911_FEATURE)).

-define(ALLOWED_FEATURES(ResellerId),
        kapps_account_config:get(ResellerId, ?KNM_CONFIG_CAT, <<"allowed_features">>, ?DEFAULT_ALLOWED_FEATURES)).

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
    PhoneNumber = knm_number:phone_number(Number),
    ResellerId = kz_services:get_reseller_id(knm_phone_number:assigned_to(PhoneNumber)),
    Allowed = ?ALLOWED_FEATURES(ResellerId),
    Possible = kz_json:get_keys(knm_phone_number:doc(PhoneNumber)),
    [provider_module(Feature, ResellerId)
     || Feature <- Possible,
        lists:member(Feature, Allowed)
    ].

-spec provider_module(ne_binary(), ne_binary()) -> ne_binary().
provider_module(?FEATURE_CNAM, ?MATCH_ACCOUNT_RAW(ResellerId)) ->
    ?CNAM_PROVIDER(ResellerId);
provider_module(?FEATURE_E911, ?MATCH_ACCOUNT_RAW(ResellerId)) ->
    provider_module(?E911_FEATURE(ResellerId), ResellerId);
provider_module(?DASH_KEY, _) ->
    <<"knm_dash_e911">>;
provider_module(?VITELITY_KEY, _) ->
    <<"knm_vitelity_e911">>;
provider_module(?TELNYX_KEY, _) ->
    <<"knm_telnyx_e911">>;
provider_module(<<"prepend">>, _) ->
    <<"knm_prepend">>;
provider_module(<<"port">>, _) ->
    <<"knm_port_notifier">>;
provider_module(<<"failover">>, _) ->
    <<"knm_failover">>;
provider_module(Other, _ResellerId) ->
    lager:warning("unmatched feature provider '~s' (~s), allowing", [Other, _ResellerId]),
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
