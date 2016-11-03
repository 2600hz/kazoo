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
-export([allowed_features/1
        ,service_name/2
        ]).

-define(DEFAULT_CNAM_PROVIDER, <<"knm_cnam_notifier">>).
-define(DEFAULT_E911_PROVIDER, <<"knm_dash_e911">>).
-define(DEFAULT_ALLOWED_FEATURES, [?FEATURE_CNAM
                                  ,?FEATURE_E911
                                  ,<<"failover">>
                                  ,<<"port">>
                                  ,<<"prepend">>
                                  ]).

-define(CNAM_PROVIDER(AccountId),
        kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, <<"cnam_provider">>, ?DEFAULT_CNAM_PROVIDER)).

-define(E911_PROVIDER(AccountId),
        kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, <<"e911_provider">>, ?DEFAULT_E911_PROVIDER)).

-define(ALLOWED_FEATURES(AccountId),
        lists:usort(
          kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, <<"allowed_features">>, ?DEFAULT_ALLOWED_FEATURES))).

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
%% @doc
%% Return whether a number has emergency services enabled
%% @end
%%--------------------------------------------------------------------
-spec has_emergency_services(knm_number:number()) -> boolean().
has_emergency_services(Number) ->
    Providers = provider_modules(Number),
    F = fun (Provider) -> apply_action(Number, 'has_emergency_services', Provider) end,
    lists:foldl(fun erlang:'or'/2
               ,'false'
               ,lists:filtermap(F, Providers)
               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% List features a number is allowedby its reseller to enable.
%% @end
%%--------------------------------------------------------------------
-spec allowed_features(knm_phone_number:knm_phone_number()) -> ne_binaries().
allowed_features(PhoneNumber) ->
    case knm_phone_number:assigned_to(PhoneNumber) of
        'undefined' -> [];
        AccountId -> ?ALLOWED_FEATURES(AccountId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% The name of the billable service associated with a feature.
%% @end
%%--------------------------------------------------------------------
-spec service_name(ne_binary(), ne_binary()) -> ne_binary().
service_name(?FEATURE_E911, AccountId) ->
    service_name(?E911_PROVIDER(AccountId));
service_name(?FEATURE_CNAM, AccountId) ->
    service_name(?CNAM_PROVIDER(AccountId));
service_name(Feature, _) ->
    service_name(Feature).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec service_name(ne_binary()) -> ne_binary().
service_name(<<"knm_dash_e911">>) -> <<"dash_e911">>;
service_name(<<"knm_telnyx_e911">>) -> <<"telnyx_e911">>;
service_name(<<"knm_vitelity_e911">>) -> <<"vitelity_e911">>;
service_name(<<"knm_cnam_notifier">>) -> <<"cnam">>;
service_name(<<"knm_telnyx_cnam">>) -> <<"telnyx_cnam">>;
service_name(<<"knm_vitelity_cnam">>) -> <<"vitelity_cnam">>;
service_name(Feature) -> Feature.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec provider_modules(knm_number:knm_number()) -> ne_binaries().
provider_modules(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AccountId = knm_phone_number:assigned_to(PhoneNumber),
    Allowed = allowed_features(PhoneNumber),
    Possible = kz_json:get_keys(knm_phone_number:doc(PhoneNumber)),
    lager:debug("allowed ~p, possible ~p", [Allowed, Possible]),
    [provider_module(Feature, AccountId)
     || Feature <- Possible,
        lists:member(Feature, Allowed)
    ].

-spec provider_module(ne_binary(), api_ne_binary()) -> ne_binary().
provider_module(?FEATURE_CNAM, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    cnam_provider(AccountId);
provider_module(?FEATURE_E911, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    e911_provider(AccountId);
provider_module(<<"prepend">>, _) ->
    <<"knm_prepend">>;
provider_module(<<"port">>, _) ->
    <<"knm_port_notifier">>;
provider_module(<<"failover">>, _) ->
    <<"knm_failover">>;
provider_module(Other, _) ->
    lager:warning("unmatched feature provider '~s', allowing", [Other]),
    Other.

-ifdef(TEST).
e911_provider(?RESELLER_ACCOUNT_ID) -> <<"knm_telnyx_e911">>;
e911_provider(AccountId) -> ?E911_PROVIDER(AccountId).
-else.
e911_provider(AccountId) -> ?E911_PROVIDER(AccountId).
-endif.

-ifdef(TEST).
cnam_provider(?RESELLER_ACCOUNT_ID) -> <<"knm_telnyx_cnam">>;
cnam_provider(AccountId) -> ?CNAM_PROVIDER(AccountId).
-else.
cnam_provider(AccountId) -> ?CNAM_PROVIDER(AccountId).
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
