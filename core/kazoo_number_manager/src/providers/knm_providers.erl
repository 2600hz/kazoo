%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%% Handle prepend feature
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_providers).

-include("knm.hrl").

-export([save/1]).
-export([delete/1]).
-export([available_features/1
        ,service_name/2
        ]).
-export([e911_caller_name/2]).

-define(DEFAULT_CNAM_PROVIDER, <<"knm_cnam_notifier">>).
-define(DEFAULT_E911_PROVIDER, <<"knm_dash_e911">>).

%% knm_cnam_notifier.erl  knm_dash_e911.erl      knm_failover.erl       knm_port_notifier.erl  knm_prepend.erl        knm_providers.erl      knm_telnyx_cnam.erl    knm_telnyx_e911.erl    knm_vitelity_cnam.erl  knm_vitelity_e911.erl

-define(CNAM_PROVIDER(AccountId),
        kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, <<"cnam_provider">>, ?DEFAULT_CNAM_PROVIDER)).

-define(E911_PROVIDER(AccountId),
        kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, <<"e911_provider">>, ?DEFAULT_E911_PROVIDER)).

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
%% List features a number is allowedby its reseller to enable.
%% @end
%%--------------------------------------------------------------------
-define(KAZOO_NUMBER_FEATURES, [?FEATURE_FAILOVER
                               ,?FEATURE_PREPEND
                               ,?FEATURE_FORCE_OUTBOUND
                               ,?FEATURE_RINGBACK
                               ]).

-define(EXTERNAL_NUMBER_FEATURES, [?FEATURE_CNAM
                                  ,?FEATURE_E911
                                  ,?FEATURE_PORT
                                  ]).

-spec available_features(knm_phone_number:knm_phone_number()) -> ne_binaries().
available_features(PhoneNumber) ->
    Allowed =
        [legacy_provider_to_feature(P)
         || P <- allowed_features(PhoneNumber)
        ],
    Denied =
        [legacy_provider_to_feature(P)
         || P <- denied_features(PhoneNumber)
        ],
    [Feature
     || Feature <- lists:usort(Allowed),
        (not lists:member(Feature, Denied))
    ].

allowed_features(PhoneNumber) ->
    maybe_reseller_allowed_features(PhoneNumber)
      ++ number_allowed_features(PhoneNumber).

maybe_reseller_allowed_features(PhoneNumber) ->
    case knm_phone_number:assigned_to(PhoneNumber) of
        'undefined' -> system_allowed_features();
        AccountId -> reseller_allowed_features(AccountId)
    end.

reseller_allowed_features(AccountId) ->
    case kapps_account_config:get_from_reseller(AccountId
                                               ,?KNM_CONFIG_CAT
                                               ,[<<"features">>, <<"allow">>]
                                               )
    of
        'undefined' -> system_allowed_features();
        Providers -> Providers
    end.

system_allowed_features() ->
    case kapps_config:get(?KNM_CONFIG_CAT
                         ,<<"providers">>
                         )
    of
        'undefined' ->
            kapps_config:get(?KNM_CONFIG_CAT
                            ,[<<"features">>, <<"allow">>]
                            ,?KAZOO_NUMBER_FEATURES);
        Providers ->
            kapps_config:get(?KNM_CONFIG_CAT
                            ,[<<"features">>, <<"allow">>]
                            ,Providers)
    end.

number_allowed_features(_PhoneNumber) ->
    [].

denied_features(PhoneNumber) ->
    maybe_reseller_denied_features(PhoneNumber)
      ++ local_denied_features(PhoneNumber)
      ++ number_denied_features(PhoneNumber).

maybe_reseller_denied_features(PhoneNumber) ->
    case knm_phone_number:assigned_to(PhoneNumber) of
        'undefined' -> ?EXTERNAL_NUMBER_FEATURES;
        AccountId -> reseller_denied_features(AccountId)
    end.

reseller_denied_features(AccountId) ->
    case kapps_account_config:get_from_reseller(AccountId
                                               ,?KNM_CONFIG_CAT
                                               ,[<<"features">>, <<"deny">>]
                                               )
    of
        'undefined' -> [];
        Providers -> Providers
    end.

local_denied_features(PhoneNumber) ->
    case knm_phone_number:module_name(PhoneNumber) of
        ?CARRIER_LOCAL -> ?EXTERNAL_NUMBER_FEATURES;
        _Else -> []
    end.


number_denied_features(_PhoneNumber) ->
    [].

legacy_provider_to_feature(<<"wnm_", Rest/binary>>) -> legacy_provider_to_feature(Rest);
legacy_provider_to_feature(<<"knm_", Rest/binary>>) -> legacy_provider_to_feature(Rest);
legacy_provider_to_feature(<<"cnam_notifier">>) -> ?FEATURE_CNAM;
legacy_provider_to_feature(<<"dash_e911">>) -> ?FEATURE_E911;
legacy_provider_to_feature(<<"port_notifier">>) -> ?FEATURE_PORT;
legacy_provider_to_feature(<<"telnyx_cnam">>) -> ?FEATURE_CNAM;
legacy_provider_to_feature(<<"telnyx_e911">>) -> ?FEATURE_E911;
legacy_provider_to_feature(<<"vitelity_cnam">>) -> ?FEATURE_CNAM;
legacy_provider_to_feature(<<"vitelity_e911">>) -> ?FEATURE_E911;
legacy_provider_to_feature(Else) ->
    lager:debug("letting ~p through", [Else]),
    Else.

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Util function to get E911 caller name defaults.
%% @end
%%--------------------------------------------------------------------
-spec e911_caller_name(knm_number:knm_number(), api_ne_binary()) -> ne_binary().
-ifdef(TEST).
e911_caller_name(_Number, ?NE_BINARY=Name) -> Name;
e911_caller_name(_Number, 'undefined') -> ?E911_NAME_DEFAULT.
-else.
e911_caller_name(_Number, ?NE_BINARY=Name) -> Name;
e911_caller_name(Number, 'undefined') ->
    AccountId = knm_phone_number:assigned_to(knm_number:phone_number(Number)),
    case kz_account:fetch(AccountId) of
        {'ok', JObj} -> kz_account:name(JObj, ?E911_NAME_DEFAULT);
        {'error', _Error} ->
            lager:error('error opening account doc ~p', [AccountId]),
            ?E911_NAME_DEFAULT
    end.
-endif.

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
    Possible0 = kz_json:get_keys(knm_phone_number:doc(PhoneNumber)),
    Possible = lists:usort(Possible0 ++ knm_phone_number:features_list(PhoneNumber)),
    AllowedBase = allowed_features(PhoneNumber),
    Allowed = case lists:member(?FEATURE_E911, Possible0) of
                  true -> AllowedBase;
                  false ->
                      %% For backward compatibility
                      AllowedBase ++ [<<"dash_e911">>, <<"vitelity_e911">>]
              end,
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
provider_module(?FEATURE_PREPEND, _) ->
    <<"knm_prepend">>;
provider_module(?FEATURE_PORT, _) ->
    <<"knm_port_notifier">>;
provider_module(?FEATURE_FAILOVER, _) ->
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
-type exec_action() :: 'save' | 'delete'.
-spec exec(knm_number:knm_number(), exec_action()) ->
                  knm_number:knm_number().
-spec exec(knm_number:knm_number(), exec_action(), ne_binaries()) ->
                  knm_number:knm_number().

exec(Number, Action) ->
    Number1 = fix_old_fields_names(Number),
    exec(Number1, Action, provider_modules(Number)).

%% @private
fix_old_fields_names(Number) ->
    PN = knm_number:phone_number(Number),
    Doc = knm_phone_number:doc(PN),
    Values = props:filter_undefined(
               [{?FEATURE_E911, kz_json:get_ne_value(<<"dash_e911">>, Doc)}
               ,{?FEATURE_E911, kz_json:get_ne_value(<<"vitelity_e911">>, Doc)}
               ]),
    ToDelete = [<<"dash_e911">>, <<"vitelity_e911">>],
    NewDoc = kz_json:set_values(Values, kz_json:delete_keys(ToDelete, Doc)),
    NewPN = knm_phone_number:update_doc(PN, NewDoc),
    knm_number:set_phone_number(Number, NewPN).

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
