%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
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
-export([available_features/1, available_features/5
        ,service_name/2
        ]).
-export([e911_caller_name/2]).

-define(DEFAULT_CNAM_PROVIDER, <<"knm_cnam_notifier">>).
-define(DEFAULT_E911_PROVIDER, <<"knm_dash_e911">>).

-define(KEY_FEATURES_ALLOW, [<<"features">>, <<"allow">>]).
-define(KEY_FEATURES_DENY, [<<"features">>, <<"deny">>]).

-define(CNAM_PROVIDER(AccountId),
        kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, <<"cnam_provider">>, ?DEFAULT_CNAM_PROVIDER)).

-define(E911_PROVIDER(AccountId),
        kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, <<"e911_provider">>, ?DEFAULT_E911_PROVIDER)).

-define(FEATURES_ALLOWED_RESELLER(AccountId),
        kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, ?KEY_FEATURES_ALLOW)).

-define(FEATURES_DENIED_RESELLER(AccountId),
        kapps_account_config:get_from_reseller(AccountId, ?KNM_CONFIG_CAT, ?KEY_FEATURES_DENY)).

-define(SYSTEM_PROVIDERS,
        kapps_config:get(?KNM_CONFIG_CAT, <<"providers">>)).

-define(FEATURES_ALLOWED_SYSTEM(Default),
        kapps_config:get(?KNM_CONFIG_CAT, ?KEY_FEATURES_ALLOW, Default)).

-define(FEATURES_ALLOWED_SYSTEM,
        ?FEATURES_ALLOWED_SYSTEM(?KAZOO_NUMBER_FEATURES)).


-record(feature_parameters, {is_local = 'false' :: boolean()
                            ,assigned_to :: api_ne_binary()
                            ,used_by :: api_ne_binary()
                            ,allowed_features = [] :: ne_binaries()
                            ,denied_features = [] :: ne_binaries()
                            }).
-type feature_parameters() :: #feature_parameters{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_numbers:collection()) -> knm_numbers:collection().
save(Number) ->
    do_exec(Number, 'save').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_numbers:collection()) -> knm_numbers:collection().
delete(Number) ->
    do_exec(Number, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% List features a number is allowed by its reseller to enable.
%% @end
%%--------------------------------------------------------------------
-spec available_features(knm_phone_number:knm_phone_number()) -> ne_binaries().
available_features(PhoneNumber) ->
    list_available_features(feature_parameters(PhoneNumber)).

-spec available_features(boolean(), api_ne_binary(), api_ne_binary(), ne_binaries(), ne_binaries()) -> ne_binaries().
available_features(IsLocal, AssignedTo, UsedBy, Allowed, Denied) ->
    list_available_features(feature_parameters(IsLocal, AssignedTo, UsedBy, Allowed, Denied)).

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
service_name(<<"knm_dash_e911">>) -> ?LEGACY_DASH_E911;
service_name(<<"knm_telnyx_e911">>) -> ?LEGACY_TELNYX_E911;
service_name(<<"knm_vitelity_e911">>) -> ?LEGACY_VITELITY_E911;
service_name(<<"knm_cnam_notifier">>) -> <<"cnam">>;
service_name(<<"knm_telnyx_cnam">>) -> <<"telnyx_cnam">>;
service_name(<<"knm_vitelity_cnam">>) -> <<"vitelity_cnam">>;
service_name(Feature) -> Feature.

-spec list_available_features(feature_parameters()) -> ne_binaries().
list_available_features(Parameters) ->
    Allowed = lists:usort([legacy_provider_to_feature(F) || F <- list_allowed_features(Parameters)]),
    Denied = lists:usort([legacy_provider_to_feature(F) || F <- list_denied_features(Parameters)]),
    [Feature
     || Feature <- Allowed,
        not lists:member(Feature, Denied)
    ].

%% @private
-spec is_local(knm_phone_number:knm_phone_number()) -> boolean().
is_local(PN) ->
    ModuleName = knm_phone_number:module_name(PN),
    ?CARRIER_LOCAL =:= ModuleName
        orelse ?CARRIER_MDN =:= ModuleName
        orelse lists:member(?FEATURE_LOCAL, knm_phone_number:features_list(PN)).

-spec feature_parameters(knm_phone_number:knm_phone_number()) -> feature_parameters().
feature_parameters(PhoneNumber) ->
    feature_parameters(is_local(PhoneNumber)
                      ,knm_phone_number:assigned_to(PhoneNumber)
                      ,knm_phone_number:used_by(PhoneNumber)
                      ,knm_phone_number:features_allowed(PhoneNumber)
                      ,knm_phone_number:features_denied(PhoneNumber)
                      ).

-spec feature_parameters(boolean(), api_ne_binary(), api_ne_binary(), ne_binaries(), ne_binaries()) ->
                                feature_parameters().
feature_parameters(IsLocal, AssignedTo, UsedBy, Allowed, Denied) ->
    #feature_parameters{is_local = IsLocal
                       ,assigned_to = AssignedTo
                       ,used_by = UsedBy
                       ,allowed_features = Allowed
                       ,denied_features = Denied
                       }.

-spec list_allowed_features(feature_parameters()) -> ne_binaries().
list_allowed_features(Parameters) ->
    case number_allowed_features(Parameters) of
        [] -> reseller_allowed_features(Parameters);
        NumberAllowed -> NumberAllowed
    end.

-spec reseller_allowed_features(feature_parameters()) -> ne_binaries().
reseller_allowed_features(#feature_parameters{assigned_to = 'undefined'}) ->
    system_allowed_features();
reseller_allowed_features(#feature_parameters{assigned_to = AccountId}) ->
    case ?FEATURES_ALLOWED_RESELLER(AccountId) of
        'undefined' -> system_allowed_features();
        Providers ->
            lager:debug("allowed number features set on reseller for ~s: ~p", [AccountId, Providers]),
            Providers
    end.

-spec system_allowed_features() -> ne_binaries().
system_allowed_features() ->
    lager:debug("allowed number features fetched from system config"),
    case ?SYSTEM_PROVIDERS of
        'undefined' -> ?FEATURES_ALLOWED_SYSTEM;
        Providers -> ?FEATURES_ALLOWED_SYSTEM(Providers)
    end.

-spec number_allowed_features(feature_parameters()) -> ne_binaries().
number_allowed_features(#feature_parameters{allowed_features = AllowedFeatures}) ->
    lager:debug("allowed number features set on number document: ~p", [AllowedFeatures]),
    AllowedFeatures.

-spec list_denied_features(feature_parameters()) -> ne_binaries().
list_denied_features(Parameters) ->
    case number_denied_features(Parameters) of
        [] ->
            reseller_denied_features(Parameters)
                ++ used_by_denied_features(Parameters);
        NumberDenied -> NumberDenied
    end.

-spec reseller_denied_features(feature_parameters()) -> ne_binaries().
reseller_denied_features(#feature_parameters{assigned_to = 'undefined'}) ->
    lager:debug("denying external number features for unassigned number"),
    ?EXTERNAL_NUMBER_FEATURES;
reseller_denied_features(#feature_parameters{assigned_to = AccountId}=Parameters) ->
    case ?FEATURES_DENIED_RESELLER(AccountId) of
        'undefined' -> local_denied_features(Parameters);
        Providers ->
            lager:debug("denied number features set on reseller for ~s: ~p", [AccountId, Providers]),
            Providers
    end.

-spec local_denied_features(feature_parameters()) -> ne_binaries().
local_denied_features(#feature_parameters{is_local = 'false'}) -> [];
local_denied_features(#feature_parameters{is_local = 'true'}) ->
    lager:debug("denying external number features for local number"),
    ?EXTERNAL_NUMBER_FEATURES.

-spec used_by_denied_features(feature_parameters()) -> ne_binaries().
used_by_denied_features(#feature_parameters{used_by = <<"trunkstore">>}) -> [];
used_by_denied_features(#feature_parameters{used_by = UsedBy}) ->
    lager:debug("denying external number features for number used by ~s", [UsedBy]),
    [?FEATURE_FAILOVER].

-spec number_denied_features(feature_parameters()) -> ne_binaries().
number_denied_features(#feature_parameters{denied_features = DeniedFeatures}) ->
    lager:debug("denied number features set on number document: ~p", [DeniedFeatures]),
    DeniedFeatures.

-spec legacy_provider_to_feature(ne_binary()) -> ne_binary().
legacy_provider_to_feature(<<"wnm_", Rest/binary>>) -> legacy_provider_to_feature(Rest);
legacy_provider_to_feature(<<"knm_", Rest/binary>>) -> legacy_provider_to_feature(Rest);
legacy_provider_to_feature(<<"cnam_notifier">>) -> ?FEATURE_CNAM;
legacy_provider_to_feature(?LEGACY_DASH_E911) -> ?FEATURE_E911;
legacy_provider_to_feature(<<"port_notifier">>) -> ?FEATURE_PORT;
legacy_provider_to_feature(<<"telnyx_cnam">>) -> ?FEATURE_CNAM;
legacy_provider_to_feature(?LEGACY_TELNYX_E911) -> ?FEATURE_E911;
legacy_provider_to_feature(<<"vitelity_cnam">>) -> ?FEATURE_CNAM;
legacy_provider_to_feature(?LEGACY_VITELITY_E911) -> ?FEATURE_E911;
legacy_provider_to_feature(Else) -> Else.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec requested_modules(knm_number:knm_number()) -> ne_binaries().
requested_modules(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AccountId = knm_phone_number:assigned_to(PhoneNumber),
    RequestedFeatures = kz_json:get_keys(knm_phone_number:doc(PhoneNumber)),
    ExistingFeatures = knm_phone_number:features_list(PhoneNumber),
    provider_modules(RequestedFeatures ++ ExistingFeatures, AccountId).

-spec allowed_modules(knm_number:knm_number()) -> ne_binaries().
allowed_modules(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AccountId = knm_phone_number:assigned_to(PhoneNumber),
    provider_modules(available_features(PhoneNumber), AccountId).

-spec provider_modules(ne_binaries(), api_ne_binary()) -> ne_binaries().
provider_modules(Features, MaybeAccountId) ->
    lists:usort(
      [provider_module(Feature, MaybeAccountId)
       || Feature <- Features
      ]).

-spec provider_module(ne_binary(), api_ne_binary()) -> ne_binary().
provider_module(?FEATURE_CNAM, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    cnam_provider(AccountId);
provider_module(?FEATURE_CNAM_INBOUND, AccountId) ->
    provider_module(?FEATURE_CNAM, AccountId);
provider_module(?FEATURE_CNAM_OUTBOUND, AccountId) ->
    provider_module(?FEATURE_CNAM, AccountId);
provider_module(?FEATURE_E911, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    e911_provider(AccountId);
provider_module(?FEATURE_PREPEND, _) ->
    <<"knm_prepend">>;
provider_module(?FEATURE_PORT, _) ->
    <<"knm_port_notifier">>;
provider_module(?FEATURE_FAILOVER, _) ->
    <<"knm_failover">>;
provider_module(Other, _) ->
    lager:debug("unmatched feature provider ~p, allowing", [Other]),
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

-spec do_exec(knm_numbers:collection(), exec_action()) -> knm_numbers:collection().
do_exec(T0=#{todo := Ns}, Action) ->
    F = fun (N, T) ->
                case knm_number:attempt(fun exec/2, [N, Action]) of
                    {ok, NewN} -> knm_numbers:ok(NewN, T);
                    {error, R} -> knm_numbers:ko(N, R, T)
                end
        end,
    lists:foldl(F, T0, Ns).

-spec exec(knm_number:knm_number(), exec_action()) -> knm_number:knm_number().
-spec exec(knm_number:knm_number(), exec_action(), ne_binaries()) ->
                  knm_number:knm_number().

exec(Number, Action=delete) ->
    RequestedModules = requested_modules(Number),
    lager:debug("requested number features: ~p", [RequestedModules]),
    exec(Number, Action, RequestedModules);
exec(Number, Action) ->
    RequestedModules = requested_modules(Number),
    lager:debug("requested number features: ~p", [RequestedModules]),
    AllowedModules = allowed_modules(Number),
    Filter = fun (Feature) -> lists:member(Feature, AllowedModules) end,
    {AllowedRequests, DeniedRequests} = lists:partition(Filter, RequestedModules),
    lager:debug("allowing number features ~p", [AllowedRequests]),
    Number1 = exec(Number, Action, AllowedRequests),
    lager:debug("denied number features ~p", [DeniedRequests]),
    exec(Number1, delete, DeniedRequests).

exec(Number, _, []) -> Number;
exec(Number, Action, [Provider|Providers]) ->
    case apply_action(Number, Action, Provider) of
        'false' -> exec(Number, Action, Providers);
        {'true', UpdatedNumber} ->
            exec(UpdatedNumber, Action, Providers)
    end.

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
