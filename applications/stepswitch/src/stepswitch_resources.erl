%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_resources).

-export([get_props/0, get_props/1, get_props/2]).
-export([endpoints/2]).
-export([reverse_lookup/1]).

-export([fetch_global_resources/0
        ,fetch_local_resources/1
        ]).
-export([maybe_add_proxies/3]).
-export([gateways_to_endpoints/4]).
-export([check_diversion_fields/1]).

-export([get_resrc_id/1
        ,get_resrc_rev/1
        ,get_resrc_name/1
        ,get_resrc_weight/1
        ,get_resrc_grace_period/1
        ,get_resrc_flags/1
        ,get_resrc_rules/1
        ,get_resrc_raw_rules/1
        ,get_resrc_cid_rules/1
        ,get_resrc_cid_raw_rules/1
        ,get_resrc_gateways/1
        ,get_resrc_is_emergency/1
        ,get_resrc_require_flags/1
        ,get_resrc_ignore_flags/1
        ,get_resrc_global/1
        ,get_resrc_format_from_uri/1
        ,get_resrc_from_uri_realm/1
        ,get_resrc_from_account_realm/1
        ,get_resrc_fax_option/1
        ,get_resrc_codecs/1
        ,get_resrc_bypass_media/1
        ,get_resrc_formatters/1
        ,get_resrc_proxies/1
        ,get_resrc_selector_marks/1
        ,get_resrc_classifier/1
        ,get_resrc_classifier_enable/1
        ]).

-export([set_resrc_id/2
        ,set_resrc_rev/2
        ,set_resrc_name/2
        ,set_resrc_weight/2
        ,set_resrc_grace_period/2
        ,set_resrc_flags/2
        ,set_resrc_rules/2
        ,set_resrc_raw_rules/2
        ,set_resrc_cid_rules/2
        ,set_resrc_cid_raw_rules/2
        ,set_resrc_gateways/2
        ,set_resrc_is_emergency/2
        ,set_resrc_require_flags/2
        ,set_resrc_ignore_flags/2
        ,set_resrc_global/2
        ,set_resrc_format_from_uri/2
        ,set_resrc_from_uri_realm/2
        ,set_resrc_from_account_realm/2
        ,set_resrc_fax_option/2
        ,set_resrc_codecs/2
        ,set_resrc_bypass_media/2
        ,set_resrc_formatters/2
        ,set_resrc_proxies/2
        ,set_resrc_selector_marks/2
        ]).


-ifdef(TEST).
-export([sip_invite_parameters/2
        ,dynamic_sip_invite_parameters/2
        ,gateway_from_jobj/2
        ,resource_from_jobj/1
        ]).
-endif.

-include("stepswitch.hrl").

-define(CONFIG_CAT, <<"number_manager">>).

-define(DEFAULT_ROUTE, kapps_config:get_binary(?SS_CONFIG_CAT, <<"default_route">>)).
-define(DEFAULT_PREFIX, kapps_config:get_binary(?SS_CONFIG_CAT, <<"default_prefix">>, <<>>)).
-define(DEFAULT_SUFFIX, kapps_config:get_binary(?SS_CONFIG_CAT, <<"default_suffix">>, <<>>)).
-define(DEFAULT_CALLER_ID_TYPE,
        kapps_config:get_binary(?SS_CONFIG_CAT, <<"default_caller_id_type">>, <<"external">>)).
-define(DEFAULT_PROGRESS_TIMEOUT,
        kapps_config:get_integer(?SS_CONFIG_CAT, <<"default_progress_timeout">>, 8)).
-define(DEFAULT_WEIGHT,
        kapps_config:get_integer(?SS_CONFIG_CAT, <<"default_weight">>, 3)).
-define(DEFAULT_RTCP_MUX, kapps_config:get_binary(?SS_CONFIG_CAT, <<"default_rtcp_mux">>)).

-record(gateway, {server :: kz_term:api_binary()
                 ,port :: kz_term:api_integer()
                 ,realm :: kz_term:api_binary()
                 ,username :: kz_term:api_binary()
                 ,password :: kz_term:api_binary()
                 ,route :: kz_term:api_binary()
                 ,prefix = <<>> :: binary()
                 ,suffix = <<>> :: binary()
                 ,codecs = [] :: kz_term:ne_binaries()
                 ,bypass_media = 'false' :: boolean()
                 ,rtcp_mux = <<>> :: binary()
                 ,caller_id_type = <<"external">> :: kz_term:ne_binary()
                 ,fax_option :: kz_term:ne_binary() | boolean()
                 ,ccvs :: kz_term:api_object()
                 ,sip_headers :: kz_term:api_object()
                 ,sip_interface :: kz_term:api_binary()
                 ,progress_timeout = 8 :: 1..100
                 ,invite_format = <<"route">> :: kz_term:ne_binary()
                 ,endpoint_type = <<"sip">> :: kz_term:ne_binary()
                 ,endpoint_options = kz_json:new() :: kz_json:object()
                 ,format_from_uri = 'false' :: boolean()
                 ,from_account_realm = 'false' :: boolean()
                 ,from_uri_realm :: kz_term:api_binary()
                 ,is_emergency = 'false' :: boolean()
                 ,force_port = 'false' :: boolean()
                 ,privacy_method = 'undefined' :: kz_term:api_binary()
                 ,privacy_hide_name = 'false' :: boolean()
                 ,privacy_hide_number = 'false' :: boolean()
                 ,invite_parameters = 'undefined' :: kz_term:api_object()
                 }).

-record(resrc, {id :: kz_term:api_binary()
               ,rev :: kz_term:api_binary()
               ,name :: kz_term:api_binary()
               ,weight = 1 :: 1..100
               ,grace_period = 3 :: non_neg_integer()
               ,flags = [] :: list()
               ,rules = [] :: list()
               ,raw_rules = [] :: list()
               ,cid_rules = [] :: list()
               ,cid_raw_rules = [] :: list()
               ,gateways = [] :: list()
               ,is_emergency = 'false' :: boolean()
               ,require_flags = 'false' :: boolean()
               ,ignore_flags = 'false' :: boolean()
               ,global = 'true' :: boolean()
               ,format_from_uri = 'false' :: boolean()
               ,from_uri_realm :: kz_term:api_binary()
               ,from_account_realm = 'false' :: boolean()
               ,fax_option :: kz_term:ne_binary() | boolean()
               ,codecs = [] :: kz_term:ne_binaries()
               ,bypass_media = 'false' :: boolean()
               ,formatters :: kz_term:api_object()
               ,proxies = [] :: kz_term:proplist()
               ,selector_marks = [] :: [tuple()]
               ,privacy_method = 'undefined' :: kz_term:api_binary()
               ,privacy_hide_name = 'false' :: boolean()
               ,privacy_hide_number = 'false' :: boolean()
               ,classifier = 'undefined' :: kz_term:api_binary()
               ,classifier_enable = 'true' :: boolean()
               ,gateway_strategy = 'sequential' :: gateway_strategy()
               }).

-type gateway_strategy() :: 'sequential' | 'random'.
-type resource() :: #resrc{}.
-type resources() :: [#resrc{}].

-type gateway() :: #gateway{}.
-type gateways() :: [#gateway{}].

-type endpoint() :: kz_json:object().
-type endpoints() :: [endpoint()].

-export_type([resource/0
             ,resources/0
             ,gateway/0
             ,gateways/0
             ,endpoint/0
             ,endpoints/0
             ]).

-compile({'no_auto_import', [get/0, get/1]}).

-spec get_props() -> kz_term:proplists().
get_props() ->
    [resource_to_props(Resource)
     || Resource <- sort_resources(get())
    ].

-spec get_props(kz_term:ne_binary()) -> kz_term:proplist() | 'undefined'.
get_props(ResourceId) ->
    case get_resource(ResourceId) of
        'undefined' -> 'undefined';
        Resource -> resource_to_props(Resource)
    end.

-spec get_props(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:proplist() | 'undefined'.
get_props(_ResourceId, 'undefined') -> 'undefined';
get_props(ResourceId, AccountId) ->
    case get_local_resource(ResourceId, AccountId) of
        'undefined' -> 'undefined';
        Resource -> resource_to_props(Resource)
    end.

-spec resource_to_props(resource()) -> kz_term:proplist().
resource_to_props(#resrc{}=Resource) ->
    props:filter_undefined(
      [{<<"Name">>, Resource#resrc.name}
      ,{<<"ID">>, Resource#resrc.id}
      ,{<<"Rev">>, Resource#resrc.rev}
      ,{<<"Weight">>, Resource#resrc.weight}
      ,{<<"Global">>, Resource#resrc.global}
      ,{<<"Format-From-URI">>, Resource#resrc.format_from_uri}
      ,{<<"From-URI-Realm">>, Resource#resrc.from_uri_realm}
      ,{<<"From-Account-Realm">>, Resource#resrc.from_account_realm}
      ,{<<"Require-Flags">>, Resource#resrc.require_flags}
      ,{<<"Ignore-Flags">>, Resource#resrc.ignore_flags}
      ,{<<"Is-Emergency">>, Resource#resrc.is_emergency}
      ,{<<"T38">>, Resource#resrc.fax_option}
      ,{<<"Bypass-Media">>, Resource#resrc.bypass_media}
      ,{<<"Grace-Period">>, Resource#resrc.grace_period}
      ,{<<"Flags">>, Resource#resrc.flags}
      ,{<<"Codecs">>, Resource#resrc.codecs}
      ,{<<"Rules">>, Resource#resrc.raw_rules}
      ,{<<"Caller-ID-Rules">>, Resource#resrc.cid_raw_rules}
      ,{<<"Formatters">>, Resource#resrc.formatters}
      ,{<<"Privacy-Method">>,  Resource#resrc.privacy_method}
      ,{<<"Privacy-Hide-Name">>, Resource#resrc.privacy_hide_name}
      ,{<<"Privacy-Hide-Number">>, Resource#resrc.privacy_hide_number}
      ]).

-spec sort_resources(resources()) -> resources().
sort_resources(Resources) ->
    lists:sort(fun(#resrc{weight=W1}, #resrc{weight=W2}) ->
                       W1 =< W2
               end, Resources).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec endpoints(kz_term:ne_binary(), kapi_offnet_resource:req()) -> endpoints().
endpoints(Number, OffnetJObj) ->
    case maybe_get_endpoints(Number, OffnetJObj) of
        [] -> [];
        Endpoints -> sort_endpoints(Endpoints)
    end.

-spec maybe_get_endpoints(kz_term:ne_binary(), kapi_offnet_resource:req()) -> endpoints().
maybe_get_endpoints(Number, OffnetJObj) ->
    case kapi_offnet_resource:hunt_account_id(OffnetJObj) of
        'undefined' -> get_global_endpoints(Number, OffnetJObj);
        HuntAccount -> maybe_get_local_endpoints(HuntAccount, Number, OffnetJObj)
    end.

-spec maybe_get_local_endpoints(kz_term:ne_binary(), kz_term:ne_binary(), kapi_offnet_resource:req()) -> endpoints().
maybe_get_local_endpoints(HuntAccount, Number, OffnetJObj) ->
    AccountId = kapi_offnet_resource:account_id(OffnetJObj),
    case kzd_accounts:is_in_account_hierarchy(HuntAccount, AccountId, 'true') of
        'false' ->
            lager:info("account ~s attempted to use local resources of ~s, but it is not allowed"
                      ,[AccountId, HuntAccount]
                      ),
            [];
        'true' ->
            lager:info("account ~s is using the local resources of ~s", [AccountId, HuntAccount]),
            get_local_endpoints(HuntAccount, Number, OffnetJObj)
    end.

-spec get_local_endpoints(kz_term:ne_binary(), kz_term:ne_binary(), kapi_offnet_resource:req()) -> endpoints().
get_local_endpoints(AccountId, Number, OffnetJObj) ->
    lager:debug("attempting to find local resources for ~s", [AccountId]),
    Flags = kapi_offnet_resource:flags(OffnetJObj, []),
    Resources = filter_resources(Flags, get(AccountId)),
    resources_to_endpoints(Resources, Number, OffnetJObj).

-spec get_global_endpoints(kz_term:ne_binary(), kapi_offnet_resource:req()) -> endpoints().
get_global_endpoints(Number, OffnetJObj) ->
    lager:debug("attempting to find global resources"),
    Flags = kapi_offnet_resource:flags(OffnetJObj, []),
    Resources = filter_resources(Flags, get()),
    resources_to_endpoints(Resources, Number, OffnetJObj).

-spec sort_endpoints(endpoints()) -> endpoints().
sort_endpoints(Endpoints) ->
    lists:sort(fun endpoint_ordering/2, Endpoints).

-spec endpoint_ordering(endpoint(), endpoint()) -> boolean().
endpoint_ordering(P1, P2) ->
    kz_json:get_integer_value(<<"Weight">>, P1, 1)
        =< kz_json:get_integer_value(<<"Weight">>, P2, 1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reverse_lookup(kz_json:object()) ->
          {'ok', kz_term:proplist()} |
          {'error', 'not_found'}.
reverse_lookup(JObj) ->
    Realm = stepswitch_util:get_realm(JObj),
    IP = kz_json:get_first_defined([<<"From-Network-Addr">>
                                   ,<<"Orig-IP">>
                                   ], JObj),
    Port = find_port(JObj),
    case maybe_find_global(IP, Port, Realm) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            AccountId = find_account_id(Realm, JObj),
            maybe_find_local(IP, Port, Realm, AccountId)
    end.

-spec find_port(kz_json:object()) -> kz_term:api_integer().
find_port(JObj) ->
    case kz_json:get_first_defined([<<"From-Network-Port">>
                                   ,<<"Orig-Port">>
                                   ], JObj)
    of
        'undefined' -> 'undefined';
        Port -> kz_term:to_integer(Port)
    end.

-spec find_account_id(kz_term:api_binary(), kz_json:object()) -> kz_term:api_binary().
find_account_id(Realm, JObj) ->
    case kz_json:get_first_defined([<<"Account-ID">>
                                   ,?CCV(<<"Account-ID">>)
                                   ], JObj)
    of
        'undefined' -> find_account_id(Realm);
        AccountId -> AccountId
    end.

-spec find_account_id(kz_term:api_binary()) -> kz_term:api_binary().
find_account_id('undefined') -> 'undefined';
find_account_id(Realm) ->
    case kapps_util:get_account_by_realm(Realm) of
        {'error', 'not_found'} -> 'undefined';
        {'ok', AccountId} -> AccountId
    end.

-spec maybe_find_global(kz_term:api_binary(), kz_term:api_integer(), kz_term:api_binary()) ->
          {'ok', kz_term:proplist()} |
          {'error', 'not_found'}.
maybe_find_global(IP, Port, Realm) ->
    search_resources(IP, Port, Realm, get()).

-spec maybe_find_local(kz_term:api_binary(), kz_term:api_integer(), kz_term:api_binary(), kz_term:api_binary()) ->
          {'ok', kz_term:proplist()} |
          {'error', 'not_found'}.
maybe_find_local(_, _, _, 'undefined') -> {'error', 'not_found'};
maybe_find_local(IP, Port, Realm, AccountId) ->
    search_resources(IP, Port, Realm, get(AccountId)).

-spec search_resources(kz_term:api_binary(), kz_term:api_integer(), kz_term:api_binary(), resources()) ->
          {'ok', kz_term:proplist()} |
          {'error', 'not_found'}.
search_resources(_IP, _Port, _Realm, []) ->
    lager:debug("failed to find matching resource for ~s:~p(~s)", [_IP, _Port, _Realm]),
    {'error', 'not_found'};
search_resources(IP, Port, Realm, [#resrc{id=Id
                                         ,gateways=Gateways
                                         ,global=Global
                                         }
                                   | Resources
                                  ]) ->
    case search_gateways(IP, Port, Realm, Gateways) of
        {'error', 'not_found'} ->
            search_resources(IP, Port, Realm, Resources);
        #gateway{realm=GatewayRealm
                ,username=Username
                ,password=Password
                ,fax_option=FaxOption
                } ->
            Props = props:filter_undefined(
                      [{'resource_id', Id}
                      ,{'global', Global}
                      ,{'realm', GatewayRealm}
                      ,{'username', Username}
                      ,{'password', Password}
                      ,{'fax_option', FaxOption}
                      ]),
            {'ok', Props}
    end.

-spec search_gateways(kz_term:api_binary(), kz_term:api_integer(), kz_term:api_binary(), gateways()) ->
          gateway() |
          {'error', 'not_found'}.
search_gateways(_, _, _, []) -> {'error', 'not_found'};
search_gateways(IP, Port, Realm, [Gateway | Gateways]) ->
    case search_gateway(IP, Port, Realm, Gateway) of
        {'error', 'not_found'} -> search_gateways(IP, Port, Realm, Gateways);
        #gateway{}=Gateway -> Gateway
    end.

-spec search_gateway(kz_term:api_binary(), kz_term:api_integer(), kz_term:api_binary(), gateway()) ->
          gateway() |
          {'error', 'not_found'}.
search_gateway(IP, Port, _, #gateway{server=IP
                                    ,port=Port
                                    ,force_port='true'
                                    }=Gateway
              ) when IP =/= 'undefined', Port =/= 'undefined' ->
    Gateway;
search_gateway(IP, _, _, #gateway{server=IP
                                 ,force_port='false'
                                 }=Gateway
              ) when IP =/= 'undefined' ->
    Gateway;
search_gateway(IP, _, _, #gateway{realm=IP
                                 ,force_port='false'
                                 }=Gateway
              ) when IP =/= 'undefined' ->
    Gateway;
search_gateway(_, _, Realm, #gateway{realm=Realm
                                    ,force_port='false'
                                    }=Gateway
              ) when Realm =/= 'undefined' ->
    Gateway;
search_gateway(_, _, Realm, #gateway{server=Realm
                                    ,force_port='false'
                                    }=Gateway
              ) when Realm =/= 'undefined' ->
    Gateway;
search_gateway(_, _, _, _) ->
    {'error', 'not_found'}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec filter_resources(kz_term:ne_binaries(), resources()) -> resources().
filter_resources([], Resources) ->
    lager:debug("no flags provided, filtering resources that require flags"),
    [Resource
     || #resrc{require_flags=RequireFlags}=Resource <- Resources,
        not RequireFlags
    ];
filter_resources(Flags, Resources) ->
    lager:debug("filtering resources by flags"),
    filter_resources(Flags, Resources, []).

-spec filter_resources(kz_term:ne_binaries(), resources(), resources()) -> resources().
filter_resources(_, [], Filtered) -> Filtered;
filter_resources(Flags, [Resource|Resources], Filtered) ->
    case resource_has_flags(Flags, Resource) of
        'false' -> filter_resources(Flags, Resources, Filtered);
        'true' -> filter_resources(Flags, Resources, [Resource | Filtered])
    end.

-spec resource_has_flags(kz_term:ne_binaries(), resource()) -> boolean().
resource_has_flags(Flags, Resource) ->
    HasFlag = fun(Flag) -> resource_has_flag(Flag, Resource) end,
    lists:all(HasFlag, Flags).

-spec resource_has_flag(kz_term:ne_binary(), resource()) -> boolean().
resource_has_flag(_, #resrc{ignore_flags='true', id=_Id}) ->
    lager:debug("resource ~s is used regardless of flags", [_Id]),
    'true';
resource_has_flag(Flag, #resrc{flags=ResourceFlags, id=_Id}) ->
    case kz_term:is_empty(Flag)
        orelse lists:member(Flag, ResourceFlags)
    of
        'true' -> 'true';
        'false' ->
            lager:debug("resource ~s does not have the required flag: ~s", [_Id, Flag]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resources_to_endpoints(resources(), kz_term:ne_binary(), kapi_offnet_resource:req()) ->
          kz_json:objects().
resources_to_endpoints(Resources, Number, OffnetJObj) ->
    ResourceMap = lists:foldl(fun resource_classifier_map/2, maps:new(), Resources),
    Classification = knm_converters:classify(Number),
    case maps:get(Classification, ResourceMap, 'undefined') of
        'undefined' ->
            %% No endpoints found based on resources with classifiers, look up by
            %% resources that doesn't have classifier
            lager:debug("no resources satisfy classifier look up, matching against resource rules..."),
            RuleResources = maps:get('no_classification', ResourceMap, []),
            build_endpoints_from_resources(RuleResources, Number, OffnetJObj);
        ClassifiedResources ->
            lager:debug("found resources to satisfy classifier ~s (number ~s), building against classification rules..."
                       ,[Classification, Number]
                       ),
            FilteredClassifiedResources = [Resource || Resource <- ClassifiedResources,
                                                       is_enabled_classifier(Resource)
                                          ],
            build_endpoints_from_resources(FilteredClassifiedResources, Number, OffnetJObj)
    end.

-spec is_enabled_classifier(resource()) -> boolean().
is_enabled_classifier(#resrc{id=Id
                            ,name=Name
                            ,classifier=Classifier
                            ,classifier_enable='false'
                            }
                     ) ->
    lager:debug("resource ~s (id ~s) classifier ~s is disabled, skipping", [Name, Id, Classifier]),
    'false';
is_enabled_classifier(_) -> 'true'.

-spec resource_classifier_map(resource(), map()) -> map().
resource_classifier_map(Resource, Map) ->
    Classification = case get_resrc_classifier(Resource) of
                         'undefined' -> 'no_classification';
                         C -> C
                     end,
    Resources = maps:get(Classification, Map, []),
    maps:put(Classification, [Resource|Resources], Map).


-spec build_endpoints_from_resources(resources(), kz_term:ne_binary(), kapi_offnet_resource:req()) -> kz_json:objects().
build_endpoints_from_resources(Resources, Number, OffnetJObj) ->
    build_endpoints_from_resources(Resources, Number, OffnetJObj, []).

-spec build_endpoints_from_resources(resources(), kz_term:ne_binary(), kapi_offnet_resource:req(), kz_json:objects()) -> kz_json:objects().
build_endpoints_from_resources([], _Number, _OffnetJObj, Endpoints) -> Endpoints;
build_endpoints_from_resources([Resource|Resources], Number, OffnetJObj, Endpoints) ->
    case maybe_resource_to_endpoints(Resource, Number, OffnetJObj, Endpoints) of
        {'error', _} -> build_endpoints_from_resources(Resources, Number, OffnetJObj, Endpoints);
        MoreEndpoints -> build_endpoints_from_resources(Resources, Number, OffnetJObj, MoreEndpoints)
    end.

-spec maybe_resource_to_endpoints(resource(), kz_term:ne_binary(), kapi_offnet_resource:req(), kz_json:objects()) ->
          {'error', 'no_match'} |
          kz_json:objects().
maybe_resource_to_endpoints(#resrc{id=Id
                                  ,name=Name
                                  ,rules=Rules
                                  ,cid_rules=CallerIdRules
                                  ,global=Global
                                  ,weight=Weight
                                  ,proxies=Proxies
                                  ,classifier=Classifier
                                  } = Resource
                           ,Number
                           ,OffnetJObj
                           ,Endpoints
                           ) ->
    CallerIdNumber = case ?RULES_HONOR_DIVERSION of
                         'false' -> kapi_offnet_resource:outbound_caller_id_number(OffnetJObj);
                         'true' -> check_diversion_fields(OffnetJObj)
                     end,
    case filter_resource_by_rules(Id, Number, Rules, CallerIdNumber, CallerIdRules) of
        {'error', 'no_match'}=Error -> Error;
        {'ok', NumberMatch} ->
            _MaybeClassifier = case kz_term:is_ne_binary(Classifier) of
                                   'true' -> Classifier;
                                   'false' -> <<"no_classifier">>
                               end,
            lager:debug("building resource ~s endpoints (classifier ~s)", [Id, _MaybeClassifier]),
            CCVUpdates = props:filter_empty(
                           [{<<"Global-Resource">>, Global}
                           ,{<<"Resource-ID">>, Id}
                           ,{<<"E164-Destination">>, Number}
                           ,{<<"E164-Origination">>, stepswitch_util:convert_to_e164_format(CallerIdNumber, 'undefined')}
                           ,{<<"DID-Classifier">>, knm_converters:classify(Number)}
                           ,{<<"Original-Number">>, kapi_offnet_resource:to_did(OffnetJObj)}
                           ]),
            Updates = [{<<"Name">>, Name}
                      ,{<<"Weight">>, Weight}
                      ],
            Gateways = get_resrc_gateways(Resource),

            EndpointList = [update_endpoint(Endpoint, Updates, CCVUpdates)
                            || Endpoint <- gateways_to_endpoints(NumberMatch, Gateways, OffnetJObj, [])
                           ],
            maybe_add_proxies(EndpointList, Proxies, Endpoints)
    end.

-spec check_diversion_fields(kapi_offnet_resource:req()) -> kz_term:ne_binary().
check_diversion_fields(OffnetJObj) ->
    case kapi_offnet_resource:custom_sip_header(OffnetJObj, <<"Diversions">>) of
        [Diversion|_] ->
            [_,CallerIdNumber,_] = binary:split(Diversion, [<<":">>,<<"@">>], ['global']),
            CallerIdNumber;
        _ ->
            kapi_offnet_resource:outbound_caller_id_number(OffnetJObj)
    end.

-spec update_endpoint(kz_json:object(), kz_term:proplist(), kz_term:proplist()) ->
          kz_json:object().
update_endpoint(Endpoint, Updates, CCVUpdates) ->
    kz_json:set_values(Updates, update_ccvs(Endpoint, CCVUpdates)).

-spec maybe_add_proxies(kz_json:objects(), kz_term:proplist(), kz_json:objects()) -> kz_json:objects().
maybe_add_proxies([], _, Acc) -> Acc;
maybe_add_proxies(Endpoints, [], Acc) -> Acc ++ Endpoints;
maybe_add_proxies([Endpoint | Endpoints], Proxies, Acc) ->
    EPs = [add_proxy(Endpoint, Proxy)  || Proxy <- Proxies],
    maybe_add_proxies(Endpoints, Proxies, Acc ++ EPs).

-spec add_proxy(kz_json:object(), {binary(), binary()}) -> kz_json:object().
add_proxy(Endpoint, {Zone, IP}) ->
    Updates = [{<<"Proxy-Zone">>, Zone}
              ,{<<"Proxy-IP">>, IP}
              ],
    kz_json:set_values(Updates ,Endpoint).

-spec filter_resource_by_rules(kz_term:ne_binary(), kz_term:ne_binary(), re:mp(), kz_term:ne_binary(), re:mp()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'no_match'}.
filter_resource_by_rules(Id, Number, Rules, CallerIdNumber, CallerIdRules) ->
    case evaluate_rules(Rules, Number) of
        {'error', 'no_match'} ->
            lager:debug("resource ~s does not match request, skipping", [Id]),
            {'error','no_match'};
        {'ok', Match} ->
            filter_resource_by_match(Id, Number, CallerIdNumber, CallerIdRules, Match)
    end.

-spec filter_resource_by_match(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), re:mp(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'no_match'}.
filter_resource_by_match(Id, Number, CallerIdNumber, CallerIdRules, Match) ->
    case evaluate_cid_rules(CallerIdRules, CallerIdNumber) of
        {'ok', 'empty_rules'} ->
            lager:debug("resource ~s matches number '~s' with regex match '~s'", [Id, Number, Match]),
            {'ok', Match};
        {'ok', _CIDMatch} ->
            lager:debug("resource ~s matches number '~s' with regex match '~s' and caller id match '~s'"
                       ,[Id, Number, Match, _CIDMatch]
                       ),
            {'ok', Match};
        {'error', 'no_match'} ->
            lager:debug("resource ~s does not match caller id number '~s', skipping", [Id, CallerIdNumber]),
            {'error','no_match'}
    end.

-spec update_ccvs(kz_json:object(), kz_term:proplist()) -> kz_json:object().
update_ccvs(Endpoint, Updates) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, kz_json:new()),
    kz_json:set_value(<<"Custom-Channel-Vars">>, kz_json:set_values(Updates, CCVs), Endpoint).

-spec evaluate_rules(re:mp(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'no_match'}.
evaluate_rules([], _) -> {'error', 'no_match'};
evaluate_rules([Rule|Rules], Number) ->
    case re:run(Number, Rule) of
        {'match', [{Start,End}]} ->
            {'ok', binary:part(Number, Start, End)};
        {'match', CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            {'ok', binary:part(Number, Start, End)};
        _ ->
            evaluate_rules(Rules, Number)
    end.

-spec evaluate_cid_rules(re:mp(), kz_term:ne_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'ok', 'empty_rules'} | %% empty rules, it`s ok, allow any number
          {'error', 'no_match'}.
evaluate_cid_rules([], _) -> {'ok','empty_rules'};
evaluate_cid_rules(CIDRules, CIDNumber) -> evaluate_rules(CIDRules, CIDNumber).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec gateways_to_endpoints(kz_term:ne_binary(), gateways(), kapi_offnet_resource:req(), kz_json:objects()) ->
          kz_json:objects().
gateways_to_endpoints(_Number, [], _OffnetJObj, Endpoints) -> Endpoints;
gateways_to_endpoints(Number, [Gateway|Gateways], OffnetJObj, Endpoints) ->
    gateways_to_endpoints(Number
                         ,Gateways
                         ,OffnetJObj
                         ,[gateway_to_endpoint(Number, Gateway, OffnetJObj) | Endpoints]
                         ).

-spec gateway_to_endpoint(kz_term:ne_binary(), gateway(), kapi_offnet_resource:req()) ->
          kz_json:object().
gateway_to_endpoint(DestinationNumber
                   ,#gateway{invite_format=InviteFormat
                            ,caller_id_type=CallerIdType
                            ,bypass_media=BypassMedia
                            ,rtcp_mux=RTCP_MUX
                            ,codecs=Codecs
                            ,username=Username
                            ,password=Password
                            ,realm=AuthRealm
                            ,ccvs=GatewayCCVs
                            ,sip_headers=SipHeaders
                            ,sip_interface=SipInterface
                            ,endpoint_type=EndpointType
                            ,endpoint_options=EndpointOptions
                            ,progress_timeout=ProgressTimeout
                            ,privacy_method = PrivacyMethod
                            ,privacy_hide_name = HideName
                            ,privacy_hide_number = HideNumber
                            }=Gateway
                   ,OffnetJObj
                   ) ->
    IsEmergency = gateway_emergency_resource(Gateway),
    {CIDName, CIDNumber} = default_gateway_cid(OffnetJObj, IsEmergency),

    RequestorCCVs = kz_json:get_ne_json_value(<<"Requestor-Custom-Channel-Vars">>, OffnetJObj, kz_json:new()),

    KVs  = [{<<"Emergency-Resource">>, IsEmergency}
           ,{<<"Matched-Number">>, DestinationNumber}
           ,{<<"Resource-Type">>, <<"offnet-termination">>}
           ,{<<"RTCP-MUX">>, RTCP_MUX}
            | gateway_from_uri_settings(Gateway)
           ],
    CCVs = kz_json:set_values(KVs, GatewayCCVs),

    kz_json:from_list(
      props:filter_empty(
        [{<<"Route">>, gateway_dialstring(Gateway, DestinationNumber)}
        ,{<<"Outbound-Callee-ID-Name">>, kz_term:to_binary(DestinationNumber)}
        ,{<<"Outbound-Callee-ID-Number">>, kz_term:to_binary(DestinationNumber)}
        ,{<<"To-DID">>, kz_term:to_binary(DestinationNumber)}
        ,{<<"Invite-Format">>, InviteFormat}
        ,{<<"Caller-ID-Type">>, CallerIdType}
        ,{<<"Bypass-Media">>, BypassMedia}
        ,{<<"Codecs">>, Codecs}
        ,{<<"Auth-User">>, Username}
        ,{<<"Auth-Password">>, Password}
        ,{<<"Auth-Realm">>, AuthRealm}
        ,{<<"Custom-SIP-Headers">>, SipHeaders}
        ,{<<"SIP-Interface">>, SipInterface}
        ,{<<"Endpoint-Type">>, EndpointType}
        ,{<<"Endpoint-Options">>, EndpointOptions}
        ,{<<"Endpoint-Progress-Timeout">>, kz_term:to_binary(ProgressTimeout)}
        ,{<<"Custom-Channel-Vars">>, CCVs}
        ,{<<"Outbound-Caller-ID-Number">>, CIDNumber}
        ,{<<"Outbound-Caller-ID-Name">>, CIDName}
        ,{<<"SIP-Invite-Parameters">>, sip_invite_parameters(Gateway, OffnetJObj)}
        ,{<<"Privacy-Method">>, PrivacyMethod}
        ,{<<"Privacy-Hide-Name">>
         ,HideName
          orelse kz_privacy:should_hide_name(OffnetJObj)
          orelse kz_privacy:should_hide_name(RequestorCCVs)
         }
        ,{<<"Privacy-Hide-Number">>
         ,HideNumber
          orelse kz_privacy:should_hide_number(OffnetJObj)
          orelse kz_privacy:should_hide_number(RequestorCCVs)
         }
         | maybe_get_t38(Gateway, OffnetJObj)
        ])).

-spec sip_invite_parameters(gateway(), kapi_offnet_resource:req()) -> kz_term:ne_binaries().
sip_invite_parameters(Gateway, OffnetJObj) ->
    static_sip_invite_parameters(Gateway)
        ++ dynamic_sip_invite_parameters(Gateway, OffnetJObj).

-spec static_sip_invite_parameters(gateway()) -> kz_term:ne_binaries().
static_sip_invite_parameters(#gateway{invite_parameters='undefined'}) -> [];
static_sip_invite_parameters(#gateway{invite_parameters=Parameters}) ->
    kz_json:get_list_value(<<"static">>, Parameters, []).

-spec dynamic_sip_invite_parameters(gateway(), kapi_offnet_resource:req()) ->
          kz_term:ne_binaries().
dynamic_sip_invite_parameters(#gateway{invite_parameters='undefined'}, _) -> [];
dynamic_sip_invite_parameters(#gateway{invite_parameters=Parameters}, OffnetJObj) ->
    CCVs = kz_json:normalize(kapi_offnet_resource:requestor_custom_channel_vars(OffnetJObj, kz_json:new())),
    CSHs = kz_json:normalize(kapi_offnet_resource:requestor_custom_sip_headers(OffnetJObj, kz_json:new())),
    Keys = kz_json:get_list_value(<<"dynamic">>, Parameters, []),
    dynamic_sip_invite_parameters(Keys, CCVs, CSHs).

-spec dynamic_sip_invite_parameters(kz_term:api_binaries(), kz_json:object(), kz_json:object()) -> kz_term:ne_binaries().
dynamic_sip_invite_parameters(Keys, CCVs, CSHs) ->
    dynamic_sip_invite_parameters(Keys, CCVs, CSHs, []).

-spec dynamic_sip_invite_parameters(kz_term:api_binaries(), kz_json:object(), kz_json:object(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
dynamic_sip_invite_parameters([], _, _, Parameters) -> Parameters;
dynamic_sip_invite_parameters([Key|Keys], CCVs, CSHs, Parameters) when is_binary(Key) ->
    case dynamic_sip_invite_value(Key, CCVs, CSHs) of
        'undefined' -> dynamic_sip_invite_parameters(Keys, CCVs, CSHs, Parameters);
        Parameter ->
            lager:debug("adding dynamic invite parameter ~s", [Parameter]),
            dynamic_sip_invite_parameters(Keys, CCVs, CSHs, [Parameter|Parameters])
    end;
dynamic_sip_invite_parameters([JObj|Keys], CCVs, CSHs, Parameters) ->
    'true' = kz_json:is_json_object(JObj),
    Key = kz_json:get_ne_value(<<"key">>, JObj),
    Tag = kz_json:get_ne_value(<<"tag">>, JObj),
    Separator = kz_json:get_first_defined([<<"separator">>, <<"seperator">>], JObj, <<"=">>),
    case dynamic_sip_invite_value(Key, CCVs, CSHs) of
        'undefined' -> dynamic_sip_invite_parameters(Keys, CCVs, CSHs, Parameters);
        Value ->
            Parameter = erlang:iolist_to_binary([Tag, Separator, Value]),
            lager:debug("adding dynamic invite parameter ~s", [Parameter]),
            dynamic_sip_invite_parameters(Keys, CCVs, CSHs, [Parameter|Parameters])
    end.

-spec dynamic_sip_invite_value(kz_term:ne_binary(), kz_json:object(), kz_json:object()) -> kz_term:api_binary().
dynamic_sip_invite_value(<<"zone">>, _, _) ->
    kz_term:to_binary(kz_nodes:local_zone());
dynamic_sip_invite_value(<<"custom_sip_headers.", Key/binary>>, _, CSHs) ->
    kz_json:get_ne_binary_value(Key, CSHs);
dynamic_sip_invite_value(<<"custom_channel_vars.", Key/binary>>, CCVs, _) ->
    kz_json:get_ne_binary_value(Key, CCVs);
dynamic_sip_invite_value(_, _, _) -> 'undefined'.

-spec default_gateway_cid(kapi_offnet_resource:req(), kz_term:api_binary()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
default_gateway_cid(OffnetJObj, 'undefined') ->
    {kapi_offnet_resource:outbound_caller_id_name(OffnetJObj)
    ,kapi_offnet_resource:outbound_caller_id_number(OffnetJObj)
    };
default_gateway_cid(OffnetJObj, <<"true">>) ->
    {stepswitch_bridge:bridge_emergency_cid_name(OffnetJObj)
    ,stepswitch_bridge:bridge_emergency_cid_number(OffnetJObj)
    }.

-spec gateway_from_uri_settings(gateway()) -> kz_term:proplist().
gateway_from_uri_settings(#gateway{format_from_uri='false'}) ->
    [{<<"Format-From-URI">>, 'false'}];
gateway_from_uri_settings(#gateway{format_from_uri='true'
                                  ,from_uri_realm=FromRealm
                                  ,realm=Realm
                                  ,from_account_realm=AccountRealm
                                  }) ->
    %% precedence: from_uri_realm -> from_account_realm -> realm
    case kz_term:is_empty(FromRealm) of
        'false' ->
            lager:debug("using resource from_uri_realm in From: ~s", [FromRealm]),
            [{<<"Format-From-URI">>, 'true'}
            ,{<<"From-URI-Realm">>, FromRealm}
            ];
        'true' when AccountRealm ->
            lager:debug("using account realm in From", []),
            [{<<"Format-From-URI">>, 'true'}
            ,{<<"From-Account-Realm">>, 'true'}
            ];
        'true' ->
            case kz_term:is_empty(Realm) of
                'true' ->
                    lager:info("format from URI configured for resource but no realm available"),
                    [{<<"Format-From-URI">>, 'false'}];
                'false' ->
                    lager:debug("using gateway realm in From: ~s", [Realm]),
                    [{<<"Format-From-URI">>, 'true'}
                    ,{<<"From-URI-Realm">>, Realm}
                    ]
            end
    end.

-spec maybe_get_t38(gateway(), kapi_offnet_resource:req()) -> kz_term:proplist().
maybe_get_t38(#gateway{fax_option=FaxOption}, OffnetJObj) ->
    Flags = kapi_offnet_resource:flags(OffnetJObj, []),
    case lists:member(<<"fax">>, Flags) of
        'false' -> [];
        'true' ->
            kapps_call_command:get_outbound_t38_settings(FaxOption
                                                        ,kapi_offnet_resource:t38_enabled(OffnetJObj)
                                                        )
    end.

-spec gateway_emergency_resource(gateway()) -> kz_term:api_binary().
gateway_emergency_resource(#gateway{is_emergency='true'}) ->
    lager:debug("gateway is part of an emergency resource"),
    <<"true">>;
gateway_emergency_resource(_) -> 'undefined'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get() -> resources().
get() -> get('undefined').

-spec get(kz_term:api_binary()) -> resources().
get('undefined') ->
    case kz_cache:fetch_local(?CACHE_NAME, 'global_resources') of
        {'ok', Resources} -> Resources;
        {'error', 'not_found'} -> fetch_global_resources()
    end;
get(AccountId) ->
    case kz_cache:fetch_local(?CACHE_NAME, {'local_resources', AccountId}) of
        {'ok', Resources} -> Resources;
        {'error', 'not_found'} -> fetch_local_resources(AccountId)
    end.

-spec get_resource(kz_term:ne_binary()) -> resource() | 'undefined'.
get_resource(ResourceId) ->
    case get('undefined') of
        [] -> 'undefined';
        Resources -> get_resource(ResourceId, Resources)
    end.

get_resource(ResourceId, [#resrc{id=ResourceId}=Resource|_Resources]) ->
    Resource;
get_resource(ResourceId, [#resrc{}|Resources]) ->
    get_resource(ResourceId, Resources);
get_resource(_ResourceId, []) ->
    'undefined'.

-spec get_local_resource(kz_term:ne_binary(), kz_term:ne_binary()) -> resource() | 'undefined'.
get_local_resource(ResourceId, AccountId) ->
    case get(AccountId) of
        [] -> 'undefined';
        Resources -> get_resource(ResourceId, Resources)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_global_resources() -> resources().
-ifdef(TEST).
fetch_global_resources() ->
    {'ok', JObjs} = kz_json:fixture(?APP, <<"fixtures/resources/global.json">>),
    resources_from_jobjs(JObjs).
-else.
fetch_global_resources() ->
    lager:debug("global resource cache miss, fetching from db"),
    ViewOptions = ['include_docs'],
    case kz_datamgr:get_results(?KZ_OFFNET_DB, ?LIST_RESOURCES_BY_ID, ViewOptions) of
        {'error', _R} ->
            lager:warning("unable to fetch global resources: ~p", [_R]),
            [];
        {'ok', JObjs} ->
            CacheProps = [{'origin', [{'db', ?KZ_OFFNET_DB, <<"resource">>}]}],
            Docs = [kz_json:get_json_value(<<"doc">>, JObj) || JObj <- JObjs],
            Resources = resources_from_jobjs(Docs),
            kz_cache:store_local(?CACHE_NAME, 'global_resources', Resources, CacheProps),
            Resources
    end.
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_local_resources(kz_term:ne_binary()) -> resources().
-ifdef(TEST).
fetch_local_resources(AccountId) ->
    {'ok', JObjs} = kz_json:fixture(?APP, <<"fixtures/resources/global.json">>),
    fetch_local_resources(AccountId, JObjs).
-else.
fetch_local_resources(AccountId) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    ViewOptions = ['include_docs'],
    lager:debug("local resource cache miss, fetching from db ~s", [AccountDb]),
    case kz_datamgr:get_results(AccountDb, ?LIST_RESOURCES_BY_ID, ViewOptions) of
        {'error', _R} ->
            lager:warning("unable to fetch local resources from ~s: ~p", [AccountId, _R]),
            [];
        {'ok', JObjs} ->
            LocalResources = fetch_local_resources(AccountId, JObjs),
            CacheProps = [{'origin', [{'db', AccountDb, <<"resource">>}]}],
            kz_cache:store_local(?CACHE_NAME, {'local_resources', AccountId}, LocalResources, CacheProps),
            LocalResources
    end.
-endif.

-spec fetch_local_resources(kz_term:ne_binary(), kz_json:objects()) -> resources().
fetch_local_resources(AccountId, JObjs) ->
    Proxies = fetch_account_dedicated_proxies(AccountId),
    resources_from_jobjs(
      [kz_json:set_values([{<<"Is-Global">>, 'false'}
                          ,{<<"Proxies">>, kz_json:from_list(Proxies)}
                          ]
                         ,kz_json:get_value(<<"doc">>, JObj)
                         )
       || JObj <- JObjs
      ]).

-spec fetch_account_dedicated_proxies(kz_term:api_binary()) -> kz_term:proplist().
fetch_account_dedicated_proxies('undefined') -> [];
fetch_account_dedicated_proxies(AccountId) ->
    UseSingle = kapps_config:get_is_true(?SS_CONFIG_CAT, <<"use_first_dedicated_proxy_only">>, 'true'),
    case kz_ips:assigned(AccountId) of
        {'ok', [IP|_]} when UseSingle -> [build_account_dedicated_proxy(IP)];
        {'ok', IPS} -> [build_account_dedicated_proxy(IP) || IP <- IPS];
        _ -> []
    end.

-spec build_account_dedicated_proxy(kz_json:object()) -> {kz_term:api_binary(), kz_term:api_binary()}.
build_account_dedicated_proxy(Proxy) ->
    Zone = kz_json:get_value(<<"zone">>, Proxy),
    ProxyIP = kz_json:get_value(<<"ip">>, Proxy),
    {Zone, ProxyIP}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec resources_from_jobjs(kzd_resources:docs()) -> resources().
resources_from_jobjs(JObjs) ->
    resources_from_jobjs(JObjs, []).

-spec resources_from_jobjs(kzd_resources:docs(), resources()) -> resources().
resources_from_jobjs([], Resources) -> Resources;
resources_from_jobjs([JObj|JObjs], Resources) ->
    case kzd_resources:enabled(JObj) of
        'false' -> resources_from_jobjs(JObjs, Resources);
        'true' -> resources_from_jobjs(JObjs, create_resource(JObj, Resources))
    end.

-spec create_resource(kzd_resources:doc(), resources()) -> resources().
create_resource(ResourceJObj, Resources) ->
    case kzd_resources:classifiers(ResourceJObj) of
        'undefined' -> [resource_from_jobj(ResourceJObj) | Resources];
        ResourceClassifiers ->
            AvailableClassifiers = kz_json:to_proplist(knm_converters:available_classifiers()),
            create_resource(kz_json:to_proplist(ResourceClassifiers)
                           ,AvailableClassifiers
                           ,ResourceJObj
                           ,Resources
                           )
    end.

-spec create_resource(kz_term:proplist(), kz_term:proplist(), kzd_resources:doc(), resources()) -> resources().
create_resource([], _ConfigClassifiers, _ResourceJObj, Resources) -> Resources;
create_resource([{ResourceClassifier, ResourceClassifierJObj}|ResourceClassifiers]
               ,ConfigClassifiers, ResourceJObj, Resources
               ) ->
    case props:get_value(ResourceClassifier, ConfigClassifiers) of
        'undefined' ->
            create_resource(ResourceClassifiers, ConfigClassifiers, ResourceJObj, Resources);
        ConfigClassifier ->
            JObj =
                create_classifier_resource(ResourceJObj
                                          ,ResourceClassifierJObj
                                          ,ResourceClassifier
                                          ,ConfigClassifier
                                          ),
            create_resource(ResourceClassifiers
                           ,ConfigClassifiers
                           ,ResourceJObj
                           ,[resource_from_jobj(JObj)
                             | Resources
                            ]
                           )
    end.

-spec create_classifier_resource(kzd_resources:doc(), kz_json:object(), kz_term:ne_binary(), kz_term:proplist()) -> kz_json:object().
create_classifier_resource(ResourceJObj, ClassifierJObj, Classifier, ConfigClassifier) ->
    DefaultRegex = kz_json:get_value(<<"regex">>, ConfigClassifier),
    DefaultEmergency = kz_json:is_true(<<"emergency">>, ConfigClassifier, 'undefined'),
    Props =
        props:filter_undefined(
          [{<<"_id">>, <<(kz_doc:id(ResourceJObj))/binary, "-", Classifier/binary>>}
          ,{<<"name">>, <<(kzd_resources:name(ResourceJObj))/binary, " - ", Classifier/binary>>}
          ,{<<"rules">>, [kz_json:get_value(<<"regex">>, ClassifierJObj, DefaultRegex)]}
          ,{<<"weight_cost">>, kz_json:get_value(<<"weight_cost">>, ClassifierJObj)}
          ,{<<"emergency">>, classifier_is_emergency(ClassifierJObj, Classifier, DefaultEmergency)}
          ,{<<"classifier">>, Classifier}
          ,{<<"classifier_enable">>, kz_json:is_true(<<"enabled">>, ClassifierJObj, 'true')}
          ]
         ),
    create_classifier_gateways(kz_json:set_values(Props, ResourceJObj), ClassifierJObj).

-spec create_classifier_gateways(kzd_resources:doc(), kz_json:object()) -> kz_json:object().
create_classifier_gateways(ResourceJObj, ClassifierJObj) ->
    Props =
        props:filter_undefined(
          [{<<"suffix">>, kz_json:get_value(<<"suffix">>, ClassifierJObj)}
          ,{<<"prefix">>, kz_json:get_value(<<"prefix">>, ClassifierJObj)}
          ]
         ),
    Gateways =
        [kz_json:set_values(Props, Gateway)
         || Gateway <- kzd_resources:gateways(ResourceJObj, [])
        ],
    kzd_resources:set_gateways(ResourceJObj, Gateways).

-spec classifier_is_emergency(kz_json:object(), kz_term:ne_binary(), boolean() | 'undefined') -> boolean().
classifier_is_emergency(ClassifierJObj, <<"emergency">>, _DefaultEmergency) ->
    kz_json:is_true(<<"emergency">>, ClassifierJObj, 'true');
classifier_is_emergency(ClassifierJObj, _Classifier, DefaultEmergency) ->
    kz_json:is_true(<<"emergency">>, ClassifierJObj, DefaultEmergency).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type rule() :: re:mp().
-type rules() :: [rule()].

-spec resource_from_jobj(kzd_resources:doc()) -> resource().
resource_from_jobj(JObj) ->
    Resource = #resrc{id=kz_doc:id(JObj)
                     ,rev=kz_doc:revision(JObj)
                     ,name=kzd_resources:name(JObj)
                     ,flags=kzd_resources:flags(JObj, [])
                     ,require_flags=kzd_resources:require_flags(JObj, 'false')
                     ,ignore_flags=kzd_resources:ignore_flags(JObj, 'false')
                     ,format_from_uri=kzd_resources:format_from_uri(JObj, 'false')
                     ,from_uri_realm=kzd_resources:from_uri_realm(JObj)
                     ,from_account_realm=kzd_resources:from_account_realm(JObj)
                     ,fax_option=kzd_resources:media_fax_option(JObj)
                     ,raw_rules=kzd_resources:rules(JObj, [])
                     ,rules=resource_rules(JObj)
                     ,cid_raw_rules=kzd_resources:cid_rules(JObj, [])
                     ,cid_rules=resource_cid_rules(JObj)
                     ,weight=resource_weight(JObj)
                     ,grace_period=resource_grace_period(JObj)
                     ,is_emergency=resource_is_emergency(JObj)
                     ,codecs=resource_codecs(JObj)
                     ,bypass_media=resource_bypass_media(JObj)
                     ,formatters=resource_formatters(JObj)
                     ,global=kz_json:is_true(<<"Is-Global">>, JObj, 'true')
                     ,proxies=kz_json:to_proplist(<<"Proxies">>, JObj)
                     ,privacy_method=kz_privacy:get_method(JObj)
                     ,privacy_hide_name=kz_privacy:should_hide_name(JObj)
                     ,privacy_hide_number=kz_privacy:should_hide_number(JObj)
                     ,classifier=kz_json:get_ne_value(<<"classifier">>, JObj)
                     ,classifier_enable=kz_json:is_true(<<"classifier_enable">>, JObj, 'true')
                     ,gateway_strategy=kz_term:to_atom(kzd_resources:gateway_strategy(JObj, <<"sequential">>), 'true')
                     },
    Gateways = gateways_from_jobjs(kzd_resources:gateways(JObj, []), Resource),
    Resource#resrc{gateways=Gateways}.

-spec resource_bypass_media(kzd_resources:doc()) -> boolean().
resource_bypass_media(JObj) ->
    Default = kapps_config:get_is_true(?SS_CONFIG_CAT, <<"default_bypass_media">>, 'false'),
    kzd_resources:media_bypass_media(JObj, Default).

-spec resource_formatters(kzd_resources:doc()) -> kz_term:api_object().
resource_formatters(JObj) ->
    Default = kapps_config:get(?SS_CONFIG_CAT, <<"default_formatters">>),
    kzd_resources:formatters(JObj, Default).

-spec resource_codecs(kzd_resources:doc()) -> kz_term:ne_binaries().
resource_codecs(JObj) ->
    DefaultAudio = kapps_config:get_ne_binaries(?SS_CONFIG_CAT, <<"default_audio_codecs">>, []),
    DefaultVideo = kapps_config:get_ne_binaries(?SS_CONFIG_CAT, <<"default_video_codecs">>, []),
    case kzd_resources:media_audio_codecs(JObj, DefaultAudio)
        ++ kzd_resources:media_video_codecs(JObj, DefaultVideo)
    of
        [] -> kapps_config:get_ne_binaries(?SS_CONFIG_CAT, <<"default_codecs">>, []);
        Codecs -> Codecs
    end.

-spec resource_rules(kzd_resources:doc()) -> rules().
resource_rules(JObj) ->
    Rules = kzd_resources:rules(JObj, []),
    lager:info("compiling resource rules for ~s / ~s: ~p"
              ,[kz_doc:account_db(JObj, <<"offnet">>), kz_doc:id(JObj), Rules]
              ),
    resource_rules(Rules, []).

-spec resource_rules(kz_term:ne_binaries(), rules()) -> rules().
resource_rules([], CompiledRules) -> CompiledRules;
resource_rules([Rule|Rules], CompiledRules) ->
    case re:compile(Rule) of
        {'ok', CompiledRule} ->
            resource_rules(Rules, [CompiledRule | CompiledRules]);
        {'error', _R} ->
            lager:warning("bad rule '~s': ~p", [Rule, _R]),
            resource_rules(Rules, CompiledRules)
    end.

-spec resource_cid_rules(kzd_resources:doc()) -> rules().
resource_cid_rules(ResourceJObj) ->
    lager:info("compiling caller id rules for ~s / ~s"
              ,[kz_doc:account_db(ResourceJObj, <<"offnet">>), kz_doc:id(ResourceJObj)]
              ),
    Rules = kzd_resources:cid_rules(ResourceJObj, []),
    resource_rules(Rules, []).

-spec resource_grace_period(kzd_resources:doc() | integer()) -> 0..100.
resource_grace_period(JObj) when not is_integer(JObj) ->
    resource_grace_period(kzd_resources:grace_period(JObj, ?DEFAULT_WEIGHT));
resource_grace_period(GracePeriod) when is_integer(GracePeriod), GracePeriod > 100 -> 100;
resource_grace_period(GracePeriod) when is_integer(GracePeriod), GracePeriod < 0 -> 0;
resource_grace_period(GracePeriod) when is_integer(GracePeriod) -> GracePeriod.

-spec resource_weight(kzd_resources:doc() | integer()) -> integer().
resource_weight(JObj) when not is_integer(JObj) ->
    resource_weight(kzd_resources:weight_cost(JObj, ?DEFAULT_WEIGHT));
resource_weight(W) when W > 100 -> 100;
resource_weight(W) when W < 1 -> 1;
resource_weight(W) -> W.

-spec resource_is_emergency(kz_json:object()) -> boolean().
resource_is_emergency(JObj) ->
    kzd_resources:emergency(JObj, 'false')
        orelse (kzd_resources:caller_id_options_type(JObj) =:= <<"emergency">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec gateways_from_jobjs(kz_json:objects(), resource()) -> gateways().
gateways_from_jobjs(JObjs, Resource) ->
    gateways_from_jobjs(JObjs, Resource, []).

-spec gateways_from_jobjs(kz_json:objects(), resource(), gateways()) -> gateways().
gateways_from_jobjs([], _, Gateways) -> Gateways;
gateways_from_jobjs([JObj|JObjs], Resource, Gateways) ->
    case kz_json:is_true(<<"enabled">>, JObj, 'true') of
        'false' -> gateways_from_jobjs(JObjs, Resource, Gateways);
        'true' ->
            G = [gateway_from_jobj(JObj, Resource) | Gateways],
            gateways_from_jobjs(JObjs, Resource, G)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec gateway_from_jobj(kz_json:object(), resource()) -> gateway().
gateway_from_jobj(GatewayJObj, #resrc{is_emergency=IsEmergency
                                     ,format_from_uri=FormatFrom
                                     ,from_uri_realm=FromRealm
                                     ,from_account_realm=FromAccountRealm
                                     ,fax_option=T38
                                     ,codecs=Codecs
                                     ,bypass_media=BypassMedia
                                     ,privacy_method=PrivacyMethod
                                     ,privacy_hide_name=HideName
                                     ,privacy_hide_number=HideNumber
                                     }) ->
    EndpointType = kz_json:get_ne_value(<<"endpoint_type">>, GatewayJObj, <<"sip">>),
    #gateway{endpoint_type = EndpointType
            ,server = kz_json:get_ne_binary_value(<<"server">>, GatewayJObj)
            ,port = kz_json:get_integer_value(<<"port">>, GatewayJObj)
            ,realm = kz_json:get_value(<<"realm">>, GatewayJObj)
            ,username = kz_json:get_value(<<"username">>, GatewayJObj)
            ,password = kz_json:get_value(<<"password">>, GatewayJObj)
            ,ccvs = gateway_ccvs(GatewayJObj)
            ,sip_headers = kz_custom_sip_headers:outbound(kz_json:get_json_value(<<"custom_sip_headers">>, GatewayJObj, kz_json:new()))
            ,sip_interface = kz_json:get_ne_value(<<"custom_sip_interface">>, GatewayJObj)
            ,invite_format = kz_json:get_value(<<"invite_format">>, GatewayJObj, <<"route">>)
            ,format_from_uri = kz_json:is_true(<<"format_from_uri">>, GatewayJObj, FormatFrom)
            ,from_uri_realm = kz_json:get_ne_value(<<"from_uri_realm">>, GatewayJObj, FromRealm)
            ,from_account_realm=kz_json:is_true(<<"from_account_realm">>, GatewayJObj, FromAccountRealm)
            ,is_emergency = gateway_is_emergency(GatewayJObj, IsEmergency)
            ,fax_option = kz_json:is_true([<<"media">>, <<"fax_option">>], GatewayJObj, T38)
            ,codecs = kz_json:get_value(<<"codecs">>, GatewayJObj, Codecs)
            ,bypass_media = kz_json:is_true(<<"bypass_media">>, GatewayJObj, BypassMedia)
            ,force_port = kz_json:is_true(<<"force_port">>, GatewayJObj)
            ,route = kz_json:get_ne_value(<<"route">>, GatewayJObj, ?DEFAULT_ROUTE)
            ,prefix = kz_json:get_binary_value(<<"prefix">>, GatewayJObj, ?DEFAULT_PREFIX)
            ,suffix = kz_json:get_binary_value(<<"suffix">>, GatewayJObj, ?DEFAULT_SUFFIX)
            ,rtcp_mux = kz_json:get_binary_value([<<"media">>, <<"rtcp_mux">>], GatewayJObj, ?DEFAULT_RTCP_MUX)
            ,caller_id_type = kz_json:get_ne_value(<<"caller_id_type">>, GatewayJObj, ?DEFAULT_CALLER_ID_TYPE)
            ,progress_timeout = kz_json:get_integer_value(<<"progress_timeout">>, GatewayJObj, ?DEFAULT_PROGRESS_TIMEOUT)
            ,endpoint_options = endpoint_options(GatewayJObj, EndpointType)
            ,privacy_method = kz_privacy:get_method(GatewayJObj, PrivacyMethod)
            ,privacy_hide_name = kz_privacy:should_hide_name(GatewayJObj, HideName)
            ,privacy_hide_number = kz_privacy:should_hide_name(GatewayJObj, HideNumber)
            ,invite_parameters=kz_json:get_ne_value(<<"invite_parameters">>, GatewayJObj)
            }.

-spec gateway_ccvs(kz_json:object()) -> kz_json:object().
gateway_ccvs(JObj) ->
    CCVs = kz_json:get_json_value(<<"custom_channel_vars">>, JObj, kz_json:new()),
    kz_custom_sip_headers:outbound(CCVs, kz_json:new()).

-spec gateway_is_emergency(kz_json:object(), boolean()) -> boolean().
gateway_is_emergency(_, 'true') -> 'true';
gateway_is_emergency(JObj, IsEmergency) ->
    kz_json:is_true(<<"emergency">>, JObj, IsEmergency).

-spec endpoint_options(kz_json:object(), kz_term:api_binary()) -> kz_json:object().
endpoint_options(JObj, <<"freetdm">>) ->
    kz_json:from_list(
      [{<<"Span">>, kz_json:get_value(<<"span">>, JObj)}
      ,{<<"Channel-Selection">>, kz_json:get_value(<<"channel_selection">>, JObj, <<"ascending">>)}
      ]);
endpoint_options(JObj, <<"skype">>) ->
    kz_json:from_list(
      [{<<"Skype-Interface">>, kz_json:get_value(<<"interface">>, JObj)}
      ,{<<"Skype-RR">>, kz_json:is_true(<<"skype_rr">>, JObj, true)}
      ]);
endpoint_options(JObj, <<"sip">>) ->
    kz_json:from_list(
      [{<<"Route-ID">>, kz_json:get_value(<<"route_id">>, JObj)}]);
endpoint_options(_, _) ->
    kz_json:new().

%%------------------------------------------------------------------------------
%% @doc Build the sip url of a resource gateway
%% @end
%%------------------------------------------------------------------------------
-spec gateway_dialstring(gateway(), kz_term:ne_binary()) -> kz_term:ne_binary().
gateway_dialstring(#gateway{route='undefined'
                           ,prefix=Prefix
                           ,suffix=Suffix
                           ,server=Server
                           ,port=Port
                           }, Number) ->
    DialStringPort =
        case not kz_term:is_empty(Port)
            andalso Port =/= 5060
        of
            'true' -> <<":", (kz_term:to_binary(Port))/binary>>;
            'false' -> <<>>
        end,
    Route =
        list_to_binary(["sip:", Prefix, Number, Suffix, "@", Server, DialStringPort]),
    lager:debug("created gateway route ~s", [Route]),
    Route;
gateway_dialstring(#gateway{route=Route}, _) ->
    lager:debug("using pre-configured gateway route ~s", [Route]),
    Route.

-spec get_resrc_id(resource()) -> kz_term:api_binary().
get_resrc_id(#resrc{id=Id}) -> Id.

-spec get_resrc_rev(resource()) -> kz_term:api_binary().
get_resrc_rev(#resrc{rev=Rev}) -> Rev.

-spec get_resrc_name(resource()) -> kz_term:api_binary().
get_resrc_name(#resrc{name=Name}) -> Name.

-spec get_resrc_weight(resource()) -> non_neg_integer().
get_resrc_weight(#resrc{weight=Weight}) -> Weight.

-spec get_resrc_grace_period(resource()) -> non_neg_integer().
get_resrc_grace_period(#resrc{grace_period=GracePeriod}) -> GracePeriod.

-spec get_resrc_flags(resource()) -> list().
get_resrc_flags(#resrc{flags=Flags}) -> Flags.

-spec get_resrc_rules(resource()) -> list().
get_resrc_rules(#resrc{rules=Rules}) -> Rules.

-spec get_resrc_raw_rules(resource()) -> list().
get_resrc_raw_rules(#resrc{raw_rules=RawRules}) -> RawRules.

-spec get_resrc_cid_rules(resource()) -> list().
get_resrc_cid_rules(#resrc{cid_rules=CIDRules}) -> CIDRules.

-spec get_resrc_cid_raw_rules(resource()) -> list().
get_resrc_cid_raw_rules(#resrc{cid_raw_rules=CIDRawRules}) -> CIDRawRules.

-spec get_resrc_gateways(resource()) -> list().
get_resrc_gateways(#resrc{gateways=Gateways, gateway_strategy='sequential'}) -> Gateways;
get_resrc_gateways(#resrc{gateways=Gateways, gateway_strategy='random'}) -> kz_term:shuffle_list(Gateways).

-spec get_resrc_is_emergency(resource()) -> boolean().
get_resrc_is_emergency(#resrc{is_emergency=IsEmergency}) -> IsEmergency.

-spec get_resrc_require_flags(resource()) -> boolean().
get_resrc_require_flags(#resrc{require_flags=RequireFlags}) -> RequireFlags.

-spec get_resrc_ignore_flags(resource()) -> boolean().
get_resrc_ignore_flags(#resrc{ignore_flags=IgnoreFlags}) -> IgnoreFlags.

-spec get_resrc_global(resource()) -> boolean().
get_resrc_global(#resrc{global=Global}) -> Global.

-spec get_resrc_format_from_uri(resource()) -> boolean().
get_resrc_format_from_uri(#resrc{format_from_uri=FormatFromUri}) -> FormatFromUri.

-spec get_resrc_from_uri_realm(resource()) -> kz_term:api_binary().
get_resrc_from_uri_realm(#resrc{from_uri_realm=FromUriRealm}) -> FromUriRealm.

-spec get_resrc_from_account_realm(resource()) -> boolean().
get_resrc_from_account_realm(#resrc{from_account_realm=FromAccountRealm}) -> FromAccountRealm.

-spec get_resrc_fax_option(resource()) -> kz_term:ne_binary() | boolean().
get_resrc_fax_option(#resrc{fax_option=FaxOption}) -> FaxOption.

-spec get_resrc_codecs(resource()) -> kz_term:ne_binaries().
get_resrc_codecs(#resrc{codecs=Codecs}) -> Codecs.

-spec get_resrc_bypass_media(resource()) -> boolean().
get_resrc_bypass_media(#resrc{bypass_media=BypassMedia}) -> BypassMedia.

-spec get_resrc_formatters(resource()) -> kz_term:api_object().
get_resrc_formatters(#resrc{formatters=Formatters}) -> Formatters.

-spec get_resrc_proxies(resource()) -> kz_term:proplist().
get_resrc_proxies(#resrc{proxies=Proxies}) -> Proxies.

-spec get_resrc_selector_marks(resource()) -> kz_term:proplist().
get_resrc_selector_marks(#resrc{selector_marks=Marks}) -> Marks.

-spec get_resrc_classifier(resource()) -> kz_term:api_binary().
get_resrc_classifier(#resrc{classifier=Classifier}) -> Classifier.

-spec get_resrc_classifier_enable(resource()) -> boolean().
get_resrc_classifier_enable(#resrc{classifier_enable=Enabled}) -> Enabled.

-spec set_resrc_id(resource(), kz_term:api_binary()) -> resource().
set_resrc_id(Resource, Id) -> Resource#resrc{id=Id}.

-spec set_resrc_rev(resource(), kz_term:api_binary()) -> resource().
set_resrc_rev(Resource, Rev) -> Resource#resrc{rev=Rev}.

-spec set_resrc_name(resource(), kz_term:api_binary()) -> resource().
set_resrc_name(Resource, Name) -> Resource#resrc{name=Name}.

-spec set_resrc_weight(resource(), non_neg_integer()) -> resource().
set_resrc_weight(Resource, Weight) -> Resource#resrc{weight=Weight}.

-spec set_resrc_grace_period(resource(), non_neg_integer()) -> resource().
set_resrc_grace_period(Resource, GracePeriod) -> Resource#resrc{grace_period=GracePeriod}.

-spec set_resrc_flags(resource(), list()) -> resource().
set_resrc_flags(Resource, Flags) -> Resource#resrc{flags=Flags}.

-spec set_resrc_rules(resource(), list()) -> resource().
set_resrc_rules(Resource, Rules) -> Resource#resrc{rules=Rules}.

-spec set_resrc_raw_rules(resource(), list()) -> resource().
set_resrc_raw_rules(Resource, RawRules) -> Resource#resrc{raw_rules=RawRules}.

-spec set_resrc_cid_rules(resource(), list()) -> resource().
set_resrc_cid_rules(Resource, CIDRules) -> Resource#resrc{cid_rules=CIDRules}.

-spec set_resrc_cid_raw_rules(resource(), list()) -> resource().
set_resrc_cid_raw_rules(Resource, CIDRawRules) -> Resource#resrc{cid_raw_rules=CIDRawRules}.

-spec set_resrc_gateways(resource(), list()) -> resource().
set_resrc_gateways(Resource, Gateways) -> Resource#resrc{gateways=Gateways}.

-spec set_resrc_is_emergency(resource(), boolean()) -> resource().
set_resrc_is_emergency(Resource, IsEmergency) -> Resource#resrc{is_emergency=IsEmergency}.

-spec set_resrc_require_flags(resource(), boolean()) -> resource().
set_resrc_require_flags(Resource, RequireFlags) -> Resource#resrc{require_flags=RequireFlags}.

-spec set_resrc_ignore_flags(resource(), boolean()) -> resource().
set_resrc_ignore_flags(Resource, IgnoreFlags) -> Resource#resrc{ignore_flags=IgnoreFlags}.

-spec set_resrc_global(resource(), boolean()) -> resource().
set_resrc_global(Resource, Global) -> Resource#resrc{global=Global}.

-spec set_resrc_format_from_uri(resource(), boolean()) -> resource().
set_resrc_format_from_uri(Resource, FormatFromUri) -> Resource#resrc{format_from_uri=FormatFromUri}.

-spec set_resrc_from_uri_realm(resource(), kz_term:api_binary()) -> resource().
set_resrc_from_uri_realm(Resource, FromUriRealm) -> Resource#resrc{from_uri_realm=FromUriRealm}.

-spec set_resrc_from_account_realm(resource(), boolean()) -> resource().
set_resrc_from_account_realm(Resource, FromAccountRealm) -> Resource#resrc{from_account_realm=FromAccountRealm}.

-spec set_resrc_fax_option(resource(), kz_term:ne_binary() | boolean()) -> resource().
set_resrc_fax_option(Resource, FaxOption) -> Resource#resrc{fax_option=FaxOption}.

-spec set_resrc_codecs(resource(), kz_term:ne_binaries()) -> resource().
set_resrc_codecs(Resource, Codecs) -> Resource#resrc{codecs=Codecs}.

-spec set_resrc_bypass_media(resource(), boolean()) -> resource().
set_resrc_bypass_media(Resource, BypassMedia) -> Resource#resrc{bypass_media=BypassMedia}.

-spec set_resrc_formatters(resource(), kz_term:api_object()) -> resource().
set_resrc_formatters(Resource, Formatters) -> Resource#resrc{formatters=Formatters}.

-spec set_resrc_proxies(resource(), kz_term:proplist()) -> resource().
set_resrc_proxies(Resource, Proxies) -> Resource#resrc{proxies=Proxies}.

-spec set_resrc_selector_marks(resource(), kz_term:proplist()) -> resource().
set_resrc_selector_marks(Resource, Marks) -> Resource#resrc{selector_marks=Marks}.
