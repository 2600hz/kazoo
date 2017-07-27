%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
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

-record(gateway, {server :: api_binary()
                 ,port :: api_integer()
                 ,realm :: api_binary()
                 ,username :: api_binary()
                 ,password :: api_binary()
                 ,route :: api_binary()
                 ,prefix = <<>> :: binary()
                 ,suffix = <<>> :: binary()
                 ,codecs = [] :: ne_binaries()
                 ,bypass_media = 'false' :: boolean()
                 ,rtcp_mux = <<>> :: binary()
                 ,caller_id_type = <<"external">> :: ne_binary()
                 ,fax_option :: ne_binary() | boolean()
                 ,sip_headers :: api_object()
                 ,sip_interface :: api_binary()
                 ,progress_timeout = 8 :: 1..100
                 ,invite_format = <<"route">> :: ne_binary()
                 ,endpoint_type = <<"sip">> :: ne_binary()
                 ,endpoint_options = kz_json:new() :: kz_json:object()
                 ,format_from_uri = 'false' :: boolean()
                 ,from_account_realm = 'false' :: boolean()
                 ,from_uri_realm :: api_binary()
                 ,is_emergency = 'false' :: boolean()
                 ,force_port = 'false' :: boolean()
                 ,privacy_mode = 'undefined' :: api_binary()
                 }).

-record(resrc, {id :: api_binary()
               ,rev :: api_binary()
               ,name :: api_binary()
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
               ,global = 'true' :: boolean()
               ,format_from_uri = 'false' :: boolean()
               ,from_uri_realm :: api_binary()
               ,from_account_realm = 'false' :: boolean()
               ,fax_option :: ne_binary() | boolean()
               ,codecs = [] :: ne_binaries()
               ,bypass_media = 'false' :: boolean()
               ,formatters :: api_objects()
               ,proxies = [] :: kz_proplist()
               ,selector_marks = [] :: [tuple()]
               ,privacy_mode = 'undefined' :: api_binary()
               ,classifier = 'undefined' :: api_binary()
               ,classifier_enable = 'undefined' :: api_boolean()
               }).

-type resource() :: #resrc{}.
-type resources() :: [#resrc{}].

-type gateway() :: #gateway{}.
-type gateways() :: [#gateway{}].

-export_type([resource/0
             ,resources/0
             ,gateway/0
             ,gateways/0
             ]).

-compile({'no_auto_import', [get/0, get/1]}).

-spec get_props() -> kz_proplists().
get_props() ->
    [resource_to_props(Resource)
     || Resource <- sort_resources(get())
    ].

-spec get_props(ne_binary()) -> kz_proplist() | 'undefined'.
get_props(ResourceId) ->
    case get_resource(ResourceId) of
        'undefined' -> 'undefined';
        Resource -> resource_to_props(Resource)
    end.

-spec get_props(ne_binary(), api_binary()) -> kz_proplist() | 'undefined'.
get_props(_ResourceId, 'undefined') -> 'undefined';
get_props(ResourceId, AccountId) ->
    case get_local_resource(ResourceId, AccountId) of
        'undefined' -> 'undefined';
        Resource -> resource_to_props(Resource)
    end.

-spec resource_to_props(resource()) -> kz_proplist().
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
      ,{<<"Is-Emergency">>, Resource#resrc.is_emergency}
      ,{<<"T38">>, Resource#resrc.fax_option}
      ,{<<"Bypass-Media">>, Resource#resrc.bypass_media}
      ,{<<"Grace-Period">>, Resource#resrc.grace_period}
      ,{<<"Flags">>, Resource#resrc.flags}
      ,{<<"Codecs">>, Resource#resrc.codecs}
      ,{<<"Rules">>, Resource#resrc.raw_rules}
      ,{<<"Caller-ID-Rules">>, Resource#resrc.cid_raw_rules}
      ,{<<"Formatters">>, Resource#resrc.formatters}
      ,{<<"Privacy-Mode">>, Resource#resrc.privacy_mode}
      ]).

-spec sort_resources(resources()) -> resources().
sort_resources(Resources) ->
    lists:sort(fun(#resrc{weight=W1}, #resrc{weight=W2}) ->
                       W1 =< W2
               end, Resources).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec endpoints(ne_binary(), kapi_offnet_resource:req()) -> kz_json:objects().
endpoints(Number, OffnetJObj) ->
    case maybe_get_endpoints(Number, OffnetJObj) of
        [] -> [];
        Endpoints -> sort_endpoints(Endpoints)
    end.

-spec maybe_get_endpoints(ne_binary(), kapi_offnet_resource:req()) -> kz_json:objects().
maybe_get_endpoints(Number, OffnetJObj) ->
    case kapi_offnet_resource:hunt_account_id(OffnetJObj) of
        'undefined' -> get_global_endpoints(Number, OffnetJObj);
        HuntAccount -> maybe_get_local_endpoints(HuntAccount, Number, OffnetJObj)
    end.

-spec maybe_get_local_endpoints(ne_binary(), ne_binary(), kapi_offnet_resource:req()) -> kz_json:objects().
maybe_get_local_endpoints(HuntAccount, Number, OffnetJObj) ->
    AccountId = kapi_offnet_resource:account_id(OffnetJObj),
    case kz_util:is_in_account_hierarchy(HuntAccount, AccountId, 'true') of
        'false' ->
            lager:info("account ~s attempted to use local resources of ~s, but it is not allowed"
                      ,[AccountId, HuntAccount]
                      ),
            [];
        'true' ->
            lager:info("account ~s is using the local resources of ~s", [AccountId, HuntAccount]),
            get_local_endpoints(HuntAccount, Number, OffnetJObj)
    end.

-spec get_local_endpoints(ne_binary(), ne_binary(), kapi_offnet_resource:req()) -> kz_json:objects().
get_local_endpoints(AccountId, Number, OffnetJObj) ->
    lager:debug("attempting to find local resources for ~s", [AccountId]),
    Flags = kapi_offnet_resource:flags(OffnetJObj, []),
    Resources = filter_resources(Flags, get(AccountId)),
    resources_to_endpoints(Resources, Number, OffnetJObj).

-spec get_global_endpoints(ne_binary(), kapi_offnet_resource:req()) -> kz_json:objects().
get_global_endpoints(Number, OffnetJObj) ->
    lager:debug("attempting to find global resources"),
    Flags = kapi_offnet_resource:flags(OffnetJObj, []),
    Resources = filter_resources(Flags, get()),
    resources_to_endpoints(Resources, Number, OffnetJObj).

-spec sort_endpoints(kz_json:objects()) -> kz_json:objects().
sort_endpoints(Endpoints) ->
    lists:sort(fun endpoint_ordering/2, Endpoints).

-spec endpoint_ordering(kz_json:object(), kz_json:object()) -> boolean().
endpoint_ordering(P1, P2) ->
    kz_json:get_value(<<"Weight">>, P1, 1)
        =< kz_json:get_value(<<"Weight">>, P2, 1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reverse_lookup(kz_json:object()) ->
                            {'ok', kz_proplist()} |
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

-spec find_port(kz_json:object()) -> api_integer().
find_port(JObj) ->
    case kz_json:get_first_defined([<<"From-Network-Port">>
                                   ,<<"Orig-Port">>
                                   ], JObj)
    of
        'undefined' -> 'undefined';
        Port -> kz_term:to_integer(Port)
    end.

-spec find_account_id(api_binary(), kz_json:object()) -> api_binary().
find_account_id(Realm, JObj) ->
    case kz_json:get_first_defined([<<"Account-ID">>
                                   ,?CCV(<<"Account-ID">>)
                                   ], JObj)
    of
        'undefined' -> find_account_id(Realm);
        AccountId -> AccountId
    end.

-spec find_account_id(api_binary()) -> api_binary().
find_account_id('undefined') -> 'undefined';
find_account_id(Realm) ->
    case kapps_util:get_account_by_realm(Realm) of
        {'error', 'not_found'} -> 'undefined';
        {'ok', AccountId} -> AccountId
    end.

-spec maybe_find_global(api_binary(), api_integer(), api_binary()) ->
                               {'ok', kz_proplist()} |
                               {'error', 'not_found'}.
maybe_find_global(IP, Port, Realm) ->
    search_resources(IP, Port, Realm, get()).

-spec maybe_find_local(api_binary(), api_integer(), api_binary(), api_binary()) ->
                              {'ok', kz_proplist()} |
                              {'error', 'not_found'}.
maybe_find_local(_, _, _, 'undefined') -> {'error', 'not_found'};
maybe_find_local(IP, Port, Realm, AccountId) ->
    search_resources(IP, Port, Realm, get(AccountId)).

-spec search_resources(api_binary(), api_integer(), api_binary(), resources()) ->
                              {'ok', kz_proplist()} |
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

-spec search_gateways(api_binary(), api_integer(), api_binary(), gateways()) ->
                             gateway() |
                             {'error', 'not_found'}.
search_gateways(_, _, _, []) -> {'error', 'not_found'};
search_gateways(IP, Port, Realm, [Gateway | Gateways]) ->
    case search_gateway(IP, Port, Realm, Gateway) of
        {'error', 'not_found'} -> search_gateways(IP, Port, Realm, Gateways);
        #gateway{}=Gateway -> Gateway
    end.

-spec search_gateway(api_binary(), api_integer(), api_binary(), gateway()) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_resources(ne_binaries(), resources()) -> resources().
filter_resources([], Resources) ->
    lager:debug("no flags provided, filtering resources that require flags"),
    [Resource
     || #resrc{require_flags=RequireFlags}=Resource <- Resources,
        not RequireFlags
    ];
filter_resources(Flags, Resources) ->
    lager:debug("filtering resources by flags"),
    filter_resources(Flags, Resources, []).

-spec filter_resources(ne_binaries(), resources(), resources()) -> resources().
filter_resources(_, [], Filtered) -> Filtered;
filter_resources(Flags, [Resource|Resources], Filtered) ->
    case resource_has_flags(Flags, Resource) of
        'false' -> filter_resources(Flags, Resources, Filtered);
        'true' -> filter_resources(Flags, Resources, [Resource | Filtered])
    end.

-spec resource_has_flags(ne_binaries(), resource()) -> boolean().
resource_has_flags(Flags, Resource) ->
    HasFlag = fun(Flag) -> resource_has_flag(Flag, Resource) end,
    lists:all(HasFlag, Flags).

-spec resource_has_flag(ne_binary(), resource()) -> boolean().
resource_has_flag(Flag, #resrc{flags=ResourceFlags, id=_Id}) ->
    case kz_term:is_empty(Flag)
        orelse lists:member(Flag, ResourceFlags)
    of
        'true' -> 'true';
        'false' ->
            lager:debug("resource ~s does not have the required flag: ~s", [_Id, Flag]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resources_to_endpoints(resources(), ne_binary(), kapi_offnet_resource:req()) ->
                                    kz_json:objects().
resources_to_endpoints(Resources, Number, OffnetJObj) ->
    ResourceMap = lists:foldl(fun resource_classifier_map/2, maps:new(), Resources),
    ConfigClassifiers = kz_json:to_proplist(knm_converters:available_classifiers()),
    classifier_resources_to_endpoints(ConfigClassifiers, ResourceMap, Number, OffnetJObj).

-spec resource_classifier_map(resource(), map()) -> map().
resource_classifier_map(Resource, Map) ->
    Classification = case get_resrc_classifier(Resource) of
                         'undefined' -> 'no_classification';
                         C -> C
                     end,
    Resources = maps:get(Classification, Map, []),
    maps:put(Classification, [Resource|Resources], Map).

-spec classifier_resources_to_endpoints(kz_proplist(), map(), ne_binary(), kapi_offnet_resource:req()) ->
                                               kz_json:objects().
classifier_resources_to_endpoints([], ResourceMap, Number, OffnetJObj) ->
    %% No endpoints found based on resrouces with classifiers, look up by
    %% resources that doesn't have classifier
    lager:debug("no classifiers satisfy resource look up, matching against resource rules..."),
    Resources = maps:get('no_classification', ResourceMap, []),
    Fun = fun(Resource, Endpoints) ->
                  maybe_resource_to_endpoints(Resource, Number, OffnetJObj, Endpoints)
          end,
    lists:foldr(Fun, [], Resources);
classifier_resources_to_endpoints([{Class, _}|Classes], ResourceMap, Number, OffnetJObj) ->
    case maps:get(Class, ResourceMap, 'undefined') of
        'undefined' ->
            classifier_resources_to_endpoints(Classes, ResourceMap, Number, OffnetJObj);
        ClassRsrcs ->
            case resources_to_endpoints(ClassRsrcs, Number, OffnetJObj, []) of
                'classifier_disabled' -> [];
                [] ->
                    lager:debug("no resources with classifier ~s matched to route number ~s", [Class, Number]),
                    classifier_resources_to_endpoints(Classes, ResourceMap, Number, OffnetJObj);
                Endpoints ->
                    lager:debug("classifier ~s satisfies resource look up to route number ~s", [Class, Number]),
                    Endpoints
            end
    end.

-spec resources_to_endpoints(resources(), ne_binary(), kapi_offnet_resource:req(), kz_json:objects() | 'classifier_disabled') ->
                                    'classifier_disabled' |
                                    kz_json:objects().
resources_to_endpoints(_Resources, _Number, _OffnetJObj, 'classifier_disabled') ->
    'classifier_disabled';
resources_to_endpoints([], _Number, _OffnetJObj, Endpoints) ->
    Endpoints;
resources_to_endpoints([Resource|Resources], Number, OffnetJObj, Endpoints) ->
    MoreEndpoints = maybe_resource_to_endpoints(Resource, Number, OffnetJObj, Endpoints),
    resources_to_endpoints(Resources, Number, OffnetJObj, MoreEndpoints).

-spec maybe_resource_to_endpoints(resource(), ne_binary(), kapi_offnet_resource:req(), kz_json:objects()) ->
                                         'classifier_disabled' |
                                         kz_json:objects().
maybe_resource_to_endpoints(#resrc{id=Id
                                  ,name=Name
                                  ,rules=Rules
                                  ,cid_rules=CallerIdRules
                                  ,gateways=Gateways
                                  ,global=Global
                                  ,weight=Weight
                                  ,proxies=Proxies
                                  ,classifier=Classifier
                                  ,classifier_enable=ClassifierEnabled
                                  }
                           ,Number
                           ,OffnetJObj
                           ,Endpoints
                           ) ->
    CallerIdNumber = case ?RULES_HONOR_DIVERSION of
                         'false' -> kapi_offnet_resource:outbound_caller_id_number(OffnetJObj);
                         'true' -> check_diversion_fields(OffnetJObj)
                     end,
    DisabledByClassifier = kz_term:is_ne_binary(Classifier)
        andalso (not ClassifierEnabled),
    case filter_resource_by_rules(Id, Number, Rules, CallerIdNumber, CallerIdRules) of
        {'error','no_match'} -> Endpoints;
        {'ok', _NumberMatch} when DisabledByClassifier ->
            lager:debug("classifier ~s is disabled by resource ~s (matched number ~s)", [Classifier, Id, _NumberMatch]),
            'classifier_disabled';
        {'ok', NumberMatch} ->
            _MaybeClassifier = case kz_term:is_ne_binary(Classifier) of
                                   'true' -> Classifier;
                                   'false' -> <<"no_classifier">>
                               end,
            lager:debug("building resource ~s endpoints (classifier ~s)", [Id, _MaybeClassifier]),
            CCVUpdates = [{<<"Global-Resource">>, kz_term:to_binary(Global)}
                         ,{<<"Resource-ID">>, Id}
                         ,{<<"E164-Destination">>, Number}
                         ,{<<"Original-Number">>, kapi_offnet_resource:to_did(OffnetJObj)}
                         ],
            Updates = [{<<"Name">>, Name}
                      ,{<<"Weight">>, Weight}
                      ],
            EndpointList = [update_endpoint(Endpoint, Updates, CCVUpdates)
                            || Endpoint <- gateways_to_endpoints(NumberMatch, Gateways, OffnetJObj, [])
                           ],
            maybe_add_proxies(EndpointList, Proxies, Endpoints)
    end.

-spec check_diversion_fields(kapi_offnet_resource:req()) -> ne_binary().
check_diversion_fields(OffnetJObj) ->
    case kapi_offnet_resource:custom_sip_header(OffnetJObj, <<"Diversions">>) of
        [Diversion|_] ->
            [_,CallerIdNumber,_] = binary:split(Diversion, [<<":">>,<<"@">>], ['global']),
            CallerIdNumber;
        _ ->
            kapi_offnet_resource:outbound_caller_id_number(OffnetJObj)
    end.

-spec update_endpoint(kz_json:object(), kz_proplist(), kz_proplist()) ->
                             kz_json:object().
update_endpoint(Endpoint, Updates, CCVUpdates) ->
    kz_json:set_values(Updates, update_ccvs(Endpoint, CCVUpdates)).

-spec maybe_add_proxies(kz_json:objects(), kz_proplist(), kz_json:objects()) -> kz_json:objects().
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

-spec filter_resource_by_rules(ne_binary(), ne_binary(), re:mp(), ne_binary(), re:mp()) ->
                                      {'ok', ne_binary()} |
                                      {'error', 'no_match'}.
filter_resource_by_rules(Id, Number, Rules, CallerIdNumber, CallerIdRules) ->
    case evaluate_rules(Rules, Number) of
        {'error', 'no_match'} ->
            lager:debug("resource ~s does not match request, skipping", [Id]),
            {'error','no_match'};
        {'ok', Match} ->
            filter_resource_by_match(Id, Number, CallerIdNumber, CallerIdRules, Match)
    end.

-spec filter_resource_by_match(ne_binary(), ne_binary(), ne_binary(), re:mp(), ne_binary()) ->
                                      {'ok', ne_binary()} |
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

-spec update_ccvs(kz_json:object(), kz_proplist()) -> kz_json:object().
update_ccvs(Endpoint, Updates) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, kz_json:new()),
    kz_json:set_value(<<"Custom-Channel-Vars">>, kz_json:set_values(Updates, CCVs), Endpoint).

-spec evaluate_rules(re:mp(), ne_binary()) ->
                            {'ok', ne_binary()} |
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

-spec evaluate_cid_rules(re:mp(), ne_binary()) ->
                                {'ok', ne_binary()} |
                                {'ok', 'empty_rules'} | %% empty rules, it`s ok, allow any number
                                {'error', 'no_match'}.
evaluate_cid_rules([], _) -> {'ok','empty_rules'};
evaluate_cid_rules(CIDRules, CIDNumber) -> evaluate_rules(CIDRules, CIDNumber).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec gateways_to_endpoints(ne_binary(), gateways(), kapi_offnet_resource:req(), kz_json:objects()) ->
                                   kz_json:objects().
gateways_to_endpoints(_Number, [], _OffnetJObj, Endpoints) -> Endpoints;
gateways_to_endpoints(Number, [Gateway|Gateways], OffnetJObj, Endpoints) ->
    gateways_to_endpoints(Number
                         ,Gateways
                         ,OffnetJObj
                         ,[gateway_to_endpoint(Number, Gateway, OffnetJObj) | Endpoints]
                         ).

-spec gateway_to_endpoint(ne_binary(), gateway(), kapi_offnet_resource:req()) ->
                                 kz_json:object().
gateway_to_endpoint(DestinationNumber
                   ,#gateway{invite_format=InviteFormat
                            ,caller_id_type=CallerIdType
                            ,bypass_media=BypassMedia
                            ,rtcp_mux=RTCP_MUX
                            ,codecs=Codecs
                            ,username=Username
                            ,password=Password
                            ,sip_headers=SipHeaders
                            ,sip_interface=SipInterface
                            ,endpoint_type=EndpointType
                            ,endpoint_options=EndpointOptions
                            ,progress_timeout=ProgressTimeout
                            ,privacy_mode=PrivacyMode
                            }=Gateway
                   ,OffnetJObj
                   ) ->

    IsEmergency = gateway_emergency_resource(Gateway),
    {CIDName, CIDNumber} = gateway_cid(OffnetJObj, IsEmergency, PrivacyMode),

    CCVs = props:filter_undefined(
             [{<<"Emergency-Resource">>, IsEmergency}
             ,{<<"Matched-Number">>, DestinationNumber}
             ,{<<"Resource-Type">>, <<"offnet-termination">>}
             ,{<<"RTCP-MUX">>, RTCP_MUX}
              | gateway_from_uri_settings(Gateway)
             ]),
    kz_json:from_list(
      props:filter_empty(
        [{<<"Route">>, gateway_dialstring(Gateway, DestinationNumber)}
        ,{<<"Callee-ID-Name">>, kz_term:to_binary(DestinationNumber)}
        ,{<<"Callee-ID-Number">>, kz_term:to_binary(DestinationNumber)}
        ,{<<"To-DID">>, kz_term:to_binary(DestinationNumber)}
        ,{<<"Invite-Format">>, InviteFormat}
        ,{<<"Caller-ID-Type">>, CallerIdType}
        ,{<<"Bypass-Media">>, BypassMedia}
        ,{<<"Codecs">>, Codecs}
        ,{<<"Auth-User">>, Username}
        ,{<<"Auth-Password">>, Password}
        ,{<<"Custom-SIP-Headers">>, SipHeaders}
        ,{<<"SIP-Interface">>, SipInterface}
        ,{<<"Endpoint-Type">>, EndpointType}
        ,{<<"Endpoint-Options">>, EndpointOptions}
        ,{<<"Endpoint-Progress-Timeout">>, kz_term:to_binary(ProgressTimeout)}
        ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
        ,{<<"Outbound-Caller-ID-Number">>, CIDNumber}
        ,{<<"Outbound-Caller-ID-Name">>, CIDName}
         | maybe_get_t38(Gateway, OffnetJObj)
        ])).

-spec gateway_cid(kapi_offnet_resource:req(), api_binary(), api_binary()) -> {ne_binary(), ne_binary()}.
gateway_cid(OffnetJObj, IsEmergency, PrivacyMode) ->
    CCVs = kz_json:get_ne_value(<<"Custom-Channel-Vars">>, OffnetJObj, kz_json:new()),
    AccountId = kapi_offnet_resource:hunt_account_id(OffnetJObj, kapi_offnet_resource:account_id(OffnetJObj)),
    DefaultCID = default_gateway_cid(OffnetJObj, IsEmergency),
    kz_privacy:maybe_cid_privacy(kz_json:set_values([{<<"Account-ID">>, AccountId}
                                                    ,{<<"Privacy-Mode">>, PrivacyMode}
                                                    ]
                                                   ,CCVs
                                                   )
                                , DefaultCID
                                ).

-spec default_gateway_cid(kapi_offnet_resource:req(), api_binary()) -> {ne_binary(), ne_binary()}.
default_gateway_cid(OffnetJObj, 'undefined') ->
    {kapi_offnet_resource:outbound_caller_id_name(OffnetJObj)
    ,kapi_offnet_resource:outbound_caller_id_number(OffnetJObj)
    };
default_gateway_cid(OffnetJObj, <<"true">>) ->
    {stepswitch_bridge:bridge_emergency_cid_name(OffnetJObj)
    ,stepswitch_bridge:bridge_emergency_cid_number(OffnetJObj)
    }.

-spec gateway_from_uri_settings(gateway()) -> kz_proplist().
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

-spec maybe_get_t38(gateway(), kapi_offnet_resource:req()) -> kz_proplist().
maybe_get_t38(#gateway{fax_option=FaxOption}, OffnetJObj) ->
    Flags = kapi_offnet_resource:flags(OffnetJObj, []),
    case lists:member(<<"fax">>, Flags) of
        'false' -> [];
        'true' ->
            kapps_call_command:get_outbound_t38_settings(FaxOption
                                                        ,kapi_offnet_resource:t38_enabled(OffnetJObj)
                                                        )
    end.

-spec gateway_emergency_resource(gateway()) -> api_binary().
gateway_emergency_resource(#gateway{is_emergency='true'}) ->
    lager:debug("gateway is part of an emergency resource"),
    <<"true">>;
gateway_emergency_resource(_) -> 'undefined'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get() -> resources().
get() -> get('undefined').

-spec get(api_binary()) -> resources().
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

-spec get_resource(ne_binary()) -> resource() | 'undefined'.
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

-spec get_local_resource(ne_binary(), ne_binary()) -> resource() | 'undefined'.
get_local_resource(ResourceId, AccountId) ->
    case get(AccountId) of
        [] -> 'undefined';
        Resources -> get_resource(ResourceId, Resources)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_global_resources() -> resources().
fetch_global_resources() ->
    lager:debug("global resource cache miss, fetching from db"),
    ViewOptions = ['include_docs'],
    case kz_datamgr:get_results(?RESOURCES_DB, ?LIST_RESOURCES_BY_ID, ViewOptions) of
        {'error', _R} ->
            lager:warning("unable to fetch global resources: ~p", [_R]),
            [];
        {'ok', JObjs} ->
            CacheProps = [{'origin', [{'db', ?RESOURCES_DB, <<"resource">>}]}],
            Docs = [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs],
            Resources = resources_from_jobjs(Docs),
            kz_cache:store_local(?CACHE_NAME, 'global_resources', Resources, CacheProps),
            Resources
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_local_resources(ne_binary()) -> resources().
fetch_local_resources(AccountId) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
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

-spec fetch_local_resources(ne_binary(), kz_json:objects()) -> resources().
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

-spec fetch_account_dedicated_proxies(api_binary()) -> kz_proplist().
fetch_account_dedicated_proxies('undefined') -> [];
fetch_account_dedicated_proxies(AccountId) ->
    case kz_ips:assigned(AccountId) of
        {'ok', IPS} -> [build_account_dedicated_proxy(IP) || IP <- IPS];
        _ -> []
    end.

-spec build_account_dedicated_proxy(kz_json:object()) -> {api_binary(), api_binary()}.
build_account_dedicated_proxy(Proxy) ->
    Zone = kz_json:get_value(<<"zone">>, Proxy),
    ProxyIP = kz_json:get_value(<<"ip">>, Proxy),
    {Zone, ProxyIP}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resources_from_jobjs(kz_json:objects()) -> resources().
-spec resources_from_jobjs(kz_json:objects(), resources()) -> resources().
resources_from_jobjs(JObjs) ->
    resources_from_jobjs(JObjs, []).

resources_from_jobjs([], Resources) -> Resources;
resources_from_jobjs([JObj|JObjs], Resources) ->
    case kz_json:is_true(<<"enabled">>, JObj, 'true') of
        'false' -> resources_from_jobjs(JObjs, Resources);
        'true' -> resources_from_jobjs(JObjs, create_resource(JObj, Resources))
    end.

-spec create_resource(kz_json:object(), resources()) -> resources().
create_resource(JObj, Resources) ->
    case kz_json:get_value(<<"classifiers">>, JObj) of
        'undefined' -> [resource_from_jobj(JObj) | Resources];
        ResourceClassifiers ->
            AvailableClassifiers = kz_json:to_proplist(knm_converters:available_classifiers()),
            create_resource(kz_json:to_proplist(ResourceClassifiers)
                           ,AvailableClassifiers
                           ,JObj
                           ,Resources
                           )
    end.

-spec create_resource(kz_proplist(), kz_proplist(), kz_json:object(), resources()) -> resources().
create_resource([], _ConfigClassifiers, _Resource, Resources) -> Resources;
create_resource([{Classifier, ClassifierJObj}|Classifiers], ConfigClassifiers, Resource, Resources) ->
    case props:get_value(Classifier, ConfigClassifiers) of
        'undefined' ->
            create_resource(Classifiers, ConfigClassifiers, Resource, Resources);
        ConfigClassifier ->
            JObj =
                create_classifier_resource(Resource
                                          ,ClassifierJObj
                                          ,Classifier
                                          ,ConfigClassifier
                                          ),
            create_resource(Classifiers
                           ,ConfigClassifiers
                           ,Resource
                           ,[resource_from_jobj(JObj)
                             | Resources
                            ]
                           )
    end.

-spec create_classifier_resource(kz_json:object(), kz_json:object(), ne_binary(), kz_proplist()) -> kz_json:object().
create_classifier_resource(Resource, ClassifierJObj, Classifier, ConfigClassifier) ->
    DefaultRegex = kz_json:get_value(<<"regex">>, ConfigClassifier),
    DefaultEmergency = kz_json:is_true(<<"emergency">>, ConfigClassifier, 'undefined'),
    Props =
        props:filter_undefined(
          [{<<"_id">>, <<(kz_json:get_value(<<"_id">>, Resource))/binary, "-", Classifier/binary>>}
          ,{<<"name">>, <<(kz_json:get_value(<<"name">>, Resource))/binary, " - ", Classifier/binary>>}
          ,{<<"rules">>, [kz_json:get_value(<<"regex">>, ClassifierJObj, DefaultRegex)]}
          ,{<<"weight_cost">>, kz_json:get_value(<<"weight_cost">>, ClassifierJObj)}
          ,{<<"emergency">>, classifier_is_emergency(ClassifierJObj, Classifier, DefaultEmergency)}
          ,{<<"classifier">>, Classifier}
          ,{<<"classifier_enable">>, kz_json:is_true(<<"enabled">>, ClassifierJObj, 'true')}
          ]
         ),
    create_classifier_gateways(kz_json:set_values(Props, Resource), ClassifierJObj).

-spec create_classifier_gateways(kz_json:object(), kz_json:object()) -> kz_json:object().
create_classifier_gateways(Resource, ClassifierJObj) ->
    Props =
        props:filter_undefined(
          [{<<"suffix">>, kz_json:get_value(<<"suffix">>, ClassifierJObj)}
          ,{<<"prefix">>, kz_json:get_value(<<"prefix">>, ClassifierJObj)}
          ]
         ),
    Gateways =
        [kz_json:set_values(Props, Gateway)
         || Gateway <- kz_json:get_value(<<"gateways">>, Resource, [])
        ],
    kz_json:set_value(<<"gateways">>, Gateways, Resource).

-spec classifier_is_emergency(kz_json:object(), ne_binary(), boolean() | 'undefined') -> boolean().
classifier_is_emergency(ClassifierJObj, <<"emergency">>, _DefaultEmergency) ->
    kz_json:is_true(<<"emergency">>, ClassifierJObj, 'true');
classifier_is_emergency(ClassifierJObj, _Classifier, DefaultEmergency) ->
    kz_json:is_true(<<"emergency">>, ClassifierJObj, DefaultEmergency).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type rule() :: re:mp().
-type rules() :: [rule()].

-spec resource_from_jobj(kz_json:object()) -> resource().
resource_from_jobj(JObj) ->
    Resource = #resrc{id=kz_doc:id(JObj)
                     ,rev=kz_doc:revision(JObj)
                     ,name=kz_json:get_value(<<"name">>, JObj)
                     ,flags=kz_json:get_value(<<"flags">>, JObj, [])
                     ,require_flags=kz_json:is_true(<<"require_flags">>, JObj)
                     ,format_from_uri=kz_json:is_true(<<"format_from_uri">>, JObj)
                     ,from_uri_realm=kz_json:get_ne_value(<<"from_uri_realm">>, JObj)
                     ,from_account_realm=kz_json:is_true(<<"from_account_realm">>, JObj)
                     ,fax_option=kz_json:is_true([<<"media">>, <<"fax_option">>], JObj)
                     ,raw_rules=kz_json:get_value(<<"rules">>, JObj, [])
                     ,rules=resource_rules(JObj)
                     ,cid_raw_rules=kz_json:get_value(<<"cid_rules">>, JObj, [])
                     ,cid_rules=resource_cid_rules(JObj)
                     ,weight=resource_weight(JObj)
                     ,grace_period=resource_grace_period(JObj)
                     ,is_emergency=resource_is_emergency(JObj)
                     ,codecs=resource_codecs(JObj)
                     ,bypass_media=resource_bypass_media(JObj)
                     ,formatters=resource_formatters(JObj)
                     ,global=kz_json:is_true(<<"Is-Global">>, JObj, 'true')
                     ,proxies=kz_json:to_proplist(<<"Proxies">>, JObj)
                     ,privacy_mode=kz_json:get_ne_value(<<"privacy_mode">>, JObj)
                     ,classifier=kz_json:get_ne_value(<<"classifier">>, JObj)
                     ,classifier_enable=kz_json:get_ne_value(<<"classifier_enable">>, JObj)
                     },
    Gateways = gateways_from_jobjs(kz_json:get_value(<<"gateways">>, JObj, [])
                                  ,Resource
                                  ),
    Resource#resrc{gateways=Gateways}.

-spec resource_bypass_media(kz_json:object()) -> boolean().
resource_bypass_media(JObj) ->
    Default = kapps_config:get_is_true(?SS_CONFIG_CAT, <<"default_bypass_media">>, 'false'),
    kz_json:is_true([<<"media">>, <<"bypass_media">>], JObj, Default).

-spec resource_formatters(kz_json:object()) -> api_objects().
resource_formatters(JObj) ->
    Default = kapps_config:get(?SS_CONFIG_CAT, <<"default_formatters">>),
    kz_json:get_value(<<"formatters">>, JObj, Default).

-spec resource_codecs(kz_json:object()) -> ne_binaries().
resource_codecs(JObj) ->
    DefaultAudio = kapps_config:get_ne_binaries(?SS_CONFIG_CAT, <<"default_audio_codecs">>, []),
    DefaultVideo = kapps_config:get_ne_binaries(?SS_CONFIG_CAT, <<"default_video_codecs">>, []),
    case kz_json:get_value([<<"media">>, <<"audio">>, <<"codecs">>], JObj, DefaultAudio)
        ++ kz_json:get_value([<<"media">>, <<"video">>, <<"codecs">>], JObj, DefaultVideo)
    of
        [] -> kapps_config:get_ne_binaries(?SS_CONFIG_CAT, <<"default_codecs">>, []);
        Codecs -> Codecs
    end.

-spec resource_rules(kz_json:object()) -> rules().
resource_rules(JObj) ->
    lager:info("compiling resource rules for ~s / ~s"
              ,[kz_doc:account_db(JObj, <<"offnet">>), kz_doc:id(JObj)]),
    Rules = kz_json:get_value(<<"rules">>, JObj, []),
    resource_rules(Rules, []).

-spec resource_rules(ne_binaries(), rules()) -> rules().
resource_rules([], CompiledRules) -> CompiledRules;
resource_rules([Rule|Rules], CompiledRules) ->
    case re:compile(Rule) of
        {'ok', CompiledRule} ->
            resource_rules(Rules, [CompiledRule | CompiledRules]);
        {'error', _R} ->
            lager:warning("bad rule '~s': ~p", [Rule, _R]),
            resource_rules(Rules, CompiledRules)
    end.

-spec resource_cid_rules(kz_json:object()) -> rules().
resource_cid_rules(JObj) ->
    lager:info("compiling resource rules for ~s / ~s"
              ,[kz_doc:account_db(JObj, <<"offnet">>), kz_doc:id(JObj)]),
    Rules = kz_json:get_value(<<"cid_rules">>, JObj, []),
    resource_rules(Rules, []).

-spec resource_grace_period(kz_json:object() | integer()) -> 0..100.
resource_grace_period(JObj) when not is_integer(JObj) ->
    resource_grace_period(kz_json:get_integer_value(<<"grace_period">>, JObj, ?DEFAULT_WEIGHT));
resource_grace_period(GracePeriod) when is_integer(GracePeriod), GracePeriod > 100 -> 100;
resource_grace_period(GracePeriod) when is_integer(GracePeriod), GracePeriod < 0 -> 0;
resource_grace_period(GracePeriod) when is_integer(GracePeriod) -> GracePeriod.

-spec resource_weight(kz_json:object() | integer()) -> integer().
resource_weight(JObj) when not is_integer(JObj) ->
    resource_weight(kz_json:get_integer_value(<<"weight_cost">>, JObj, ?DEFAULT_WEIGHT));
resource_weight(W) when W > 100 -> 100;
resource_weight(W) when W < 1 -> 1;
resource_weight(W) -> W.

-spec resource_is_emergency(kz_json:object()) -> boolean().
resource_is_emergency(JObj) ->
    kz_json:is_true(<<"emergency">>, JObj)
        orelse (kz_json:get_value([<<"caller_id_options">>, <<"type">>], JObj) =:= <<"emergency">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec gateway_from_jobj(kz_json:object(), resource()) -> gateway().
gateway_from_jobj(JObj, #resrc{is_emergency=IsEmergency
                              ,format_from_uri=FormatFrom
                              ,from_uri_realm=FromRealm
                              ,from_account_realm=FromAccountRealm
                              ,fax_option=T38
                              ,codecs=Codecs
                              ,bypass_media=BypassMedia
                              ,privacy_mode=PrivacyMode
                              }) ->
    EndpointType = kz_json:get_ne_value(<<"endpoint_type">>, JObj, <<"sip">>),
    #gateway{endpoint_type = EndpointType
            ,server = kz_json:get_ne_binary_value(<<"server">>, JObj)
            ,port = kz_json:get_integer_value(<<"port">>, JObj)
            ,realm = kz_json:get_value(<<"realm">>, JObj)
            ,username = kz_json:get_value(<<"username">>, JObj)
            ,password = kz_json:get_value(<<"password">>, JObj)
            ,sip_headers = kz_json:get_ne_value(<<"custom_sip_headers">>, JObj)
            ,sip_interface = kz_json:get_ne_value(<<"custom_sip_interface">>, JObj)
            ,invite_format = kz_json:get_value(<<"invite_format">>, JObj, <<"route">>)
            ,format_from_uri = kz_json:is_true(<<"format_from_uri">>, JObj, FormatFrom)
            ,from_uri_realm = kz_json:get_ne_value(<<"from_uri_realm">>, JObj, FromRealm)
            ,from_account_realm=kz_json:is_true(<<"from_account_realm">>, JObj, FromAccountRealm)
            ,is_emergency = gateway_is_emergency(JObj, IsEmergency)
            ,fax_option = kz_json:is_true([<<"media">>, <<"fax_option">>], JObj, T38)
            ,codecs = kz_json:get_value(<<"codecs">>, JObj, Codecs)
            ,bypass_media = kz_json:is_true(<<"bypass_media">>, JObj, BypassMedia)
            ,force_port = kz_json:is_true(<<"force_port">>, JObj)
            ,route = kz_json:get_ne_value(<<"route">>, JObj, ?DEFAULT_ROUTE)
            ,prefix = kz_json:get_binary_value(<<"prefix">>, JObj, ?DEFAULT_PREFIX)
            ,suffix = kz_json:get_binary_value(<<"suffix">>, JObj, ?DEFAULT_SUFFIX)
            ,rtcp_mux = kz_json:get_binary_value([<<"media">>, <<"rtcp_mux">>], JObj, ?DEFAULT_RTCP_MUX)
            ,caller_id_type = kz_json:get_ne_value(<<"caller_id_type">>, JObj, ?DEFAULT_CALLER_ID_TYPE)
            ,progress_timeout = kz_json:get_integer_value(<<"progress_timeout">>, JObj, ?DEFAULT_PROGRESS_TIMEOUT)
            ,endpoint_options = endpoint_options(JObj, EndpointType)
            ,privacy_mode=kz_json:get_value(<<"privacy_mode">>, JObj, PrivacyMode)
            }.

-spec gateway_is_emergency(kz_json:object(), boolean()) -> boolean().
gateway_is_emergency(_, 'true') -> 'true';
gateway_is_emergency(JObj, IsEmergency) ->
    kz_json:is_true(<<"emergency">>, JObj, IsEmergency).

-spec endpoint_options(kz_json:object(), api_binary()) -> kz_json:object().
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
endpoint_options(JObj, <<"amqp">>) ->
    Server = kz_json:get_value(<<"server">>, JObj),
    User = kz_json:get_value(<<"username">>, JObj),
    Password = kz_json:get_value(<<"password">>, JObj),
    Broker = <<"amqp://", User/binary, ":", Password/binary, "@", Server/binary>>,

    kz_json:from_list(
      [{<<"AMQP-Broker">>, Broker}
      ,{<<"Exchange-ID">>, kz_json:get_value(<<"amqp_exchange">>, JObj)}
      ,{<<"Exchange-Type">>, kz_json:get_value(<<"amqp_exchange_type">>, JObj)}
      ,{<<"Route-ID">>, kz_json:get_value(<<"route_id">>, JObj)}
      ,{<<"System-ID">>, kz_json:get_value(<<"system_id">>, JObj)}
      ,{<<"Broker-Name">>, kz_json:get_value(<<"broker_name">>, JObj, kz_binary:rand_hex(6))}
      ,{<<"Exchange-Options">>, kz_json:get_value(<<"amqp_exchange_options">>, JObj, ?DEFAULT_AMQP_EXCHANGE_OPTIONS)}
      ]);
endpoint_options(JObj, <<"sip">>) ->
    kz_json:from_list(
      [{<<"Route-ID">>, kz_json:get_value(<<"route_id">>, JObj)}
      ]);
endpoint_options(_, _) ->
    kz_json:new().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the sip url of a resource gateway
%% @end
%%--------------------------------------------------------------------
-spec gateway_dialstring(gateway(), ne_binary()) -> ne_binary().
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

-spec get_resrc_id(resource()) -> api_binary().
-spec get_resrc_rev(resource()) -> api_binary().
-spec get_resrc_name(resource()) -> api_binary().
-spec get_resrc_weight(resource()) -> non_neg_integer().
-spec get_resrc_grace_period(resource()) -> non_neg_integer().
-spec get_resrc_flags(resource()) -> list().
-spec get_resrc_rules(resource()) -> list().
-spec get_resrc_raw_rules(resource()) -> list().
-spec get_resrc_cid_rules(resource()) -> list().
-spec get_resrc_cid_raw_rules(resource()) -> list().
-spec get_resrc_gateways(resource()) -> list().
-spec get_resrc_is_emergency(resource()) -> boolean().
-spec get_resrc_require_flags(resource()) -> boolean().
-spec get_resrc_global(resource()) -> boolean().
-spec get_resrc_format_from_uri(resource()) -> boolean().
-spec get_resrc_from_uri_realm(resource()) -> api_binary().
-spec get_resrc_from_account_realm(resource()) -> boolean().
-spec get_resrc_fax_option(resource()) -> ne_binary() | boolean().
-spec get_resrc_codecs(resource()) -> ne_binaries().
-spec get_resrc_bypass_media(resource()) -> boolean().
-spec get_resrc_formatters(resource()) -> api_objects().
-spec get_resrc_proxies(resource()) -> kz_proplist().
-spec get_resrc_selector_marks(resource()) -> kz_proplist().
-spec get_resrc_classifier(resource()) -> api_binary().
-spec get_resrc_classifier_enable(resource()) -> api_boolean().

get_resrc_id(#resrc{id=Id}) -> Id.
get_resrc_rev(#resrc{rev=Rev}) -> Rev.
get_resrc_name(#resrc{name=Name}) -> Name.
get_resrc_weight(#resrc{weight=Weight}) -> Weight.
get_resrc_grace_period(#resrc{grace_period=GracePeriod}) -> GracePeriod.
get_resrc_flags(#resrc{flags=Flags}) -> Flags.
get_resrc_rules(#resrc{rules=Rules}) -> Rules.
get_resrc_raw_rules(#resrc{raw_rules=RawRules}) -> RawRules.
get_resrc_cid_rules(#resrc{cid_rules=CIDRules}) -> CIDRules.
get_resrc_cid_raw_rules(#resrc{cid_raw_rules=CIDRawRules}) -> CIDRawRules.
get_resrc_gateways(#resrc{gateways=Gateways}) -> Gateways.
get_resrc_is_emergency(#resrc{is_emergency=IsEmergency}) -> IsEmergency.
get_resrc_require_flags(#resrc{require_flags=RequireFlags}) -> RequireFlags.
get_resrc_global(#resrc{global=Global}) -> Global.
get_resrc_format_from_uri(#resrc{format_from_uri=FormatFromUri}) -> FormatFromUri.
get_resrc_from_uri_realm(#resrc{from_uri_realm=FromUriRealm}) -> FromUriRealm.
get_resrc_from_account_realm(#resrc{from_account_realm=FromAccountRealm}) -> FromAccountRealm.
get_resrc_fax_option(#resrc{fax_option=FaxOption}) -> FaxOption.
get_resrc_codecs(#resrc{codecs=Codecs}) -> Codecs.
get_resrc_bypass_media(#resrc{bypass_media=BypassMedia}) -> BypassMedia.
get_resrc_formatters(#resrc{formatters=Formatters}) -> Formatters.
get_resrc_proxies(#resrc{proxies=Proxies}) -> Proxies.
get_resrc_selector_marks(#resrc{selector_marks=Marks}) -> Marks.
get_resrc_classifier(#resrc{classifier=Classifier}) -> Classifier.
get_resrc_classifier_enable(#resrc{classifier_enable=Enabled}) -> Enabled.

-spec set_resrc_id(resource(), api_binary()) -> resource().
-spec set_resrc_rev(resource(), api_binary()) -> resource().
-spec set_resrc_name(resource(), api_binary()) -> resource().
-spec set_resrc_weight(resource(), non_neg_integer()) -> resource().
-spec set_resrc_grace_period(resource(), non_neg_integer()) -> resource().
-spec set_resrc_flags(resource(), list()) -> resource().
-spec set_resrc_rules(resource(), list()) -> resource().
-spec set_resrc_raw_rules(resource(), list()) -> resource().
-spec set_resrc_cid_rules(resource(), list()) -> resource().
-spec set_resrc_cid_raw_rules(resource(), list()) -> resource().
-spec set_resrc_gateways(resource(), list()) -> resource().
-spec set_resrc_is_emergency(resource(), boolean()) -> resource().
-spec set_resrc_require_flags(resource(), boolean()) -> resource().
-spec set_resrc_global(resource(), boolean()) -> resource().
-spec set_resrc_format_from_uri(resource(), boolean()) -> resource().
-spec set_resrc_from_uri_realm(resource(), api_binary()) -> resource().
-spec set_resrc_from_account_realm(resource(), boolean()) -> resource().
-spec set_resrc_fax_option(resource(), ne_binary() | boolean()) -> resource().
-spec set_resrc_codecs(resource(), ne_binaries()) -> resource().
-spec set_resrc_bypass_media(resource(), boolean()) -> resource().
-spec set_resrc_formatters(resource(), api_objects()) -> resource().
-spec set_resrc_proxies(resource(), kz_proplist()) -> resource().
-spec set_resrc_selector_marks(resource(), kz_proplist()) -> resource().

set_resrc_id(Resource, Id) -> Resource#resrc{id=Id}.
set_resrc_rev(Resource, Rev) -> Resource#resrc{rev=Rev}.
set_resrc_name(Resource, Name) -> Resource#resrc{name=Name}.
set_resrc_weight(Resource, Weight) -> Resource#resrc{weight=Weight}.
set_resrc_grace_period(Resource, GracePeriod) -> Resource#resrc{grace_period=GracePeriod}.
set_resrc_flags(Resource, Flags) -> Resource#resrc{flags=Flags}.
set_resrc_rules(Resource, Rules) -> Resource#resrc{rules=Rules}.
set_resrc_raw_rules(Resource, RawRules) -> Resource#resrc{raw_rules=RawRules}.
set_resrc_cid_rules(Resource, CIDRules) -> Resource#resrc{cid_rules=CIDRules}.
set_resrc_cid_raw_rules(Resource, CIDRawRules) -> Resource#resrc{cid_raw_rules=CIDRawRules}.
set_resrc_gateways(Resource, Gateways) -> Resource#resrc{gateways=Gateways}.
set_resrc_is_emergency(Resource, IsEmergency) -> Resource#resrc{is_emergency=IsEmergency}.
set_resrc_require_flags(Resource, RequireFlags) -> Resource#resrc{require_flags=RequireFlags}.
set_resrc_global(Resource, Global) -> Resource#resrc{global=Global}.
set_resrc_format_from_uri(Resource, FormatFromUri) -> Resource#resrc{format_from_uri=FormatFromUri}.
set_resrc_from_uri_realm(Resource, FromUriRealm) -> Resource#resrc{from_uri_realm=FromUriRealm}.
set_resrc_from_account_realm(Resource, FromAccountRealm) -> Resource#resrc{from_account_realm=FromAccountRealm}.
set_resrc_fax_option(Resource, FaxOption) -> Resource#resrc{fax_option=FaxOption}.
set_resrc_codecs(Resource, Codecs) -> Resource#resrc{codecs=Codecs}.
set_resrc_bypass_media(Resource, BypassMedia) -> Resource#resrc{bypass_media=BypassMedia}.
set_resrc_formatters(Resource, Formatters) -> Resource#resrc{formatters=Formatters}.
set_resrc_proxies(Resource, Proxies) -> Resource#resrc{proxies=Proxies}.
set_resrc_selector_marks(Resource, Marks) -> Resource#resrc{selector_marks=Marks}.
