%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
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

-include("stepswitch.hrl").
-include_lib("whistle/src/wh_json.hrl").

-define(CONFIG_CAT, <<"number_manager">>).

-define(DEFAULT_ROUTE, whapps_config:get_binary(?SS_CONFIG_CAT, <<"default_route">>)).
-define(DEFAULT_PREFIX, whapps_config:get_binary(?SS_CONFIG_CAT, <<"default_prefix">>, <<>>)).
-define(DEFAULT_SUFFIX, whapps_config:get_binary(?SS_CONFIG_CAT, <<"default_suffix">>, <<>>)).
-define(DEFAULT_CALLER_ID_TYPE,
        whapps_config:get_binary(?SS_CONFIG_CAT, <<"default_caller_id_type">>, <<"external">>)).
-define(DEFAULT_PROGRESS_TIMEOUT,
        whapps_config:get_integer(?SS_CONFIG_CAT, <<"default_progress_timeout">>, 8)).

-record(gateway, {
           server :: api_binary()
           ,port :: api_integer()
           ,realm :: api_binary()
           ,username :: api_binary()
           ,password :: api_binary()
           ,route :: api_binary()
           ,prefix = <<>> :: binary()
           ,suffix = <<>> :: binary()
           ,codecs = [] :: ne_binaries()
           ,bypass_media = 'false' :: boolean()
           ,caller_id_type = <<"external">> :: ne_binary()
           ,fax_option :: ne_binary() | boolean()
           ,sip_headers :: api_object()
           ,sip_interface :: api_binary()
           ,progress_timeout = 8 :: 1..100
           ,invite_format = <<"route">> :: ne_binary()
           ,endpoint_type = <<"sip">> :: ne_binary()
           ,endpoint_options = wh_json:new() :: wh_json:object()
           ,format_from_uri = 'false' :: boolean()
           ,from_uri_realm :: api_binary()
           ,is_emergency = 'false' :: boolean()
           ,force_port = 'false' :: boolean()
         }).

-record(resrc, {
           id :: api_binary()
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
           ,fax_option :: ne_binary() | boolean()
           ,codecs = [] :: ne_binaries()
           ,bypass_media = 'false' :: boolean()
           ,formatters :: api_objects()
           ,proxies = [] :: wh_proplist()
         }).

-type resource() :: #resrc{}.
-type resources() :: [#resrc{}].

-type gateway() :: #gateway{}.
-type gateways() :: [#gateway{}].

-compile({'no_auto_import', [get/0, get/1]}).

-spec get_props() -> wh_proplists().
get_props() ->
    [resource_to_props(Resource)
     || Resource <- sort_resources(get())
    ].

-spec get_props(ne_binary()) -> wh_proplist() | 'undefined'.
get_props(ResourceId) ->
    case get_resource(ResourceId) of
        'undefined' -> 'undefined';
        Resource -> resource_to_props(Resource)
    end.

-spec get_props(ne_binary(), api_binary()) -> wh_proplist() | 'undefined'.
get_props(_ResourceId, 'undefined') -> 'undefined';
get_props(ResourceId, AccountId) ->
    case get_local_resource(ResourceId, AccountId) of
        'undefined' -> 'undefined';
        Resource -> resource_to_props(Resource)
    end.

-spec resource_to_props(resource()) -> wh_proplist().
resource_to_props(#resrc{}=Resource) ->
    props:filter_undefined(
      [{<<"Name">>, Resource#resrc.name}
       ,{<<"ID">>, Resource#resrc.id}
       ,{<<"Rev">>, Resource#resrc.rev}
       ,{<<"Weight">>, Resource#resrc.weight}
       ,{<<"Global">>, Resource#resrc.global}
       ,{<<"Format-From-URI">>, Resource#resrc.format_from_uri}
       ,{<<"From-URI-Realm">>, Resource#resrc.from_uri_realm}
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
-spec endpoints(ne_binary(), wapi_offnet_resource:req()) -> wh_json:objects().
endpoints(Number, OffnetJObj) ->
    case maybe_get_endpoints(Number, OffnetJObj) of
        [] -> [];
        Endpoints -> sort_endpoints(Endpoints)
    end.

-spec maybe_get_endpoints(ne_binary(), wapi_offnet_resource:req()) -> wh_json:objects().
maybe_get_endpoints(Number, OffnetJObj) ->
    case wapi_offnet_resource:hunt_account_id(OffnetJObj) of
        'undefined' -> get_global_endpoints(Number, OffnetJObj);
        HuntAccount -> maybe_get_local_endpoints(HuntAccount, Number, OffnetJObj)
    end.

-spec maybe_get_local_endpoints(ne_binary(), ne_binary(), wapi_offnet_resource:req()) -> wh_json:objects().
maybe_get_local_endpoints(HuntAccount, Number, OffnetJObj) ->
    AccountId = wapi_offnet_resource:account_id(OffnetJObj),
    case wh_util:is_in_account_hierarchy(HuntAccount, AccountId, 'true') of
        'false' ->
            lager:info("account ~s attempted to use local resources of ~s, but it is not allowed"
                       ,[AccountId, HuntAccount]
                      ),
            [];
        'true' ->
            lager:info("account ~s is using the local resources of ~s", [AccountId, HuntAccount]),
            get_local_endpoints(HuntAccount, Number, OffnetJObj)
    end.

-spec get_local_endpoints(ne_binary(), ne_binary(), wapi_offnet_resource:req()) -> wh_json:objects().
get_local_endpoints(AccountId, Number, OffnetJObj) ->
    lager:debug("attempting to find local resources for ~s", [AccountId]),
    Flags = wapi_offnet_resource:flags(OffnetJObj, []),
    Resources = filter_resources(Flags, get(AccountId)),
    resources_to_endpoints(Resources, Number, OffnetJObj).

-spec get_global_endpoints(ne_binary(), wapi_offnet_resource:req()) -> wh_json:objects().
get_global_endpoints(Number, OffnetJObj) ->
    lager:debug("attempting to find global resources"),
    Flags = wapi_offnet_resource:flags(OffnetJObj, []),
    Resources = filter_resources(Flags, get()),
    resources_to_endpoints(Resources, Number, OffnetJObj).

-spec sort_endpoints(wh_json:objects()) -> wh_json:objects().
sort_endpoints(Endpoints) ->
    lists:sort(fun endpoint_ordering/2, Endpoints).

-spec endpoint_ordering(wh_json:object(), wh_json:object()) -> boolean().
endpoint_ordering(P1, P2) ->
    wh_json:get_value(<<"Weight">>, P1, 1)
        =< wh_json:get_value(<<"Weight">>, P2, 1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reverse_lookup(wh_json:object()) ->
                            {'ok', wh_proplist()} |
                            {'error', 'not_found'}.
reverse_lookup(JObj) ->
    Realm = stepswitch_util:get_realm(JObj),
    IP = wh_json:get_first_defined([<<"From-Network-Addr">>
                                    ,<<"Orig-IP">>
                                   ], JObj),
    Port = find_port(JObj),
    case maybe_find_global(IP, Port, Realm) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            AccountId = find_account_id(Realm, JObj),
            maybe_find_local(IP, Port, Realm, AccountId)
    end.

-spec find_port(wh_json:object()) -> api_integer().
find_port(JObj) ->
    case wh_json:get_first_defined([<<"From-Network-Port">>
                                    ,<<"Orig-Port">>
                                   ], JObj)
    of
        'undefined' -> 'undefined';
        Port -> wh_util:to_integer(Port)
    end.

-spec find_account_id(api_binary(), wh_json:object()) -> api_binary().
find_account_id(Realm, JObj) ->
    case wh_json:get_first_defined([<<"Account-ID">>
                                    ,?CCV(<<"Account-ID">>)
                                   ], JObj)
    of
        'undefined' -> find_account_id(Realm);
        AccountId -> AccountId
    end.

-spec find_account_id(api_binary()) -> api_binary().
find_account_id('undefined') -> 'undefined';
find_account_id(Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
        {'error', 'not_found'} -> 'undefined';
        {'ok', AccountId} -> AccountId
    end.

-spec maybe_find_global(api_binary(), api_integer(), api_binary()) ->
                               {'ok', wh_proplist()} |
                               {'error', 'not_found'}.
maybe_find_global(IP, Port, Realm) ->
    search_resources(IP, Port, Realm, get()).

-spec maybe_find_local(api_binary(), api_integer(), api_binary(), api_binary()) ->
                              {'ok', wh_proplist()} |
                              {'error', 'not_found'}.
maybe_find_local(_, _, _, 'undefined') -> {'error', 'not_found'};
maybe_find_local(IP, Port, Realm, AccountId) ->
    search_resources(IP, Port, Realm, get(AccountId)).

-spec search_resources(api_binary(), api_integer(), api_binary(), resources()) ->
                              {'ok', wh_proplist()} |
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
              ) when IP =/= 'undefined' andalso Port =/= 'undefined' ->
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
    case wh_util:is_empty(Flag)
        orelse lists:member(Flag, ResourceFlags)
    of
        'true' -> 'true';
        'false' ->
            lager:debug("resource ~s does not have the required flag: ~s"
                        ,[_Id, Flag]
                       ),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resources_to_endpoints(resources(), ne_binary(), wapi_offnet_resource:req()) ->
                                    wh_json:objects().
-spec resources_to_endpoints(resources(), ne_binary(), wapi_offnet_resource:req(), wh_json:objects()) ->
                                    wh_json:objects().
resources_to_endpoints(Resources, Number, OffnetJObj) ->
    resources_to_endpoints(Resources, Number, OffnetJObj, []).

resources_to_endpoints([], _Number, _OffnetJObj, Endpoints) ->
    lists:reverse(Endpoints);
resources_to_endpoints([Resource|Resources], Number, OffnetJObj, Endpoints) ->
    MoreEndpoints = maybe_resource_to_endpoints(Resource, Number, OffnetJObj, Endpoints),
    resources_to_endpoints(Resources, Number, OffnetJObj, MoreEndpoints).

-spec maybe_resource_to_endpoints(resource(), ne_binary(), wapi_offnet_resource:req(), wh_json:objects()) ->
                                         wh_json:objects().
maybe_resource_to_endpoints(#resrc{id=Id
                                   ,name=Name
                                   ,rules=Rules
                                   ,cid_rules=CallerIdRules
                                   ,gateways=Gateways
                                   ,global=Global
                                   ,weight=Weight
                                   ,proxies=Proxies
                                  }
                            ,Number
                            ,OffnetJObj
                            ,Endpoints
                           ) ->
    CallerIdNumber = case ?RULES_HONOR_DIVERSION of
                         'false' -> wapi_offnet_resource:outbound_caller_id_number(OffnetJObj);
                         'true' -> check_diversion_fields(OffnetJObj)
                     end,
    case filter_resource_by_rules(Id, Number, Rules, CallerIdNumber, CallerIdRules) of
        {'error','no_match'} -> Endpoints;
        {'ok', NumberMatch} ->
            lager:debug("building resource ~s endpoints", [Id]),
            CCVUpdates = [{<<"Global-Resource">>, wh_util:to_binary(Global)}
                          ,{<<"Resource-ID">>, Id}
                          ,{<<"E164-Destination">>, Number}
                          ,{<<"Original-Number">>, wapi_offnet_resource:to_did(OffnetJObj)}
                         ],
            Updates = [{<<"Name">>, Name}
                       ,{<<"Weight">>, Weight}
                      ],
            EndpointList = [update_endpoint(Endpoint, Updates, CCVUpdates, OffnetJObj)
                            || Endpoint <- gateways_to_endpoints(NumberMatch, Gateways, OffnetJObj, [])
                           ],
            maybe_add_proxies(EndpointList, Proxies, Endpoints)
    end.

-spec check_diversion_fields(wapi_offnet_resource:req()) -> ne_binary().
check_diversion_fields(OffnetJObj) ->
    case wh_json:get_value([<<"Custom-SIP-Headers">>,<<"Diversions">>], OffnetJObj) of
        [Diversion|_] ->
            [_,CallerIdNumber,_] = binary:split(Diversion, [<<":">>,<<"@">>], ['global']),
            CallerIdNumber;
        _ ->
            wapi_offnet_resource:outbound_caller_id_number(OffnetJObj)
    end.

-spec update_endpoint(wh_json:object(), wh_proplist(), wh_proplist(), wapi_offnet_resource:req()) -> wh_json:object().
update_endpoint(Endpoint, Updates, CCVUpdates, OffnetJObj) ->
    Updated = wh_json:set_values(Updates ,update_ccvs(Endpoint, CCVUpdates)),
    maybe_apply_formatters(Updated, OffnetJObj).

-spec maybe_apply_formatters(wh_json:object(), wapi_offnet_resource:req()) -> wh_json:object().
maybe_apply_formatters(Endpoint, OffnetJObj) ->
    SIPHeaders = stepswitch_util:get_sip_headers(OffnetJObj),
    AccountId = wapi_offnet_resource:account_id(OffnetJObj),

    stepswitch_formatters:apply(maybe_add_sip_headers(Endpoint, SIPHeaders)
                                ,props:get_value(<<"Formatters">>
                                                 ,endpoint_props(Endpoint, AccountId)
                                                 ,wh_json:new()
                                                )
                                ,'outbound'
                               ).

-spec maybe_add_sip_headers(wh_json:object(), wh_json:object()) -> wh_json:object().
maybe_add_sip_headers(Endpoint, SIPHeaders) ->
    LocalSIPHeaders = wh_json:get_value(<<"Custom-SIP-Headers">>, Endpoint, wh_json:new()),

    case wh_json:merge_jobjs(SIPHeaders, LocalSIPHeaders) of
        ?EMPTY_JSON_OBJECT -> Endpoint;
        MergedHeaders -> wh_json:set_value(<<"Custom-SIP-Headers">>, MergedHeaders, Endpoint)
    end.

-spec endpoint_props(wh_json:object(), api_binary()) -> wh_proplist().
endpoint_props(Endpoint, AccountId) ->
    ResourceId = wh_json:get_value(?CCV(<<"Resource-ID">>), Endpoint),
    case wh_json:is_true(?CCV(<<"Global-Resource">>), Endpoint) of
        'true' ->
            empty_list_on_undefined(stepswitch_resources:get_props(ResourceId));
        'false' ->
            empty_list_on_undefined(stepswitch_resources:get_props(ResourceId, AccountId))
    end.

-spec empty_list_on_undefined(wh_proplist() | 'undefined') -> wh_proplist().
empty_list_on_undefined('undefined') -> [];
empty_list_on_undefined(L) -> L.

-spec maybe_add_proxies(wh_json:objects(), wh_proplist(), wh_json:objects()) -> wh_json:objects().
maybe_add_proxies([], _, Acc) -> Acc;
maybe_add_proxies(Endpoints, [], Acc) -> Acc ++ Endpoints;
maybe_add_proxies([Endpoint | Endpoints], Proxies, Acc) ->
    EPs = [add_proxy(Endpoint, Proxy)  || Proxy <- Proxies],
    maybe_add_proxies(Endpoints, Proxies, Acc ++ EPs).

-spec add_proxy(wh_json:object(), {binary(), binary()}) -> wh_json:object().
add_proxy(Endpoint, {Zone, IP}) ->
    Updates = [{<<"Proxy-Zone">>, Zone}
               ,{<<"Proxy-IP">>, IP}
              ],
    wh_json:set_values(Updates ,Endpoint).

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

-spec update_ccvs(wh_json:object(), wh_proplist()) -> wh_json:object().
update_ccvs(Endpoint, Updates) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, wh_json:new()),
    wh_json:set_value(<<"Custom-Channel-Vars">>, wh_json:set_values(Updates, CCVs), Endpoint).

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
        _ -> evaluate_rules(Rules, Number)
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
-spec gateways_to_endpoints(ne_binary(), gateways(), wapi_offnet_resource:req(), wh_json:objects()) ->
                                   wh_json:objects().
gateways_to_endpoints(_Number, [], _OffnetJObj, Endpoints) -> Endpoints;
gateways_to_endpoints(Number, [Gateway|Gateways], OffnetJObj, Endpoints) ->
    gateways_to_endpoints(Number
                          ,Gateways
                          ,OffnetJObj
                          ,[gateway_to_endpoint(Number, Gateway, OffnetJObj) | Endpoints]
                         ).

-spec gateway_to_endpoint(ne_binary(), gateway(), wapi_offnet_resource:req()) ->
                                 wh_json:object().
gateway_to_endpoint(Number
                    ,#gateway{invite_format=InviteFormat
                              ,caller_id_type=CallerIdType
                              ,bypass_media=BypassMedia
                              ,codecs=Codecs
                              ,username=Username
                              ,password=Password
                              ,sip_headers=SipHeaders
                              ,sip_interface=SipInterface
                              ,endpoint_type=EndpointType
                              ,endpoint_options=EndpointOptions
                              ,progress_timeout=ProgressTimeout
                             }=Gateway
                    ,OffnetJObj
                   ) ->
    CCVs = props:filter_empty(
             [{<<"Emergency-Resource">>, gateway_emergency_resource(Gateway)}
              ,{<<"Matched-Number">>, Number}
              | gateway_from_uri_settings(Gateway)
             ]),
    wh_json:from_list(
      props:filter_empty(
        [{<<"Route">>, gateway_dialstring(Gateway, Number)}
         ,{<<"Callee-ID-Name">>, wh_util:to_binary(Number)}
         ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
         ,{<<"To-DID">>, wh_util:to_binary(Number)}
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
         ,{<<"Endpoint-Progress-Timeout">>, wh_util:to_binary(ProgressTimeout)}
         ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
         ,{<<"Outbound-Caller-ID-Number">>, wapi_offnet_resource:outbound_caller_id_number(OffnetJObj)}
         ,{<<"Outbound-Caller-ID-Name">>, wapi_offnet_resource:outbound_caller_id_name(OffnetJObj)}
         | maybe_get_t38(Gateway, OffnetJObj)
        ])).

-spec gateway_from_uri_settings(gateway()) -> wh_proplist().
gateway_from_uri_settings(#gateway{format_from_uri='true'
                                   ,from_uri_realm=Realm
                                  }) ->
    lager:debug("using gateway from_uri_realm in From: ~s", [Realm]),
    [{<<"Format-From-URI">>, 'true'}
     ,{<<"From-URI-Realm">>, Realm}
    ];
gateway_from_uri_settings(#gateway{format_from_uri='false'
                                   ,realm='undefined'
                                  }) ->
    [{<<"Format-From-URI">>, 'false'}];
gateway_from_uri_settings(#gateway{format_from_uri='false'
                                   ,realm=Realm
                                  }) ->
    lager:debug("using gateway realm in From: ~s", [Realm]),
    [{<<"Format-From-URI">>, 'true'}
     ,{<<"From-URI-Realm">>, Realm}
    ].

-spec maybe_get_t38(gateway(), wapi_offnet_resource:req()) -> wh_proplist().
maybe_get_t38(#gateway{fax_option=FaxOption}, OffnetJObj) ->
    Flags = wapi_offnet_resource:flags(OffnetJObj, []),
    case lists:member(<<"fax">>, Flags) of
        'false' -> [];
        'true' ->
            whapps_call_command:get_outbound_t38_settings(
              FaxOption
              ,wapi_offnet_resource:t38_enabled(OffnetJObj)
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
    case wh_cache:fetch_local(?STEPSWITCH_CACHE, 'global_resources') of
        {'ok', Resources} -> Resources;
        {'error', 'not_found'} -> fetch_global_resources()
    end;
get(AccountId) ->
    case wh_cache:fetch_local(?STEPSWITCH_CACHE, {'local_resources', AccountId}) of
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
    case couch_mgr:get_results(?RESOURCES_DB, ?LIST_RESOURCES_BY_ID, ViewOptions) of
        {'error', _R} ->
            lager:warning("unable to fetch global resources: ~p", [_R]),
            [];
        {'ok', JObjs} ->
            CacheProps = [{'origin', [{'db', ?RESOURCES_DB, <<"resource">>}]}],
            Docs = [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs],
            Resources = resources_from_jobjs(Docs),
            wh_cache:store_local(?STEPSWITCH_CACHE, 'global_resources', Resources, CacheProps),
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
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = ['include_docs'],
    lager:debug("local resource cache miss, fetching from db ~s", [AccountDb]),
    case couch_mgr:get_results(AccountDb, ?LIST_RESOURCES_BY_ID, ViewOptions) of
        {'error', _R} ->
            lager:warning("unable to fetch local resources from ~s: ~p", [AccountId, _R]),
            [];
        {'ok', JObjs} ->
            LocalResources = fetch_local_resources(AccountId, JObjs),
            CacheProps = [{'origin', [{'db', AccountDb, <<"resource">>}]}],
            wh_cache:store_local(?STEPSWITCH_CACHE, {'local_resources', AccountId}, LocalResources, CacheProps),
            LocalResources
    end.

-spec fetch_local_resources(ne_binary(), wh_json:objects()) -> resources().
fetch_local_resources(AccountId, JObjs) ->
    Proxies = fetch_account_dedicated_proxies(AccountId),
    resources_from_jobjs(
      [wh_json:set_values([{<<"Is-Global">>, 'false'}
                           ,{<<"Proxies">>, wh_json:from_list(Proxies)}
                          ]
                          ,wh_json:get_value(<<"doc">>, JObj)
                         )
       || JObj <- JObjs
      ]).

-spec fetch_account_dedicated_proxies(api_binary()) -> wh_proplist().
fetch_account_dedicated_proxies('undefined') -> [];
fetch_account_dedicated_proxies(AccountId) ->
    case kz_ips:assigned(AccountId) of
        {'ok', IPS} -> [build_account_dedicated_proxy(IP) || IP <- IPS];
        _ -> []
    end.

-spec build_account_dedicated_proxy(wh_json:object()) -> {api_binary(), api_binary()}.
build_account_dedicated_proxy(Proxy) ->
    Zone = wh_json:get_value(<<"zone">>, Proxy),
    ProxyIP = wh_json:get_value(<<"ip">>, Proxy),
    {Zone, ProxyIP}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resources_from_jobjs(wh_json:objects()) -> resources().
-spec resources_from_jobjs(wh_json:objects(), resources()) -> resources().
resources_from_jobjs(JObjs) ->
    resources_from_jobjs(JObjs, []).

resources_from_jobjs([], Resources) -> Resources;
resources_from_jobjs([JObj|JObjs], Resources) ->
    case wh_json:is_true(<<"enabled">>, JObj, 'true') of
        'false' -> resources_from_jobjs(JObjs, Resources);
        'true' -> resources_from_jobjs(JObjs, create_resource(JObj, Resources))
    end.

-spec create_resource(wh_json:object(), resources()) -> resources().
create_resource(JObj, Resources) ->
    case wh_json:get_value(<<"classifiers">>, JObj) of
        'undefined' -> [resource_from_jobj(JObj) | Resources];
        ResourceClassifiers ->
            AccountId = wh_doc:account_id(JObj),
            create_resource(wh_json:to_proplist(ResourceClassifiers)
                            ,wh_json:to_proplist(wnm_util:available_classifiers(AccountId))
                            ,JObj
                            ,Resources
                           )
    end.

-spec create_resource(wh_proplist(), wh_proplist(), wh_json:object(), resources()) -> resources().
create_resource([], _ConfigClassifiers, _Resource, Resources) -> Resources;
create_resource([{Classifier, ClassifierJObj}|Classifiers], ConfigClassifiers, Resource, Resources) ->
    case (ConfigClassifier = props:get_value(Classifier, ConfigClassifiers)) =/= 'undefined'
        andalso wh_json:is_true(<<"enabled">>, ClassifierJObj, 'true')
    of
        'false' -> create_resource(Classifiers, ConfigClassifiers, Resource, Resources);
        'true' ->
            JObj =
                create_classifier_resource(
                  Resource
                  ,ClassifierJObj
                  ,Classifier
                  ,wh_json:get_value(<<"regex">>, ConfigClassifier)
                 ),
            create_resource(
              Classifiers
              ,ConfigClassifiers
              ,Resource
              ,[resource_from_jobj(JObj)
                | Resources
               ]
             )
    end.

-spec create_classifier_resource(wh_json:object(), wh_json:object(), ne_binary(), ne_binary()) -> wh_json:object().
create_classifier_resource(Resource, ClassifierJObj, Classifier, DefaultRegex) ->
    Props =
        props:filter_undefined(
          [{<<"_id">>, <<(wh_json:get_value(<<"_id">>, Resource))/binary, "-", Classifier/binary>>}
           ,{<<"name">>, <<(wh_json:get_value(<<"name">>, Resource))/binary, " - ", Classifier/binary>>}
           ,{<<"rules">>, [wh_json:get_value(<<"regex">>, ClassifierJObj, DefaultRegex)]}
           ,{<<"weight_cost">>, wh_json:get_value(<<"weight_cost">>, ClassifierJObj)}
          ]
         ),
    create_classifier_gateways(wh_json:set_values(Props, Resource), ClassifierJObj).

-spec create_classifier_gateways(wh_json:object(), wh_json:object()) -> wh_json:object().
create_classifier_gateways(Resource, ClassifierJObj) ->
    Props =
        props:filter_undefined(
          [{<<"suffix">>, wh_json:get_value(<<"suffix">>, ClassifierJObj)}
           ,{<<"prefix">>, wh_json:get_value(<<"prefix">>, ClassifierJObj)}
          ]
         ),
    Gateways =
        [wh_json:set_values(Props, Gateway)
         || Gateway <- wh_json:get_value(<<"gateways">>, Resource, [])
        ],
    wh_json:set_value(<<"gateways">>, Gateways, Resource).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type rule() :: re:mp().
-type rules() :: [rule()].

-spec resource_from_jobj(wh_json:object()) -> resource().
resource_from_jobj(JObj) ->
    Resource = #resrc{id=wh_doc:id(JObj)
                      ,rev=wh_doc:revision(JObj)
                      ,name=wh_json:get_value(<<"name">>, JObj)
                      ,flags=wh_json:get_value(<<"flags">>, JObj, [])
                      ,require_flags=wh_json:is_true(<<"require_flags">>, JObj)
                      ,format_from_uri=wh_json:is_true(<<"format_from_uri">>, JObj)
                      ,from_uri_realm=wh_json:get_ne_value(<<"from_uri_realm">>, JObj)
                      ,fax_option=wh_json:is_true([<<"media">>, <<"fax_option">>], JObj)
                      ,raw_rules=wh_json:get_value(<<"rules">>, JObj, [])
                      ,rules=resource_rules(JObj)
                      ,cid_raw_rules=wh_json:get_value(<<"cid_rules">>, JObj, [])
                      ,cid_rules=resource_cid_rules(JObj)
                      ,weight=resource_weight(JObj)
                      ,grace_period=resource_grace_period(JObj)
                      ,is_emergency=resource_is_emergency(JObj)
                      ,codecs=resource_codecs(JObj)
                      ,bypass_media=resource_bypass_media(JObj)
                      ,formatters=resource_formatters(JObj)
                      ,global=wh_json:is_true(<<"Is-Global">>, JObj, 'true')
                      ,proxies=wh_json:to_proplist(<<"Proxies">>, JObj)
                     },
    Gateways = gateways_from_jobjs(wh_json:get_value(<<"gateways">>, JObj, [])
                                   ,Resource
                                  ),
    Resource#resrc{gateways=Gateways}.

-spec resource_bypass_media(wh_json:object()) -> boolean().
resource_bypass_media(JObj) ->
    Default = whapps_config:get_is_true(?SS_CONFIG_CAT, <<"default_bypass_media">>, 'false'),
    wh_json:is_true([<<"media">>, <<"bypass_media">>], JObj, Default).

-spec resource_formatters(wh_json:object()) -> api_objects().
resource_formatters(JObj) ->
    Default = whapps_config:get(?SS_CONFIG_CAT, <<"default_formatters">>),
    wh_json:get_value(<<"formatters">>, JObj, Default).

-spec resource_codecs(wh_json:object()) -> ne_binaries().
resource_codecs(JObj) ->
    DefaultAudio = whapps_config:get(?SS_CONFIG_CAT, <<"default_audio_codecs">>, []),
    DefaultVideo = whapps_config:get(?SS_CONFIG_CAT, <<"default_video_codecs">>, []),
    case wh_json:get_value([<<"media">>, <<"audio">>, <<"codecs">>], JObj, DefaultAudio)
        ++ wh_json:get_value([<<"media">>, <<"video">>, <<"codecs">>], JObj, DefaultVideo)
    of
        [] -> whapps_config:get(?SS_CONFIG_CAT, <<"default_codecs">>, []);
        Codecs -> Codecs
    end.

-spec resource_rules(wh_json:object()) -> rules().
resource_rules(JObj) ->
    lager:info("compiling resource rules for ~s / ~s"
               ,[wh_doc:account_db(JObj, <<"offnet">>), wh_doc:id(JObj)]),
    Rules = wh_json:get_value(<<"rules">>, JObj, []),
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

-spec resource_cid_rules(wh_json:object()) -> rules().
resource_cid_rules(JObj) ->
    lager:info("compiling resource rules for ~s / ~s"
               ,[wh_doc:account_db(JObj, <<"offnet">>), wh_doc:id(JObj)]),
    Rules = wh_json:get_value(<<"cid_rules">>, JObj, []),
    resource_rules(Rules, []).

-spec resource_grace_period(wh_json:object() | integer()) -> 0..100.
resource_grace_period(JObj) when not is_integer(JObj) ->
    Default = whapps_config:get_integer(?SS_CONFIG_CAT, <<"default_weight">>, 3),
    resource_grace_period(wh_json:get_integer_value(<<"grace_period">>, JObj, Default));
resource_grace_period(GracePeriod) when is_integer(GracePeriod), GracePeriod > 100 -> 100;
resource_grace_period(GracePeriod) when is_integer(GracePeriod), GracePeriod < 0 -> 0;
resource_grace_period(GracePeriod) when is_integer(GracePeriod) -> GracePeriod.

-spec resource_weight(wh_json:object() | integer()) -> integer().
resource_weight(JObj) when not is_integer(JObj) ->
    Default = whapps_config:get_integer(?SS_CONFIG_CAT, <<"default_weight">>, 1),
    resource_weight(wh_json:get_integer_value(<<"weight_cost">>, JObj, Default));
resource_weight(W) when W > 100 -> 100;
resource_weight(W) when W < 1 -> 1;
resource_weight(W) -> W.

-spec resource_is_emergency(wh_json:object()) -> boolean().
resource_is_emergency(JObj) ->
    (wh_json:get_value([<<"caller_id_options">>, <<"type">>], JObj) =:= <<"emergency">>)
        orelse wh_json:is_true(<<"emergency">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec gateways_from_jobjs(wh_json:objects(), resource()) -> gateways().
gateways_from_jobjs(JObjs, Resource) ->
    gateways_from_jobjs(JObjs, Resource, []).

-spec gateways_from_jobjs(wh_json:objects(), resource(), gateways()) -> gateways().
gateways_from_jobjs([], _, Gateways) -> Gateways;
gateways_from_jobjs([JObj|JObjs], Resource, Gateways) ->
    case wh_json:is_true(<<"enabled">>, JObj, 'true') of
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
-spec gateway_from_jobj(wh_json:object(), resource()) -> gateway().
gateway_from_jobj(JObj, #resrc{is_emergency=IsEmergency
                               ,format_from_uri=FormatFrom
                               ,from_uri_realm=FromRealm
                               ,fax_option=T38
                               ,codecs=Codecs
                               ,bypass_media=BypassMedia
                              }) ->
    EndpointType = wh_json:get_ne_value(<<"endpoint_type">>, JObj, <<"sip">>),
    #gateway{endpoint_type = EndpointType
             ,server = wh_json:get_ne_binary_value(<<"server">>, JObj)
             ,port = wh_json:get_integer_value(<<"port">>, JObj)
             ,realm = wh_json:get_value(<<"realm">>, JObj)
             ,username = wh_json:get_value(<<"username">>, JObj)
             ,password = wh_json:get_value(<<"password">>, JObj)
             ,sip_headers = wh_json:get_ne_value(<<"custom_sip_headers">>, JObj)
             ,sip_interface = wh_json:get_ne_value(<<"custom_sip_interface">>, JObj)
             ,invite_format = wh_json:get_value(<<"invite_format">>, JObj, <<"route">>)
             ,format_from_uri = wh_json:is_true(<<"format_from_uri">>, JObj, FormatFrom)
             ,from_uri_realm = wh_json:get_ne_value(<<"from_uri_realm">>, JObj, FromRealm)
             ,is_emergency = wh_json:is_true(<<"emergency">>, JObj, IsEmergency)
             ,fax_option = wh_json:is_true([<<"media">>, <<"fax_option">>], JObj, T38)
             ,codecs = wh_json:get_value(<<"codecs">>, JObj, Codecs)
             ,bypass_media = wh_json:is_true(<<"bypass_media">>, JObj, BypassMedia)
             ,force_port = wh_json:is_true(<<"force_port">>, JObj)
             ,route = wh_json:get_ne_value(<<"route">>, JObj, ?DEFAULT_ROUTE)
             ,prefix = wh_json:get_binary_value(<<"prefix">>, JObj, ?DEFAULT_PREFIX)
             ,suffix = wh_json:get_binary_value(<<"suffix">>, JObj, ?DEFAULT_SUFFIX)
             ,caller_id_type = wh_json:get_ne_value(<<"caller_id_type">>, JObj, ?DEFAULT_CALLER_ID_TYPE)
             ,progress_timeout = wh_json:get_integer_value(<<"progress_timeout">>, JObj, ?DEFAULT_PROGRESS_TIMEOUT)
             ,endpoint_options = endpoint_options(JObj, EndpointType)
            }.

-spec endpoint_options(wh_json:object(), api_binary()) -> wh_json:object().
endpoint_options(JObj, <<"freetdm">>) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Span">>, wh_json:get_value(<<"span">>, JObj)}
         ,{<<"Channel-Selection">>, wh_json:get_value(<<"channel_selection">>, JObj, <<"ascending">>)}
        ]));
endpoint_options(JObj, <<"skype">>) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Skype-Interface">>, wh_json:get_value(<<"interface">>, JObj)}
         ,{<<"Skype-RR">>, wh_json:is_true(<<"skype_rr">>, JObj, true)}
        ]));
endpoint_options(JObj, <<"amqp">>) ->
    Server = wh_json:get_value(<<"server">>, JObj),
    User = wh_json:get_value(<<"username">>, JObj),
    Password = wh_json:get_value(<<"password">>, JObj),
    Broker = <<"amqp://", User/binary, ":", Password/binary, "@", Server/binary>>,

    wh_json:from_list(
      props:filter_undefined(
        [{<<"AMQP-Broker">>, Broker}
         ,{<<"Exchange-ID">>, wh_json:get_value(<<"amqp_exchange">>, JObj)}
         ,{<<"Exchange-Type">>, wh_json:get_value(<<"amqp_exchange_type">>, JObj)}
         ,{<<"Route-ID">>, wh_json:get_value(<<"route_id">>, JObj)}
         ,{<<"System-ID">>, wh_json:get_value(<<"system_id">>, JObj)}
         ,{<<"Broker-Name">>, wh_json:get_value(<<"broker_name">>, JObj, wh_util:rand_hex_binary(6))}
         ,{<<"Exchange-Options">>, wh_json:get_value(<<"amqp_exchange_options">>, JObj, ?DEFAULT_AMQP_EXCHANGE_OPTIONS)}
        ]
       )
     );
endpoint_options(JObj, <<"sip">>) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Route-ID">>, wh_json:get_value(<<"route_id">>, JObj)}]
       )
     );
endpoint_options(_, _) -> wh_json:new().

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
        case not wh_util:is_empty(Port)
            andalso Port =/= 5060
        of
            'true' -> <<":", (wh_util:to_binary(Port))/binary>>;
            'false' -> <<>>
        end,
    Route =
        list_to_binary(["sip:", Prefix, Number, Suffix, "@", Server, DialStringPort]),
    lager:debug("created gateway route ~s", [Route]),
    Route;
gateway_dialstring(#gateway{route=Route}, _) ->
    lager:debug("using pre-configured gateway route ~s", [Route]),
    Route.
