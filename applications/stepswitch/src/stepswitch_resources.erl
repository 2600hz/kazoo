%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_resources).

-export([reverse_lookup/1]).
-export([endpoints/2]).

-include("stepswitch.hrl").

-record(gateway, {
           resource_id :: api_binary()
          ,server :: api_binary()
          ,realm :: api_binary()
          ,username :: api_binary()
          ,password :: api_binary()
          ,route :: api_binary()
          ,prefix = <<>> :: binary()
          ,suffix = <<>> :: binary()
          ,codecs = [] :: ne_binaries()
          ,bypass_media = 'false' :: boolean()
          ,caller_id_type = <<"external">> :: ne_binary()
          ,t38_setting = 'false' :: boolean()
          ,sip_headers :: 'undefined' | wh_json:object()
          ,sip_interface :: api_binary()
          ,progress_timeout = 8 :: 1..100
          ,invite_format = <<"route">> :: ne_binary()
          ,endpoint_type = <<"sip">> :: ne_binary()
          ,endpoint_options = wh_json:new() :: wh_json:object()
          ,format_from_uri = false :: boolean()
         }).

-record(resrc, {
           id = <<>> :: binary()
          ,rev = <<>> :: binary()
          ,weight = 1 :: 1..100
          ,grace_period = 3 :: non_neg_integer()
          ,flags = [] :: list()
          ,rules = [] :: list()
          ,gateways = [] :: list()
          ,is_emergency = 'true' :: boolean()
          ,global = 'true' :: boolean()
         }).

-type resource() :: #resrc{}.
-type resources() :: [#resrc{},...] | [].

-type gateway() :: #gateway{}.
-type gateways() :: [#gateway{},...] | [].

-compile({'no_auto_import', [get/0
                             ,get/1
                            ]}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
reverse_lookup(JObj) -> 
    Realm = stepswitch_util:get_realm(JObj),
    IP = wh_json:get_first_defined([<<"From-Network-Addr">>
                                    ,<<"Orig-IP">>
                                   ], JObj),
    case maybe_find_global(IP, Realm) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
            AccountId = find_account_id(Realm, JObj),
            maybe_find_local(IP, Realm, AccountId)
    end.

find_account_id(Realm, JObj) ->
    case wh_json:get_first_defined([<<"Account-ID">>
                                    ,?CCV(<<"Account-ID">>)
                                   ], JObj)
    of
        'undefined' -> find_account_id(Realm);
        AccountId -> AccountId
    end.

find_account_id('undefined') -> 'undefined';
find_account_id(Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
        {'error', 'not_found'} -> 'undefined';
        {'ok', AccountId} -> AccountId
    end.

maybe_find_global(IP, Realm) ->
    search_resources(IP, Realm, get()).

maybe_find_local(_, _, 'undefined') -> {'error', 'not_found'};
maybe_find_local(IP, Realm, AccountId) ->
    search_resources(IP, Realm, get(AccountId)).

search_resources(_, _, []) -> {'error', 'not_found'};
search_resources(IP, Realm, [#resrc{id=Id
                                    ,gateways=Gateways
                                    ,global=Global}
                             | Resources
                            ]) ->
    case search_gateways(IP, Realm, Gateways) of
        {'error', 'not_found'} -> search_resources(IP, Realm, Resources);
        #gateway{}=Gateway ->
            {'ok', [{'resource_id', Id}
                    ,{'global', Global}
                    ,{'realm', Gateway#gateway.realm}
                    ,{'username', Gateway#gateway.username}
                    ,{'password', Gateway#gateway.password}
                   ]}
    end.

search_gateways(_, _, []) -> {'error', 'not_found'};
search_gateways(IP, Realm, [Gateway | Gateways]) ->
    case search_gateway(IP, Realm, Gateway) of
        {'error', 'not_found'} -> search_gateways(IP, Realm, Gateways);
        #gateway{}=Gateway -> Gateway
    end.

search_gateway(IP, _, #gateway{realm=IP}=Gateway) -> Gateway;
search_gateway(IP, _, #gateway{server=IP}=Gateway) -> Gateway;
search_gateway(_, Realm, #gateway{realm=Realm}=Gateway) -> Gateway;
search_gateway(_, Realm, #gateway{server=Realm}=Gateway) -> Gateway;
search_gateway(_, _, _) -> {'error', 'not_found'}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
endpoints(Number, JObj) ->
    case maybe_get_endpoints(Number, JObj) of
        [] -> maybe_correct_shortdial(Number, JObj);
        Endpoints -> Endpoints
    end.

maybe_correct_shortdial(Number, JObj) ->    
    case stepswitch_util:correct_shortdial(Number, JObj) of
        'undefined' -> [];
        CorrectedNumber ->
            maybe_get_endpoints(CorrectedNumber, JObj)
    end.

maybe_get_endpoints(Number, JObj) ->
    case wh_json:get_value(<<"Hunt-Account-ID">>, JObj) of
        'undefined' -> get_global_endpoints(Number, JObj);
        HuntAccount ->
            maybe_get_local_endpoints(HuntAccount, Number, JObj)
    end.

maybe_get_local_endpoints(HuntAccount, Number, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    case wh_util:is_in_account_hierarchy(AccountId, HuntAccount) of
        'false' -> get_global_endpoints(Number, JObj);
        'true' -> get_local_endpoints(AccountId, Number, JObj)
    end.    

get_local_endpoints(AccountId, Number, JObj) ->
    case get(AccountId) of
        [] -> get_global_endpoints(Number, JObj);
        Resources ->
            Flags = wh_json:get_value(<<"Flags">>, JObj, []),
            resources_to_endpoints(filter_resources(Flags, Resources),  Number, JObj)
    end.

get_global_endpoints(Number, JObj) ->
    Flags = wh_json:get_value(<<"Flags">>, JObj, []),
    Resources = filter_resources(Flags, get()),
    resources_to_endpoints(Resources, Number, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
filter_resources([], Resources) -> Resources;
filter_resources(Flags, Resources) ->
    filter_resources(Flags, Resources, []).

filter_resources(_, [], Filtered) -> Filtered;
filter_resources(Flags, [Resource|Resources], Filtered) -> 
    case resource_has_flags(Flags, Resource) of
        'false' -> filter_resources(Flags, Resources, Filtered);
        'true' ->
            %% TODO: this is reversing the order of the resources!
            filter_resources(Flags, Resources, [Resource | Filtered])
    end.

resource_has_flags(Flags, #resrc{flags=ResourceFlags}) ->
    lists:all(fun(Flag) -> 
                      wh_util:is_empty(Flag)
                          orelse lists:member(Flag, ResourceFlags)
              end, Flags).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
resources_to_endpoints(Resources, Number, JObj) ->
    resources_to_endpoints(Resources, Number, JObj, []).

resources_to_endpoints([], _, _, Endpoints) -> lists:reverse(Endpoints);
resources_to_endpoints([Resource|Resources], Number, JObj, Endpoints) ->
    case maybe_resource_to_endpoints(Resource, Number, JObj, Endpoints) of
        'undefined' -> 
            resources_to_endpoints(Resources, Number, JObj, Endpoints);
        MoreEndpoints ->
            resources_to_endpoints(Resources, Number, JObj, MoreEndpoints)
    end.

maybe_resource_to_endpoints(#resrc{rules=Rules
                                   ,gateways=Gateways}
                            ,Number, JObj, Endpoints) ->
    case evaluate_rules(Rules, Number) of
        {'error', 'no_match'} -> Endpoints;
        {'ok', Match} ->
            gateways_to_endpoints(Match, Gateways, JObj, Endpoints)
    end.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
gateways_to_endpoints(_, [], _, Endpoints) -> Endpoints;
gateways_to_endpoints(Number, [Gateway|Gateways], JObj, Endpoints) ->
    gateways_to_endpoints(Number, Gateways, JObj
                          ,[gateway_to_endpoint(Number, Gateway, JObj) | Endpoints]).
    
gateway_to_endpoint(Number, Gateway, JObj) ->
    FromUri = case Gateway#gateway.format_from_uri of
                  'false' -> 'undefined';
                  'true' -> endpoint_from_uri(JObj, Gateway)
              end,
    lager:debug("setting from-uri to ~p on gateway ~p", [FromUri, Gateway#gateway.resource_id]),
    
    FaxSettings = get_outbound_t38_settings(Gateway#gateway.t38_setting
                                            ,wh_json:get_value(<<"Fax-T38-Enabled">>, JObj)),

    CCVs = props:filter_undefined(
             [{<<"Resource-ID">>, Gateway#gateway.resource_id}
              ,{<<"From-URI">>, FromUri}
             ]),
    
    props:filter_empty(
      [{<<"Route">>, gateway_dialstring(Gateway, Number)}            
       ,{<<"Callee-ID-Name">>, wh_util:to_binary(Number)}
       ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
       ,{<<"To-DID">>, wh_util:to_binary(Number)}
       ,{<<"Invite-Format">>, Gateway#gateway.invite_format}
       ,{<<"Caller-ID-Type">>, Gateway#gateway.caller_id_type}
       ,{<<"Bypass-Media">>, Gateway#gateway.bypass_media}
       ,{<<"Codecs">>, Gateway#gateway.codecs}
       ,{<<"Auth-User">>, Gateway#gateway.username}
       ,{<<"Auth-Password">>, Gateway#gateway.password}
       ,{<<"SIP-Headers">>, Gateway#gateway.sip_headers}
       ,{<<"SIP-Interface">>, Gateway#gateway.sip_interface}
       ,{<<"Endpoint-Type">>, Gateway#gateway.endpoint_type}
       ,{<<"Endpoint-Options">>, Gateway#gateway.endpoint_options}
       ,{<<"Endpoint-Progress-Timeout">>, wh_util:to_binary(Gateway#gateway.progress_timeout)}
       ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
       | FaxSettings
      ]).

endpoint_from_uri(JObj, #gateway{realm='undefined'}=Gateway) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    case wh_util:get_account_realm(AccountId) of
        'undefined' -> 'undefined';
        Realm -> endpoint_from_uri(JObj, Gateway#gateway{realm=Realm})
    end;
endpoint_from_uri(JObj, #gateway{realm=Realm}) ->
    case wh_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                                    ,<<"Emergency-Caller-ID-Number">>
                                   ], JObj)
    of
        'undefined' -> 'undefined';
        CallerId ->
            <<"sip:", CallerId/binary, "@", Realm/binary>>
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get() -> get('undefined').
     
get('undefined') ->
    case wh_cache:fetch_local(?STEPSWITCH_CACHE, 'global_resources') of
        {'ok', Resources} -> Resources;
        {'error', 'not_found'} -> fetch_global_resources()
    end;
get(AccountId) ->
    case wh_cache:fetch_local(?STEPSWITCH_CACHE, {'local_resources', AccountId}) of
        {'ok', Resources} -> Resources;
        {'error', 'not_found'} ->fetch_local_resources(AccountId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_global_resources() -> resources().
fetch_global_resources() ->
    ViewOptions = ['include_docs'],
    case couch_mgr:get_results(?RESOURCES_DB, ?LIST_RESOURCES_BY_ID, ViewOptions) of
        {'error', _} -> [];
        {'ok', JObjs} ->
            Resources = resources_from_jobjs([wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]),
            wh_cache:store_local(?STEPSWITCH_CACHE, 'global_resources', Resources),
            Resources
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
fetch_local_resources(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = ['include_docs'],
    case couch_mgr:get_results(AccountDb, ?LIST_RESOURCES_BY_ID, ViewOptions) of
        {'error', _} -> [];
        {'ok', JObjs} ->
            Resources = resources_from_jobjs([wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]),
            LocalResources = [Resource#resrc{global='false'} || Resource <- Resources],
            wh_cache:store_local(?STEPSWITCH_CACHE, {'local_resources', AccountId}, LocalResources),
            LocalResources
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
resources_from_jobjs(JObjs) ->
    sort_resources(resources_from_jobjs(JObjs, [])).

resources_from_jobjs([], Resources) -> Resources;
resources_from_jobjs([JObj|JObjs], Resources) -> 
    case wh_json:is_true(<<"enabled">>, JObj, 'true') of
        'false' -> resources_from_jobjs(JObjs, Resources);
        'true' ->
            resources_from_jobjs(JObjs, [resource_from_jobj(JObj) | Resources])
    end.

sort_resources(Resources) ->
    lists:sort(fun(#resrc{weight=W1}, #resrc{weight=W2}) ->
                       W1 =< W2
               end, Resources).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
resource_from_jobj(JObj) ->
    Id = wh_json:get_value(<<"_id">>, JObj),
    lager:debug("loading resource ~s", [Id]),
    Gateways = gateways_from_jobjs(wh_json:get_value(<<"gateways">>, JObj, []), Id),
    #resrc{id = Id
           ,gateways = Gateways
           ,rev = wh_json:get_value(<<"_rev">>, JObj)
           ,flags = wh_json:get_value(<<"flags">>, JObj, [])
           ,rules = resource_rules(JObj)
           ,weight = resource_weight(JObj)
           ,grace_period = resource_grace_period(JObj)
           ,is_emergency = resource_is_emergency(JObj)
          }.

resource_rules(JObj) ->
    Rules = wh_json:get_value(<<"rules">>, JObj, []),
    resource_rules(Rules, []).
    
resource_rules([], CompiledRules) -> CompiledRules;
resource_rules([Rule|Rules], CompiledRules) ->
    case re:compile(Rule) of
        {'ok', CompiledRule} ->
            resource_rules(Rules, [CompiledRule | CompiledRules]);
        {'error', _R} ->
            lager:warning("bad rule '~s', ~p", [Rule, _R]),
            resource_rules(Rules, CompiledRules)
    end.

resource_grace_period(JObj) when not is_integer(JObj) ->
    Default = whapps_config:get_integer(<<"stepswitch">>, <<"default_weight">>, 3),
    wh_json:get_integer_value(<<"grace_period">>, JObj, Default);
resource_grace_period(GracePeriod) when GracePeriod > 100 -> 100;
resource_grace_period(GracePeriod) when GracePeriod < 0 -> 0;
resource_grace_period(GracePeriod) -> GracePeriod.

-spec resource_weight(wh_json:object() | integer()) -> integer().
resource_weight(JObj) when not is_integer(JObj) ->
    Default = whapps_config:get_integer(<<"stepswitch">>, <<"default_weight">>, 1),
    resource_weight(wh_json:get_integer_value(<<"weight_cost">>, JObj, Default));
resource_weight(W) when W > 100 -> 100;
resource_weight(W) when W < 1 -> 1;
resource_weight(W) -> W.

resource_is_emergency(#resrc{is_emergency=Emergency}) -> Emergency;
resource_is_emergency(JObj) ->
    (wh_json:get_value([<<"caller_id_options">>, <<"type">>], JObj) =:= <<"emergency">>)
        orelse wh_json:is_true(<<"emergency">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
gateways_from_jobjs(JObjs, Id) ->
    gateways_from_jobjs(JObjs, Id, []).

gateways_from_jobjs([], _, Gateways) -> Gateways;
gateways_from_jobjs([JObj|JObjs], Id, Gateways) ->
    case wh_json:is_true(<<"enabled">>, JObj, 'true') of
        'false' -> gateways_from_jobjs(JObjs, Id, Gateways);
        'true' ->
            G = [gateway_from_jobj(JObj, Id) | Gateways],
            gateways_from_jobjs(JObjs, Id, G)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
gateway_from_jobj(JObj, Id) ->
    EndpointType = endpoint_type(JObj),
    #gateway{'resource_id' = Id
             ,'server' = wh_json:get_value(<<"server">>, JObj)
             ,'realm' = wh_json:get_value(<<"realm">>, JObj)
             ,'username' =  wh_json:get_value(<<"username">>, JObj)
             ,'password' = wh_json:get_value(<<"password">>, JObj)
             ,'sip_headers' = wh_json:get_ne_value(<<"custom_sip_headers">>, JObj)
             ,'sip_interface' = wh_json:get_ne_value(<<"custom_sip_interface">>, JObj)
             ,'invite_format' = wh_json:get_value(<<"invite_format">>, JObj, <<"route">>)
             ,'format_from_uri' = wh_json:is_true(<<"format_from_uri">>, JObj)
             ,'route' = gateway_route(JObj)
             ,'prefix' = gateway_prefix(JObj)
             ,'suffix' = gateway_suffix(JObj)
             ,'codecs' = gateway_codecs(JObj)
             ,'t38_setting' = gateway_t38(JObj)
             ,'bypass_media' = gateway_bypass_media(JObj)
             ,'caller_id_type' = gateway_caller_id_type(JObj)
             ,'progress_timeout' = gateway_progress_timeout(JObj)
             ,'endpoint_type' = endpoint_type(JObj)
             ,'endpoint_options' = endpoint_options(JObj, EndpointType)
            }.

gateway_route(JObj) ->
    Default = whapps_config:get_binary(<<"stepswitch">>, <<"default_route">>),
    wh_json:get_ne_value(<<"route">>, JObj, Default).

gateway_prefix(JObj) ->
    Default = whapps_config:get_binary(<<"stepswitch">>, <<"default_prefix">>, <<>>),
    wh_json:get_binary_value(<<"prefix">>, JObj, Default).

gateway_suffix(JObj) ->
    Default = whapps_config:get_binary(<<"stepswitch">>, <<"default_suffix">>, <<>>),
    wh_json:get_binary_value(<<"suffix">>, JObj, Default).

gateway_codecs(JObj) ->
    Default = whapps_config:get(<<"stepswitch">>, <<"default_codecs">>, []),
    wh_json:get_value(<<"codecs">>, JObj, Default).

gateway_t38(JObj) ->
    Default = whapps_config:get_is_true(<<"stepswitch">>, <<"default_t38_settings">>, 'false'),
    wh_json:is_true(<<"t38_setting">>, JObj, Default).
    
gateway_bypass_media(JObj) ->
    Default = whapps_config:get_is_true(<<"stepswitch">>, <<"default_bypass_media">>, 'false'),
    wh_json:is_true(<<"bypass_media">>, JObj, Default).

gateway_caller_id_type(JObj) ->
    Default = whapps_config:get_binary(<<"stepswitch">>, <<"default_caller_id_type">>, <<"external">>),
    wh_json:get_ne_value(<<"caller_id_type">>, JObj, Default).

gateway_progress_timeout(JObj) ->
    Default = whapps_config:get_integer(<<"stepswitch">>, <<"default_progress_timeout">>, 8),
    wh_json:get_integer_value(<<"progress_timeout">>, JObj, Default).
    
endpoint_type(JObj) ->
    wh_json:get_ne_value(<<"endpoint_type">>, JObj, <<"sip">>).

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
endpoint_options(_, _) -> wh_json:new().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the sip url of a resource gateway
%% @end
%%--------------------------------------------------------------------
-spec gateway_dialstring(#gateway{}, ne_binary()) -> ne_binary().
gateway_dialstring(#gateway{route='undefined'
                            ,prefix=Prefix
                            ,suffix=Suffix
                            ,server=Server
                           }, Number) ->
    list_to_binary(["sip:"
                    ,wh_util:to_binary(Prefix)
                    ,Number
                    ,wh_util:to_binary(Suffix)
                    ,"@"
                    ,wh_util:to_binary(Server)
                   ]);
gateway_dialstring(#gateway{route=Route}, _) -> Route.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the t38 settings for an endpoint based on carrier and device
%% @end
%%--------------------------------------------------------------------
-spec get_outbound_t38_settings(ne_binary(), api_binary()) -> wh_proplist().
get_outbound_t38_settings(CarrierFlag, 'undefined') ->
    get_outbound_t38_settings(CarrierFlag);
get_outbound_t38_settings(CarrierFlag, <<"auto">>) ->
    get_outbound_t38_settings(CarrierFlag, 'true');
get_outbound_t38_settings(CarrierFlag, CallerFlag) when is_binary(CallerFlag) ->
    get_outbound_t38_settings(CarrierFlag, wh_util:is_true(CallerFlag));
get_outbound_t38_settings('true', 'true') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
     ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
     ,{<<"Enable-T38-Passthrough">>, 'true'}
     ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_outbound_t38_settings('true', 'false') ->
    [{<<"Enable-T38-Fax">>, 'true'}
     ,{<<"Enable-T38-Fax-Request">>, 'true'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, <<"self">>}
    ];
get_outbound_t38_settings('false', 'false') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
     ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
     ,{<<"Enable-T38-Passthrough">>, 'true'}
     ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_outbound_t38_settings('false','true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
     ,{<<"Enable-T38-Fax-Request">>, 'true'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, <<"peer">>}
    ].

-spec get_outbound_t38_settings(ne_binary()) -> wh_proplist().
get_outbound_t38_settings('true') ->
    [{<<"Enable-T38-Fax">>, 'true'}
     ,{<<"Enable-T38-Fax-Request">>, 'true'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ];
get_outbound_t38_settings('false') ->
    [{<<"Enable-T38-Fax">>, 'undefined'}
     ,{<<"Enable-T38-Fax-Request">>, 'undefined'}
     ,{<<"Enable-T38-Passthrough">>, 'undefined'}
     ,{<<"Enable-T38-Gateway">>, 'undefined'}
    ].
