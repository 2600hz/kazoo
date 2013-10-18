%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_resources).

-export([get_props/0]).
-export([endpoints/2]).
-export([reverse_lookup/1]).

-include("stepswitch.hrl").

-record(gateway, {
           server :: api_binary()
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
          ,format_from_uri = 'false' :: boolean()
          ,from_uri_realm :: api_binary()
          ,is_emergency = 'false' :: boolean()
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
          ,gateways = [] :: list()
          ,is_emergency = 'false' :: boolean()
          ,require_flags = 'false' :: boolean()
          ,global = 'true' :: boolean()
          ,format_from_uri = 'false' :: boolean()
          ,from_uri_realm :: api_binary()
          ,t38_setting = 'false' :: boolean()
          ,codecs = [] :: ne_binaries()
          ,bypass_media = 'false' :: boolean()
         }).

-type resource() :: #resrc{}.
-type resources() :: [#resrc{},...] | [].

-type gateway() :: #gateway{}.
-type gateways() :: [#gateway{},...] | [].

-compile({'no_auto_import', [get/0
                             ,get/1
                            ]}).

get_props() ->
    [resource_to_props(Resource) 
     || Resource <- sort_resources(get())
    ].

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
       ,{<<"T38">>, Resource#resrc.t38_setting}
       ,{<<"Bypass-Media">>, Resource#resrc.bypass_media}
       ,{<<"Grace-Period">>, Resource#resrc.grace_period}
       ,{<<"Flags">>, Resource#resrc.flags}
       ,{<<"Codecs">>, Resource#resrc.codecs}
       ,{<<"Rules">>, Resource#resrc.raw_rules}
      ]).

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
-spec endpoints(ne_binary(), wh_json:object()) -> wh_json:objects().
endpoints(Number, JObj) ->
    case maybe_get_endpoints(Number, JObj) of
        [] -> sort_endpoints(maybe_correct_shortdial(Number, JObj));
        Endpoints -> sort_endpoints(Endpoints)
    end.

-spec maybe_correct_shortdial(ne_binary(), wh_json:object()) -> wh_json:objects().
maybe_correct_shortdial(Number, JObj) ->    
    case stepswitch_util:correct_shortdial(Number, JObj) of
        'undefined' -> [];
        CorrectedNumber ->
            maybe_get_endpoints(CorrectedNumber, JObj)
    end.

-spec maybe_get_endpoints(ne_binary(), wh_json:object()) -> wh_json:objects().
maybe_get_endpoints(Number, JObj) ->
    case wh_json:get_value(<<"Hunt-Account-ID">>, JObj) of
        'undefined' -> get_global_endpoints(Number, JObj);
        HuntAccount ->
            maybe_get_local_endpoints(HuntAccount, Number, JObj)
    end.

-spec maybe_get_local_endpoints(ne_binary(), ne_binary(), wh_json:object()) -> wh_json:objects().
maybe_get_local_endpoints(HuntAccount, Number, JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    case wh_util:is_in_account_hierarchy(HuntAccount, AccountId, 'true') of
        'false' -> 
            lager:info("account ~s attempted to use local resources of ~s, but it is not allowed"
                       ,[AccountId, HuntAccount]),
            [];
        'true' ->
            lager:info("account ~s is using the local resources of ~s", [AccountId, HuntAccount]),
            get_local_endpoints(HuntAccount, Number, JObj)
    end.    

-spec get_local_endpoints(ne_binary(), ne_binary(), wh_json:object()) -> wh_json:objects().
get_local_endpoints(AccountId, Number, JObj) ->
    lager:debug("attempting to find local resources"),
    Flags = wh_json:get_value(<<"Flags">>, JObj, []),
    Resources = filter_resources(Flags, get(AccountId)),
    resources_to_endpoints(Resources,  Number, JObj).

-spec get_global_endpoints(ne_binary(), wh_json:object()) -> wh_json:objects().
get_global_endpoints(Number, JObj) ->
    lager:debug("attempting to find global resources"),
    Flags = wh_json:get_value(<<"Flags">>, JObj, []),
    Resources = filter_resources(Flags, get()),
    resources_to_endpoints(Resources, Number, JObj).

-spec sort_endpoints(wh_json:objects()) -> wh_json:objects().
sort_endpoints(Endpoints) ->
    lists:sort(fun(P1, P2) ->
                       props:get_value(<<"Weight">>, P1, 1) 
                           =< props:get_value(<<"Weight">>, P2, 1)
               end, Endpoints).
  
%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec reverse_lookup(wh_json:object()) -> {'ok', wh_proplist()} | {'error', 'not_found'}.
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

-spec maybe_find_global(api_binary(), api_binary()) -> {'ok', wh_proplist()} | {'error', 'not_found'}.
maybe_find_global(IP, Realm) ->
    search_resources(IP, Realm, get()).

-spec maybe_find_local(api_binary(), api_binary(), api_binary()) ->
                              {'ok', wh_proplist()} | {'error', 'not_found'}.
maybe_find_local(_, _, 'undefined') -> {'error', 'not_found'};
maybe_find_local(IP, Realm, AccountId) ->
    search_resources(IP, Realm, get(AccountId)).

-spec search_resources(api_binary(), api_binary(), resources()) -> {'ok', wh_proplist()} | {'error', 'not_found'}.
search_resources(_, _, []) -> {'error', 'not_found'};
search_resources(IP, Realm, [#resrc{id=Id
                                    ,gateways=Gateways
                                    ,global=Global}
                             | Resources
                            ]) ->
    case search_gateways(IP, Realm, Gateways) of
        {'error', 'not_found'} -> search_resources(IP, Realm, Resources);
        #gateway{}=Gateway ->
            lager:debug("found matching resource ~s for ip(~s) or realm(~s) of call"
                        ,[Id, IP, Realm]),
            Props = props:filter_undefined(
                      [{'resource_id', Id}
                       ,{'global', Global}
                       ,{'realm', Gateway#gateway.realm}
                       ,{'username', Gateway#gateway.username}
                       ,{'password', Gateway#gateway.password}
                      ]),
            {'ok', Props}
    end.

-spec search_gateways(api_binary(), api_binary(), gateways()) -> gateway() | {'error', 'not_found'}.
search_gateways(_, _, []) -> {'error', 'not_found'};
search_gateways(IP, Realm, [Gateway | Gateways]) ->
    case search_gateway(IP, Realm, Gateway) of
        {'error', 'not_found'} -> search_gateways(IP, Realm, Gateways);
        #gateway{}=Gateway -> Gateway
    end.

-spec search_gateway(api_binary(), api_binary(), gateway()) -> gateway() | {'error', 'not_found'}.
search_gateway(IP, _, #gateway{realm=IP}=Gateway) when IP =/= 'undefined' -> Gateway;
search_gateway(IP, _, #gateway{server=IP}=Gateway) when IP =/= 'undefined' -> Gateway;
search_gateway(_, Realm, #gateway{realm=Realm}=Gateway) when Realm =/= 'undefined' -> Gateway;
search_gateway(_, Realm, #gateway{server=Realm}=Gateway) when Realm =/= 'undefined' -> Gateway;
search_gateway(_, _, _) -> {'error', 'not_found'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_resources(ne_binaries(), resources()) -> resources().
filter_resources([], Resources) ->
    lager:debug("no flags provided, filtering resources that require flags"),
    [Resource || Resource <- Resources
                     ,not Resource#resrc.require_flags
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
resource_has_flags(Flags, #resrc{flags=ResourceFlags, id=Id}) ->
    lists:all(fun(Flag) -> 
                      case wh_util:is_empty(Flag)
                          orelse lists:member(Flag, ResourceFlags)
                      of
                          'true' -> 'true';
                          'false' ->
                              lager:debug("resource ~s does not have the required flag: ~s"
                                          ,[Id, Flag]),
                              'false'
                      end
              end, Flags).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resources_to_endpoints(resources(), ne_binary(), wh_json:object()) -> wh_json:objects().
resources_to_endpoints(Resources, Number, JObj) ->
    resources_to_endpoints(Resources, Number, JObj, []).

-spec resources_to_endpoints(resources(), ne_binary(), wh_json:object(), wh_json:objects()) -> wh_json:objects().
resources_to_endpoints([], _, _, Endpoints) -> 
    lager:debug("found ~p endpoints", [length(Endpoints)]),
    lists:reverse(Endpoints);
resources_to_endpoints([Resource|Resources], Number, JObj, Endpoints) ->
    case maybe_resource_to_endpoints(Resource, Number, JObj, Endpoints) of
        'undefined' -> 
            resources_to_endpoints(Resources, Number, JObj, Endpoints);
        MoreEndpoints ->
            resources_to_endpoints(Resources, Number, JObj, MoreEndpoints)
    end.

-spec maybe_resource_to_endpoints(resource(), ne_binary(), wh_json:object(), wh_json:objects()) -> wh_json:objects().
maybe_resource_to_endpoints(#resrc{id=Id
                                   ,name=Name
                                   ,rules=Rules
                                   ,gateways=Gateways
                                   ,global=Global
                                   ,weight=Weight}
                            ,Number, JObj, Endpoints) ->
    case evaluate_rules(Rules, Number) of
        {'error', 'no_match'} ->
            lager:debug("resource ~s does not match request, skipping", [Id]),
            Endpoints;
        {'ok', Match} ->
            lager:debug("building resource ~s endpoints with regex match: ~s"
                        ,[Id, Match]),
            Updates = [{<<"Global-Resource">>, wh_util:to_binary(Global)}
                       ,{<<"Resource-ID">>, Id}
                      ],
            [[{<<"Name">>, Name}
              ,{<<"Weight">>, Weight}
              | update_ccvs(Endpoint, Updates)
             ]
             || Endpoint <- gateways_to_endpoints(Match, Gateways, JObj, [])
            ] ++ Endpoints
    end.

-spec update_ccvs(wh_proplist(), wh_proplist()) -> wh_proplist().
update_ccvs(Props, Updates) ->
    CCVs = props:get_value(<<"Custom-Channel-Vars">>, Props, wh_json:new()),
    props:set_value(<<"Custom-Channel-Vars">>, wh_json:set_values(Updates, CCVs), Props).

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec gateways_to_endpoints(ne_binary(), gateways(), wh_json:object(), wh_json:objects()) -> wh_json:objects().
gateways_to_endpoints(_, [], _, Endpoints) -> Endpoints;
gateways_to_endpoints(Number, [Gateway|Gateways], JObj, Endpoints) ->
    gateways_to_endpoints(Number, Gateways, JObj
                          ,[gateway_to_endpoint(Number, Gateway, JObj) | Endpoints]).
    
-spec gateway_to_endpoint(ne_binary(), gateway(), wh_json:object()) -> wh_json:object().
gateway_to_endpoint(Number, Gateway, JObj) ->
    CCVs = props:filter_undefined(
             [{<<"Emergency-Resource">>, gateway_emergency_resource(Gateway)}
              ,{<<"Format-From-URI">>, Gateway#gateway.format_from_uri}
              ,{<<"From-URI-Realm">>, Gateway#gateway.from_uri_realm}
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
       | get_outbound_t38_settings(Gateway#gateway.t38_setting
                                   ,wh_json:get_value(<<"Fax-T38-Enabled">>, JObj))
      ]).

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
            CacheProps = [{'origin', fetch_global_cache_origin(JObjs, [])}],
            Resources = resources_from_jobjs([wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]),
            wh_cache:store_local(?STEPSWITCH_CACHE, 'global_resources', Resources, CacheProps),
            Resources
    end.

-spec fetch_global_cache_origin(wh_json:objects(), wh_proplist()) -> wh_proplist().
fetch_global_cache_origin([], Props) -> Props;
fetch_global_cache_origin([JObj|JObjs], Props) -> 
    Id = wh_json:get_value(<<"id">>, JObj),
    fetch_global_cache_origin(JObjs, [{'db', ?RESOURCES_DB, Id}|Props]).
     
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
            CacheProps = [{'origin', fetch_global_cache_origin(JObjs, AccountDb, [])}],
            Resources = resources_from_jobjs([wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]),
            LocalResources = [Resource#resrc{global='false'} || Resource <- Resources],
            wh_cache:store_local(?STEPSWITCH_CACHE, {'local_resources', AccountId}, LocalResources, CacheProps),
            LocalResources
    end.

-spec fetch_global_cache_origin(wh_json:objects(), ne_binary(), wh_proplist()) -> wh_proplist().
fetch_global_cache_origin([], _, Props) -> Props;
fetch_global_cache_origin([JObj|JObjs], AccountDb, Props) -> 
    Id = wh_json:get_value(<<"id">>, JObj),
    fetch_global_cache_origin(JObjs, AccountDb, [{'db', AccountDb, Id}|Props]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resources_from_jobjs(wh_json:objects()) -> resources().
resources_from_jobjs(JObjs) -> resources_from_jobjs(JObjs, []).

-spec resources_from_jobjs(wh_json:objects(), resources()) -> resources().
resources_from_jobjs([], Resources) -> Resources;
resources_from_jobjs([JObj|JObjs], Resources) -> 
    case wh_json:is_true(<<"enabled">>, JObj, 'true') of
        'false' -> resources_from_jobjs(JObjs, Resources);
        'true' ->
            resources_from_jobjs(JObjs, [resource_from_jobj(JObj) | Resources])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-type rule() :: re:mp().
-type rules() :: [rule(),...] | [].

-spec resource_from_jobj(wh_json:object()) -> resource().
resource_from_jobj(JObj) ->
    Resource = #resrc{id=wh_json:get_value(<<"_id">>, JObj)
                      ,rev=wh_json:get_value(<<"_rev">>, JObj)
                      ,name=wh_json:get_value(<<"name">>, JObj)
                      ,flags=wh_json:get_value(<<"flags">>, JObj, [])
                      ,require_flags=wh_json:is_true(<<"require_flags">>, JObj)
                      ,format_from_uri=wh_json:is_true(<<"format_from_uri">>, JObj)
                      ,from_uri_realm=wh_json:get_ne_value(<<"from_uri_realm">>, JObj)
                      ,t38_setting=wh_json:is_true([<<"media">>, <<"fax_option">>], JObj)
                      ,raw_rules=wh_json:get_value(<<"rules">>, JObj, [])
                      ,rules=resource_rules(JObj)
                      ,weight=resource_weight(JObj)
                      ,grace_period=resource_grace_period(JObj)
                      ,is_emergency=resource_is_emergency(JObj)
                      ,codecs=resource_codecs(JObj)
                      ,bypass_media=resource_bypass_media(JObj)
                     },
    Gateways = gateways_from_jobjs(wh_json:get_value(<<"gateways">>, JObj, [])
                                   ,Resource),
    Resource#resrc{gateways=Gateways}.

-spec resource_bypass_media(wh_json:object()) -> boolean().
resource_bypass_media(JObj) ->
    Default = whapps_config:get_is_true(<<"stepswitch">>, <<"default_bypass_media">>, 'false'),
    wh_json:is_true([<<"media">>, <<"bypass_media">>], JObj, Default).

-spec resource_codecs(wh_json:object()) -> ne_binaries().
resource_codecs(JObj) ->
    DefaultAudio = whapps_config:get(<<"stepswitch">>, <<"default_audio_codecs">>, []),
    DefaultVideo = whapps_config:get(<<"stepswitch">>, <<"default_video_codecs">>, []),
    case wh_json:get_value([<<"media">>, <<"audio">>, <<"codecs">>], JObj, DefaultAudio)
        ++ wh_json:get_value([<<"media">>, <<"video">>, <<"codecs">>], JObj, DefaultVideo)
    of
        [] -> whapps_config:get(<<"stepswitch">>, <<"default_codecs">>, []);
        Codecs -> Codecs
    end.

-spec resource_rules(wh_json:object()) -> rules().
resource_rules(JObj) ->
    lager:info("compiling resource rules for ~s/~s"
               ,[wh_json:get_value(<<"pvt_account_db">>, JObj, <<"offnet">>)
                 ,wh_json:get_value(<<"_id">>, JObj)
                ]),
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

-spec resource_grace_period(wh_json:object() | integer()) -> 0..100.
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

-spec resource_is_emergency(resource() | wh_json:object()) -> boolean().
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
                               ,t38_setting=T38
                               ,codecs=Codecs
                               ,bypass_media=BypassMedia}) ->
    EndpointType = wh_json:get_ne_value(<<"endpoint_type">>, JObj, <<"sip">>),
    #gateway{endpoint_type=EndpointType
             ,server=wh_json:get_value(<<"server">>, JObj)
             ,realm=wh_json:get_value(<<"realm">>, JObj)
             ,username=wh_json:get_value(<<"username">>, JObj)
             ,password=wh_json:get_value(<<"password">>, JObj)
             ,sip_headers=wh_json:get_ne_value(<<"custom_sip_headers">>, JObj)
             ,sip_interface=wh_json:get_ne_value(<<"custom_sip_interface">>, JObj)
             ,invite_format=wh_json:get_value(<<"invite_format">>, JObj, <<"route">>)
             ,format_from_uri=wh_json:is_true(<<"format_from_uri">>, JObj, FormatFrom)
             ,from_uri_realm=wh_json:get_ne_value(<<"from_uri_realm">>, JObj, FromRealm)
             ,is_emergency=wh_json:is_true(<<"emergency">>, JObj, IsEmergency)
             ,t38_setting=wh_json:is_true(<<"fax_option">>, JObj, T38)
             ,codecs=wh_json:get_value(<<"codecs">>, JObj, Codecs)
             ,bypass_media=wh_json:is_true(<<"bypass_media">>, JObj, BypassMedia)
             ,route=gateway_route(JObj)
             ,prefix=gateway_prefix(JObj)
             ,suffix=gateway_suffix(JObj)
             ,caller_id_type=gateway_caller_id_type(JObj)
             ,progress_timeout=gateway_progress_timeout(JObj)
             ,endpoint_options=endpoint_options(JObj, EndpointType)
            }.

-spec gateway_route(wh_json:object()) -> api_binary().
gateway_route(JObj) ->
    Default = whapps_config:get_binary(<<"stepswitch">>, <<"default_route">>),
    wh_json:get_ne_value(<<"route">>, JObj, Default).

-spec gateway_prefix(wh_json:object()) -> binary().
gateway_prefix(JObj) ->
    Default = whapps_config:get_binary(<<"stepswitch">>, <<"default_prefix">>, <<>>),
    wh_json:get_binary_value(<<"prefix">>, JObj, Default).

-spec gateway_suffix(wh_json:object()) -> binary().
gateway_suffix(JObj) ->
    Default = whapps_config:get_binary(<<"stepswitch">>, <<"default_suffix">>, <<>>),
    wh_json:get_binary_value(<<"suffix">>, JObj, Default).
    
-spec gateway_caller_id_type(wh_json:object()) -> ne_binary().
gateway_caller_id_type(JObj) ->
    Default = whapps_config:get_binary(<<"stepswitch">>, <<"default_caller_id_type">>, <<"external">>),
    wh_json:get_ne_value(<<"caller_id_type">>, JObj, Default).

-spec gateway_progress_timeout(wh_json:object()) -> integer().
gateway_progress_timeout(JObj) ->
    Default = whapps_config:get_integer(<<"stepswitch">>, <<"default_progress_timeout">>, 8),
    wh_json:get_integer_value(<<"progress_timeout">>, JObj, Default).

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
                           }, Number) ->
    Route = list_to_binary(["sip:"
                            ,wh_util:to_binary(Prefix)
                            ,Number
                            ,wh_util:to_binary(Suffix)
                            ,"@"
                            ,wh_util:to_binary(Server)
                           ]),
    lager:debug("created gateway route ~s", [Route]),
    Route;
gateway_dialstring(#gateway{route=Route}, _) ->
    lager:debug("using pre-configured gateway route ~s", [Route]),
    Route.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the t38 settings for an endpoint based on carrier and device
%% @end
%%--------------------------------------------------------------------
-spec get_outbound_t38_settings(ne_binary(), api_binary()) -> wh_proplist().
get_outbound_t38_settings(<<"auto">>, CallerFlag) ->
    get_outbound_t38_settings('true', CallerFlag);
get_outbound_t38_settings(CarrierFlag, <<"auto">>) ->
    get_outbound_t38_settings(CarrierFlag, 'true');
get_outbound_t38_settings(CarrierFlag, 'undefined') ->
    get_outbound_t38_settings(wh_util:is_true(CarrierFlag));
get_outbound_t38_settings(CarrierFlag, CallerFlag) when not is_boolean(CarrierFlag) ->
    get_outbound_t38_settings(wh_util:is_true(CarrierFlag), CallerFlag);
get_outbound_t38_settings(CarrierFlag, CallerFlag) when not is_boolean(CallerFlag) ->
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
