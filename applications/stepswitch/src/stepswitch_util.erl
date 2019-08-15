%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_util).

-export([get_realm/1]).
-export([get_inbound_destination/1]).
-export([get_inbound_origination/1]).
-export([get_outbound_destination/1]).
-export([correct_shortdial/2]).
-export([get_sip_headers/1]).
-export([format_endpoints/4]).
-export([default_realm/1]).
-export([route_by/0]).
-export([resources_to_endpoints/3]).
-export([json_to_template_props/1]).

-include("stepswitch.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_realm(kz_term:api_binary() | kz_json:object()) -> kz_term:api_ne_binary().
get_realm('undefined') -> 'undefined';
get_realm(From) when is_binary(From) ->
    case binary:split(From, <<"@">>) of
        [_, Realm] -> Realm;
        _Else -> 'undefined'
    end;
get_realm(JObj) ->
    AuthRealm = kz_json:get_ne_binary_value(<<"Auth-Realm">>, JObj),
    case kz_term:is_empty(AuthRealm)
        orelse kz_network_utils:is_ipv4(AuthRealm)
        orelse kz_network_utils:is_ipv6(AuthRealm)
    of
        'false' -> AuthRealm;
        'true' ->
            get_realm(kz_json:get_ne_binary_value(<<"From">>, JObj))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_inbound_destination(kz_json:object()) -> kz_term:ne_binary().
get_inbound_destination(JObj) ->
    {Number, _} = kapps_util:get_destination(JObj, ?APP_NAME, <<"inbound_user_field">>),
    convert_to_e164_format(Number, ?SS_CONFIG_CAT, <<"assume_inbound_e164">>).

%%------------------------------------------------------------------------------
%% @doc Convert number to e164 format depending on config values set in system_config db
%% @end
%%------------------------------------------------------------------------------
-spec convert_to_e164_format(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
convert_to_e164_format(Number, ConfigDoc, ConfigKey) ->
    case kapps_config:get_is_true(ConfigDoc, ConfigKey, 'false') of
        'true' -> assume_e164(Number);
        'false' -> knm_converters:normalize(Number)
    end.

%%------------------------------------------------------------------------------
%% @doc Get the origination DID of a inbound call if its a number, else return undefined
%% @end
%%------------------------------------------------------------------------------
-spec get_inbound_origination(kz_json:object()) -> kz_term:ne_binary().
get_inbound_origination(JObj) ->
    {Number, _} = kapps_util:get_origination(JObj, ?APP_NAME, <<"inbound_user_origination_field">>),
    case knm_converters:is_reconcilable(Number) of
        'true' ->
            convert_to_e164_format(Number, ?SS_CONFIG_CAT, <<"assume_inbound_origination_e164">>);
        'false' ->
            'undefined'
    end.

-spec assume_e164(kz_term:ne_binary()) -> kz_term:ne_binary().
assume_e164(<<$+, _/binary>> = Number) -> Number;
assume_e164(Number) -> <<$+, Number/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_outbound_destination(kapi_offnet_resource:req()) -> kz_term:ne_binary().
get_outbound_destination(OffnetReq) ->
    Number = kapi_offnet_resource:to_did(OffnetReq),
    case kapi_offnet_resource:bypass_e164(OffnetReq) of
        'false' -> knm_converters:normalize(Number);
        'true' -> Number
    end.

%%------------------------------------------------------------------------------
%% @doc if the given number is shorter then a known caller id then try
%% to pad the front of the dialed number with values from the
%% callerid.
%% @end
%%------------------------------------------------------------------------------
-spec correct_shortdial(kz_term:ne_binary(), kapi_offnet_resource:req()) -> kz_term:api_binary().
correct_shortdial(Number, OffnetReq) ->
    CIDNum = case stepswitch_bridge:bridge_outbound_cid_number(OffnetReq) of
                 'undefined' -> Number;
                 N -> N
             end,
    DeniedCallRestrictions = kapi_offnet_resource:denied_call_restrictions(OffnetReq),
    correct_shortdial(Number, CIDNum, DeniedCallRestrictions).

-spec correct_shortdial(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:api_ne_binary().
correct_shortdial(<<"+", Number/binary>>, CIDNum, DeniedCallRestrictions) ->
    correct_shortdial(Number, CIDNum, DeniedCallRestrictions);
correct_shortdial(Number, <<"+", CIDNum/binary>>, DeniedCallRestrictions) ->
    correct_shortdial(Number, CIDNum, DeniedCallRestrictions);
correct_shortdial(Number, CIDNum, DeniedCallRestrictions) when is_binary(CIDNum) ->
    case shortdial_correction_length(Number, CIDNum) of
        0 ->
            lager:debug("unable to correct shortdial ~s via CID ~s"
                       ,[Number, CIDNum]
                       ),
            'undefined';
        Length ->
            try_to_correct(Number, CIDNum, DeniedCallRestrictions, Length)
    end.

-spec try_to_correct(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), non_neg_integer()) -> kz_term:api_ne_binary().
try_to_correct(Number, CIDNum, DeniedCallRestrictions, Length) ->
    Correction = kz_binary:truncate_right(CIDNum, Length),
    CorrectedNumber = knm_converters:normalize(<<Correction/binary, Number/binary>>),
    lager:debug("corrected shortdial ~s via CID ~s to ~s"
               ,[Number, CIDNum, CorrectedNumber]
               ),
    case should_deny_reclassified_number(CorrectedNumber, DeniedCallRestrictions) of
        'true' ->
            lager:info("unable to correct shortdial ~s via CID ~s due to a call restriction"
                      ,[Number, CIDNum]
                      ),
            'undefined';
        'false' ->
            CorrectedNumber
    end.

-spec should_deny_reclassified_number(kz_term:ne_binary(), kz_json:object()) -> boolean().
should_deny_reclassified_number(CorrectedNumber, DeniedCallRestrictions) ->
    Classification = knm_converters:classify(CorrectedNumber),
    lager:debug("re-classified corrected number ~s as ~s, testing for call restrictions"
               ,[CorrectedNumber, Classification]
               ),
    kz_json:get_ne_binary_value([Classification, <<"action">>], DeniedCallRestrictions)
        =:= <<"deny">>.

-spec shortdial_correction_length(kz_term:ne_binary(), kz_term:ne_binary()) -> integer().
shortdial_correction_length(Number, CIDNum) ->
    case kapps_config:get_integer(?SS_CONFIG_CAT, <<"fixed_length_shortdial_correction">>) of
        'undefined' ->
            Length = byte_size(CIDNum) - byte_size(Number),
            maybe_constrain_shortdial_correction(Length);
        FixedLength -> FixedLength
    end.

-spec maybe_constrain_shortdial_correction(integer()) -> integer().
maybe_constrain_shortdial_correction(Length) ->
    MaxCorrection = kapps_config:get_integer(?SS_CONFIG_CAT, <<"max_shortdial_correction">>, 5),
    MinCorrection = kapps_config:get_integer(?SS_CONFIG_CAT, <<"min_shortdial_correction">>, 2),
    case Length =< MaxCorrection
        andalso Length >= MinCorrection
    of
        'true' -> Length;
        'false' -> 0
    end.

-spec get_sip_headers(kapi_offnet_resource:req()) -> kz_json:object().
get_sip_headers(OffnetReq) ->
    SIPHeaders = kapi_offnet_resource:custom_sip_headers(OffnetReq, kz_json:new()),
    case get_diversions(SIPHeaders) of
        'undefined' ->
            maybe_remove_diversions(SIPHeaders);
        Diversions ->
            lager:debug("setting diversions ~p", [Diversions]),
            kz_json:set_value(<<"Diversions">>
                             ,Diversions
                             ,SIPHeaders
                             )
    end.

-spec maybe_remove_diversions(kz_json:object()) -> kz_json:object().
maybe_remove_diversions(JObj) ->
    kz_json:delete_key(<<"Diversions">>, JObj).


-spec get_diversions(kz_json:object()) ->
                            'undefined' |
                            kz_term:ne_binaries().
get_diversions(JObj) ->
    Inception = kz_json:get_value(<<"Inception">>, JObj),
    Diversions = kz_json:get_value(<<"Diversions">>, JObj, []),
    get_diversions(Inception, Diversions).

-spec get_diversions(kz_term:api_binary(), kz_term:ne_binaries()) ->
                            'undefined' |
                            kz_term:ne_binaries().
get_diversions('undefined', _Diversion) -> 'undefined';
get_diversions(_Inception, []) -> 'undefined';
get_diversions(Inception, Diversions) ->
    Fs = [{fun kzsip_diversion:set_address/2, <<"sip:", Inception/binary>>}
         ,{fun kzsip_diversion:set_counter/2, find_diversion_count(Diversions) + 1}
         ],
    [kzsip_diversion:to_binary(
       lists:foldl(fun({F, V}, D) -> F(D, V) end
                  ,kzsip_diversion:new()
                  ,Fs
                  )
      )
    ].

-spec find_diversion_count(kz_term:ne_binaries()) -> non_neg_integer().
find_diversion_count(Diversions) ->
    lists:max([kzsip_diversion:counter(
                 kzsip_diversion:from_binary(Diversion)
                )
               || Diversion <- Diversions
              ]).

-spec format_endpoints(kz_json:objects(), kz_term:ne_binary(), kz_term:ne_binary(), kapi_offnet_resource:req()) -> kz_json:objects().
format_endpoints(Endpoints, Name, Number, OffnetReq) ->
    EndpointFilter = build_filter_fun(Name, Number),
    format_endpoints(Endpoints, Name, Number, OffnetReq, EndpointFilter).

-type filter_fun() :: fun(({kz_term:ne_binary(), kz_term:ne_binary()}) -> boolean()).
-spec build_filter_fun(kz_term:ne_binary(), kz_term:ne_binary()) -> filter_fun().
build_filter_fun(Name, Number) ->
    fun({?KEY_OUTBOUND_CALLER_ID_NUMBER, N}) when N =:= Number -> 'false';
       ({?KEY_OUTBOUND_CALLER_ID_NAME, N}) when N =:= Name -> 'false';
       (_Else) -> 'true'
    end.

-spec format_endpoints(kz_json:objects(), kz_term:api_binary(), kz_term:api_binary(), kapi_offnet_resource:req(), filter_fun()) ->
                              kz_json:objects().
format_endpoints(Endpoints, Name, Number, OffnetReq, FilterFun) ->
    SIPHeaders = get_sip_headers(OffnetReq),
    AccountId = kapi_offnet_resource:hunt_account_id(OffnetReq
                                                    ,kapi_offnet_resource:account_id(OffnetReq)
                                                    ),
    [format_endpoint(set_endpoint_caller_id(Endpoint, Name, Number)
                    ,Number, FilterFun, OffnetReq, SIPHeaders, AccountId
                    )
     || Endpoint <- Endpoints
    ].

-spec default_realm(kapi_offnet_resource:req()) -> kz_term:api_binary().
default_realm(OffnetReq) ->
    case kapi_offnet_resource:from_uri_realm(OffnetReq) of
        'undefined' -> kapi_offnet_resource:account_realm(OffnetReq);
        Realm -> Realm
    end.

-spec set_endpoint_caller_id(kz_json:object(), kz_term:api_binary(), kz_term:api_binary()) -> kz_json:object().
set_endpoint_caller_id(Endpoint, Name, Number) ->
    kz_json:insert_values(props:filter_undefined(
                            [{?KEY_OUTBOUND_CALLER_ID_NUMBER, Number}
                            ,{?KEY_OUTBOUND_CALLER_ID_NAME, Name}
                            ]
                           )
                         ,Endpoint
                         ).

-spec format_endpoint(kz_json:object(), kz_term:api_binary(), filter_fun(), kapi_offnet_resource:req(), kz_json:object(), kz_term:ne_binary()) ->
                             kz_json:object().
format_endpoint(Endpoint, Number, FilterFun, OffnetReq, SIPHeaders, AccountId) ->
    FormattedEndpoint = apply_formatters(Endpoint, SIPHeaders, AccountId),
    FilteredEndpoint = kz_json:filter(FilterFun, FormattedEndpoint),
    maybe_endpoint_format_from(FilteredEndpoint, Number, OffnetReq).

-spec apply_formatters(kz_json:object(), kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
apply_formatters(Endpoint, SIPHeaders, AccountId) ->
    kz_formatters:apply(maybe_add_sip_headers(Endpoint, SIPHeaders)
                       ,props:get_value(<<"Formatters">>
                                       ,endpoint_props(Endpoint, AccountId)
                                       ,kz_json:new()
                                       )
                       ,'outbound'
                       ).

-spec endpoint_props(kz_json:object(), kz_term:api_binary()) -> kz_term:proplist().
endpoint_props(Endpoint, AccountId) ->
    ResourceId = kz_json:get_value(?CCV(<<"Resource-ID">>), Endpoint),
    case kz_json:is_true(?CCV(<<"Global-Resource">>), Endpoint) of
        'true' ->
            empty_list_on_undefined(stepswitch_resources:get_props(ResourceId));
        'false' ->
            empty_list_on_undefined(stepswitch_resources:get_props(ResourceId, AccountId))
    end.

-spec empty_list_on_undefined(kz_term:proplist() | 'undefined') -> kz_term:proplist().
empty_list_on_undefined('undefined') -> [];
empty_list_on_undefined(L) -> L.

-spec maybe_add_sip_headers(kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_add_sip_headers(Endpoint, SIPHeaders) ->
    LocalSIPHeaders = kz_json:get_value(<<"Custom-SIP-Headers">>, Endpoint, kz_json:new()),

    case kz_json:merge_jobjs(SIPHeaders, LocalSIPHeaders) of
        ?EMPTY_JSON_OBJECT -> Endpoint;
        MergedHeaders -> kz_json:set_value(<<"Custom-SIP-Headers">>, MergedHeaders, Endpoint)
    end.

-spec maybe_endpoint_format_from(kz_json:object(), kz_term:ne_binary(), kapi_offnet_resource:req()) ->
                                        kz_json:object().
maybe_endpoint_format_from(Endpoint, Number, OffnetReq) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, kz_json:new()),
    case kz_json:is_true(<<"Format-From-URI">>, CCVs) of
        'true' -> endpoint_format_from(Endpoint, Number, OffnetReq, CCVs);
        'false' ->
            kz_json:set_value(<<"Custom-Channel-Vars">>
                             ,kz_json:delete_keys([<<"Format-From-URI">>
                                                  ,<<"From-URI-Realm">>
                                                  ,<<"From-Account-Realm">>
                                                  ]
                                                 ,CCVs
                                                 )
                             ,Endpoint
                             )
    end.

-spec endpoint_format_from(kz_json:object(), kz_term:ne_binary(), kapi_offnet_resource:req(), kz_json:object()) ->
                                  kz_json:object().
endpoint_format_from(Endpoint, Number, OffnetReq, CCVs) ->
    FromNumber = kz_json:get_ne_value(?KEY_OUTBOUND_CALLER_ID_NUMBER, Endpoint, Number),
    case get_endpoint_format_from(OffnetReq, CCVs) of
        <<_/binary>> = Realm ->
            FromURI = <<"sip:", FromNumber/binary, "@", Realm/binary>>,
            lager:debug("setting resource ~s from-uri to ~s"
                       ,[kz_json:get_value(<<"Resource-ID">>, CCVs)
                        ,FromURI
                        ]),
            UpdatedCCVs = kz_json:set_value(<<"From-URI">>, FromURI, CCVs),
            kz_json:set_value(<<"Custom-Channel-Vars">>
                             ,kz_json:delete_keys([<<"Format-From-URI">>
                                                  ,<<"From-URI-Realm">>
                                                  ,<<"From-Account-Realm">>
                                                  ]
                                                 ,UpdatedCCVs
                                                 )
                             ,Endpoint
                             );
        _ ->
            kz_json:set_value(<<"Custom-Channel-Vars">>
                             ,kz_json:delete_keys([<<"Format-From-URI">>
                                                  ,<<"From-URI-Realm">>
                                                  ,<<"From-Account-Realm">>
                                                  ]
                                                 ,CCVs
                                                 )
                             ,Endpoint
                             )
    end.

-spec get_endpoint_format_from(kapi_offnet_resource:req(), kz_json:object()) -> kz_term:api_binary().
get_endpoint_format_from(OffnetReq, CCVs) ->
    case kz_json:is_true(<<"From-Account-Realm">>, CCVs) of
        'true' -> kapi_offnet_resource:account_realm(OffnetReq);
        'false' -> kz_json:get_value(<<"From-URI-Realm">>, CCVs, default_realm(OffnetReq))
    end.

-spec route_by() -> atom().
route_by() ->
    RouteBy = kapps_config:get_ne_binary(?SS_CONFIG_CAT, <<"route_by">>, ?DEFAULT_ROUTE_BY),
    case kz_module:ensure_loaded(RouteBy) of
        'false' -> kz_term:to_atom(?DEFAULT_ROUTE_BY);
        Module -> Module
    end.

-spec resources_to_endpoints(stepswitch_resources:resources(), kz_term:ne_binary(), kapi_offnet_resource:req()) ->
                                    kz_json:objects().
resources_to_endpoints(Resources, Number, OffnetJObj) ->
    resources_to_endpoints(Resources, Number, OffnetJObj, []).

-spec resources_to_endpoints(stepswitch_resources:resources(), kz_term:ne_binary(), kapi_offnet_resource:req(), kz_json:objects()) ->
                                    kz_json:objects().
resources_to_endpoints([], _Number, _OffnetJObj, Endpoints) ->
    lists:reverse(Endpoints);
resources_to_endpoints([Resource|Resources], Number, OffnetJObj, Endpoints) ->
    MoreEndpoints = maybe_resource_to_endpoints(Resource, Number, OffnetJObj, Endpoints),
    resources_to_endpoints(Resources, Number, OffnetJObj, MoreEndpoints).

-spec maybe_resource_to_endpoints(stepswitch_resources:resource(), kz_term:ne_binary(), kapi_offnet_resource:req(), kz_json:objects()) ->
                                         kz_json:objects().
maybe_resource_to_endpoints(Resource
                           ,Number
                           ,OffnetJObj
                           ,Endpoints
                           ) ->
    Id = stepswitch_resources:get_resrc_id(Resource),
    Name = stepswitch_resources:get_resrc_name(Resource),
    Gateways = stepswitch_resources:get_resrc_gateways(Resource),
    Global = stepswitch_resources:get_resrc_global(Resource),
    Weight = stepswitch_resources:get_resrc_weight(Resource),
    Proxies = stepswitch_resources:get_resrc_proxies(Resource),
    %% TODO: update CID Number from regex_cid_rules result
    %% DestinationNumber = maybe_update_number(Resource, Number),
    DestinationNumber = Number,
    lager:debug("building resource ~s endpoints", [Id]),
    CCVUpdates = [{<<"Global-Resource">>, kz_term:to_binary(Global)}
                 ,{<<"Resource-ID">>, Id}
                 ,{<<"E164-Destination">>, DestinationNumber}
                 ,{<<"Original-Number">>, kapi_offnet_resource:to_did(OffnetJObj)}
                 ],
    Updates = [{<<"Name">>, Name}
              ,{<<"Weight">>, Weight}
              ],
    EndpointList = [kz_json:set_values(Updates ,update_ccvs(Endpoint, CCVUpdates))
                    || Endpoint <- stepswitch_resources:gateways_to_endpoints(DestinationNumber, Gateways, OffnetJObj, [])
                   ],
    stepswitch_resources:maybe_add_proxies(EndpointList, Proxies, Endpoints).

-spec update_ccvs(kz_json:object(), kz_term:proplist()) -> kz_json:object().
update_ccvs(Endpoint, Updates) ->
    CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, kz_json:new()),
    kz_json:set_value(<<"Custom-Channel-Vars">>, kz_json:set_values(Updates, CCVs), Endpoint).

%%-spec maybe_update_number(stepswitch_resources:resource(), kz_term:ne_binary()) -> stepswitch_resources:resource().
%%maybe_update_number(Resource, Number) ->
%%    SelectorsResult = stepswitch_resources:get_resrc_selector_marks(Resource),
%%    props:get_value('regex_number_match', SelectorsResult, Number).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec json_to_template_props(kz_term:api_object()) -> 'undefined' | kz_term:proplist().
json_to_template_props('undefined') -> 'undefined';
json_to_template_props(JObj) ->
    normalize_proplist(kz_json:recursive_to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_proplist(kz_term:proplist()) -> kz_term:proplist().
normalize_proplist(Props) ->
    [normalize_proplist_element(Elem) || Elem <- Props].

-spec normalize_proplist_element({kz_term:proplist_key(), kz_term:proplist_value()}) ->
                                        {kz_term:proplist_key(), kz_term:proplist_value()}.
normalize_proplist_element({K, V}) when is_list(V) ->
    {normalize_value(K), normalize_proplist(V)};
normalize_proplist_element({K, V}) when is_binary(V) ->
    {normalize_value(K), kz_html:escape(V)};
normalize_proplist_element({K, V}) ->
    {normalize_value(K), V};
normalize_proplist_element(Else) ->
    Else.

-spec normalize_value(binary()) -> binary().
normalize_value(Value) ->
    binary:replace(kz_term:to_lower_binary(Value), <<"-">>, <<"_">>, ['global']).
