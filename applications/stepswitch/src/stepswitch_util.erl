%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_util).

-export([get_realm/1]).
-export([get_inbound_destination/1]).
-export([get_outbound_destination/1]).
-export([lookup_number/1]).
-export([correct_shortdial/2]).
-export([get_sip_headers/1]).
-export([format_endpoints/4]).
-export([default_realm/1]).

-include("stepswitch.hrl").
-include_lib("whistle/src/wh_json.hrl").
-include_lib("whistle/include/wapi_offnet_resource.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_realm(api_binary() | wh_json:object()) -> api_binary().
get_realm('undefined') -> 'undefined';
get_realm(From) when is_binary(From) ->
    case binary:split(From, <<"@">>) of
        [_, Realm] -> Realm;
        _Else -> 'undefined'
    end;
get_realm(JObj) ->
    AuthRealm = wh_json:get_value(<<"Auth-Realm">>, JObj),
    case wh_util:is_empty(AuthRealm)
        orelse wh_network_utils:is_ipv4(AuthRealm)
        orelse wh_network_utils:is_ipv6(AuthRealm)
    of
        'false' -> AuthRealm;
        'true' ->
            get_realm(wh_json:get_value(<<"From">>, JObj))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_inbound_destination(wh_json:object()) -> ne_binary().
get_inbound_destination(JObj) ->
    {Number, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"inbound_user_field">>),
    case whapps_config:get_is_true(?SS_CONFIG_CAT, <<"assume_inbound_e164">>, 'false') of
        'true' -> assume_e164(Number);
        'false' -> wnm_util:to_e164(Number)
    end.

-spec assume_e164(ne_binary()) -> ne_binary().
assume_e164(<<$+, _/binary>> = Number) -> Number;
assume_e164(Number) -> <<$+, Number/binary>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_outbound_destination(wapi_offnet_resource:req()) -> ne_binary().
get_outbound_destination(OffnetReq) ->
    Number = wapi_offnet_resource:to_did(OffnetReq),
    case wapi_offnet_resource:bypass_e164(OffnetReq) of
        'false' -> wnm_util:to_e164(Number);
        'true' -> Number
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_number(ne_binary()) ->
                           {'ok', ne_binary(), knm_number:number_properties()} |
                           {'error', any()}.
lookup_number(Number) ->
    Num = wnm_util:normalize_number(Number),
    case kz_cache:fetch_local(?STEPSWITCH_CACHE, cache_key_number(Num)) of
        {'ok', {AccountId, Props}} ->
            lager:debug("found number properties in stepswitch cache"),
            {'ok', AccountId, Props};
        {'error', 'not_found'} -> fetch_number(Num)
    end.

-spec fetch_number(ne_binary()) ->
                          {'ok', ne_binary(), knm_number:number_properties()} |
                          {'error', any()}.
fetch_number(Num) ->
    case wh_number_manager:lookup_account_by_number(Num) of
        {'ok', AccountId, Props} ->
            CacheProps = [{'origin'
                          ,[{'db', wnm_util:number_to_db_name(Num), Num}
                           ,{'type', <<"number">>}
                           ]
                          }
                         ],
            kz_cache:store_local(?STEPSWITCH_CACHE, cache_key_number(Num), {AccountId, Props}, CacheProps),
            lager:debug("~s is associated with account ~s", [Num, AccountId]),
            {'ok', AccountId, Props};
        {'error', Reason}=E ->
            lager:debug("~s is not associated with any account, ~p", [Num, Reason]),
            E
    end.

-spec cache_key_number(ne_binary()) -> {'stepswitch_number', ne_binary()}.
cache_key_number(Number) ->
    {'stepswitch_number', Number}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% if the given number is shorter then a known caller id then try
%% to pad the front of the dialed number with values from the
%% callerid.
%% @end
%%--------------------------------------------------------------------
-spec correct_shortdial(ne_binary(), ne_binary() | wapi_offnet_resource:req()) -> api_binary().
correct_shortdial(<<"+", Number/binary>>, CIDNum) ->
    correct_shortdial(Number, CIDNum);
correct_shortdial(Number, <<"+", CIDNum/binary>>) ->
    correct_shortdial(Number, CIDNum);
correct_shortdial(Number, CIDNum) when is_binary(CIDNum) ->
    MaxCorrection = whapps_config:get_integer(?SS_CONFIG_CAT, <<"max_shortdial_correction">>, 5),
    MinCorrection = whapps_config:get_integer(?SS_CONFIG_CAT, <<"min_shortdial_correction">>, 2),
    case is_binary(CIDNum) andalso (size(CIDNum) - size(Number)) of
        Length when Length =< MaxCorrection, Length >= MinCorrection ->
            Correction = wh_util:truncate_right_binary(CIDNum, Length),
            CorrectedNumber = wnm_util:to_e164(<<Correction/binary, Number/binary>>),
            lager:debug("corrected shortdial ~s via CID ~s to ~s"
                       ,[Number, CIDNum, CorrectedNumber]),
            CorrectedNumber;
        _ ->
            lager:debug("unable to correct shortdial ~s via CID ~s"
                        ,[Number, CIDNum]),
            'undefined'
    end;
correct_shortdial(Number, OffnetReq) ->
    CIDNum = case stepswitch_bridge:bridge_outbound_cid_number(OffnetReq) of
                 'undefined' -> Number;
                 N -> N
             end,
    correct_shortdial(Number, CIDNum).

-spec get_sip_headers(wapi_offnet_resource:req()) -> wh_json:object().
get_sip_headers(OffnetReq) ->
    SIPHeaders = wapi_offnet_resource:custom_sip_headers(OffnetReq, wh_json:new()),
    case get_diversions(SIPHeaders) of
        'undefined' ->
            maybe_remove_diversions(SIPHeaders);
        Diversions ->
            lager:debug("setting diversions ~p", [Diversions]),
            wh_json:set_value(<<"Diversions">>
                              ,Diversions
                              ,SIPHeaders
                             )
    end.

-spec maybe_remove_diversions(wh_json:object()) -> wh_json:object().
maybe_remove_diversions(JObj) ->
    wh_json:delete_key(<<"Diversions">>, JObj).

-spec get_diversions(wh_json:object()) ->
                            'undefined' |
                            ne_binaries().
-spec get_diversions(api_binary(), ne_binaries()) ->
                            'undefined' |
                            ne_binaries().

get_diversions(JObj) ->
    Inception = wh_json:get_value(<<"Inception">>, JObj),
    Diversions = wh_json:get_value(<<"Diversions">>, JObj, []),
    get_diversions(Inception, Diversions).

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

-spec find_diversion_count(ne_binaries()) -> non_neg_integer().
find_diversion_count(Diversions) ->
    lists:max([kzsip_diversion:counter(
                 kzsip_diversion:from_binary(Diversion)
                )
               || Diversion <- Diversions
              ]).

format_endpoints(Endpoints, Name, Number, OffnetReq) ->
    EndpointFilter = build_filter_fun(Name, Number),
    format_endpoints(Endpoints, Name, Number, OffnetReq, EndpointFilter).

-type filter_fun() :: fun(({ne_binary(), ne_binary()}) -> boolean()).
-spec build_filter_fun(ne_binary(), ne_binary()) -> filter_fun().
build_filter_fun(Name, Number) ->
    fun({?KEY_OUTBOUND_CALLER_ID_NUMBER, N}) when N =:= Number -> 'false';
       ({?KEY_OUTBOUND_CALLER_ID_NAME, N}) when N =:= Name -> 'false';
       (_Else) -> 'true'
    end.

-spec format_endpoints(wh_json:objects(), api_binary(), api_binary(), wapi_offnet_resource:req(), filter_fun()) ->
                              wh_json:objects().
format_endpoints(Endpoints, Name, Number, OffnetReq, FilterFun) ->
    SIPHeaders = stepswitch_util:get_sip_headers(OffnetReq),
    AccountId = wapi_offnet_resource:hunt_account_id(
                  OffnetReq
                  ,wapi_offnet_resource:account_id(OffnetReq)
                 ),
    [format_endpoint(set_endpoint_caller_id(Endpoint, Name, Number)
                     ,Number, FilterFun, OffnetReq, SIPHeaders, AccountId
                    )
     || Endpoint <- Endpoints
    ].

-spec default_realm(wapi_offnet_resource:req()) -> api_binary().
default_realm(OffnetReq) ->
    case wapi_offnet_resource:from_uri_realm(OffnetReq) of
        'undefined' -> wapi_offnet_resource:account_realm(OffnetReq);
        Realm -> Realm
    end.

-spec set_endpoint_caller_id(wh_json:object(), api_binary(), api_binary()) -> wh_json:object().
set_endpoint_caller_id(Endpoint, Name, Number) ->
    wh_json:insert_values(props:filter_undefined(
                            [{?KEY_OUTBOUND_CALLER_ID_NUMBER, Number}
                            ,{?KEY_OUTBOUND_CALLER_ID_NAME, Name}
                            ]
                           )
                          ,Endpoint
                         ).

-spec format_endpoint(wh_json:object(), api_binary(), filter_fun(), wapi_offnet_resource:req(), wh_json:object(), ne_binary()) ->
                             wh_json:object().
format_endpoint(Endpoint, Number, FilterFun, OffnetReq, SIPHeaders, AccountId) ->
    FormattedEndpoint = apply_formatters(Endpoint, SIPHeaders, AccountId),
    FilteredEndpoint = wh_json:filter(FilterFun, FormattedEndpoint),
    maybe_endpoint_format_from(FilteredEndpoint, Number, OffnetReq).

-spec apply_formatters(wh_json:object(), wh_json:object(), ne_binary()) -> wh_json:object().
apply_formatters(Endpoint, SIPHeaders, AccountId) ->
    stepswitch_formatters:apply(maybe_add_sip_headers(Endpoint, SIPHeaders)
                                ,props:get_value(<<"Formatters">>
                                                 ,endpoint_props(Endpoint, AccountId)
                                                 ,wh_json:new()
                                                )
                                ,'outbound'
                               ).

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

-spec maybe_add_sip_headers(wh_json:object(), wh_json:object()) -> wh_json:object().
maybe_add_sip_headers(Endpoint, SIPHeaders) ->
    LocalSIPHeaders = wh_json:get_value(<<"Custom-SIP-Headers">>, Endpoint, wh_json:new()),

    case wh_json:merge_jobjs(SIPHeaders, LocalSIPHeaders) of
        ?EMPTY_JSON_OBJECT -> Endpoint;
        MergedHeaders -> wh_json:set_value(<<"Custom-SIP-Headers">>, MergedHeaders, Endpoint)
    end.

-spec maybe_endpoint_format_from(wh_json:object(), ne_binary(), wapi_offnet_resource:req()) ->
                                        wh_json:object().
maybe_endpoint_format_from(Endpoint, Number, OffnetReq) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, Endpoint, wh_json:new()),
    case wh_json:is_true(<<"Format-From-URI">>, CCVs) of
        'true' -> endpoint_format_from(Endpoint, Number, OffnetReq, CCVs);
        'false' ->
            wh_json:set_value(<<"Custom-Channel-Vars">>
                              ,wh_json:delete_keys([<<"Format-From-URI">>
                                                    ,<<"From-URI-Realm">>
                                                    ,<<"From-Account-Realm">>
                                                   ]
                                                   ,CCVs
                                                  )
                              ,Endpoint
                             )
    end.

-spec endpoint_format_from(wh_json:object(), ne_binary(), wapi_offnet_resource:req(), wh_json:object()) ->
                                  wh_json:object().
endpoint_format_from(Endpoint, Number, OffnetReq, CCVs) ->
    FromNumber = wh_json:get_ne_value(?KEY_OUTBOUND_CALLER_ID_NUMBER, Endpoint, Number),
    case get_endpoint_format_from(OffnetReq, CCVs) of
        <<_/binary>> = Realm ->
            FromURI = <<"sip:", FromNumber/binary, "@", Realm/binary>>,
            lager:debug("setting resource ~s from-uri to ~s"
                        ,[wh_json:get_value(<<"Resource-ID">>, CCVs)
                          ,FromURI
                         ]),
            UpdatedCCVs = wh_json:set_value(<<"From-URI">>, FromURI, CCVs),
            wh_json:set_value(<<"Custom-Channel-Vars">>
                              ,wh_json:delete_keys([<<"Format-From-URI">>
                                                    ,<<"From-URI-Realm">>
                                                    ,<<"From-Account-Realm">>
                                                   ]
                                                   ,UpdatedCCVs
                                                  )
                              ,Endpoint
                             );
        _ ->
            wh_json:set_value(<<"Custom-Channel-Vars">>
                              ,wh_json:delete_keys([<<"Format-From-URI">>
                                                    ,<<"From-URI-Realm">>
                                                    ,<<"From-Account-Realm">>
                                                   ]
                                                   ,CCVs
                                                  )
                              ,Endpoint
                             )
    end.

-spec get_endpoint_format_from(wapi_offnet_resource:req(), wh_json:object()) -> api_binary().
get_endpoint_format_from(OffnetReq, CCVs) ->
    DefaultRealm = default_realm(OffnetReq),
    case wh_json:is_true(<<"From-Account-Realm">>, CCVs) of
        'true' -> DefaultRealm;
        'false' -> wh_json:get_value(<<"From-URI-Realm">>, CCVs, DefaultRealm)
    end.
