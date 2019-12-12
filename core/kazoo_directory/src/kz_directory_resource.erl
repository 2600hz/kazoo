%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_directory_resource).

-export([profile/2, profile/3]).

-include("kazoo_directory.hrl").

-type resource_context() :: map().
-type resource_param() :: {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), knm_options:extra_options()}.

-spec profile(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
profile(EndpointId, AccountId) ->
    profile(EndpointId, AccountId, []).

-spec profile(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
profile(EndpointId, AccountId, Options) ->
    case number(Options) of
        {'error', _} = Err -> Err;
        Number -> lookup_account(EndpointId, AccountId, Number, Options)
    end.

-spec lookup_account(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
lookup_account(EndpointId, AccountId, Number, Options) ->
    case knm_numbers:lookup_account(Number) of
        {'error', _R} = Error ->
            lager:info("unable to determine account for number ~s: ~p",[Number, _R]),
            Error;
        {'ok', _, NumberProps} ->
            generate_profile({EndpointId, AccountId, Options, NumberProps})
    end.

-spec number(kz_term:proplist()) -> {'error', 'not_resource'} | kz_term:ne_binary().
number(Options) ->
    case props:get_value('cauth', Options) of
        'undefined' -> {'error', 'not_resource'};
        JObj -> case kz_json:get_ne_binary_value(<<"URI-User">>, JObj) of
                    'undefined' -> {'error', 'not_resource'};
                    Num -> knm_converters:normalize(Num)
                end
    end.

-spec set_account_id(resource_context()) -> resource_context().
set_account_id(#{ccvs := CCVs, profile := Profile, number := #{account_id := AccountId}} = Map) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    {'ok', Account} = kzd_accounts:fetch(AccountId, 'accounts'),
    ResellerId = kz_services_reseller:get_id(AccountId),
    Realm = kzd_accounts:realm(Account),
    Map#{ccvs => [{<<"Account-ID">>, AccountId}
                 ,{<<"Account-Realm">>, Realm}
                 ,{<<"Realm">>, Realm}
                 ,{<<"Reseller-ID">>, ResellerId}
                  | CCVs
                 ]
        ,profile => [{<<"Account-ID">>, AccountId}
                    ,{<<"Reseller-ID">>, ResellerId}
                     | Profile
                    ]
        ,account_id => AccountId
        ,master_id => MasterAccountId
        ,anonymous_cid_number => kz_privacy:anonymous_caller_id_number(AccountId)
        ,anonymous_cid_name => kz_privacy:anonymous_caller_id_name(AccountId)
        }.

-spec set_inception(resource_context()) -> resource_context().
set_inception(#{number := #{number := Number}, options := #{cauth := CustomAuth}, ccvs := CCVs} = Map) ->
    Realm = kz_json:get_value(<<"URI-Realm">>, CustomAuth),
    Request = list_to_binary([Number, "@", Realm]),
    Map#{ccvs => [{<<"Inception">>, Request} | CCVs]}.

-spec set_resource_type(resource_context()) -> resource_context().
set_resource_type(#{ccvs := CCVs} = Map) ->
    Map#{ccvs => [{<<"Resource-Type">>, <<"offnet-origination">>} | CCVs]}.

-spec load_resource(kz_term:ne_binaries(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', term()}.
load_resource([], _DocId) ->
    {'error', 'not_found'};
load_resource([DBName |DBs], DocId) ->
    case kz_datamgr:open_cache_doc(DBName, DocId) of
        {'ok', _} = OK -> OK;
        _ -> load_resource(DBs, DocId)
    end.

%% for now we're still doing a backup lookup in offnet db
%% until a proper conversion is made.
%% resources should go into account db / master
%% offnet should be dropped.
%% trusted acls which are being generated from `allow_carrier`
%% in ecallmgr
%% should be created as resources in master or account
-spec find_resource_id(resource_context()) -> resource_context().
find_resource_id(#{endpoint_id := EndpointId, realm_id := MasterId, master_id := MasterId} = Map) ->
    Map#{resource => load_resource([kzs_util:format_account_db(MasterId)
                                   ,?KZ_OFFNET_DB
                                   ], EndpointId
                                  )
        };
find_resource_id(#{endpoint_id := EndpointId, realm_id := AccountId} = Map) ->
    Map#{resource => kz_datamgr:open_cache_doc(kzs_util:format_account_db(AccountId), EndpointId)}.

-spec set_resource_id(resource_context()) -> resource_context().
set_resource_id(#{resource := {'ok', Resource}, ccvs := CCVs, profile := Profile} = Map) ->
    Map#{ccvs => [{<<"Resource-ID">>, kz_doc:id(Resource)}
                 ,{<<"Global-Resource">>, kz_json:is_true(<<"Is-Global">>, Resource)}
                 ,{<<"Authorizing-Type">>, <<"resource">>}
                  | CCVs
                 ]
        ,profile => [{<<"Resource-ID">>, kz_doc:id(Resource)}
                    ,{<<"Resource-Formatters">>, kzd_resources:formatters(Resource)}
                     | Profile
                    ]
        };
set_resource_id(#{resource := {'error', _}
                 ,endpoint_id := EndpointId
                 ,ccvs := CCVs
                 ,profile := Profile
                 ,realm_id := MasterId
                 ,master_id := MasterId
                 } = Map) ->
    Map#{ccvs => [{<<"Resource-ID">>, EndpointId}
                 ,{<<"Global-Resource">>, 'true'}
                 ,{<<"Authorizing-Type">>, <<"resource">>}
                  | CCVs
                 ]
        ,profile => [{<<"Resource-ID">>, EndpointId}
                     | Profile
                    ]
        };
set_resource_id(Map) -> Map.

-spec maybe_add_t38_settings(resource_context()) -> resource_context().
maybe_add_t38_settings(#{resource := {'ok', Resource}, ccvs := CCVs} = Map) ->
    Map#{ccvs => [{<<"Resource-Fax-Option">>, kzd_resources:fax_option(Resource)} | CCVs]};
maybe_add_t38_settings(Map) -> Map.

-spec set_ignore_display_updates(resource_context()) -> resource_context().
set_ignore_display_updates(#{ccvs := CCVs} = Map) ->
    Map#{ccvs => [{<<"Ignore-Display-Updates">>, 'true'} | CCVs]}.

-spec maybe_set_ringback(resource_context()) -> resource_context().
maybe_set_ringback(#{ccvs := CCVs, number := #{ringback_media_id := MediaId}} = Map) ->
    Map#{ccvs => [{<<"Ringback-Media">>, MediaId} | CCVs]};
maybe_set_ringback(Map) -> Map.

-spec maybe_set_transfer_media(resource_context()) -> resource_context().
maybe_set_transfer_media(#{ccvs := CCVs, number := #{transfer_media_id := MediaId}} = Map) ->
    Map#{ccvs => [{<<"Transfer-Media">>, MediaId} | CCVs]};
maybe_set_transfer_media(Map) -> Map.

-spec maybe_prepend(resource_context()) -> resource_context().
maybe_prepend(#{profile := Profile, number := #{prepend := Text}} = Map) ->
    Map#{profile => [{<<"Prepend-CID-Name">>, Text} | Profile]};
maybe_prepend(Map) -> Map.

-spec set_e164_destination(resource_context()) -> resource_context().
set_e164_destination(#{account_id := AccountId, ccvs := CCVs, number := #{number := Number}} = Map) ->
    Map#{ccvs => [{<<"E164-Destination">>, knm_converters:normalize(Number, AccountId)} | CCVs]}.

-spec set_e164_origination(resource_context()) -> resource_context().
set_e164_origination(#{is_anonymous := 'true'} = Map) -> Map;
set_e164_origination(#{account_id := AccountId, caller_id_number := Number, ccvs := CCVs} = Map) ->
    Map#{ccvs => [{<<"E164-Origination">>, knm_converters:normalize(Number, AccountId)} | CCVs]}.

-spec set_did_classifier(resource_context()) -> resource_context().
set_did_classifier(#{is_anonymous := 'true'} = Map) -> Map;
set_did_classifier(#{ccvs := CCVs, number := #{number := Number}} = Map) ->
    Map#{ccvs => [{<<"DID-Classifier">>, knm_converters:classify(Number)} | CCVs]}.

-spec set_anonymous(resource_context()) -> resource_context().
set_anonymous(#{options := #{cauth := CustomAuth}, profile := Profile} = Map) ->
    case kz_json:is_true(<<"From-Is-Anonymous">>, CustomAuth) of
        'true' -> Map#{is_anonymous => 'true'
                      ,profile => [{<<"Number-Is-Anonymous">>, 'true'} | Profile]
                      };
        'false' -> Map
    end.

-spec set_caller_number(resource_context()) -> resource_context().
set_caller_number(#{is_anonymous := 'true'} = Map) -> Map;
set_caller_number(#{account_id := AccountId, options := #{cauth := CustomAuth}} = Map) ->
    Number = kz_json:get_ne_binary_value(<<"From-User">>, CustomAuth),
    Map#{caller_id_number => Number
        ,caller_id_is_reconcilable => knm_converters:is_reconcilable(Number, AccountId)
        }.

-spec maybe_lookup_cnam(resource_context()) -> resource_context().
maybe_lookup_cnam(#{is_anonymous := 'true'} = Map) -> Map;
maybe_lookup_cnam(#{caller_id_number := Number
                   ,caller_id_is_reconcilable := 'true'
                   ,number := #{inbound_cnam_enabled := 'true'}
                   ,ccvs := CCVs
                   } = Map) ->
    try
        CNam = cnam:lookup(Number),
        FromCache = kz_json:get_ne_binary_value([<<"Custom-Channel-Vars">>, <<"CNAM-From-Cache">>], CNam),
        case kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, CNam) of
            'undefined' -> Map#{caller_id_name => Number};
            CIDName -> Map#{caller_id_name => CIDName
                           ,ccvs => [{<<"CNAM-From-Cache">>, FromCache} | CCVs]
                           }
        end
    catch
        _Ex:_Err:ST ->
            lager:error("error running cnam for ~s : ~p / ~p", [Number, _Ex, _Err]),
            kz_log:log_stacktrace(ST),
            Map
    end;
maybe_lookup_cnam(Map) -> Map.

-spec set_cid_type(resource_context()) -> resource_context().
set_cid_type(#{options := #{fetch_type := FetchType}} = Map) ->
    Map#{cid_type => cid_type(FetchType)}.

-spec cid_type(kz_term:ne_binary()) -> kz_term:ne_binary().
cid_type(<<"sip_auth">>) -> <<"Caller">>;
cid_type(_) -> <<"Callee">>.

-spec set_cid_number(resource_context()) -> resource_context().
set_cid_number(#{is_anonymous := 'true'
                ,cid_type := CIDType
                ,profile := Profile
                ,anonymous_cid_number := CIDNumber
                } = Map) ->
    Map#{profile => [{<<CIDType/binary,"-ID-Number">>, CIDNumber}
                    ,{<<"Endpoint-Caller-ID-Number">>, CIDNumber}
                     | Profile
                    ]};
set_cid_number(#{caller_id_number := CIDNumber
                ,cid_type := CIDType
                ,profile := Profile
                } = Map) ->
    Map#{profile => [{<<CIDType/binary,"-ID-Number">>, CIDNumber}
                    ,{<<"Endpoint-Caller-ID-Number">>, CIDNumber}
                     | Profile
                    ]}.

-spec set_cid_name(resource_context()) -> resource_context().
set_cid_name(#{is_anonymous := 'true'
              ,cid_type := CIDType
              ,profile := Profile
              ,anonymous_cid_name := CIDName
              } = Map) ->
    Map#{profile => [{<<CIDType/binary,"-ID-Name">>, CIDName}
                    ,{<<"Endpoint-Caller-ID-Name">>, CIDName}
                     | Profile
                    ]};
set_cid_name(#{caller_id_name := CIDName
              ,cid_type := CIDType
              ,profile := Profile
              } = Map) ->
    Map#{profile => [{<<CIDType/binary,"-ID-Name">>, CIDName}
                    ,{<<"Endpoint-Caller-ID-Name">>, CIDName}
                     | Profile
                    ]};
set_cid_name(#{caller_id_number := CIDName
              ,cid_type := CIDType
              ,profile := Profile
              } = Map) ->
    Map#{profile => [{<<CIDType/binary,"-ID-Name">>, CIDName}
                    ,{<<"Endpoint-Caller-ID-Name">>, CIDName}
                     | Profile
                    ]}.

-spec maybe_transition_port_in(knm_options:extra_options()) -> 'ok' | pid().
maybe_transition_port_in(NumberProps) ->
    case knm_options:has_pending_port(NumberProps) of
        'false' -> 'ok';
        'true' -> kz_process:spawn(fun transition_port_in/1, [NumberProps])
    end.

-spec transition_port_in(knm_options:extra_options()) -> 'ok'.
transition_port_in(NumberProps) ->
    Number = knm_options:number(NumberProps),
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    Comment = <<(?APP_NAME)/binary, "-", (?APP_VERSION)/binary, " automagic">>,
    Metadata = knm_port_request:transition_metadata(MasterAccountId, 'undefined', Comment),
    case knm_port_request:get(Number) of
        {'ok', PortReq} ->
            _ = knm_port_request:transition_to_complete(PortReq, Metadata),
            'ok';
        {'error', 'not_found'} ->
            _ = knm_port_request:compatibility_transition(NumberProps, Metadata),
            'ok';
        {'error', _Reason} ->
            lager:debug("failed to transition pending port number ~s: ~p", [Number, _Reason])
    end.

-spec generate_profile(resource_param()) -> {'ok', kz_json:object()}.
generate_profile({EndpointId, RealmId, Options, NumberProps}) ->

    _ = maybe_transition_port_in(NumberProps),

    Map = #{endpoint_id => EndpointId
           ,realm_id => RealmId
           ,options => maps:from_list(Options)
           ,number => maps:from_list(NumberProps)
           ,ccvs => []
           ,profile => []
           },

    Routines = [fun set_cid_type/1
               ,fun set_account_id/1
               ,fun set_inception/1
               ,fun set_resource_type/1
               ,fun find_resource_id/1
               ,fun set_resource_id/1
               ,fun maybe_add_t38_settings/1
               ,fun set_anonymous/1
               ,fun set_caller_number/1
               ,fun maybe_prepend/1
               ,fun maybe_lookup_cnam/1
               ,fun set_e164_destination/1
               ,fun set_e164_origination/1
               ,fun set_did_classifier/1
               ,fun set_ignore_display_updates/1
               ,fun maybe_set_ringback/1
               ,fun maybe_set_transfer_media/1
               ,fun set_cid_number/1
               ,fun set_cid_name/1
               ],

    #{ccvs := CCVs, profile := CPVs} = kz_maps:exec(Routines, Map),

    Profile = [{<<"Endpoint-Type">>, <<"resource">>}
              ,{<<"Domain-Name">>, RealmId}
              ,{<<"User-ID">>, EndpointId}
              ,{<<"Custom-Profile-Vars">>, kz_json:from_list(CPVs)}
              ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
              ],
    {'ok', kz_json:from_list(Profile)}.
