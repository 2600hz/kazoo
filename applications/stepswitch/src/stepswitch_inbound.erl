%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle route requests from carrier resources
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_inbound).

-export([handle_req/2]).

-include("stepswitch.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    _ = kz_util:put_callid(JObj),
    'true' = kapi_route:req_v(JObj),
    case kz_json:get_ne_value(?CCV(<<"Account-ID">>), JObj) of
        'undefined' -> maybe_relay_request(JObj);
        AccountId ->
            lager:debug("fetch-id ~s already has account-id ~s, skipping."
                       ,[kapi_route:fetch_id(JObj), AccountId]
                       )
    end.

%%------------------------------------------------------------------------------
%% @doc handle a request inbound from offnet
%% @end
%%------------------------------------------------------------------------------
-spec maybe_relay_request(kz_json:object()) -> 'ok'.
maybe_relay_request(JObj) ->
    Number = stepswitch_util:get_inbound_destination(JObj),
    case knm_number:lookup_account(Number) of
        {'error', _R} ->
            lager:info("unable to determine account for fetch-id ~s, ~s: ~p"
                      ,[kapi_route:fetch_id(JObj), Number, _R]);
        {'ok', _, NumberProps} ->
            lager:debug("running routines for number ~s, fetch-id : ~s"
                       ,[Number, kapi_route:fetch_id(JObj)]),
            Routines = [fun set_account_id/2
                       ,fun set_ignore_display_updates/2
                       ,fun set_inception/2
                       ,fun set_resource_type/2
                       ,fun set_e164_destination/2
                       ,fun set_e164_origination/2
                       ,fun set_did_classifier/2
                       ,fun maybe_find_resource/2
                       ,fun maybe_format_destination/2
                       ,fun maybe_set_ringback/2
                       ,fun maybe_set_transfer_media/2
                       ,fun maybe_lookup_cnam/2
                       ,fun maybe_add_prepend/2
                       ,fun maybe_block_call/2
                       ,fun maybe_transition_port_in/2
                       ],
            _ = lists:foldl(fun(F, J) -> F(NumberProps, J) end
                           ,JObj
                           ,Routines
                           ),
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc determine the e164 format of the inbound number
%% @end
%%------------------------------------------------------------------------------
-spec set_account_id(knm_number_options:extra_options(), kz_json:object()) ->
                            kz_json:object().
set_account_id(NumberProps, JObj) ->
    AccountId = knm_number_options:account_id(NumberProps),
    kz_json:set_value(?CCV(<<"Account-ID">>), AccountId, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_ignore_display_updates(knm_number_options:extra_options(), kz_json:object()) ->
                                        kz_json:object().
set_ignore_display_updates(_, JObj) ->
    kz_json:set_value(?CCV(<<"Ignore-Display-Updates">>), <<"true">>, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_inception(knm_number_options:extra_options(), kz_json:object()) ->
                           kz_json:object().
set_inception(_, JObj) ->
    Request = kz_json:get_value(<<"Request">>, JObj),
    kz_json:set_value(?CCV(<<"Inception">>), Request, JObj).

%%------------------------------------------------------------------------------
%% @doc Set the E164 number destination
%% @end
%%------------------------------------------------------------------------------
-spec set_e164_destination(knm_number_options:extra_options(), kz_json:object()) ->
                                  kz_json:object().
set_e164_destination(_, JObj) ->
    Number = stepswitch_util:get_inbound_destination(JObj),
    kz_json:set_value(?CCV(<<"E164-Destination">>), Number, JObj).

%%------------------------------------------------------------------------------
%% @doc Set the E164 number origination
%% @end
%%------------------------------------------------------------------------------
-spec set_e164_origination(knm_number_options:extra_options(), kz_json:object()) ->
                                  kz_json:object().
set_e164_origination(_, JObj) ->
    Number = stepswitch_util:get_inbound_origination(JObj),
    kz_json:set_value(?CCV(<<"E164-Origination">>), Number, JObj).

%%------------------------------------------------------------------------------
%% @doc Set the destination number did classification
%% @end
%%------------------------------------------------------------------------------
-spec set_did_classifier(knm_number_options:extra_options(), kz_json:object()) ->
                                kz_json:object().
set_did_classifier(_, JObj) ->
    Number = stepswitch_util:get_inbound_destination(JObj),
    kz_json:set_value(?CCV(<<"DID-Classifier">>), knm_converters:classify(Number), JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_resource_type(knm_number_options:extra_options(), kz_json:object()) ->
                               kz_json:object().
set_resource_type(_, JObj) ->
    kz_json:set_value(?CCV(<<"Resource-Type">>), <<"offnet-origination">>, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_find_resource(knm_number_options:extra_options(), kz_json:object()) ->
                                 kz_json:object().
maybe_find_resource(_, JObj) ->
    case stepswitch_resources:reverse_lookup(JObj) of
        {'error', 'not_found'} -> JObj;
        {'ok', ResourceProps} ->
            Routines = [fun add_resource_id/2
                       ,fun maybe_add_t38_settings/2
                       ],
            lists:foldl(fun(F, J) ->  F(J, ResourceProps) end
                       ,JObj
                       ,Routines
                       )
    end.

-spec add_resource_id(kz_json:object(), kz_term:proplist()) -> kz_json:object().
add_resource_id(JObj, ResourceProps) ->
    ResourceId = props:get_value('resource_id', ResourceProps),
    kz_json:set_values([{?CCV(<<"Resource-ID">>), ResourceId}
                       ,{?CCV(<<"Global-Resource">>), props:get_is_true('global', ResourceProps)}
                       ,{?CCV(<<"Authorizing-Type">>), <<"resource">>}
                        %% TODO
                        %% we need to make sure that Authorizing-ID is used
                        %% with Authorizing-Type in ALL kapps
                        %% when this is done remove the comment below
                        %% ,{?CCV(<<"Authorizing-ID">>), ResourceId}
                       ]
                      ,JObj
                      ).

-spec maybe_add_t38_settings(kz_json:object(), kz_term:proplist()) -> kz_json:object().
maybe_add_t38_settings(JObj, ResourceProps) ->
    case props:get_value('fax_option', ResourceProps) of
        'true' ->
            kz_json:set_value(?CCV(<<"Resource-Fax-Option">>)
                             ,props:get_value('fax_option', ResourceProps)
                             ,JObj
                             );
        <<"auto">> ->
            kz_json:set_value(?CCV(<<"Resource-Fax-Option">>)
                             ,props:get_value('fax_option', ResourceProps)
                             ,JObj
                             );
        _ -> JObj
    end.

-spec maybe_format_destination(knm_number_options:extra_options(), kz_json:object()) ->
                                      kz_json:object().
maybe_format_destination(_, JObj) ->
    case kz_json:get_value(?CCV(<<"Resource-ID">>), JObj) of
        'undefined' -> JObj;
        ResourceId ->
            case stepswitch_resources:get_props(ResourceId) of
                'undefined' -> JObj;
                Resource ->
                    Formatters = props:get_value(<<"Formatters">>, Resource, kz_json:new()),
                    kz_formatters:apply(JObj, Formatters, 'inbound')
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_set_ringback(knm_number_options:extra_options(), kz_json:object()) ->
                                kz_json:object().
maybe_set_ringback(NumberProps, JObj) ->
    case knm_number_options:ringback_media_id(NumberProps) of
        'undefined' -> JObj;
        MediaId ->
            kz_json:set_value(?CCV(<<"Ringback-Media">>), MediaId, JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc determine the e164 format of the inbound number
%% @end
%%------------------------------------------------------------------------------
-spec maybe_set_transfer_media(knm_number_options:extra_options(), kz_json:object()) ->
                                      kz_json:object().
maybe_set_transfer_media(NumberProps, JObj) ->
    case knm_number_options:transfer_media_id(NumberProps) of
        'undefined' -> JObj;
        MediaId ->
            kz_json:set_value(?CCV(<<"Transfer-Media">>), MediaId, JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc build the JSON to set the custom channel vars with the calls
%% account and authorizing  ID
%% @end
%%------------------------------------------------------------------------------
-spec maybe_lookup_cnam(knm_number_options:extra_options(), kz_json:object()) ->
                               kz_json:object().
maybe_lookup_cnam(NumberProps, JObj) ->
    case knm_number_options:inbound_cnam_enabled(NumberProps) of
        'false' -> JObj;
        'true' -> stepswitch_cnam:lookup(JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_prepend(knm_number_options:extra_options(), kz_json:object()) ->
                               kz_json:object().
maybe_add_prepend(NumberProps, JObj) ->
    case knm_number_options:prepend(NumberProps) of
        'undefined' -> JObj;
        Prepend -> kz_json:set_value(<<"Prepend-CID-Name">>, Prepend, JObj)
    end.

-spec maybe_block_call(knm_number_options:extra_options(), kz_json:object()) -> kz_json:object().
maybe_block_call(_, JObj) ->
    case block_call_routines(JObj) of
        'true' -> JObj;
        'false' -> relay_request(JObj)
    end.

-spec block_call_routines(kz_json:object()) -> boolean().
block_call_routines(JObj) ->
    Routines = [{fun should_block_anonymous/1, {<<"433">>, <<"Anonymity Disallowed">>}}
               ,{fun is_blacklisted/1, {<<"603">>, <<"Decline">>}}
               ],
    lists:any(fun(Routine) -> block_call_routine(Routine, JObj) end, Routines).

-type block_call_fun() :: fun((kz_json:object()) -> boolean()).
-type block_call_resp() :: {kz_term:ne_binary(), kz_term:ne_binary()}.
-type block_call_arg() :: {block_call_fun(), block_call_resp()}.

-spec block_call_routine(block_call_arg(), kz_json:object()) -> boolean().
block_call_routine({Fun, {Code, Msg}}, JObj) ->
    case Fun(JObj) of
        'true' -> send_error_response(JObj, Code, Msg);
        'false' -> 'false'
    end.

-spec should_block_anonymous(kz_json:object()) -> boolean().
should_block_anonymous(JObj) ->
    kz_privacy:should_block_anonymous(JObj)
        orelse (kz_privacy:is_anonymous(JObj)
                andalso kz_json:is_true(<<"should_block_anonymous">>, get_blacklist(JObj))
               ).

%%------------------------------------------------------------------------------
%% @doc relay a route request once populated with the new properties
%% @end
%%------------------------------------------------------------------------------
-spec relay_request(kz_json:object()) -> kz_json:object().
relay_request(JObj) ->
    kapi_route:publish_req(JObj),
    lager:debug("relaying route request ~s", [kapi_route:fetch_id(JObj)]),
    JObj.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_transition_port_in(knm_number_options:extra_options(), kz_json:object()) -> 'ok'.
maybe_transition_port_in(NumberProps, JObj) ->
    case knm_number_options:has_pending_port(NumberProps) of
        'false' -> 'ok';
        'true' -> transition_port_in(NumberProps, JObj)
    end.

-spec transition_port_in(knm_number_options:extra_options(), kz_term:api_object()) -> 'ok'.
transition_port_in(NumberProps, _JObj) ->
    Number = knm_number_options:number(NumberProps),
    {ok, MasterAccountId} = kapps_util:get_master_account_id(),
    Comment = <<(?APP_NAME)/binary, "-", (?APP_VERSION)/binary, " automagic">>,
    Metadata = knm_port_request:transition_metadata(MasterAccountId, undefined, Comment),
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_blacklisted(kz_json:object()) -> boolean().
is_blacklisted(JObj) ->
    is_number_blacklisted(get_blacklist(JObj), JObj).

-spec is_number_blacklisted(kz_json:object(), kz_json:object()) -> boolean().
is_number_blacklisted(Blacklist, JObj) ->
    Number = kz_json:get_value(<<"Caller-ID-Number">>, JObj),
    Normalized = knm_converters:normalize(Number),
    case kz_json:get_value(Normalized, Blacklist) of
        'undefined' -> 'false';
        _ -> lager:info("~s(~s) is blacklisted", [Number, Normalized]),
             'true'
    end.

-spec get_blacklists(kz_term:ne_binary()) ->
                            {'ok', kz_term:ne_binaries()} |
                            {'error', any()}.
get_blacklists(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _R}=E ->
            lager:error("could not open account doc ~s : ~p", [AccountId, _R]),
            E;
        {'ok', Doc} ->
            case kz_json:get_value(<<"blacklists">>, Doc, []) of
                [] -> {'error', 'undefined'};
                [_|_]=Blacklists-> {'ok', Blacklists};
                _ -> {'error', 'miss_configured'}
            end
    end.

-spec get_blacklist(kz_json:object()) -> kz_json:object().
get_blacklist(JObj) ->
    AccountId = kz_json:get_ne_value(?CCV(<<"Account-ID">>), JObj),
    case get_blacklists(AccountId) of
        {'error', _R} -> kz_json:new();
        {'ok', Blacklists} -> get_blacklist(AccountId, Blacklists)
    end.

-spec get_blacklist(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_json:object().
get_blacklist(AccountId, Blacklists) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    lists:foldl(fun(BlacklistId, Acc) ->
                        case kz_datamgr:open_cache_doc(AccountDb, BlacklistId) of
                            {'error', _R} ->
                                lager:error("could not open ~s in ~s: ~p", [BlacklistId, AccountDb, _R]),
                                Acc;
                            {'ok', Doc} ->
                                Numbers = kz_json:get_value(<<"numbers">>, Doc, kz_json:new()),
                                BlackList = maybe_set_block_anonymous(Numbers, kz_json:is_true(<<"should_block_anonymous">>, Doc)),
                                kz_json:merge_jobjs(Acc, BlackList)
                        end
                end
               ,kz_json:new()
               ,Blacklists
               ).

-spec maybe_set_block_anonymous(kz_json:object(), boolean()) -> kz_json:object().
maybe_set_block_anonymous(JObj, 'false') -> JObj;
maybe_set_block_anonymous(JObj, 'true') ->
    kz_json:set_value(<<"should_block_anonymous">>, 'true', JObj).

-spec send_error_response(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'true'.
send_error_response(JObj, Code, Message) ->
    lager:debug("sending error response: ~s ~s", [Code, Message]),
    Resp = [{<<"Msg-ID">>, kz_api:msg_id(JObj)}
           ,{<<"Method">>, <<"error">>}
           ,{<<"Route-Error-Code">>, Code}
           ,{<<"Route-Error-Message">>, Message}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_route:publish_resp(kz_api:server_id(JObj), Resp),
    'true'.
