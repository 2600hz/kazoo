%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_vitelity_cnam).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_phone_number:record()) -> knm_phone_number:record().
save(PN) ->
    State = knm_phone_number:state(PN),
    save(PN, State).

-spec save(knm_phone_number:record(), kz_term:ne_binary()) -> knm_phone_number:record().
save(PN, ?NUMBER_STATE_RESERVED) ->
    handle_outbound_cnam(PN);
save(PN, ?NUMBER_STATE_IN_SERVICE) ->
    handle_outbound_cnam(PN);
save(PN, ?NUMBER_STATE_PORT_IN) ->
    handle_outbound_cnam(PN);
save(PN, _State) ->
    PN.

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    _ = remove_inbound_cnam(PN),
    knm_providers:deactivate_features(PN
                                     ,[?FEATURE_CNAM_INBOUND
                                      ,?FEATURE_CNAM_OUTBOUND
                                      ,?FEATURE_CNAM
                                      ]
                                     ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_outbound_cnam(knm_phone_number:record()) -> knm_phone_number:record().
handle_outbound_cnam(PN) ->
    IsDryRun = knm_phone_number:dry_run(PN),
    Feature = knm_phone_number:feature(PN, ?FEATURE_CNAM_OUTBOUND),
    Doc = knm_phone_number:doc(PN),
    CurrentCNAM = kz_json:get_ne_value(?CNAM_DISPLAY_NAME, Feature),
    case kz_json:get_ne_value([?FEATURE_CNAM, ?CNAM_DISPLAY_NAME], Doc) of
        'undefined' ->
            PN1 = knm_providers:deactivate_feature(PN, ?FEATURE_CNAM_OUTBOUND),
            handle_inbound_cnam(PN1);
        CurrentCNAM ->
            handle_inbound_cnam(PN);
        NewCNAM when IsDryRun ->
            lager:debug("dry run: cnam display name changed to ~s", [NewCNAM]),
            PN1 = knm_providers:activate_feature(PN, {?FEATURE_CNAM_OUTBOUND, NewCNAM}),
            handle_inbound_cnam(PN1);
        NewCNAM ->
            lager:debug("cnam display name changed to ~s, updating", [NewCNAM]),
            PN1 = try_update_outbound_cnam(PN, NewCNAM),
            handle_inbound_cnam(PN1)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec try_update_outbound_cnam(knm_phone_number:record(), kz_term:ne_binary()) ->
          knm_phone_number:record().
try_update_outbound_cnam(PN, NewCNAM) ->
    DID = knm_phone_number:number(PN),
    case
        knm_vitelity_util:query_vitelity(
          knm_vitelity_util:build_uri(
            outbound_cnam_options(DID, NewCNAM)
           )
         )
    of
        {'error', E} -> knm_errors:unspecified(E, PN);
        {'ok', XML} -> process_outbound_xml_resp(PN, NewCNAM, XML)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec outbound_cnam_options(kz_term:ne_binary(), kz_term:ne_binary()) ->
          knm_vitelity_util:query_options().
outbound_cnam_options(DID, NewCNAM) ->
    [{'qs', [{'cmd', <<"lidb">>}
            ,{'did', knm_converters:to_npan(DID)}
            ,{'name', NewCNAM}
            ,{'xml', <<"yes">>}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_outbound_xml_resp(knm_phone_number:record(), kz_term:ne_binary(), kz_term:text()) ->
          knm_phone_number:record().
process_outbound_xml_resp(PN, FeatureData, XML_binary) ->
    XML = unicode:characters_to_list(XML_binary),
    try xmerl_scan:string(XML) of
        {#xmlElement{name='content'
                    ,content=Children
                    }
        ,_Left
        } -> process_outbound_resp(PN, FeatureData, Children);
        _ -> knm_errors:unspecified('unknown_resp_format', PN)
    catch
        _E:_R ->
            knm_errors:unspecified('invalid_resp_format', PN)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_outbound_resp(knm_phone_number:record(), kz_term:ne_binary(), kz_types:xml_els()) ->
          knm_phone_number:record().
process_outbound_resp(PN, FeatureData, Children) ->
    case knm_vitelity_util:xml_resp_status_msg(Children) of
        <<"ok">> -> check_outbound_response_tag(PN, FeatureData, Children);
        <<"fail">> ->
            Msg = knm_vitelity_util:xml_resp_error_msg(Children),
            knm_errors:unspecified(Msg, PN)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_outbound_response_tag(knm_phone_number:record(), kz_term:ne_binary(), kz_types:xml_els()) ->
          knm_phone_number:record().
check_outbound_response_tag(PN, NewCNAM, Children) ->
    case knm_vitelity_util:xml_resp_response_msg(Children) of
        'undefined' -> knm_errors:unspecified('resp_tag_not_found', PN);
        <<"ok">> ->
            FeatureData = kz_json:from_list([{?CNAM_DISPLAY_NAME, NewCNAM}]),
            PN1 = knm_providers:activate_feature(PN, {?FEATURE_CNAM_OUTBOUND, FeatureData}),
            publish_cnam_update(PN1),
            PN1;
        Msg ->
            lager:debug("resp was not ok, was ~s", [Msg]),
            knm_errors:unspecified(Msg, PN)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec handle_inbound_cnam(knm_phone_number:record()) -> knm_phone_number:record().
handle_inbound_cnam(PN) ->
    IsDryRun = knm_phone_number:dry_run(PN),
    handle_inbound_cnam(PN, IsDryRun).

-spec handle_inbound_cnam(knm_phone_number:record(), boolean()) -> knm_phone_number:record().
handle_inbound_cnam(PN, 'true') ->
    Doc = knm_phone_number:doc(PN),
    case kz_json:is_true([?FEATURE_CNAM, ?CNAM_INBOUND_LOOKUP], Doc) of
        'false' ->
            knm_providers:deactivate_features(PN, [?FEATURE_CNAM_INBOUND
                                                  ,?CNAM_INBOUND_LOOKUP
                                                  ]);
        'true' ->
            FeatureData = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, true}]),
            knm_providers:activate_feature(PN, {?FEATURE_CNAM_INBOUND, FeatureData})
    end;
handle_inbound_cnam(PN, 'false') ->
    Doc = knm_phone_number:doc(PN),
    case kz_json:is_true([?FEATURE_CNAM, ?CNAM_INBOUND_LOOKUP], Doc) of
        'false' -> remove_inbound_cnam(PN);
        'true' -> add_inbound_cnam(PN)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_inbound_cnam(knm_phone_number:record()) -> knm_phone_number:record().
remove_inbound_cnam(PN) ->
    DID = knm_phone_number:number(PN),
    _ = knm_vitelity_util:query_vitelity(
          knm_vitelity_util:build_uri(
            remove_inbound_options(DID)
           )
         ),
    knm_providers:deactivate_features(PN, [?FEATURE_CNAM_INBOUND
                                          ,?CNAM_INBOUND_LOOKUP
                                          ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_inbound_options(kz_term:ne_binary()) -> knm_vitelity_util:query_options().
remove_inbound_options(PN) ->
    [{'qs', [{'did', knm_converters:to_npan(PN)}
            ,{'cmd', <<"cnamdisable">>}
            ,{'xml', <<"yes">>}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_inbound_cnam(knm_phone_number:record()) ->
          knm_phone_number:record().
add_inbound_cnam(PN) ->
    DID = knm_phone_number:number(PN),
    case
        knm_vitelity_util:query_vitelity(
          knm_vitelity_util:build_uri(
            inbound_options(DID)
           )
         )
    of
        {'ok', XML} -> process_xml_resp(PN, XML);
        {'error', _E} ->
            knm_errors:unspecified('unknown_error', PN)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec inbound_options(kz_term:ne_binary()) -> knm_vitelity_util:query_options().
inbound_options(DID) ->
    [{'qs', [{'did', knm_converters:to_npan(DID)}
            ,{'cmd', <<"cnamenable">>}
            ,{'xml', <<"yes">>}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_xml_resp(knm_phone_number:record(), kz_term:text()) ->
          knm_phone_number:record().
process_xml_resp(PN, XML) ->
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(PN, XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to process XML: ~s: ~p", [_E, _R]),
            knm_errors:unspecified('invalid_resp_server', PN)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec process_xml_content_tag(knm_phone_number:record(), kz_types:xml_el()) ->
          knm_phone_number:record().
process_xml_content_tag(PN, #xmlElement{name='content'
                                       ,content=Children
                                       }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            Msg = knm_vitelity_util:xml_resp_error_msg(Els),
            knm_errors:unspecified(Msg, PN);
        <<"ok">> ->
            FeatureData = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, true}]),
            knm_providers:activate_feature(PN, {?FEATURE_CNAM_INBOUND, FeatureData})
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_cnam_update(knm_phone_number:record()) -> 'ok'.
publish_cnam_update(PN) ->
    Feature = knm_phone_number:feature(PN, ?FEATURE_CNAM),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PN)}
             ,{<<"Number-State">>, knm_phone_number:state(PN)}
             ,{<<"Local-Number">>, knm_phone_number:module_name(PN) =:= ?CARRIER_LOCAL}
             ,{<<"Number">>, knm_util:pretty_print(knm_phone_number:number(PN))}
             ,{<<"Acquired-For">>, knm_phone_number:auth_by(PN)}
             ,{<<"Cnam">>, case Feature of 'undefined' -> kz_json:new(); _ -> Feature end}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_cnam_request/1).
