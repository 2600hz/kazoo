%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_vitelity_cnam).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
-spec save(knm_number:knm_number(), ne_binary()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    handle_outbound_cnam(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    handle_outbound_cnam(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    handle_outbound_cnam(Number);
save(Number, _State) ->
    Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    _ = remove_inbound_cnam(Number),
    knm_services:deactivate_features(Number
                                    ,[?FEATURE_CNAM_INBOUND
                                     ,?FEATURE_CNAM_OUTBOUND
                                     ,?FEATURE_CNAM
                                     ]
                                    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_outbound_cnam(knm_number:knm_number()) -> knm_number:knm_number().
handle_outbound_cnam(Number) ->
    IsDryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    PhoneNumber = knm_number:phone_number(Number),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM_OUTBOUND),
    Doc = knm_phone_number:doc(PhoneNumber),
    CurrentCNAM = kz_json:get_ne_value(?CNAM_DISPLAY_NAME, Feature),
    case kz_json:get_ne_value([?FEATURE_CNAM, ?CNAM_DISPLAY_NAME], Doc) of
        'undefined' ->
            Number1 = knm_services:deactivate_feature(Number, ?FEATURE_CNAM_OUTBOUND),
            handle_inbound_cnam(Number1);
        CurrentCNAM ->
            handle_inbound_cnam(Number);
        NewCNAM when IsDryRun ->
            lager:debug("dry run: cnam display name changed to ~s", [NewCNAM]),
            Number1 = knm_services:activate_feature(Number, {?FEATURE_CNAM_OUTBOUND, NewCNAM}),
            handle_inbound_cnam(Number1);
        NewCNAM ->
            lager:debug("cnam display name changed to ~s, updating", [NewCNAM]),
            Number1 = try_update_outbound_cnam(Number, NewCNAM),
            handle_inbound_cnam(Number1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec try_update_outbound_cnam(knm_number:knm_number(), ne_binary()) ->
                                      knm_number:knm_number().
try_update_outbound_cnam(Number, NewCNAM) ->
    DID = knm_phone_number:number(knm_number:phone_number(Number)),
    case
        knm_vitelity_util:query_vitelity(
          knm_vitelity_util:build_uri(
            outbound_cnam_options(DID, NewCNAM)
           )
         )
    of
        {'error', E} -> knm_errors:unspecified(E, Number);
        {'ok', XML} -> process_outbound_xml_resp(Number, NewCNAM, XML)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec outbound_cnam_options(ne_binary(), ne_binary()) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_outbound_xml_resp(knm_number:knm_number(), ne_binary(), text()) ->
                                       knm_number:knm_number().
process_outbound_xml_resp(Number, FeatureData, XML_binary) ->
    XML = unicode:characters_to_list(XML_binary),
    try xmerl_scan:string(XML) of
        {#xmlElement{name='content'
                    ,content=Children
                    }
        ,_Left
        } -> process_outbound_resp(Number, FeatureData, Children);
        _ -> knm_errors:unspecified('unknown_resp_format', Number)
    catch
        _E:_R ->
            knm_errors:unspecified('invalid_resp_format', Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_outbound_resp(knm_number:knm_number(), ne_binary(), xml_els()) ->
                                   knm_number:knm_number().
process_outbound_resp(Number, FeatureData, Children) ->
    case knm_vitelity_util:xml_resp_status_msg(Children) of
        <<"ok">> -> check_outbound_response_tag(Number, FeatureData, Children);
        <<"fail">> ->
            Msg = knm_vitelity_util:xml_resp_error_msg(Children),
            knm_errors:unspecified(Msg, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_outbound_response_tag(knm_number:knm_number(), ne_binary(), xml_els()) ->
                                         knm_number:knm_number().
check_outbound_response_tag(Number, NewCNAM, Children) ->
    case knm_vitelity_util:xml_resp_response_msg(Children) of
        'undefined' -> knm_errors:unspecified('resp_tag_not_found', Number);
        <<"ok">> ->
            FeatureData = kz_json:from_list([{?CNAM_DISPLAY_NAME, NewCNAM}]),
            Number1 = knm_services:activate_feature(Number, {?FEATURE_CNAM_OUTBOUND, FeatureData}),
            _ = publish_cnam_update(Number1),
            Number1;
        Msg ->
            lager:debug("resp was not ok, was ~s", [Msg]),
            knm_errors:unspecified(Msg, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_inbound_cnam(knm_number:knm_number()) -> knm_number:knm_number().
-spec handle_inbound_cnam(knm_number:knm_number(), boolean()) -> knm_number:knm_number().
handle_inbound_cnam(Number) ->
    IsDryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    handle_inbound_cnam(Number, IsDryRun).

handle_inbound_cnam(Number, 'true') ->
    Doc = knm_phone_number:doc(knm_number:phone_number(Number)),
    case kz_json:is_true([?FEATURE_CNAM, ?CNAM_INBOUND_LOOKUP], Doc) of
        false ->
            knm_services:deactivate_features(Number, [?FEATURE_CNAM_INBOUND
                                                     ,?CNAM_INBOUND_LOOKUP
                                                     ]);
        'true' ->
            FeatureData = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, true}]),
            knm_services:activate_feature(Number, {?FEATURE_CNAM_INBOUND, FeatureData})
    end;
handle_inbound_cnam(Number, 'false') ->
    Doc = knm_phone_number:doc(knm_number:phone_number(Number)),
    case kz_json:is_true([?FEATURE_CNAM, ?CNAM_INBOUND_LOOKUP], Doc) of
        'false' -> remove_inbound_cnam(Number);
        'true' -> add_inbound_cnam(Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_inbound_cnam(knm_number:knm_number()) -> knm_number:knm_number().
remove_inbound_cnam(Number) ->
    DID = knm_phone_number:number(knm_number:phone_number(Number)),
    _ = knm_vitelity_util:query_vitelity(
          knm_vitelity_util:build_uri(
            remove_inbound_options(DID)
           )
         ),
    knm_services:deactivate_features(Number, [?FEATURE_CNAM_INBOUND
                                             ,?CNAM_INBOUND_LOOKUP
                                             ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_inbound_options(ne_binary()) -> knm_vitelity_util:query_options().
remove_inbound_options(Number) ->
    [{'qs', [{'did', knm_converters:to_npan(Number)}
            ,{'cmd', <<"cnamdisable">>}
            ,{'xml', <<"yes">>}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_inbound_cnam(knm_number:knm_number()) ->
                              knm_number:knm_number().
add_inbound_cnam(Number) ->
    DID = knm_phone_number:number(knm_number:phone_number(Number)),
    case
        knm_vitelity_util:query_vitelity(
          knm_vitelity_util:build_uri(
            inbound_options(DID)
           )
         )
    of
        {'ok', XML} -> process_xml_resp(Number, XML);
        {'error', _E} ->
            knm_errors:unspecified('unknown_error', Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec inbound_options(ne_binary()) -> knm_vitelity_util:query_options().
inbound_options(DID) ->
    [{'qs', [{'did', knm_converters:to_npan(DID)}
            ,{'cmd', <<"cnamenable">>}
            ,{'xml', <<"yes">>}
             | knm_vitelity_util:default_options()
            ]}
    ,{'uri', knm_vitelity_util:api_uri()}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_resp(knm_number:knm_number(), text()) ->
                              knm_number:knm_number().
process_xml_resp(Number, XML) ->
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(Number, XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to process XML: ~s: ~p", [_E, _R]),
            knm_errors:unspecified('invalid_resp_server', Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_content_tag(knm_number:knm_number(), xml_el()) ->
                                     knm_number:knm_number().
process_xml_content_tag(Number, #xmlElement{name='content'
                                           ,content=Children
                                           }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            Msg = knm_vitelity_util:xml_resp_error_msg(Els),
            knm_errors:unspecified(Msg, Number);
        <<"ok">> ->
            FeatureData = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, true}]),
            knm_services:activate_feature(Number, {?FEATURE_CNAM_INBOUND, FeatureData})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_cnam_update(knm_number:knm_number()) -> 'ok'.
publish_cnam_update(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    Feature = knm_phone_number:feature(PhoneNumber, ?FEATURE_CNAM),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(PhoneNumber)}
             ,{<<"Number-State">>, knm_phone_number:state(PhoneNumber)}
             ,{<<"Local-Number">>, knm_phone_number:module_name(PhoneNumber) =:= ?CARRIER_LOCAL}
             ,{<<"Number">>, knm_util:pretty_print(knm_phone_number:number(PhoneNumber))}
             ,{<<"Acquired-For">>, knm_phone_number:auth_by(PhoneNumber)}
             ,{<<"Cnam">>, case Feature of 'undefined' -> kz_json:new(); _ -> Feature end}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapi_notify_publisher:cast(Notify, fun kapi_notifications:publish_cnam_request/1).
