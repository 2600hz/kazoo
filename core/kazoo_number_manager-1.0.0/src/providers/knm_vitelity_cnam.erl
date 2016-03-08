%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_vitelity_cnam).

-export([save/1]).
-export([delete/1]).

-include("../knm.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%--------------------------------------------------------------------
-spec save(knm_phone_number:knm_number()) -> number_return().
-spec save(knm_phone_number:knm_number(), ne_binary()) -> number_return().
save(Number) ->
    State = knm_phone_number:state(Number),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    handle_outbound_cnam(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    handle_outbound_cnam(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    handle_outbound_cnam(Number);
save(Number, _State) -> {'ok', Number}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_phone_number:knm_number()) ->
                    {'ok', knm_phone_number:knm_number()}.
delete(Number) ->
    knm_services:deactivate_features(Number, [<<"inbound_cnam">>, <<"outbound_cnam">>, <<"cnam">>]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_outbound_cnam(knm_phone_number:knm_number()) -> number_return().
-spec handle_outbound_cnam(knm_phone_number:knm_number(), boolean()) -> number_return().
handle_outbound_cnam(Number) ->
    handle_outbound_cnam(Number, knm_phone_number:dry_run(Number)).

handle_outbound_cnam(Number, 'true') ->
    Features = knm_phone_number:features(Number),
    Doc = knm_phone_number:doc(Number),
    CurrentCNAM = wh_json:get_ne_value([<<"cnam">>, <<"display_name">>], Features),
    case wh_json:get_ne_value([?PVT_FEATURES, <<"cnam">>, <<"display_name">>], Doc) of
        'undefined' ->
            {'ok', Number1} = knm_services:deactivate_feature(Number, <<"outbound_cnam">>),
            handle_inbound_cnam(Number1);
        CurrentCNAM ->
            {'ok', Number1} = knm_services:deactivate_feature(Number, <<"outbound_cnam">>),
            handle_inbound_cnam(Number1);
        NewCNAM ->
            lager:debug("dry run: cnam display name changed to ~s", [NewCNAM]),
            {'ok', Number1} = knm_services:activate_feature(Number, <<"outbound_cnam">>),
            handle_inbound_cnam(Number1)
    end;
handle_outbound_cnam(Number, 'false') ->
    Features = knm_phone_number:features(Number),
    Doc = knm_phone_number:doc(Number),
    CurrentCNAM = wh_json:get_ne_value([<<"cnam">>, <<"display_name">>], Features),
    case wh_json:get_ne_value([?PVT_FEATURES, <<"cnam">>, <<"display_name">>], Doc) of
        'undefined' ->
            {'ok', Number1} = knm_services:deactivate_feature(Number, <<"outbound_cnam">>),
            handle_inbound_cnam(Number1);
        CurrentCNAM ->
            {'ok', Number1} = knm_services:deactivate_feature(Number, <<"outbound_cnam">>),
            handle_inbound_cnam(Number1);
        NewCNAM ->
            lager:debug("cnam display name changed to ~s, updating Vitelity", [NewCNAM]),
            {'ok', Number1} = try_update_outbound_cnam(Number, NewCNAM),
            handle_inbound_cnam(Number1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec try_update_outbound_cnam(knm_phone_number:knm_number(), ne_binary()) ->
                                      number_return().
try_update_outbound_cnam(Number, NewCNAM) ->
    DID = knm_phone_number:number(Number),
    case
        knm_vitelity_util:query_vitelity(
          knm_vitelity_util:build_uri(
            outbound_cnam_options(DID, NewCNAM)
           )
         )
    of
        {'error', _R}=Error -> Error;
        {'ok', XML} ->
            process_outbound_xml_resp(Number, XML)
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
             ,{'did', (knm_converters:default()):to_npan(DID)}
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
-spec process_outbound_xml_resp(knm_phone_number:knm_number(), text()) ->
                                       number_return().
process_outbound_xml_resp(Number, XML) ->
    try xmerl_scan:string(XML) of
        {#xmlElement{name='content'
                     ,content=Children
                    }
         ,_Left
        } ->
            process_outbound_resp(Number, Children);
        _ ->
            {'error', 'unknown_resp_format'}
    catch
        _E:_R ->
            {'error', 'invalid_resp_format'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_outbound_resp(knm_phone_number:knm_number(), xml_els()) ->
                                   number_return().
process_outbound_resp(Number, Children) ->
    case knm_vitelity_util:xml_resp_status_msg(Children) of
        <<"ok">> ->
            check_outbound_response_tag(Number, Children);
        <<"fail">> ->
            {'error', knm_vitelity_util:xml_resp_error_msg(Children)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec check_outbound_response_tag(knm_phone_number:knm_number(), xml_els()) ->
                                         number_return().
check_outbound_response_tag(Number, Children) ->
    case knm_vitelity_util:xml_resp_response_msg(Children) of
        'undefined' ->
            {'error', 'resp_tag_not_found'};
        <<"ok">> ->
            {'ok', Number1} = knm_services:activate_feature(Number, <<"outbound_cnam">>),
            _ = publish_cnam_update(Number1),
            {'ok', Number1};
        Msg ->
            lager:debug("resp was not ok, was ~s", [Msg]),
            {'error', Msg}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_inbound_cnam(knm_phone_number:knm_number()) -> number_return().
-spec handle_inbound_cnam(knm_phone_number:knm_number(), boolean()) -> number_return().
handle_inbound_cnam(Number) ->
    handle_inbound_cnam(Number, knm_phone_number:dry_run(Number)).

handle_inbound_cnam(Number, 'true') ->
    Doc = knm_phone_number:doc(Number),
    case wh_json:is_true([?PVT_FEATURES, <<"cnam">>, <<"inbound_lookup">>], Doc) of
        'false' ->
            knm_services:deactivate_feature(Number, <<"inbound_cnam">>);
        'true' ->
            knm_services:activate_feature(Number, <<"inbound_cnam">>)
    end;
handle_inbound_cnam(Number, 'false') ->
    Doc = knm_phone_number:doc(Number),
    case wh_json:is_true([?PVT_FEATURES, <<"cnam">>, <<"inbound_lookup">>], Doc) of
        'false' -> remove_inbound_cnam(Number);
        'true' -> add_inbound_cnam(Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_inbound_cnam(knm_phone_number:knm_number()) ->
                                 {'ok', knm_phone_number:knm_number()}.
remove_inbound_cnam(Number) ->
    DID = knm_phone_number:number(Number),
    _ = knm_vitelity_util:query_vitelity(
          knm_vitelity_util:build_uri(
            remove_inbound_options(DID)
           )
         ),
    knm_services:deactivate_feature(Number, <<"inbound_cnam">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_inbound_options(ne_binary()) -> knm_vitelity_util:query_options().
remove_inbound_options(Number) ->
    [{'qs', [{'did', (knm_converters:default()):to_npan(Number)}
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
-spec add_inbound_cnam(knm_phone_number:knm_number()) -> number_return().
add_inbound_cnam(Number) ->
    DID = knm_phone_number:number(Number),
    case
        knm_vitelity_util:query_vitelity(
          knm_vitelity_util:build_uri(
            inbound_options(DID)
           )
         )
    of
        {'ok', XML} -> process_xml_resp(Number, XML);
        {'error', _E} -> {'error', 'unknown_error'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec inbound_options(ne_binary()) -> knm_vitelity_util:query_options().
inbound_options(DID) ->
    [{'qs', [{'did', (knm_converters:default()):to_npan(DID)}
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
-spec process_xml_resp(knm_phone_number:knm_number(), text()) -> number_return().
process_xml_resp(Number, XML) ->
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(Number, XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to process XML: ~s: ~p", [_E, _R]),
            {'error', 'invalid_resp_server'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec process_xml_content_tag(knm_phone_number:knm_number(), xml_el()) ->
                                     number_return().
process_xml_content_tag(Number, #xmlElement{name='content'
                                            ,content=Children
                                           }) ->
    Els = kz_xml:elements(Children),
    case knm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            {'error', knm_vitelity_util:xml_resp_error_msg(Els)};
        <<"ok">> ->
            knm_services:activate_feature(Number, <<"inbound_cnam">>)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_cnam_update(knm_phone_number:knm_number()) -> 'ok'.
publish_cnam_update(Number) ->
    Features = knm_phone_number:features(Number),
    Notify = [{<<"Account-ID">>, knm_phone_number:assigned_to(Number)}
              ,{<<"Number-State">>, knm_phone_number:state(Number)}
              ,{<<"Local-Number">>, knm_phone_number:module_name(Number) =:= ?LOCAL_CARRIER}
              ,{<<"Number">>, knm_phone_number:number(Number)}
              ,{<<"Acquired-For">>, knm_phone_number:auth_by(Number)}
              ,{<<"Cnam">>, wh_json:get_value(<<"cnam">>, Features, wh_json:new())}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_cnam_request(Notify).
