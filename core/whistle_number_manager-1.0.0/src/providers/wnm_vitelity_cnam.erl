%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wnm_vitelity_cnam).

-export([save/1
         ,delete/1
        ]).

-include("../wnm.hrl").

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% produce notifications if the cnam object changes
%% @end
%%--------------------------------------------------------------------
-spec save(wnm_number()) -> wnm_number().
save(#number{state = <<"reserved">>} = Number) ->
    update_cnam_features(Number);
save(#number{state = <<"in_service">>} = Number) ->
    update_cnam_features(Number);
save(#number{state = <<"port_in">>} = Number) ->
    update_cnam_features(Number);
save(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted
%% @end
%%--------------------------------------------------------------------
-spec delete(wnm_number()) -> wnm_number().
delete(#number{features=Features}=N) ->
    N#number{features=remove_all_cnam_features(Features)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_cnam_features(wnm_number()) -> wnm_number().
update_cnam_features(#number{}=N) ->
    handle_outbound_cnam(N).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_outbound_cnam(wnm_number()) -> wnm_number().
handle_outbound_cnam(#number{current_number_doc=CurrentJObj
                             ,number_doc=JObj
                             ,features=Features
                            }=N) ->
    CurrentCNAM = wh_json:get_ne_value([<<"cnam">>, <<"display_name">>], CurrentJObj),
    case wh_json:get_ne_value([<<"cnam">>, <<"display_name">>], JObj) of
        'undefined' ->
            handle_inbound_cnam(N#number{features=sets:del_element(<<"outbound_cnam">>, Features)});
        CurrentCNAM ->
            handle_inbound_cnam(N#number{features=sets:add_element(<<"outbound_cnam">>, Features)});
        NewCNAM ->
            lager:debug("cnam display name changed to ~s, updating Vitelity", [NewCNAM]),
            N1 = try_update_outbound_cnam(N, NewCNAM),
            handle_inbound_cnam(N1)
    end.

-spec try_update_outbound_cnam(wnm_number(), ne_binary()) -> wnm_number().
try_update_outbound_cnam(#number{number=DID}=N, NewCNAM) ->
    case wnm_vitelity_util:query_vitelity(
           wnm_vitelity_util:build_uri(
             outbound_cnam_options(DID, NewCNAM)
            ))
    of
        {'ok', XML} ->
            process_outbound_xml_resp(N, XML);
        {'error', _E} ->
            wnm_number:error_provider_error(<<"failed to query provider">>, N)
    end.

-spec outbound_cnam_options(ne_binary(), ne_binary()) -> list().
outbound_cnam_options(DID, NewCNAM) ->
    [{'qs', [{'cmd', <<"lidb">>}
             ,{'did', DID}
             ,{'name', NewCNAM}
             ,{'xml', <<"yes">>}
             | wnm_vitelity_util:default_options()
            ]}
      ,{'uri', wnm_vitelity_util:default_options()}
     ].

-spec process_outbound_xml_resp(wnm_number(), text()) -> wnm_number().
process_outbound_xml_resp(N, XML) ->
    try xmerl_string:scan(XML) of
        {#xmlElement{name='content'
                     ,content=Children
                    }, _} ->
            process_outbound_resp(N, Children);
        _ ->
            wnm_number:error_provider_error(<<"unknown response format">>, N)
    catch
        _E:_R ->
            wnm_number:error_provider_error(<<"unknown response format">>, N)
    end.

-spec process_outbound_resp(wnm_number(), xml_els()) -> wnm_number().
process_outbound_resp(N, Children) ->
    case wnm_vitelity_util:xml_resp_status_msg(Children) of
        <<"ok">> ->
            check_outbound_response_tag(N, Children);
        <<"fail">> ->
            wnm_number:error_provider_error(wnm_vitelity_util:xml_resp_error_msg(Children), N)
    end.

-spec check_outbound_response_tag(wnm_number(), xml_els()) -> wnm_number().
check_outbound_response_tag(N, Children) ->
    case wnm_vitelity_util:xml_resp_response_msg(Children) of
        'undefined' ->
            wnm_number:error_provider_error(<<"response tag not found">>, N);
        <<"ok">> ->
            N1 = wnm_number:activate_feature(<<"outbound_cnam">>, N),
            _ = publish_cnam_update(N1),
            N1;
        Msg ->
            lager:debug("resp was not ok, was ~s", [Msg]),
            wnm_number:error_provider_error(Msg, N)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_inbound_cnam(wnm_number()) -> wnm_number().
handle_inbound_cnam(#number{number_doc=JObj}=N) ->
    case wh_json:is_true([<<"cnam">>, <<"inbound_lookup">>], JObj) of
        'false' -> remove_inbound_cnam(N);
        'true' -> add_inbound_cnam(N)
    end.

-spec remove_inbound_cnam(wnm_number()) -> wnm_number().
remove_inbound_cnam(#number{features=Features
                           ,number=Number
                          }=N) ->
    _ = wnm_vitelity_util:query_vitelity(
          wnm_vitelity_util:build_uri(
            remove_inbound_options(Number))
         ),
    N#number{features=sets:del_element(<<"inbound_cnam">>, Features)}.

-spec remove_inbound_options(ne_binary()) -> list().
remove_inbound_options(Number) ->
    [{'qs', [{'did', Number}
             ,{'cmd', <<"cnamdisable">>}
             ,{'xml', <<"yes">>}
             | wnm_vitelity_util:default_options()
            ]}
     ,{'uri', wnm_vitelity_util:api_uri()}
    ].

-spec add_inbound_cnam(wnm_number()) -> wnm_number().
add_inbound_cnam(#number{number=Number}=N) ->
    case wnm_vitelity_util:query_vitelity(
           wnm_vitelity_util:build_uri(
             inbound_options(Number))
          )
    of
        {'ok', XML} -> process_xml_resp(N, XML);
        {'error', _E} -> wnm_number:error_provider_fault(<<"unknown error">>, N)
    end.

-spec inbound_options(ne_binary()) -> list().
inbound_options(Number) ->
    [{'qs', [{'did', Number}
             ,{'cmd', <<"cnamenable">>}
             ,{'xml', <<"yes">>}
             | wnm_vitelity_util:default_options()
            ]}
     ,{'uri', wnm_vitelity_util:api_uri()}
    ].

-spec process_xml_resp(wnm_number(), text()) -> wnm_number().
process_xml_resp(N, XML) ->
    try xmerl_scan:string(XML) of
        {XmlEl, _} -> process_xml_content_tag(N, XmlEl)
    catch
        _E:_R ->
            lager:debug("failed to process XML: ~s: ~p", [_E, _R]),
            wnm_number:error_provider_fault(<<"invalid response from server">>, N)
    end.

-spec process_xml_content_tag(wnm_number(), xml_el()) -> wnm_number().
process_xml_content_tag(N, #xmlElement{name='content'
                                       ,content=Children
                                      }) ->
    Els = kz_xml:elements(Children),
    case wnm_vitelity_util:xml_resp_status_msg(Els) of
        <<"fail">> ->
            wnm_number:error_provider_fault(wnm_vitelity_util:xml_resp_error_msg(Els), N);
        <<"ok">> ->
            wnm_number:activate_feature(<<"inbound_cnam">>, N)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_all_cnam_features(set()) -> set().
remove_all_cnam_features(Features) ->
    Routines = [fun(F) -> sets:del_element(<<"inbound_cnam">>, F) end
                ,fun(F) -> sets:del_element(<<"outbound_cnam">>, F) end
                ,fun(F) -> sets:del_element(<<"cnam">>, F) end
               ],
    lists:foldl(fun(F, Feature) -> F(Feature) end, Features, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_cnam_update(wnm_number()) -> 'ok'.
publish_cnam_update(#number{number=Number
                            ,state=State
                            ,assigned_to=AssignedTo
                            ,module_name=ModuleName
                            ,auth_by=AuthBy
                            ,number_doc=JObj
                           }) ->
    Notify = [{<<"Account-ID">>, AssignedTo}
              ,{<<"Number-State">>, State}
              ,{<<"Local-Number">>, ModuleName =:= 'wnm_local'}
              ,{<<"Number">>, Number}
              ,{<<"Acquired-For">>, AuthBy}
              ,{<<"Cnam">>, wh_json:get_value(<<"cnam">>, JObj, wh_json:new())}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_cnam_request(Notify).
