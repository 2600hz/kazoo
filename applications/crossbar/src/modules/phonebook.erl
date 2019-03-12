%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Common functions for interacting with the 2600Hz phonebook service
%%% @author Sean Wysor
%%% @end
%%%-----------------------------------------------------------------------------
-module(phonebook).

-export([maybe_create_port_in/1
        ,maybe_add_comment/2
        ,maybe_cancel_port_in/1

        ,should_send_to_phonebook/1
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_port_request.hrl").

-define(MOD_CONFIG_CAT, <<"crossbar.phonebook">>).

%%------------------------------------------------------------------------------
%% @doc exported functions
%% @end
%%------------------------------------------------------------------------------

-spec maybe_create_port_in(cb_context:context()) -> {'ok', kz_json:object() | 'disabled'} | {'error', kz_term:ne_binary() | {integer(), kz_json:object()}}.
maybe_create_port_in(Context) ->
    case should_send_to_phonebook(Context) of
        'true' ->
            create_port_in(cb_context:doc(Context), cb_context:auth_token(Context));
        'false' -> {'ok', 'disabled'}
    end.

-spec maybe_add_comment(cb_context:context(), kz_json:objects()) -> {'ok', kz_json:object() | 'disabled'} | {'error', kz_term:ne_binary() | {integer(), kz_json:object()}}.
maybe_add_comment(Context, Comment) ->
    case should_send_to_phonebook(Context) of
        'true' ->
            add_comment(cb_context:doc(Context), cb_context:auth_token(Context), Comment);
        'false' -> {'ok', 'disabled'}
    end.

-spec maybe_cancel_port_in(cb_context:context()) -> {'ok', kz_json:object() | 'disabled'} | {'error', kz_term:ne_binary() | {integer(), kz_json:object()}}.
maybe_cancel_port_in(Context) ->
    case should_send_to_phonebook(Context) of
        'true' ->
            %%TODO: implement support for this in phonebook
            %% cancel_port_in(cb_context:doc(Context), cb_context:auth_token(Context));
            %%UNTIL THEN: just return disabled
            {'ok', 'disabled'};
        'false' -> {'ok', 'disabled'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_send_to_phonebook(cb_context:context()) -> boolean().
should_send_to_phonebook(Context) ->
    MasterId = case kapps_util:get_master_account_id() of
                   {'ok', Id} -> Id;
                   {'error', _} ->
                       cb_context:fetch(Context, 'port_authority_id')
               end,
    phonebook_enabled()
        andalso cb_context:fetch(Context, 'port_authority_id', MasterId) =:= MasterId
        andalso not req_from_phonebook(Context).

-spec phonebook_enabled() -> boolean().
phonebook_enabled() ->
    kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enabled">>, 'false').

-spec req_from_phonebook(cb_context:context()) -> boolean().
req_from_phonebook(Context) ->
    case cb_context:req_header(Context, <<"User-Agent">>) of
        <<"phonebook">> -> 'true';
        _ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_port_in(kz_json:object(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', kz_term:ne_binary() | {integer(), kz_json:object()}}.
create_port_in(JObj, AuthToken) ->
    Url = phonebook_uri([<<"accounts">>
                        ,kz_doc:account_id(JObj)
                        ,<<"ports">>
                        ,<<"in">>
                        ]),
    Data = kz_json:set_value(<<"data">>, kz_doc:public_fields(JObj), kz_json:new()),
    lager:debug("creating port in request to phonebook via ~s: ~p", [Url, Data]),
    Response = kz_http:put(Url, req_headers(AuthToken), kz_json:encode(Data)),
    handle_resp(Response, JObj, <<"create">>, Url).

-spec add_comment(kz_json:object(), kz_term:ne_binary(), kz_json:objects()) -> {'ok', kz_json:object()} | {'error', kz_term:ne_binary() | {integer(), kz_json:object()}}.
add_comment(JObj, AuthToken, Comment) ->
    Url = phonebook_uri([<<"accounts">>
                        ,kz_doc:account_id(JObj)
                        ,<<"ports">>
                        ,<<"in">>
                        ,kz_doc:id(JObj)
                        ,<<"notes">>
                        ]),
    Data = kz_json:set_value(<<"data">>, Comment, kz_json:new()),
    lager:debug("adding comment to phonebook via ~s: ~p", [Url, Data]),
    Response = kz_http:put(Url, req_headers(AuthToken), kz_json:encode(Data)),
    handle_resp(Response, JObj, <<"comment">>, Url).

%% implement support for this in phonebook
%% -spec cancel_port_in(kz_json:object(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', kz_term:ne_binary() | kz_json:object()}.
%% cancel_port_in(JObj, AuthToken) ->
%%     AccountId = kz_doc:account_id(JObj),
%%     PortId = kz_doc:id(JObj),
%%     Url = phonebook_uri([<<"accounts">>, AccountId, <<"ports">>, <<"in">>, PortId]),
%%     lager:debug("accounts delete via ~s", [Url]),
%%     Response = kz_http:delete(Url, req_headers(AuthToken)),
%%     handle_resp(Response, JObj, <<"cancel">>, Url).

-spec handle_resp(kz_http:ret(), kz_json:object(), kz_term:ne_binary(), string()) -> {'ok', kz_json:object()} | {'error', kz_term:ne_binary() | {integer(), kz_json:object()}}.
handle_resp({'ok', 200, _, Resp}, _, _, _) ->
    RespJObj = kz_json:decode(Resp),
    case kz_json:get_ne_binary_value(<<"status">>, RespJObj) of
        'undefined' -> {'error', <<"invalid response from phonebook">>};
        <<"error">> -> {'error', RespJObj};
        <<"success">> -> {'ok', RespJObj}
    end;
handle_resp({'ok', Code, _, Resp}, JObj, Type, Url) ->
    lager:warning("phonebook error ~b response: ~p", [Code, Resp]),
    Response = kz_json:decode(Resp),
    Message = kz_json:get_ne_binary_value(<<"message">>, Response, <<"unknown error">>),
    Data = kz_json:get_json_value(<<"data">>, Response, kz_json:new()),
    Prop = props:filter_empty(
             [{<<"http_code">>, Code}
             ,{<<"error_message">>, Message}
             ,{<<"request_id">>, kz_json:get_value(<<"request_id">>, Response)}
             ,{<<"response_code">>, kz_json:get_value(<<"code">>, Response)}
             ,{<<"phonebook_url">>, kz_term:to_binary(Url)}
              | kz_json:recursive_to_proplist(Data)
             ]),
    create_alert(Prop, JObj, Type),
    {'error', {Code, Response}};
handle_resp({'error', {'connect_failed', _}}, JObj, Type, Url) ->
    lager:error("connection failed to phonebook url ~s", [Url]),
    Prop = [{<<"error_message">>, <<"connection failed to phonebook">>}
           ,{<<"phonebook_url">>, kz_term:to_binary(Url)}
           ],
    create_alert(Prop, JObj, Type),
    {'error', <<"failed to connect to phonebook">>};
handle_resp({'error', {'malformed_url', _}}, JObj, Type, Url) ->
    lager:error("phonebook fatal error malformed_url ~s", [Url]),
    Prop = [{<<"error_message">>, <<"malformed_url">>}
           ,{<<"phonebook_url">>, kz_term:to_binary(Url)}
           ],
    create_alert(Prop, JObj, Type),
    {'error', <<"malformed_url">>};
handle_resp(Error, JObj, Type, Url) ->
    lager:error("phonebook unknown error ~p", [Error]),
    Message = kz_term:to_binary(io_lib:format("~p", [Error])),
    Prop = [{<<"error_message">>, Message}
           ,{<<"phonebook_url">>, kz_term:to_binary(Url)}
           ],
    create_alert(Prop, JObj, Type),
    {'error', Message}.


-spec phonebook_uri(iolist()) -> string().
phonebook_uri(ExplodedPath) ->
    Url = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"phonebook_url">>),
    Uri = kz_util:uri(Url, ExplodedPath),
    kz_term:to_list(Uri).

-spec create_alert(kz_term:proplist(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
create_alert(Response, JObj, Type) ->
    Subject = <<"Phonebook request failed">>,
    Msg = kz_term:to_binary(io_lib:format("Unable to submit port update to phonebook for port: ~s"
                                         ,[kzd_port_requests:name(JObj)])
                           ),
    Props = [{<<"request_type">>, Type}
            ,{<<"port_id">>, kz_doc:id(JObj)}
            ,{<<"phone_numbers">>, kz_json:get_keys(kzd_port_requests:numbers(JObj))}
            ,{<<"account_id">>, kz_doc:account_id(JObj)}
            ,{<<"port_state">>, kz_json:get_ne_binary_value(?PORT_PVT_STATE, JObj)}
             | Response
            ],
    kz_notify:detailed_alert(Subject, Msg, Props, []).

-spec req_headers(kz_term:ne_binary()) -> kz_term:proplist().
req_headers(Token) ->
    props:filter_undefined(
      [{"Content-Type", "application/json"}
      ,{"X-Auth-Token", kz_term:to_list(Token)}
      ,{"X-Kazoo-Cluster-ID", get_cluster_id()}
      ,{"User-Agent", kz_term:to_list(erlang:node())}
      ]).

-spec get_cluster_id() -> nonempty_string().
get_cluster_id() ->
    case kapps_config:get_string(?MOD_CONFIG_CAT, <<"cluster_id">>) of
        'undefined' ->
            ClusterId = kz_binary:rand_hex(16),
            {'ok', _JObj} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"cluster_id">>, ClusterId),
            kz_term:to_list(ClusterId);
        ClusterId -> ClusterId
    end.
