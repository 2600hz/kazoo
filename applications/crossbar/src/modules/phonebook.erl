%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc Common functions for interacting with the 2600Hz phonebook service
%%% @author Sean Wysor
%%% @end
%%%-----------------------------------------------------------------------------
-module(phonebook).

-export([maybe_create_port_in/1
        ,maybe_add_comment/2
        ,maybe_cancel_port_in/1
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<"crossbar.phonebook">>).


%%------------------------------------------------------------------------------
%% @doc exported functions
%% @end
%%------------------------------------------------------------------------------

-spec maybe_create_port_in(cb_context:context()) -> cb_context:context().
maybe_create_port_in(Context) ->
    case is_phonebook_request(Context) of
        'true' ->
            create_port_in(cb_context:doc(Context), cb_context:auth_token(Context)),
            Context;
        'false' -> Context
    end.

-spec maybe_add_comment(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
maybe_add_comment(Context, Comment) ->
    case is_phonebook_request(Context)
        andalso not req_from_phonebook(Context)
    of
        'true' ->
            add_comment(cb_context:doc(Context), cb_context:auth_token(Context), Comment),
            Context;
        'false' -> Context
    end.

-spec maybe_cancel_port_in(cb_context:context()) -> cb_context:context().
maybe_cancel_port_in(Context) ->
    case is_phonebook_request(Context) of
        'true' ->
            cancel_port_in(cb_context:doc(Context), cb_context:auth_token(Context)),
            Context;
        'false' -> Context
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec phonebook_enabled() -> boolean().
phonebook_enabled() ->
    kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"enabled">>, 'false').

-spec req_from_phonebook(cb_context:context()) -> boolean().
req_from_phonebook(Context) ->
    case cb_context:req_header(Context, <<"User-Agent">>) of
        <<"phonebook">> -> 'true';
        _ -> 'false'
    end.

-spec is_phonebook_request(cb_context:context()) -> 'ok'.
is_phonebook_request(Context) ->
    cb_context:resp_status(Context) =:= 'success'
        andalso phonebook_enabled().

-spec create_port_in(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
create_port_in(JObj, AuthToken) ->
    Url = phonebook_uri([<<"accounts">>
                        ,kz_doc:account_id(JObj)
                        ,<<"ports">>
                        ,<<"in">>
                        ]),
    lager:debug("creating port in request to phonebook via ~s: ~p", [Url, JObj]),
    Response = kz_http:put(Url, req_headers(AuthToken), JObj),
    handle_resp(Response, JObj, <<"create">>).

-spec add_comment(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
add_comment(JObj, AuthToken, Comment) ->
    Url = phonebook_uri([<<"accounts">>
                        ,kz_doc:account_id(JObj)
                        ,<<"ports">>
                        ,<<"in">>
                        ,kz_doc:id(JObj)
                        ,<<"notes">>
                        ]),
    Data = kz_json:set_value(<<"data">>, Comment, kz_json:new()),
    lager:debug("adding comment to phonebook via ~s", [Url]),
    Response = kz_http:put(Url, req_headers(AuthToken), kz_json:encode(Data)),
    handle_resp(Response, JObj, <<"comment">>).

-spec cancel_port_in(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
cancel_port_in(JObj, AuthToken) ->
    AccountId = kz_doc:account_id(JObj),
    PortId = kz_doc:id(JObj),
    Url = phonebook_uri([<<"accounts">>, AccountId, <<"ports">>, <<"in">>, PortId]),
    lager:debug("accounts delete via ~s", [Url]),
    Response = kz_http:delete(Url, req_headers(AuthToken)),
    handle_resp(Response, JObj, <<"cancel">>).

-spec handle_resp(kz_http:ret(), kz_json:object(), kz_term:ne_binary()) -> 'ok' | {'error', any}.
handle_resp({'ok', 200, _, Resp}, _, _) ->
    lager:debug("phonebook success ~s", [Resp]);
handle_resp({'ok', Code, _, Resp}, JObj, Type) ->
    lager:warning("phonebook error ~b response: ~p", [Code, Resp]),
    create_alert(kz_json:decode(Resp), JObj, Type);
handle_resp({'error', Msg}, JObj, Type) ->
    lager:error("phonebook fatal error ~p", [Msg]),
    create_alert(Msg, JObj, Type);
handle_resp(Error, JObj, Type) ->
    lager:error("phonebook unknown error ~p", [Error]),
    create_alert(Error, JObj, Type).

-spec phonebook_uri(iolist()) -> iolist().
phonebook_uri(ExplodedPath) ->
    Url = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"phonebook_url">>),
    Uri = kz_util:uri(Url, ExplodedPath),
    kz_term:to_list(Uri).

-spec create_alert(any(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
create_alert(Response, JObj, Type) ->
    Subject = <<"Phonebook Request Error">>,
    Msg = <<"Error updating "
            "port request: ~s "
            "for numbers: ~p "
            "on account: ~s "
            "in state: ~s "
            "with response ~p"
          >>,
    Args = [Type
           ,kz_json:get_keys(kzd_port_requests:numbers(JObj))
           ,kz_doc:account_id(JObj)
           ,kzd_port_requests:port_state(JObj)
           ,Response
           ],
    kz_notify:system_alert(Subject, Msg, Args, []).

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
