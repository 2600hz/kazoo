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
    case send_to_phonebook(Context) of
        'true' ->
            create_port_in(cb_context:doc(Context), cb_context:auth_token(Context)),
            Context;
        'false' -> Context
    end.

-spec maybe_add_comment(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
maybe_add_comment(Context, Comment) ->
    case send_to_phonebook(Context)
        andalso not req_from_phonebook(Context)
    of
        'true' ->
            add_comment(cb_context:doc(Context), cb_context:auth_token(Context), Comment),
            Context;
        'false' -> Context
    end.

-spec maybe_cancel_port_in(cb_context:context()) -> cb_context:context().
maybe_cancel_port_in(Context) ->
    case send_to_phonebook(Context) of
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

-spec send_to_phonebook(cb_context:context()) -> 'ok'.
send_to_phonebook(Context) ->
    cb_context:resp_status(Context) =:= 'success'
        andalso phonebook_enabled().

-spec create_port_in(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
create_port_in(JObj, AuthToken) ->
    send_create(JObj, AuthToken, kz_doc:account_id(JObj)).

-spec add_comment(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
add_comment(JObj, AuthToken, Comment) ->
    send_comment(Comment, AuthToken, kz_doc:account_id(JObj), kz_doc:id(JObj)).

-spec cancel_port_in(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
cancel_port_in(JObj, AuthToken) ->
    send_cancel(AuthToken, kz_doc:account_id(JObj), kz_doc:id(JObj)).


%%------------------------------------------------------------------------------
%% @doc Send phonebook request
%% @end
%%------------------------------------------------------------------------------
-spec send_create(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_create(JObj, AuthToken, AccountId) ->
    Url = phonebook_uri([<<"accounts">>, AccountId, <<"ports">>, <<"in">>]),
    lager:debug("creating port in request to phonebook via ~s: ~s", [Url, JObj]),
    handle_resp(send_req('put', Url, AuthToken, JObj), AccountId).

-spec send_comment(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_comment(Comment, AuthToken, AccountId, PortId) ->
    Url = phonebook_uri([<<"accounts">>, AccountId, <<"ports">>, <<"in">>, PortId, <<"notes">>]),
    lager:debug("adding comment to phonebook via ~s", [Url]),
    handle_resp(send_req('put', Url, AuthToken, Comment), AccountId).

-spec send_cancel(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_cancel(PortId, AuthToken, AccountId) ->
    Url = phonebook_uri([<<"accounts">>, AccountId, <<"ports">>, <<"in">>, PortId]),
    lager:debug("accounts delete via ~s", [Url]),
    handle_resp(send_req('delete', Url, AuthToken, 'undefined'), AccountId).

-spec send_req('put'|'delete', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_req('put', Url, AuthToken, JObj) ->
    Data = kz_json:encode(JObj),
    Headers = req_headers(AuthToken),
    kz_http:put(Url, Headers, Data);
send_req('delete', Url, AuthToken, _) ->
    Headers = req_headers(AuthToken),
    kz_http:delete(Url, Headers).

-spec handle_resp(kz_http:ret(), kz_term:ne_binary()) -> 'ok'.
handle_resp({'ok', 200, _, Resp}, _) ->
    lager:debug("phonebook success ~s", [Resp]);
handle_resp({'ok', Code, _, Resp}, AccountId) ->
    lager:warning("phonebook error ~p. ~s", [Code, Resp]),
    create_alert(kz_json:decode(Resp), AccountId);
handle_resp(_Error, _) ->
    lager:error("phonebook fatal error ~p", [_Error]).

-spec phonebook_uri(iolist()) -> iolist().
phonebook_uri(ExplodedPath) ->
    Url = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"phonebook_url">>),
    Uri = kz_util:uri(Url, ExplodedPath),
    kz_term:to_list(Uri).

-spec create_alert(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
create_alert(JObj, AccountId) ->
    Subject = <<"Phonebook Request Error">>,

    Msg = iolib:format(<<"Error while trying to handle port request for numbers ~p on account ~s in state ~s">>
                      ,[kzd_port_request:numbers(JObj)
                       ,AccountId
                       ,kzd_port_request:state(JObj)
                       ]
                      ),
    kz_notify:system_alert(Subject, Msg, []).

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
