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

send_to_phonebook(Context) ->
    cb_context:resp_status(Context) =:= 'success'
        andalso phonebook_enabled().

-spec create_port_in(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
create_port_in(JObj, AuthToken) ->
    send_req('create_port_in'
            ,JObj

            ,AuthToken
            ,kz_doc:account_id(JObj)
            ,'undefined'
            ).

-spec add_comment(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> 'ok'.
add_comment(JObj, AuthToken, Comment) ->
    send_req('comment'
            ,Comment
            ,AuthToken
            ,kz_doc:account_id(JObj)
            ,kz_doc:id(JObj)
            ).

-spec cancel_port_in(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
cancel_port_in(JObj, AuthToken) ->
    send_req('cancel_port_in'
            ,'undefined'
            ,AuthToken
            ,kz_doc:account_id(JObj)
            ,kz_doc:id(JObj)
            ).


%%------------------------------------------------------------------------------
%% @doc Send phonebook request
%% @end
%%------------------------------------------------------------------------------
-spec send_req(atom(), kz_term:api_object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) -> 'ok'.
send_req('create_port_in', JObj, AuthToken, AccountId, _) ->
    Data = kz_json:encode(JObj),
    Headers = req_headers(AuthToken),
    UrlString = req_uri('create_port_in', AccountId),
    lager:debug("creating port in request to phonebook via ~s: ~s", [UrlString, Data]),
    Resp = kz_http:put(UrlString, Headers, Data),
    handle_resp(Resp, AccountId);
send_req('comment', Comment, AuthToken, AccountId, PortId) ->
    Data = kz_json:encode(Comment),
    Headers = req_headers(AuthToken),
    UrlString = req_uri('comment', AccountId, PortId),
    lager:debug("adding comment to phonebook via ~s", [UrlString]),
    Resp = kz_http:put(UrlString, Headers, Data),
    handle_resp(Resp, AccountId);
send_req('cancel_port_in', _, AuthToken, AccountId, _) ->
    Headers = req_headers(AuthToken),
    UrlString = req_uri('accounts', AccountId),
    lager:debug("accounts delete via ~s", [UrlString]),
    Resp = kz_http:delete(UrlString, Headers),
    handle_resp(Resp, AccountId).

-spec req_uri('create_port_in', kz_term:ne_binary()) -> iolist().
req_uri('create_port_in', AccountId) ->
    phonebook_uri([<<"accounts">>, AccountId, <<"ports">>, <<"in">>]).

-spec req_uri('comment'|'cancel_port_in', kz_term:ne_binary(), kz_term:ne_binary()) -> iolist().
req_uri('comment', AccountId, PortId) ->
    phonebook_uri([<<"accounts">>, AccountId, <<"ports">>, <<"in">>, PortId, <<"notes">>]);
req_uri('cancel_port_in', AccountId, PortId) ->
    phonebook_uri([<<"accounts">>, AccountId, <<"ports">>, <<"in">>, PortId]).

-spec phonebook_uri(iolist()) -> iolist().
phonebook_uri(ExplodedPath) ->
    Url = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"phonebook_url">>),
    Uri = kz_util:uri(Url, ExplodedPath),
    binary:bin_to_list(Uri).

-spec handle_resp(kz_http:ret(), kz_term:ne_binary()) -> 'ok'.
handle_resp({'ok', 200, _, Resp}, _) ->
    lager:debug("phonebook success ~s", [Resp]);
handle_resp({'ok', Code, _, Resp}, AccountId) ->
    lager:warning("phonebook error ~p. ~s", [Code, Resp]),
    create_alert(kz_json:decode(Resp), AccountId);
handle_resp(_Error, _) ->
    lager:error("phonebook fatal error ~p", [_Error]).


create_alert(JObj, AccountId) ->
    Props = [{<<"metadata">>, JObj}
            ,{<<"category">>, <<"phonebook">>}
            ],

    From = [kz_json:from_list([{<<"type">>, <<"account">>}
                              ,{<<"value">>, AccountId}
                              ])
           ],

    To = [kz_json:from_list([{<<"type">>, AccountId}
                            ,{<<"value">>, <<"admins">>}
                            ])
         ,kz_json:from_list([{<<"type">>, kz_services_reseller:get_id(AccountId)}
                            ,{<<"value">>, <<"admins">>}
                            ])
         ],

    Title = <<"Phonebook Error">>,
    Msg = <<"Error trying to handle portin">>,
    {'ok', AlertJObj} = kapps_alert:create(Title, Msg, From, To, Props),
    {ok, _} = kapps_alert:save(AlertJObj),
    ok.

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
