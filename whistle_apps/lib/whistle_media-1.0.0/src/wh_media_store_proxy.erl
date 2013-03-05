%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handles storage proxy requests for media binaries
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(wh_media_store_proxy).

-export([init/3
         ,terminate/2
         ,handle/2
        ]).

-include("whistle_media.hrl").

-spec init/3 :: ({_, _}, #http_req{}, wh_proplist()) ->
                        {'ok', #http_req{}, ne_binaries()} |
                        {'shutdown',  #http_req{}, 'ok'}.
init({_Transport, _Proto}, Req0, _Opts) ->
    put(callid, wh_util:rand_hex_binary(16)),
    case cowboy_http_req:path_info(Req0) of
        {[_, _, _]=PathTokens, Req1} ->
            is_authentic(PathTokens, Req1);
        {_Else, Req1} ->
            {ok, Req2} = cowboy_http_req:reply(404, Req1),
            {shutdown, Req2, ok}
    end.

-spec is_authentic/2 :: (ne_binaries(), #http_req{}) ->
                                {'ok', #http_req{}, ne_binaries()} |
                                {'shutdown',  #http_req{}, 'ok'}.
is_authentic(PathTokens, Req0) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"proxy_store_authenticate">>, true) of
        false -> 
            lager:debug("store proxy does not require authentication", []),
            {ok, Req0, PathTokens};
        true ->
            maybe_basic_authentication(PathTokens, Req0)
    end.

-spec maybe_basic_authentication/2 :: (ne_binaries(), #http_req{}) ->
                                              {'ok', #http_req{}, ne_binaries()} |
                                              {'shutdown',  #http_req{}, 'ok'}.
maybe_basic_authentication(PathTokens, Req0) ->
    case credentials(Req0) of
        {undefined, undefined, Req1} ->
            lager:debug("proxy store request did not provide basic authentication", []),
            maybe_acl_authentication(PathTokens, Req1);        
        {Username, Password, Req1} ->
            maybe_basic_authentication(Username, Password, PathTokens, Req1)
    end.

-spec maybe_basic_authentication/4 :: (ne_binary(), ne_binary(), ne_binaries(), #http_req{}) ->
                                              {'ok', #http_req{}, ne_binaries()} |
                                              {'shutdown',  #http_req{}, 'ok'}.
maybe_basic_authentication(Username, Password, PathTokens, Req1) ->
    AuthUsername = whapps_config:get_binary(?CONFIG_CAT, <<"proxy_username">>, <<>>),
    AuthPassword = whapps_config:get_binary(?CONFIG_CAT, <<"proxy_password">>, <<>>),
    case (not wh_util:is_empty(AuthUsername)) andalso (not wh_util:is_empty(AuthPassword))
        andalso Username =:= AuthUsername andalso Password =:= AuthPassword 
    of
        true ->  {ok, Req1, PathTokens};
        false -> 
            lager:debug("request did not provide valid credentials", []),
            {shutdown, unauthorized(Req1), ok}
    end.    

-spec maybe_acl_authentication/2 :: (ne_binaries(), #http_req{}) ->
                                            {'ok', #http_req{}, ne_binaries()} |
                                            {'shutdown',  #http_req{}, 'ok'}.
maybe_acl_authentication(PathTokens, Req0) ->
    ACLs = whapps_config:get(?CONFIG_CAT, <<"proxy_store_acls">>, [<<"127.0.0.0/24">>]),
    {IpTuple, Req1} = cowboy_http_req:peer_addr(Req0),
    Ip = wh_network_utils:iptuple_to_binary(IpTuple),
    maybe_acl_authentication(ACLs, Ip, PathTokens, Req1).

-spec maybe_acl_authentication/4 :: (ne_binaries(), ne_binary(), ne_binaries(), #http_req{}) ->
                                            {'ok', #http_req{}, ne_binaries()} |
                                            {'shutdown',  #http_req{}, 'ok'}.
maybe_acl_authentication([], Ip, _, Req0) ->
    lager:debug("ip address ~s can not be authenticated via ACLs", [Ip]),
    {shutdown, unauthorized(Req0), ok};
maybe_acl_authentication([ACL|ACLs], Ip, PathTokens, Req0) ->
    case wh_network_utils:verify_cidr(Ip, ACL) of
        true -> 
            lager:debug("ip address ~s was authenticated via ACLs", [Ip]),
            {ok, Req0, PathTokens};
        false ->
            maybe_acl_authentication(ACLs, Ip, PathTokens, Req0)
    end.

-spec credentials/1 :: (#http_req{}) -> {api_binary(), api_binary(), #http_req{}}.
credentials(Req0) ->    
    case cowboy_http_req:header('Authorization', Req0) of
        {undefined, Req1} ->
                {undefined, undefined, Req1};
        {Authorization, Req1} ->
            {Username, Password} = credentials_from_header(Authorization),
            {Username, Password, Req1}
    end.

-spec credentials_from_header/1 :: (ne_binary()) -> {api_binary(), api_binary()}.
credentials_from_header(AuthorizationHeader) ->
    case binary:split(AuthorizationHeader, <<$ >>) of
        [<<"Basic">>, EncodedCredentials] ->
            decoded_credentials(EncodedCredentials);
        _ ->
            {undefined, undefined}
    end.

-spec decoded_credentials/1 :: (ne_binary()) -> {api_binary(), api_binary()}.
decoded_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<$:>>) of
        [Username, Password] ->
            {Username, Password};
        _ ->
            {undefined, undefined}
    end.

-spec unauthorized/1 :: (#http_req{}) -> #http_req{}.
unauthorized(Req0) ->    
    {ok, Req1} = cowboy_http_req:set_resp_header(<<"Www-Authenticate">>, <<"Basic realm=\"Kazoo Media Storage Proxy\"">>, Req0),
    {ok, Req2} = cowboy_http_req:set_resp_body(unauthorized_body(), Req1),
    {ok, Req3} = cowboy_http_req:reply(401, Req2),
    Req3.

-spec unauthorized_body/0 :: () -> ne_binary().
unauthorized_body() ->
    <<"
    <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
     \"http://www.w3.org/TR/1999/REC-html401-19991224/loose.dt\">
    <HTML>
      <HEAD>
        <TITLE>Error</TITLE>
        <META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\">
      </HEAD>
      <BODY><H1>401 Unauthorized.</H1></BODY>
    </HTML>
    ">>.

-spec handle/2 :: (#http_req{}, ne_binaries()) -> {'ok', #http_req{}, 'ok'}.
handle(Req0, [Db, Id, Attachment]) ->
    is_appropriate_content_type(Db, Id, Attachment, Req0).

-spec is_appropriate_content_type/4 :: (ne_binary(), ne_binary(), ne_binary(), #http_req{}) ->
                                               {'ok', #http_req{}, 'ok'}.
is_appropriate_content_type(Db, Id, Attachment, Req0) ->
    case cowboy_http_req:header('Content-Type', Req0) of
        {<<"audio/", _/binary>> = CT, Req1}->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Db, Id, Attachment, CT, Req1);
        {_CT, Req1} ->
            lager:debug("inappropriate content-type via headers: ~s", [_CT]),
            is_appropriate_extension(Db, Id, Attachment, Req1)
    end.

-spec is_appropriate_extension/4 :: (ne_binary(), ne_binary(), ne_binary(), #http_req{}) -> {'ok', #http_req{}, 'ok'}.  
is_appropriate_extension(Db, Id, Attachment, Req0) ->
    Extension = filename:extension(Attachment),
    case wh_mime_types:from_extension(Extension) of
        <<"audio/", _/binary>> = CT->
            lager:debug("found content-type via extension: ~s", [CT]),
            try_to_store(Db, Id, Attachment, CT, Req0);
        _CT ->
            lager:debug("inappropriate content-type via extension: ~s", [_CT]),
            {ok, Req1} = cowboy_http_req:reply(415, Req0),
            {ok, Req1, ok}
    end.

-spec ensure_extension_present/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), #http_req{}) ->
                                            {'ok', #http_req{}, 'ok'}.
ensure_extension_present(Db, Id, Attachment, CT, Req0) ->
    case wh_util:is_empty(filename:extension(Attachment))
        andalso wh_mime_types:to_extension(CT)
    of
        false ->
            try_to_store(Db, Id, Attachment, CT, Req0);
        ?NE_BINARY = Extension ->
            try_to_store(Db, Id, <<Attachment/binary, ".", Extension/binary>>, CT, Req0);
         _Else ->
            lager:debug("unable to correct missing extension for content-type: ~s", [CT]),
            {ok, Req1} = cowboy_http_req:reply(400, Req0),
            {ok, Req1, ok}
    end.

-spec try_to_store/5 :: (ne_binary(), ne_binary(), ne_binary(), ne_binary(), #http_req{}) ->
                                {'ok', #http_req{}, 'ok'}.
try_to_store(Db, Id, Attachment, CT, Req0) ->
    DbName = wh_util:format_account_id(Db, encoded),
    {ok, Contents, Req1} = cowboy_http_req:body(Req0),
    Options = [{content_type, wh_util:to_list(CT)}
               ,{content_length, byte_size(Contents)}
              ],
    lager:debug("putting ~s onto ~s(~s): ~s", [Attachment, Id, DbName, CT]),
    case couch_mgr:put_attachment(DbName, Id, Attachment, Contents, Options) of
        {ok, JObj} ->
            lager:debug("successfully stored(~p) ~p ~p ~p", [CT, DbName, Id, Attachment]),
            {ok, success(JObj, Req1), ok};
        {error, conflict} ->
            maybe_resolve_conflict(DbName, Id, Attachment, Contents, Options, Req1);
        {error, Reason} ->
            lager:debug("unable to store file: ~p", [Reason]),
            {ok, failure(Reason, Req1), ok}
    end.

-spec maybe_resolve_conflict(ne_binary(), ne_binary(), ne_binary(), binary(), wh_proplist(), #http_req{}) ->
                                    {'ok', #http_req{}, 'ok'}.
maybe_resolve_conflict(DbName, Id, Attachment, Contents, Options, Req0) ->
    timer:sleep(5000),
    lager:debug("putting ~s onto ~s(~s): ~-800p", [Attachment, Id, DbName, Options]),
    case couch_mgr:put_attachment(DbName, Id, Attachment, Contents, Options) of
        {ok, JObj} ->
            lager:debug("successfully stored ~p ~p ~p", [DbName, Id, Attachment]),
            {ok, success(JObj, Req0), ok};
        {error, conflict} ->
            maybe_resolve_conflict(DbName, Id, Attachment, Contents, Options, Req0);
        {error, Reason} ->
            lager:debug("unable to store file: ~p", [Reason]),
            {ok, failure(Reason, Req0), ok}
    end.

-spec success/2 :: (wh_json:object(), #http_req{}) -> #http_req{}.
success(JObj, Req0) ->
    Body = io_lib:format("~s~n", [wh_json:encode(wh_json:set_value(<<"ok">>, true, JObj))]),
    {ok, Req1} = cowboy_http_req:set_resp_body(Body, Req0),
    {ok, Req2} = cowboy_http_req:reply(200, Req1),
    Req2.

-spec failure/2 :: (term(), #http_req{}) -> #http_req{}.
failure(Reason, Req0) ->
    Body = io_lib:format("~p~n", [Reason]),
    {ok, Req1} = cowboy_http_req:set_resp_body(Body, Req0),
    {ok, Req2} = cowboy_http_req:reply(500, Req1),
    Req2.

-spec terminate/2 :: (#http_req{}, _) -> 'ok'.
terminate(_Req, _State) ->
    ok.
