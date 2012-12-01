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

init({_Transport, _Proto}, Req0, _Opts) ->
    put(callid, wh_util:rand_hex_binary(16)),
    case cowboy_http_req:path_info(Req0) of
        {[_, _, _]=PathTokens, Req1} ->
            is_authentic(PathTokens, Req1);
        {_Else, Req1} ->
            {ok, Req2} = cowboy_http_req:reply(404, Req1),
            {shutdown, Req2, ok}
    end.

is_authentic(PathTokens, Req0) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"proxy_store_authenticate">>, true) of
        false -> 
            lager:debug("store proxy does not require authentication", []),
            {ok, Req0, PathTokens};
        true ->
            maybe_basic_authentication(PathTokens, Req0)
    end.

maybe_basic_authentication(PathTokens, Req0) ->
    case credentials(Req0) of
        {undefined, undefined, Req1} ->
            lager:debug("proxy store request did not provide basic authentication", []),
            maybe_acl_authentication(PathTokens, Req1);        
        {Username, Password, Req1} ->
            maybe_basic_authentication(Username, Password, PathTokens, Req1)
    end.

maybe_basic_authentication(Username, Password, PathTokens, Req1) ->
    AuthUsername = whapps_config:get_binary(?CONFIG_CAT, <<"proxy_store_username">>, <<>>),
    AuthPassword = whapps_config:get_binary(?CONFIG_CAT, <<"proxy_store_password">>, <<>>),
    case (not wh_util:is_empty(AuthUsername)) andalso (not wh_util:is_empty(AuthPassword))
        andalso Username =:= AuthUsername andalso Password =:= AuthPassword 
    of
        true ->  {ok, Req1, PathTokens};
        false -> 
            lager:debug("request did not provide valid credentials", []),
            {shutdown, unauthorized(Req1), ok}
    end.    

maybe_acl_authentication(PathTokens, Req0) ->
    ACLs = whapps_config:get(?CONFIG_CAT, <<"proxy_store_acls">>, [<<"127.0.0.0/24">>]),
    {IpTuple, Req1} = cowboy_http_req:peer_addr(Req0),
    Ip = wh_network_utils:iptuple_to_binary(IpTuple),
    maybe_acl_authentication(ACLs, Ip, PathTokens, Req1).

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

credentials(Req0) ->    
    case cowboy_http_req:header('Authorization', Req0) of
        {undefined, Req1} ->
                {undefined, undefined, Req1};
        {Authorization, Req1} ->
            {Username, Password} = credentials_from_header(Authorization),
            {Username, Password, Req1}
    end.

credentials_from_header(AuthorizationHeader) ->
    case binary:split(AuthorizationHeader, <<$ >>) of
        [<<"Basic">>, EncodedCredentials] ->
            decoded_credentials(EncodedCredentials);
        _ ->
            {undefined, undefined}
    end.

decoded_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<$:>>) of
        [Username, Password] ->
            {Username, Password};
        _ ->
            {undefined, undefined}
    end.

unauthorized(Req0) ->    
    {ok, Req1} = cowboy_http_req:set_resp_header(<<"Www-Authenticate">>, <<"Basic realm=\"Kazoo Media Storage Proxy\"">>, Req0),
    {ok, Req2} = cowboy_http_req:set_resp_body(unauthorized_body(), Req1),
    {ok, Req3} = cowboy_http_req:reply(401, Req2),
    Req3.

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

handle(Req0, [Db, Id, Attachment]) ->
    is_appropriate_content_type(Db, Id, Attachment, Req0).

is_appropriate_content_type(Db, Id, Attachment, Req0) ->
    case cowboy_http_req:header('Content-Type', Req0) of
        {<<"audio/", _/binary>> = CT, Req1}->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Db, Id, Attachment, CT, Req1);
        {_CT, Req1} ->
            lager:debug("inappropriate content-type via headers: ~s", [_CT]),
            is_appropriate_extension(Db, Id, Attachment, Req1)
    end.
          
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

try_to_store(Db, Id, Attachment, CT, Req0) ->
    Conflicts = case get(conflicts) of undefined -> 0; Count -> Count end,
    DbName = wh_util:format_account_id(Db, encoded),
    {ok, Contents, Req1} = cowboy_http_req:body(Req0),    
    Options = [{content_type, wh_util:to_list(CT)}],
    case couch_mgr:put_attachment(DbName, Id, Attachment, Contents, Options) of
        {ok, _} ->
            lager:debug("successfully stored(~p) ~p ~p ~p", [CT, DbName, Id, Attachment]),
            {ok, Req2} = cowboy_http_req:reply(200, Req1),
            {ok, Req2, ok};
        {error, conflict} when Conflicts < 2 ->
            put(conflicts, Conflicts + 1),
            try_to_store(Db, Id, Attachment, CT, Req1);
        {error, _R} ->
            lager:debug("unable to store file: ~p", [_R]),
            {ok, Req2} = cowboy_http_req:reply(500, Req1),
            {ok, Req2, ok}
    end.

terminate(_Req, _State) ->
    ok.
