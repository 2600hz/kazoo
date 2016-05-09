%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handles storage proxy requests for media binaries
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(kz_media_store_proxy).

-export([init/3
         ,terminate/3
         ,handle/2
        ]).

-include("kazoo_media.hrl").

-spec init({any(), any()}, cowboy_req:req(), kz_proplist()) ->
                  {'ok', cowboy_req:req(), ne_binaries()} |
                  {'shutdown',  cowboy_req:req(), 'ok'}.
init({_Transport, _Proto}, Req0, _Opts) ->
    kz_util:put_callid(kz_term:rand_hex_binary(16)),
    case cowboy_req:path_info(Req0) of
        {[_]=PathTokens, Req1} -> is_authentic(PathTokens, Req1);
        {[_, _, _, _, _]=PathTokens, Req1} -> is_authentic(PathTokens, Req1);
        {_Else, Req1} ->
            lager:debug("unexpected path: ~p", [_Else]),
            {'ok', Req2} = cowboy_req:reply(404, Req1),
            {'shutdown', Req2, 'ok'}
    end.

-spec is_authentic(ne_binaries(), cowboy_req:req()) ->
                          {'ok', cowboy_req:req(), ne_binaries()} |
                          {'shutdown',  cowboy_req:req(), 'ok'}.
is_authentic(PathTokens, Req0) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"proxy_store_authenticate">>, 'true') of
        'false' ->
            lager:debug("store proxy does not require authentication"),
            {'ok', Req0, PathTokens};
        'true' ->
            maybe_basic_authentication(PathTokens, Req0)
    end.

-spec maybe_basic_authentication(ne_binaries(), cowboy_req:req()) ->
                                        {'ok', cowboy_req:req(), ne_binaries()} |
                                        {'shutdown',  cowboy_req:req(), 'ok'}.
maybe_basic_authentication(PathTokens, Req0) ->
    case credentials(Req0) of
        {'undefined', 'undefined', Req1} ->
            lager:debug("proxy store request did not provide basic authentication", []),
            maybe_acl_authentication(PathTokens, Req1);
        {Username, Password, Req1} ->
            maybe_basic_authentication(Username, Password, PathTokens, Req1)
    end.

-spec maybe_basic_authentication(ne_binary(), ne_binary(), ne_binaries(), cowboy_req:req()) ->
                                        {'ok', cowboy_req:req(), ne_binaries()} |
                                        {'shutdown',  cowboy_req:req(), 'ok'}.
maybe_basic_authentication(Username, Password, PathTokens, Req1) ->
    AuthUsername = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_username">>, <<>>),
    AuthPassword = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_password">>, <<>>),
    case
        not kz_term:is_empty(AuthUsername) andalso
        not kz_term:is_empty(AuthPassword) andalso
        Username == AuthUsername andalso
        Password == AuthPassword
    of
        'true' ->  {'ok', Req1, PathTokens};
        'false' ->
            lager:debug("request did not provide valid credentials", []),
            {'shutdown', unauthorized(Req1), 'ok'}
    end.

-spec maybe_acl_authentication(ne_binaries(), cowboy_req:req()) ->
                                      {'ok', cowboy_req:req(), ne_binaries()} |
                                      {'shutdown',  cowboy_req:req(), 'ok'}.
maybe_acl_authentication(PathTokens, Req0) ->
    ACLs = kapps_config:get(?CONFIG_CAT, <<"proxy_store_acls">>, [<<"127.0.0.0/24">>]),
    {{IpTuple, _PeerPort}, Req1} = cowboy_req:peer(Req0),
    Ip = kz_network_utils:iptuple_to_binary(IpTuple),
    maybe_acl_authentication(ACLs, Ip, PathTokens, Req1).

-spec maybe_acl_authentication(ne_binaries(), ne_binary(), ne_binaries(), cowboy_req:req()) ->
                                      {'ok', cowboy_req:req(), ne_binaries()} |
                                      {'shutdown',  cowboy_req:req(), 'ok'}.
maybe_acl_authentication([], Ip, _, Req0) ->
    lager:debug("ip address ~s can not be authenticated via ACLs", [Ip]),
    {'shutdown', unauthorized(Req0), 'ok'};
maybe_acl_authentication([ACL|ACLs], Ip, PathTokens, Req0) ->
    case kz_network_utils:verify_cidr(Ip, ACL) of
        'true' ->
            lager:debug("ip address ~s was authenticated via ACLs", [Ip]),
            {'ok', Req0, PathTokens};
        'false' ->
            maybe_acl_authentication(ACLs, Ip, PathTokens, Req0)
    end.

-spec credentials(cowboy_req:req()) -> {api_binary(), api_binary(), cowboy_req:req()}.
credentials(Req0) ->
    case cowboy_req:header(<<"authorization">>, Req0) of
        {'undefined', Req1} ->
                {'undefined', 'undefined', Req1};
        {Authorization, Req1} ->
            {Username, Password} = credentials_from_header(Authorization),
            {Username, Password, Req1}
    end.

-spec credentials_from_header(ne_binary()) -> {api_binary(), api_binary()}.
credentials_from_header(AuthorizationHeader) ->
    case binary:split(AuthorizationHeader, <<$\s>>) of
        [<<"Basic">>, EncodedCredentials] ->
            decoded_credentials(EncodedCredentials);
        _ ->
            {'undefined', 'undefined'}
    end.

-spec decoded_credentials(ne_binary()) -> {api_binary(), api_binary()}.
decoded_credentials(EncodedCredentials) ->
    DecodedCredentials = base64:decode(EncodedCredentials),
    case binary:split(DecodedCredentials, <<$:>>) of
        [Username, Password] ->
            {Username, Password};
        _ ->
            {'undefined', 'undefined'}
    end.

-spec unauthorized(cowboy_req:req()) -> cowboy_req:req().
unauthorized(Req0) ->
    Req1 = cowboy_req:set_resp_header(<<"Www-Authenticate">>, <<"Basic realm=\"Kazoo Media Storage Proxy\"">>, Req0),
    Req2 = cowboy_req:set_resp_body(unauthorized_body(), Req1),
    {'ok', Req3} = cowboy_req:reply(401, Req2),
    Req3.

-spec unauthorized_body() -> ne_binary().
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

-spec handle(cowboy_req:req(), ne_binaries()) ->
                    {'ok', cowboy_req:req(), 'ok'}.
handle(Req0, [Url, _AName]) ->
   {Db, Id, Attachment, Options} = binary_to_term(base64:decode(Url)),
    Path = #media_store_path{db = Db
                            ,id = Id
                            ,att = Attachment
                            ,opt = Options
                            },
    is_appropriate_content_type(Path, Req0).

-spec is_appropriate_content_type(media_store_path(), cowboy_req:req()) ->
                                         {'ok', cowboy_req:req(), 'ok'}.
is_appropriate_content_type(Path, Req0) ->
    case cowboy_req:header(<<"content-type">>, Req0) of
        {<<"audio/", _/binary>> = CT, Req1}->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Path, CT, Req1);
        {<<"video/", _/binary>> = CT, Req1}->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Path, CT, Req1);
        {<<"image/", _/binary>> = CT, Req1}->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Path, CT, Req1);
        {_CT, Req1} ->
            lager:debug("inappropriate content-type via headers: ~s", [_CT]),
            is_appropriate_extension(Path, Req1)
    end.

-spec is_appropriate_extension(media_store_path(), cowboy_req:req()) ->
                                      {'ok', cowboy_req:req(), 'ok'}.
is_appropriate_extension(#media_store_path{att=Attachment}=Path, Req0) ->
    Extension = filename:extension(Attachment),
    case kz_mime:from_extension(Extension) of
        <<"audio/", _/binary>> = CT->
            lager:debug("found content-type via extension: ~s", [CT]),
            try_to_store(Path, CT, Req0);
        <<"video/", _/binary>> = CT->
            lager:debug("found content-type via extension: ~s", [CT]),
            try_to_store(Path, CT, Req0);
        <<"image/", _/binary>> = CT->
            lager:debug("found content-type via extension: ~s", [CT]),
            try_to_store(Path, CT, Req0);
        _CT ->
            lager:debug("inappropriate content-type via extension: ~s", [_CT]),
            {'ok', Req1} = cowboy_req:reply(415, Req0),
            {'ok', Req1, 'ok'}
    end.

-spec ensure_extension_present(media_store_path(), ne_binary(), cowboy_req:req()) ->
                                      {'ok', cowboy_req:req(), 'ok'}.
ensure_extension_present(#media_store_path{att=Attachment}=Path, CT, Req0) ->
    case kz_term:is_empty(filename:extension(Attachment))
        andalso kz_mime:to_extension(CT)
    of
        'false' ->
            try_to_store(Path, CT, Req0);
        ?NE_BINARY = Extension ->
            try_to_store(Path#media_store_path{att= <<Attachment/binary, ".", Extension/binary>>}, CT, Req0);
        _Else ->
            lager:debug("unable to correct missing extension for content-type: ~s", [CT]),
            {'ok', Req1} = cowboy_req:reply(400, Req0),
            {'ok', Req1, 'ok'}
    end.

-spec try_to_store(media_store_path(), ne_binary(), cowboy_req:req()) ->
                          {'ok', cowboy_req:req(), 'ok'}.
try_to_store(#media_store_path{db=Db
                              ,id=Id
                              ,att=Attachment
                              ,opt=Opts
                              }, CT, Req0) ->
    {'ok', Contents, Req1} = cowboy_req:body(Req0),
    Options = [{'content_type', kz_term:to_list(CT)}
               ,{'content_length', byte_size(Contents)}
               | Opts
              ],
    lager:debug("putting ~s onto ~s(~s): ~s", [Attachment, Id, Db, CT]),
    case kz_datamgr:put_attachment(Db, Id, Attachment, Contents, Options) of
        {'ok', JObj} ->
            lager:debug("successfully stored(~p) ~p ~p ~p", [CT, Db, Id, Attachment]),
            {'ok', success(JObj, Req1), 'ok'};
        {'error', Reason} ->
            lager:debug("unable to store file: ~p", [Reason]),
            {'ok', failure(Reason, Req1), 'ok'}
    end.

-spec success(kz_json:object(), cowboy_req:req()) -> cowboy_req:req().
success(JObj, Req0) ->
    Body = io_lib:format("~s~n", [kz_json:encode(kz_json:set_value(<<"ok">>, 'true', JObj))]),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    {'ok', Req2} = cowboy_req:reply(200, Req1),
    Req2.

-spec failure(any(), cowboy_req:req()) -> cowboy_req:req().
failure(Reason, Req0) ->
    Body = io_lib:format("~p~n", [Reason]),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    {'ok', Req2} = cowboy_req:reply(500, Req1),
    Req2.

-spec terminate(any(), cowboy_req:req(), any()) -> cowboy_req:req().
terminate(_Reason, Req, _State) ->
    lager:debug("terminating store proxy: ~p", [_Reason]),
    Req.
