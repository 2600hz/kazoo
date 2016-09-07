%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
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

-record(state, {media :: media_store_path()
               ,filename :: binary()
               ,file :: file:io_device()
               }).

-type state() :: #state{}.

-type body() :: {'ok', binary(), cowboy_req:req()} | {'more', binary(), cowboy_req:req()}.
-type validate_request_ret() :: {'ok', media_store_path()} | {'error', integer()}.
-type handler_return() :: {'ok', cowboy_req:req(), state()} | {'shutdown',  cowboy_req:req(), 'ok'}.

-spec init({any(), any()}, cowboy_req:req(), kz_proplist()) -> handler_return().
init({_Transport, _Proto}, Req, _Opts) ->
    kz_util:put_callid(kz_util:rand_hex_binary(16)),
    case authenticate(Req) of
        'true' ->
            validate_request(cowboy_req:path_info(Req));
        'false' ->
            lager:debug("request did not provide valid credentials"),
            {'shutdown', unauthorized(Req), 'ok'}
    end.

-spec authenticate(cowboy_req:req()) -> boolean().
authenticate(Req) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"proxy_store_authenticate">>, 'true') of
        'false' -> 'true';
        'true' -> maybe_basic_authentication(Req)
    end.

-spec maybe_basic_authentication(cowboy_req:req()) -> boolean().
maybe_basic_authentication(Req) ->
    case credentials(Req) of
        {'undefined', 'undefined', Req1} ->
            lager:debug("proxy store request did not provide basic authentication", []),
            maybe_acl_authentication(Req1);
        {Username, Password, _Req1} ->
            maybe_basic_authentication(Username, Password)
    end.

-spec maybe_basic_authentication(ne_binary(), ne_binary()) -> boolean().
maybe_basic_authentication(Username, Password) ->
    AuthUsername = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_username">>, <<>>),
    AuthPassword = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_password">>, <<>>),
    not kz_util:is_empty(AuthUsername)
        andalso not kz_util:is_empty(AuthPassword)
        andalso Username == AuthUsername
        andalso Password == AuthPassword.

-spec maybe_acl_authentication(cowboy_req:req()) -> boolean().
maybe_acl_authentication(Req) ->
    ACLs = kapps_config:get(?CONFIG_CAT, <<"proxy_store_acls">>, [<<"127.0.0.0/24">>]),
    {{IpTuple, _PeerPort}, _Req1} = cowboy_req:peer(Req),
    Ip = kz_network_utils:iptuple_to_binary(IpTuple),
    maybe_acl_authentication(ACLs, Ip).

-spec maybe_acl_authentication(ne_binaries(), ne_binary()) -> boolean().
maybe_acl_authentication([], Ip) ->
    lager:debug("ip address ~s can not be authenticated via ACLs", [Ip]),
    'false';
maybe_acl_authentication([ACL|ACLs], Ip) ->
    case kz_network_utils:verify_cidr(Ip, ACL) of
        'true' ->
            lager:debug("ip address ~s was authenticated via ACLs", [Ip]),
            'true';
        'false' ->
            maybe_acl_authentication(ACLs, Ip)
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

-spec handle(cowboy_req:req(), state()) -> handler_return().
handle(Req, State) ->
    case cowboy_req:body(Req) of
        {'error', Reason} -> handle_error(Reason, Req, State);
        Body -> handle_body(Body, State)
    end.

-spec handle_body(body(), state()) -> handler_return().
handle_body({'more', Contents, Req}, #state{file=Device}=State) ->
    case file:write(Device, Contents) of
        'ok' -> handle(Req, State);
        {'error', Reason} -> failure(Reason, Req)
    end;
handle_body({'ok', Contents, Req}, #state{file=Device}=State) ->
    case file:write(Device, Contents) of
        'ok' -> handle_close(Req, State, file:close(Device));
        {'error', Reason} -> failure(Reason, Req)
    end.

handle_close(Req, State, 'ok') ->
    store(State, Req);
handle_close(Req, _State, {'error', Reason}) ->
    failure(Reason, Req).

handle_error(Reason, Req, #state{file=Device}=State) ->
    _ = file:close(Device),
    lager:debug("received error ~p requesting for body", [Reason]),
    reply_error(500, State, Req).

-spec validate_request({list(), cowboy_req:req()}) -> handler_return().
validate_request({[EncodedUrl, _Filename], Req}) ->
    case decode_url(EncodedUrl) of
        'error' -> reply_error(404, Req);
        Path -> validate_request(Path, Req)
    end;
validate_request({_Else , Req}) ->
    lager:debug("unexpected path: ~p", [_Else]),
    reply_error(404, Req).

-spec validate_request(media_store_path(), cowboy_req:req()) -> handler_return().
validate_request(Path, Req) ->
    case is_appropriate_content_type(Path, Req) of
        {'ok', NewPath} -> setup_context(NewPath, Req);
        {'error', Code} -> reply_error(Code, Req)
    end.

setup_context(#media_store_path{att=Attachment}=Path, Req) ->
    Filename = list_to_binary(["/tmp/", kz_util:rand_hex_binary(16), "_", Attachment]),
    case file:open(Filename, ['write', 'exclusive']) of
        {'ok', IODevice} ->
            State = #state{media=Path
                          ,filename=Filename
                          ,file=IODevice
                          },
            {'ok', Req, State};
        {'error', Reason} ->
            lager:debug("error ~p opening file ~s", [Reason, Filename]),
            reply_error(500, Req)
    end.

-spec decode_url(ne_binary()) -> media_store_path() | 'error'.
decode_url(Url) ->
    try binary_to_term(base64:decode(Url)) of
        {Db, Id, Attachment, Options} ->
            #media_store_path{db = Db
                             ,id = Id
                             ,att = Attachment
                             ,opt = Options
                             }
    catch
        _:_ -> 'error'
    end.

-spec is_appropriate_content_type(media_store_path(), cowboy_req:req()) -> validate_request_ret().
is_appropriate_content_type(Path, Req0) ->
    case cowboy_req:header(<<"content-type">>, Req0) of
        {<<"audio/", _/binary>> = CT, _Req1}->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Path, CT);
        {<<"video/", _/binary>> = CT, _Req1}->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Path, CT);
        {<<"image/", _/binary>> = CT, _Req1}->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Path, CT);
        {_CT, _Req1} ->
            lager:debug("inappropriate content-type via headers: ~s", [_CT]),
            is_appropriate_extension(Path)
    end.

-spec is_appropriate_extension(media_store_path()) -> validate_request_ret().
is_appropriate_extension(#media_store_path{att=Attachment}=Path) ->
    Extension = filename:extension(Attachment),
    case kz_mime:from_extension(Extension) of
        <<"audio/", _/binary>> = CT->
            lager:debug("found content-type via extension: ~s", [CT]),
            {'ok', add_content_type(Path, CT)};
        <<"video/", _/binary>> = CT->
            lager:debug("found content-type via extension: ~s", [CT]),
            {'ok', add_content_type(Path, CT)};
        <<"image/", _/binary>> = CT->
            lager:debug("found content-type via extension: ~s", [CT]),
            {'ok', add_content_type(Path, CT)};
        _CT ->
            lager:debug("inappropriate content-type via extension: ~s", [_CT]),
            {'error', 415}
    end.

-spec add_content_type(media_store_path(), ne_binary()) -> media_store_path().
add_content_type(#media_store_path{opt=Options}= Path, CT) ->
    NewOptions = props:set_value('content_type', kz_util:to_list(CT), Options),
    Path#media_store_path{opt=NewOptions}.

-spec ensure_extension_present(media_store_path(), ne_binary()) -> validate_request_ret().
ensure_extension_present(#media_store_path{att=Attachment}=Path, CT) ->
    case kz_util:is_empty(filename:extension(Attachment))
        andalso kz_mime:to_extension(CT)
    of
        'false' ->
            {'ok', add_content_type(Path, CT)};
        ?NE_BINARY = Extension ->
            NewName = <<Attachment/binary, ".", Extension/binary>>,
            NewPath = Path#media_store_path{att=NewName},
            {'ok', add_content_type(NewPath, CT)};
        _Else ->
            lager:debug("unable to correct missing extension for content-type: ~s", [CT]),
            {'error', 400}
    end.

-spec store(state(), cowboy_req:req()) -> {'ok', cowboy_req:req(), state()}.
store(#state{filename=Filename, media=Path}=State, Req) ->
    case file:read_file(Filename) of
        {'ok', Data} -> store(Path, Data, State, Req);
        {'error', Reason} ->
            lager:debug("error ~p opening file ~s", [Reason, Filename]),
            reply_error(500, State, Req)
    end.



-spec store(media_store_path(), ne_binary(), state(), cowboy_req:req()) -> {'ok', cowboy_req:req(), state()}.
store(#media_store_path{db=Db
                       ,id=Id
                       ,att=Attachment
                       ,opt=Options
                       }, Contents, State, Req0) ->
    lager:debug("putting ~s onto ~s(~s)", [Attachment, Id, Db]),
    case kz_datamgr:put_attachment(Db, Id, Attachment, Contents, Options) of
        {'ok', JObj} ->
            lager:debug("successfully stored(~p) ~p ~p", [Db, Id, Attachment]),
            {'ok', success(JObj, Req0), State};
        {'error', Reason} ->
            lager:debug("unable to store file: ~p", [Reason]),
            {'ok', failure(Reason, Req0), State}
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

-spec reply_error(integer(), cowboy_req:req()) -> {'shutdown', cowboy_req:req(), 'ok'}.
reply_error(Code, Req0) ->
    {'ok', Req1} = cowboy_req:reply(Code, Req0),
    {'shutdown', Req1, 'ok'}.

-spec reply_error(integer(), state(), cowboy_req:req()) -> {'shutdown', cowboy_req:req(), state()}.
reply_error(Code, State, Req0) ->
    {'ok', Req1} = cowboy_req:reply(Code, Req0),
    {'shutdown', Req1, State}.

-spec terminate(any(), cowboy_req:req(), any()) -> cowboy_req:req().
terminate(_Reason, Req, 'ok') ->
    Req;
terminate(_Reason, Req, #state{file=Device, filename=Filename}) ->
    catch(file:close(Device)),
    catch(file:delete(Filename)),
    Req.
