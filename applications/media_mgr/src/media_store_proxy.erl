%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Handles storage proxy requests for media binaries
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(media_store_proxy).
-behaviour(cowboy_handler).

-export([init/2
        ,terminate/3
        ]).

-include("media.hrl").

-record(state, {media :: media_store_path()
               ,filename :: binary()
               ,file :: file:io_device()
               }).

-type state() :: #state{}.

-type body() :: {'ok', binary(), cowboy_req:req()} | {'more', binary(), cowboy_req:req()}.
-type validate_request_ret() :: {'ok', media_store_path()} | {'error', reply_code()}.
-type handler_return() :: {'ok', cowboy_req:req(), 'ok' | state()}.

-type reply_code() :: 200 | 400 | 404 | 500.

-spec init(cowboy_req:req(), kz_term:proplist()) -> handler_return().
init(Req, _Opts) ->
    kz_log:put_callid(kz_binary:rand_hex(16)),
    check_authn(Req, authenticate(Req)).

-spec check_authn(cowboy_req:req(), boolean()) -> handler_return().
check_authn(Req, 'true') ->
    check_validation(Req, cowboy_req:path_info(Req));
check_authn(Req, 'false') ->
    lager:info("request did not provide valid credentials"),
    {'ok', unauthorized(Req), 'ok'}.

-spec check_validation(cowboy_req:req(), kz_term:ne_binaries()) -> handler_return().
check_validation(Req, [EncodedUrl, _Filename]) ->
    check_url(Req, decode_url(EncodedUrl));
check_validation(Req, _Else) ->
    lager:info("unexpected path in request: ~p", [_Else]),
    reply_error(404, Req).

-spec check_url(cowboy_req:req(), 'error' | media_store_path()) -> handler_return().
check_url(Req, 'error') ->
    lager:info("url decoding failed on ~p", [hd(cowboy_req:path_info(Req))]),
    reply_error(404, Req);
check_url(Req, Path) ->
    validate_path_request(Req, Path).

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

-spec maybe_basic_authentication(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
maybe_basic_authentication(Username, Password) ->
    AuthUsername = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_username">>, <<>>),
    AuthPassword = kapps_config:get_binary(?CONFIG_CAT, <<"proxy_password">>, <<>>),
    not kz_term:is_empty(AuthUsername)
        andalso not kz_term:is_empty(AuthPassword)
        andalso Username =:= AuthUsername
        andalso Password =:= AuthPassword.

-spec maybe_acl_authentication(cowboy_req:req()) -> boolean().
maybe_acl_authentication(Req) ->
    ACLs = kapps_config:get(?CONFIG_CAT, <<"proxy_store_acls">>, [<<"127.0.0.0/24">>]),
    {IpTuple, _PeerPort} = cowboy_req:peer(Req),
    Ip = kz_network_utils:iptuple_to_binary(IpTuple),
    maybe_acl_authentication(ACLs, Ip).

-spec maybe_acl_authentication(kz_term:ne_binaries(), kz_term:ne_binary()) -> boolean().
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

-spec credentials(cowboy_req:req()) -> {kz_term:api_binary(), kz_term:api_binary(), cowboy_req:req()}.
credentials(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        'undefined' ->
            {'undefined', 'undefined', Req};
        Authorization ->
            {Username, Password} = credentials_from_header(Authorization),
            {Username, Password, Req}
    end.

-spec credentials_from_header(kz_term:ne_binary()) -> {kz_term:api_binary(), kz_term:api_binary()}.
credentials_from_header(AuthorizationHeader) ->
    case binary:split(AuthorizationHeader, <<$\s>>) of
        [<<"Basic">>, EncodedCredentials] ->
            decoded_credentials(EncodedCredentials);
        _ ->
            {'undefined', 'undefined'}
    end.

-spec decoded_credentials(kz_term:ne_binary()) -> {kz_term:api_binary(), kz_term:api_binary()}.
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
    cowboy_req:reply(401, Req2).

-spec unauthorized_body() -> kz_term:ne_binary().
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
    handle_body(cowboy_req:read_body(Req), State).

-spec handle_body(body(), state()) -> handler_return().
handle_body({'more', Contents, Req}, #state{file=Device}=State) ->
    lager:debug("recv part of file"),
    case file:write(Device, Contents) of
        'ok' -> handle(Req, State);
        {'error', Reason} -> {'ok', failure(Reason, Req), State}
    end;
handle_body({'ok', <<>>, Req}, #state{file=Device}=State) ->
    lager:debug("recv empty req body"),
    handle_close(Req, State, file:close(Device));
handle_body({'ok', Contents, Req}, #state{file=Device}=State) ->
    lager:debug("recv file"),
    case file:write(Device, Contents) of
        'ok' -> handle_close(Req, State, file:close(Device));
        {'error', Reason} -> {'ok', failure(Reason, Req), State}
    end.

handle_close(Req, State, 'ok') ->
    store(State, Req);
handle_close(Req, State, {'error', Reason}) ->
    {'ok', failure(Reason, Req), State}.

-spec validate_path_request(cowboy_req:req(), media_store_path()) -> handler_return().
validate_path_request(Req, Path) ->
    case is_appropriate_content_type(Req, Path) of
        {'ok', NewPath} -> setup_context(Req, NewPath);
        {'error', Code} -> reply_error(Code, Req)
    end.

-spec setup_context(cowboy_req:req(), media_store_path()) -> handler_return().
setup_context(Req, #media_store_path{att=Attachment}=Path) ->
    Filename = list_to_binary(["/tmp/", kz_binary:rand_hex(16), "_", Attachment]),
    case file:open(Filename, ['write', 'exclusive']) of
        {'ok', IODevice} ->
            lager:debug("opened ~s for writing", [Filename]),
            State = #state{media=Path
                          ,filename=Filename
                          ,file=IODevice
                          },
            handle(Req, State);
        {'error', Reason} ->
            lager:debug("error ~p opening file ~s", [Reason, Filename]),
            reply_error(500, Req)
    end.

-spec decode_url(kz_term:ne_binary()) -> media_store_path() | 'error'.
decode_url(Url) ->
    try binary_to_term(base64:decode(kz_http_util:urldecode(Url))) of
        {Db, Id, Attachment, Options} ->
            #media_store_path{db = Db
                             ,id = Id
                             ,att = Attachment
                             ,opt = Options
                             }
    catch
        _:_ -> 'error'
    end.

-spec is_appropriate_content_type(cowboy_req:req(), media_store_path()) -> validate_request_ret().
is_appropriate_content_type(Req, Path) ->
    case cowboy_req:header(<<"content-type">>, Req) of
        <<"audio/", _/binary>> = CT ->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Path, CT);
        <<"video/", _/binary>> = CT ->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Path, CT);
        <<"image/", _/binary>> = CT ->
            lager:debug("found content-type via header: ~s", [CT]),
            ensure_extension_present(Path, CT);
        _CT ->
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

-spec add_content_type(media_store_path(), kz_term:ne_binary()) -> media_store_path().
add_content_type(#media_store_path{opt=Options}= Path, CT) ->
    NewOptions = props:set_value('content-type', CT, Options),
    Path#media_store_path{opt=NewOptions}.

-spec ensure_extension_present(media_store_path(), kz_term:ne_binary()) -> validate_request_ret().
ensure_extension_present(#media_store_path{att=Attachment}=Path, CT) ->
    case kz_term:is_empty(filename:extension(Attachment))
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
        {'ok', Data} ->
            store(Path, Data, State, Req);
        {'error', Reason} ->
            lager:info("error ~p opening file ~s", [Reason, Filename]),
            {'ok', failure(Reason, Req), State}
    end.

-spec store(media_store_path(), kz_term:ne_binary(), state(), cowboy_req:req()) ->
          {'ok', cowboy_req:req(), state()}.
store(#media_store_path{db=Db
                       ,id=Id
                       ,att=Attachment
                       ,opt=Options
                       }, Contents, State, Req) ->
    lager:debug("putting ~s onto ~s(~s)", [Attachment, Id, Db]),
    case kz_datamgr:put_attachment(Db, Id, Attachment, Contents, Options) of
        {'ok', JObj} ->
            lager:debug("successfully stored(~p) ~p ~p", [Db, Id, Attachment]),
            {'ok', success(JObj, [], Req), State};
        {'ok', JObj, Props} ->
            lager:debug("successfully stored(~p) ~p ~p", [Db, Id, Attachment]),
            {'ok', success(JObj, Props, Req), State};
        {'error', Reason} ->
            lager:debug("unable to store file: ~p", [Reason]),
            {'ok', failure(Reason, Req), State};
        {'error', Reason, _Extended} ->
            lager:debug("unable to store file: ~p", [Reason]),
            {'ok', failure(Reason, Req), State}
    end.

-spec success(kz_json:object(), kz_term:proplist(), cowboy_req:req()) -> cowboy_req:req().
success(JObj, Props, Req0) ->
    Body = io_lib:format("~s~n", [kz_json:encode(kz_json:set_value(<<"ok">>, 'true', JObj))]),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    Headers = maps:from_list([{kz_term:to_binary(H), kz_term:to_binary(V)}
                              || {H, V} <- props:get_value('headers', Props, [])
                             ]),
    lager:info("replying with 200"),
    cowboy_req:reply(200, Headers, Req1).

-spec failure(any(), cowboy_req:req()) -> cowboy_req:req().
failure(Reason, Req0) ->
    Body = io_lib:format("~p~n", [Reason]),
    Req1 = cowboy_req:set_resp_body(Body, Req0),
    lager:info("replying with 500 error: ~s", [Body]),
    cowboy_req:reply(500, Req1).

-spec reply_error(reply_code(), cowboy_req:req()) -> {'ok', cowboy_req:req(), 'ok'}.
reply_error(Code, Req) ->
    lager:info("replying with error ~p", [Code]),
    {'ok', cowboy_req:reply(Code, Req), 'ok'}.

-spec terminate(any(), cowboy_req:req(), any()) -> 'ok'.
terminate(_Reason, _Req, #state{file=Device, filename=Filename}) ->
    catch(file:close(Device)),
    catch(file:delete(Filename)),
    lager:debug("closed files and terminated handler: ~p", [_Reason]);
terminate(_Reason, _Req, _) ->
    lager:debug("terminating handler: ~p", [_Reason]).
