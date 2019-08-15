%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2019, 2600Hz
%%% @doc Simple URL Storage for attachments.
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_ftp).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

%%%=============================================================================
%%% gen_attachment behaviour callbacks (API)
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec put_attachment(gen_attachment:settings()
                    ,gen_attachment:db_name()
                    ,gen_attachment:doc_id()
                    ,gen_attachment:att_name()
                    ,gen_attachment:contents()
                    ,gen_attachment:options()
                    ) -> gen_attachment:put_response().
put_attachment(Params, DbName, DocId, AName, Contents, Options) ->
    #{url := BaseUrlParam} = Params,
    DocUrlField = maps:get('document_url_field', Params, 'undefined'),
    BaseUrl = kz_binary:strip_right(BaseUrlParam, $/),
    Url = list_to_binary([BaseUrl, "/", kz_att_util:format_url(Params, {DbName, DocId, AName})]),

    case send_request(Url, Contents) of
        'ok' ->
            lager:debug("attachment ~s of document ~s/~s uploaded to ~s"
                       ,[AName, DocId, DbName, Url]
                       ),
            {'ok', url_fields(DocUrlField, Url)};
        Resp ->
            Routines = [{fun kz_att_error:set_req_url/2, Url}
                        | kz_att_error:put_routines(Params, DbName, DocId, AName,
                                                    Contents, Options)
                       ],
            handle_ftp_error_response(Resp, Routines)
    end.

-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, DbName, DocId, AName) ->
    case kz_json:get_value(<<"url">>, HandlerProps) of
        'undefined' ->
            Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
            kz_att_error:new('invalid_data', Routines);
        Url ->
            case fetch_attachment(Url) of
                {'ok', _Bin} = Resp ->
                    Resp;
                Resp ->
                    Routines = [{fun kz_att_error:set_req_url/2, Url}
                                | kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName)
                               ],
                    handle_ftp_error_response(Resp, Routines)
            end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_request(kz_term:ne_binary(), kz_term:ne_binary()) ->
                          'ok' | {'error', binary(), binary() | atom() | term()}.
send_request(Url, Contents) ->
    case http_uri:parse(kz_term:to_list(Url)) of
        {'ok',{'ftp', UserPass, Host, Port, FullPath,_Query}} ->
            send_request(Host, Port, UserPass, FullPath, Contents);
        _ -> {'error', <<"error parsing url: ", Url/binary>>}
    end.

-spec send_request(string(), integer(), string(), string(), binary()) ->
                          'ok' | {'error', binary() | atom() | term()}.
send_request(Host, Port, UserPass, FullPath, Contents) ->
    {User, Pass} = case string:tokens(UserPass, ":") of
                       [U, P] -> {U, P};
                       _ -> ftp_anonymous_user_pass()
                   end,
    Dir = filename:dirname(FullPath),
    File = filename:basename(FullPath),
    try
        Options = [{'port', Port}],
        case ftp:open(Host, Options) of
            {'ok', Pid} ->
                Routines = [fun() -> ftp:user(Pid, User, Pass) end
                           ,fun() -> ftp:type(Pid, 'binary') end
                           ,fun() -> ftp:cd(Pid, Dir) end
                           ,fun() -> ftp:send_bin(Pid, Contents, File) end
                           ],
                Res = ftp_cmds(Routines),
                catch(ftp:close(Pid)),
                case Res of
                    'ok' -> 'ok';
                    Resp -> Resp
                end;
            Resp -> Resp
        end
    catch
        _Exc:Err ->
            lager:debug("error ~p / ~p sending file ~s to ~s", [_Exc, Err, FullPath, Host]),
            {'error', Err}
    end.

-spec ftp_cmds(list()) -> 'ok' | {'error', any()}.
ftp_cmds([]) -> 'ok';
ftp_cmds([Fun|Funs]) ->
    case Fun() of
        'ok' -> ftp_cmds(Funs);
        Other -> Other
    end.

-spec ftp_anonymous_user_pass() -> {string(), string()}.
ftp_anonymous_user_pass() ->
    Domain = kz_term:to_list(kz_util:node_hostname()),
    {"anonymous", "kazoo@" ++ Domain}.

url_fields('undefined', Url) ->
    [{'attachment', [{<<"url">>, Url}]}];
url_fields(DocUrlField, Url) ->
    [{'attachment', [{<<"url">>, Url}]}
    ,{'document', [{DocUrlField, Url}]}
    ].

-spec fetch_attachment(kz_term:ne_binary()) -> {'ok', binary()} |
                                               {'error', binary() | atom() | term()}.
fetch_attachment(Url) ->
    {_, Host, File, _, _} = kz_http_util:urlsplit(Url),
    try ftp:open(kz_term:to_list(Host)) of
        {'ok', Pid} -> handle_fetch(Pid, ftp:recv_bin(Pid, kz_term:to_list(File)));
        {'error', _Reason}=Err ->
            lager:debug("error '~p' opening ftp connection to ~s for saving ~s",
                        [_Reason, Host, File]),
            Err
    catch
        _E:Reason ->
            lager:debug("exception '~p/~p' opening ftp connection to ~s for saving ~s",
                        [_E, Reason, Host, File]),
            {'error', Reason}
    end.

-spec handle_fetch(pid(), tuple()) -> {'ok', binary()} | {'error', any()}.
handle_fetch(Pid, {'ok', _Bin}=OK) ->
    ftp:close(Pid),
    OK;
handle_fetch(Pid, {'error', _Reason}=Err) ->
    lager:debug("error transferring file from ftp server : ~p", [_Reason]),
    ftp:close(Pid),
    Err.

-spec handle_ftp_error_response({'error', binary() | atom() | term()},
                                kz_att_error:update_routines()) -> kz_att_error:error().
handle_ftp_error_response({'error', Reason}, Routines)
  when is_atom(Reason); is_binary(Reason) ->
    lager:error("ftp error ~p", [Reason]),
    kz_att_error:new(Reason, Routines);
handle_ftp_error_response(_E, Routines) ->
    lager:error("ftp request error ~p", [_E]),
    kz_att_error:new('request_error', Routines).
