%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% Simple Url Storage for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------
-module(kz_att_ftp).

-include("kz_att.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([put_attachment/6]).
-export([fetch_attachment/4]).

-spec put_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> any().
put_attachment(Params, DbName, DocId, AName, Contents, Options) ->
    #{url := BaseUrlParam} = Params,
    {'ok', JObj} = kz_datamgr:open_cache_doc(DbName, DocId, Options),
    Args = [{<<"attachment">>, AName}
           ,{<<"id">>, DocId}
           ],
    Fields = maps:get('field_list', Params, default_format()),
    FieldSeparator = maps:get('field_separator', Params, <<"/">>),
    DocUrlField = maps:get('document_url_field', Params, 'undefined'),
    BaseUrl = kz_binary:strip_right(BaseUrlParam, $/),
    Url = list_to_binary([BaseUrl, "/", format_url(Fields, JObj, Args, FieldSeparator)]),

    case send_request(Url, Contents) of
        'ok' ->
            lager:debug("attachment ~s of document ~s/~s uploaded to ~s"
                       ,[AName, DocId, DbName, Url]
                       ),
            {'ok', url_fields(DocUrlField, Url)};
        {'error', _Err} = Error ->
            lager:debug("error '~p' uploading attachment ~s of document ~s/~s to ~s"
                       ,[_Err, AName, DocId, DbName, Url]
                       ),
            Error
    end.

-spec send_request(ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
send_request(Url, Contents) ->
    case http_uri:parse(kz_term:to_list(Url)) of
        {'ok',{'ftp', UserPass, Host, Port, FullPath,_Query}} ->
            send_request(Host, Port, UserPass, FullPath, Contents);
        _ -> {'error', <<"error parsing url : ", Url/binary>>}
    end.

-spec send_request(string(), integer(), string(), string(), binary()) -> 'ok' | {'error', any()}.
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
                    {'error', Error} -> {'error', term_to_binary(Error)}
                end;
            {'error', Error} -> {'error', term_to_binary(Error)}
        end
    catch
        _Exc:Err ->
            lager:debug("error ~p / ~p sending file ~s to ~s", [_Exc, Err, FullPath, Host]),
            {'error', term_to_binary(Err)}
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

-spec fetch_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary()) -> any().
fetch_attachment(HandlerProps, _DbName, _DocId, _AName) ->
    case kz_json:get_value(<<"url">>, HandlerProps) of
        'undefined' -> {'error', 'invalid_data'};
        Url -> fetch_attachment(Url)
    end.

-spec fetch_attachment(ne_binary()) -> {'ok', binary()} | {'error', any()}.
fetch_attachment(Url) ->
    {_, Host, File, _, _} = kz_http_util:urlsplit(Url),
    try ftp:open(kz_term:to_list(Host)) of
        {'ok', Pid} -> handle_fetch(Pid, ftp:recv_bin(Pid, kz_term:to_list(File)));
        {'error', _Reason}=Err ->
            lager:debug("error '~p' opening ftp connection to ~s for saving ~s", [_Reason, Host, File]),
            Err
    catch
        _E:Reason ->
            lager:debug("exception '~p/~p' opening ftp connection to ~s for saving ~s", [_E, Reason, Host, File]),
            {'error', Reason}
    end.

-spec handle_fetch(pid(), tuple()) -> {'ok', binary()} | {'error', any()}.
handle_fetch(Pid, {'ok', _Bin}=OK) ->
    ftp:close(Pid),
    OK;
handle_fetch(Pid, {'error', _Reason}=Err) ->
    lager:debug("error transfering file from ftp server : ~p", [_Reason]),
    ftp:close(Pid),
    Err.

format_url(Fields, JObj, Args, Separator) ->
    FormattedFields = lists:foldl(fun(F, Acc) ->
                                          format_url_field(JObj, Args, F, Acc)
                                  end
                                 ,[]
                                 ,Fields
                                 ),
    Reversed = lists:reverse(FormattedFields),
    kz_binary:join(Reversed, Separator).

format_url_field(JObj, Args, Fields, Acc)
  when is_list(Fields) ->
    [format_url(Fields, JObj, Args, <<>>) | Acc];
format_url_field(JObj, Args, #{<<"arg">> := Arg}, Fields) ->
    format_url_field(JObj, Args, {arg, Arg}, Fields);
format_url_field(_JObj, Args, {arg, Arg}, Fields) ->
    case props:get_value(Arg, Args) of
        'undefined' -> Fields;
        V -> [kz_util:uri_encode(V) | Fields]
    end;
format_url_field(JObj, Args, #{<<"field">> := Field}, Fields) ->
    format_url_field(JObj, Args, {field, Field}, Fields);
format_url_field(JObj, _Args, {field, Field}, Fields) ->
    case kz_json:get_value(Field, JObj) of
        'undefined' -> Fields;
        V -> [kz_util:uri_encode(V) | Fields]
    end;
format_url_field(_JObj, _Args, Field, Fields) ->
    [Field | Fields].

default_format() ->
    [{field, <<"pvt_account_id">>}
    ,{field, <<"owner_id">>}
    ,{arg, <<"id">>}
    ,{arg, <<"attachment">>}
    ].
