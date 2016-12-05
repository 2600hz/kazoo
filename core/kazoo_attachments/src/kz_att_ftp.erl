%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
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
    BaseUrl = kz_util:strip_right_binary(BaseUrlParam, $/),
    Url = list_to_binary([BaseUrl, "/", format_url(Fields, JObj, Args, FieldSeparator)]),

    case send_request(Url, Contents) of
        'ok' -> {'ok', url_fields(DocUrlField, Url)};
        {'error', _} = Error -> Error
    end.

-spec send_request(ne_binary(), ne_binary()) -> 'ok' | {'error', any()}.
send_request(Url, Contents) ->
    {_, Host, File, _, _} = kz_http_util:urlsplit(Url),
    case ftp:open(Host) of
        {'ok', Pid} -> handle_send(Pid, ftp:send_bin(Pid, Contents, File));
        {'error', _Reason}=Err ->
            lager:debug("error '~p' opening ftp connection to ~s for saving ~s", [_Reason, Host, File]),
            Err
    end.

-spec handle_send(pid(), 'ok' | {'error', any()}) -> 'ok' | {'error', any()}.
handle_send(Pid, 'ok') -> ftp:close(Pid);
handle_send(Pid, {'error', _Reason}=Err) ->
    lager:debug("error transfering file to ftp server : ~p", [_Reason]),
    ftp:close(Pid),
    Err.

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
    case ftp:open(Host) of
        {'ok', Pid} -> handle_fetch(Pid, ftp:recv_bin(Pid, File));
        {'error', _Reason}=Err ->
            lager:debug("error '~p' opening ftp connection to ~s for saving ~s", [_Reason, Host, File]),
            Err
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
    kz_util:join_binary(Reversed, Separator).

format_url_field(JObj, Args, Fields, Acc)
  when is_list(Fields) ->
    [format_url(Fields, JObj, Args, <<>>) | Acc];
format_url_field(_JObj, Args, {arg, Arg}, Fields) ->
    case props:get_value(Arg, Args) of
        'undefined' -> Fields;
        V -> [kz_util:uri_encode(V) | Fields]
    end;
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
    ,{args, <<"id">>}
    ,{arg, <<"attachment">>}
    ].
