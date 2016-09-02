%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% Simple Url Storage for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------
-module(kz_att_http).

-include("kz_att.hrl").

-define(MAX_REDIRECTS, 10).

%% ====================================================================
%% API functions
%% ====================================================================

-export([put_attachment/6]).
-export([fetch_attachment/4]).

-spec put_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> any().
put_attachment(Params, DbName, DocId, AName, Contents, Options) ->
    #{url := BaseUrlParam, verb := Verb} = Params,
    {'ok', JObj} = kz_datamgr:open_cache_doc(DbName, DocId, Options),
    Args = [{<<"attachment">>, AName}
           ,{<<"id">>, DocId}
           ],
    Fields = maps:get('field_list', Params, default_format()),
    FieldSeparator = maps:get('field_separator', Params, <<"/">>),
    DocUrlField = maps:get('document_url_field', Params, 'undefined'),
    BaseUrl = kz_util:strip_right_binary(BaseUrlParam, $/),
    Url = list_to_binary([BaseUrl, "/", format_url(Fields, JObj, Args, FieldSeparator)]),
    Headers = [{'content_type', props:get_value('content_type', Options, kz_mime:from_filename(AName))}],

    case send_request(Url, kz_util:to_atom(Verb, 'true'), Headers, Contents) of
        {'ok', NewUrl, _Body, _Debug} -> {'ok', url_fields(DocUrlField, NewUrl)};
        {'error', _} = Error -> Error
    end.

send_request(Url, Verb, Headers, Contents) ->
    send_request(Url, Verb, Headers, Contents, 0, kz_json:new()).

send_request(_Url, _Verb, _Headers, _Contents, Redirects, _)
  when Redirects > ?MAX_REDIRECTS ->
    {'error', 'too_many_redirects'};
send_request(Url, Verb, Headers, Contents, Redirects, Debug) ->
    case kz_http:req(Verb, Url, Headers, Contents) of
        {'ok', Code, ReplyHeaders, Body} when
              is_integer(Code)
              andalso Code >= 200
              andalso Code =< 299 ->
            {'ok', Url, Body, add_debug(Debug, Url, Code, ReplyHeaders)};
        {'ok', Code, ReplyHeaders, _Body} when
              Code =:= 301;
              Code =:= 302 ->
            NewDebug = add_debug(Debug, Url, Code, ReplyHeaders),
            Fun = fun(URL, Tries, Data) -> send_request(URL, Verb, Headers, Contents, Tries, Data) end,
            maybe_redirect(Url, ReplyHeaders, Redirects, NewDebug, Fun);
        _E -> {'error', _E}
    end.

maybe_redirect(ToUrl, Headers, Redirects, Debug, Fun) ->
    case props:get_value("location", Headers) of
        'undefined' ->
            lager:debug("request to ~s got redirection but no 'location' header was found", [ToUrl]),
            {'error', 'redirection_with_no_location'};
        Url ->
            maybe_redirect_loop(ToUrl, Url, Redirects, Debug, Fun)
    end.

maybe_redirect_loop(ToUrl, Url, Redirects, Debug, Fun) ->
    case kz_json:get_value(Url, Debug) of
        'undefined' ->
            lager:debug("request to ~s redirected to ~s.",[ToUrl, Url]),
            Fun(Url, Redirects + 1, Debug);
        _ ->
            lager:debug("the request ~s got redirect to ~s but this is already in the list of visited urls",[ToUrl, Url]),
            {'error', 'redirect_loop_detected'}
    end.

add_debug(Debug, Url, Code, Headers) ->
    kz_json:set_values([{[Url, <<"code">>], Code}
                       ,{[Url, <<"headers">>], kz_json:from_list(Headers)}
                       ], Debug).


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

fetch_attachment(URL) ->
    case fetch_attachment(URL, 0, kz_json:new()) of
        {'ok', _Url, Body, _Debug} -> {'ok', Body};
        {'error', _} = Error -> Error
    end.

-spec fetch_attachment(ne_binary(), integer(), kz_json:object()) -> any().
fetch_attachment(_Url, Redirects, _)
  when Redirects > ?MAX_REDIRECTS ->
    {'error', 'too_many_redirects'};
fetch_attachment(Url, Redirects, Debug) ->
    case kz_http:get(Url) of
        {'ok', 200, _Headers, Body} -> {'ok', Body};
        {'ok', Code, Headers, _Body} when
              Code =:= 301;
              Code =:= 302 ->
            NewDebug = add_debug(Debug, Url, Code, Headers),
            Fun = fun(URL, N, Data) -> fetch_attachment(URL, N, Data) end,
            maybe_redirect(Url, Headers, Redirects, NewDebug, Fun);
        {'ok', _, _Headers, Body} -> {'error', Body};
        _R -> {'error', <<"error getting from url">>}
    end.

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
