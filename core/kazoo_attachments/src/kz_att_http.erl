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

    case kz_http:req(kz_util:to_atom(Verb, 'true'), Url, Headers, Contents) of
        {'ok', Code, _Headers, _Body} when
              is_integer(Code)
              andalso Code >= 200
              andalso Code =< 299 ->
            {'ok', [{'attachment', [{<<"url">>, Url}]}
                    | add_document_url_field(DocUrlField, Url)
                   ]};
        _E -> _E
    end.

add_document_url_field('undefined', _) -> [];
add_document_url_field(DocUrlField, Url) -> [{'document', [{DocUrlField, Url}]}].

-spec fetch_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary()) -> any().
fetch_attachment(HandlerProps, _DbName, _DocId, _AName) ->
    case kz_json:get_value(<<"url">>, HandlerProps) of
        'undefined' -> {'error', 'invalid_data'};
        Url ->
            case kz_http:get(Url) of
                {'ok', 200, _Headers, Body} -> {'ok', Body};
                {'ok', _, _Headers, Body} -> {'error', Body};
                _R -> {'error', <<"error getting from url">>}
            end
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
