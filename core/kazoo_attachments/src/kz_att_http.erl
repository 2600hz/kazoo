%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
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


put_attachment(Params, DbName, DocId, AName, Contents, Options) ->
    #{url := BaseUrl, verb := Verb} = Params,
    {'ok', JObj} = kz_datamgr:open_cache_doc(DbName, DocId, Options),
    Args = [{<<"attachment">>, AName}
            ,{<<"id">>, DocId}
           ],
    Fields = maps:get(field_list, Params, default_format()),
    DocUrlField = maps:get(document_url_field, Params, 'undefined'),
    Url = list_to_binary([BaseUrl, "/", format_url(Fields, JObj, Args)]),
    Headers = [{'content_type', props:get_value('content_type', Options, kz_mime:from_filename(AName))}],

    case kz_http:req(kz_util:to_atom(Verb, 'true'), Url, Headers, Contents) of
        {'ok', Code, _Headers, _Body} when
              is_integer(Code) andalso Code >= 200 andalso Code =< 299 ->
            {'ok', [{'attachment', [{<<"url">>, Url}]}
                    | add_document_url_field(DocUrlField, Url)
                   ]};
        _E -> _E
    end.

add_document_url_field('undefined', _) -> [];
add_document_url_field(DocUrlField, Url) -> [{'document', [{DocUrlField, Url}]}].

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

format_url(Fields, JObj, Args) ->
    kz_util:join_binary(
      lists:reverse(
        lists:foldl(fun(F, Acc) -> format_url_field(JObj, Args, F, Acc) end, [], Fields)
        ), <<"/">>).

format_url_field(_JObj, Args, <<":", Arg/binary>>, Fields) ->
    case props:get_value(Arg, Args) of
        'undefined' -> Fields;
        V -> [V | Fields]
    end;
format_url_field(JObj, _Args, Field, Fields) ->
    case kz_json:get_value(Field, JObj) of
        'undefined' -> Fields;
        V -> [V | Fields]
    end.


default_format() ->
    [<<"pvt_account_id">>, <<"owner_id">>, <<":id">>, <<":attachment">>
    ].
