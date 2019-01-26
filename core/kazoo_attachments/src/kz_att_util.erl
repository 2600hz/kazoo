%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Some utilities to work with attachment.
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_util).

-export([sha256/1, sha256_mac/2, sha_mac/2
        ,md5/1
        ,format_url/2, format_url/3, format_url/4
        ,combined_path/2
        ,path_from_settings/1
        ,headers_as_binaries/1
        ]).

-export([default_format_url_fields/0]).

-include("kz_att.hrl").

-spec sha_mac(iodata(), iodata()) -> binary().
sha_mac(K, S) ->
    try crypto:hmac('sha', K, S)
    catch
        'error':'undef' ->
            R0 = crypto:hmac_init('sha', K),
            R1 = crypto:hmac_update(R0, S),
            crypto:hmac_final(R1)
    end.

-spec sha256_mac(iodata(), iodata()) -> binary().
sha256_mac(K, S) ->
    try crypto:hmac('sha256', K, S)
    catch
        'error':'undef' ->
            R0 = crypto:hmac_init('sha256', K),
            R1 = crypto:hmac_update(R0, S),
            crypto:hmac_final(R1)
    end.

-spec sha256(iodata()) -> binary().
sha256(V) ->
    crypto:hash('sha256', V).

-spec md5(iodata()) -> binary().
md5(V) ->
    crypto:hash('md5', V).

-spec format_url(gen_attachment:settings(), attachment_info()) -> kz_term:ne_binary().
format_url(#{file := Path}, _AttInfo) -> Path;
format_url(Map, AttInfo) ->
    format_url(Map, AttInfo, default_format_url_fields()).

-spec format_url(gen_attachment:settings(), attachment_info(), url_fields()) -> kz_term:ne_binary().
format_url(#{file := Path}, _AttInfo, _Format) -> Path;
format_url(Map, {DbName, DocId, _AName}=DocInfo, Format) ->
    {'ok', JObj} = kz_datamgr:open_cache_doc(DbName, DocId),

    format_url_from_metadata(Map, DocInfo, Format, JObj).

-spec format_url_from_metadata(gen_attachment:settings(), attachment_info(), url_fields(), kz_json:object()) -> kz_term:ne_binary().
format_url_from_metadata(Map, {DbName, DocId, AName}, Format, JObj) ->
    Args = [{<<"attachment">>, AName}
           ,{<<"id">>, DocId}
           ,{<<"account_id">>, kz_doc:account_id(JObj, <<"unknown_account">>)}
           ,{<<"db">>, kz_util:uri_decode(DbName)}
           ],
    format_url(Map, kz_doc:public_fields(JObj), Args, Format).

-spec format_url(gen_attachment:settings(), kz_json:object(), kz_term:proplist(), url_fields()) -> kz_term:ne_binary().
format_url(#{file := Path}, _JObj, _Args, _DefaultFields) -> Path;
format_url(Params, JObj, Args, DefaultFields) ->
    Fields = maps:get('field_list', Params, DefaultFields),
    FieldSeparator = maps:get('field_separator', Params, <<"/">>),

    BaseUrl = do_format_url(Fields, JObj, Args, FieldSeparator),

    case path_from_settings(Params) of
        'undefined' -> BaseUrl;
        Path -> list_to_binary([Path, "/", BaseUrl])
    end.

-spec do_format_url(url_fields(), kz_json:object(), kz_term:proplist(), binary()) -> kz_term:ne_binary().
do_format_url(Fields, JObj, Args, Separator) ->
    FormattedFields = lists:foldl(fun(F, Acc) ->
                                          format_url_field(JObj, Args, F, Acc)
                                  end
                                 ,[]
                                 ,Fields
                                 ),
    Reversed = lists:reverse(FormattedFields),
    kz_binary:join(Reversed, Separator).

-spec format_url_field(kz_json:object(), kz_term:proplist(), url_field(), kz_term:ne_binaries()) ->
                              kz_term:ne_binaries().
format_url_field(JObj, Args, #{<<"group">> := Arg}, Acc) ->
    format_url_field(JObj, Args, {'group', Arg}, Acc);
format_url_field(JObj, Args, {'group', Arg}, Acc) ->
    [do_format_url(Arg, JObj, Args, <<>>) | Acc];
format_url_field(JObj, Args, #{<<"arg">> := Arg}, Fields) ->
    format_url_field(JObj, Args, {'arg', Arg}, Fields);
format_url_field(_JObj, Args, {'arg', Arg}, Fields) ->
    case props:get_value(Arg, Args) of
        'undefined' -> Fields;
        V -> [kz_util:uri_encode(V) | Fields]
    end;
format_url_field(JObj, Args, #{<<"field">> := Field}, Fields) ->
    format_url_field(JObj, Args, {'field', Field}, Fields);
format_url_field(JObj, _Args, {'field', Field}, Fields) ->
    case kz_json:get_ne_binary_value(Field, JObj) of
        'undefined' -> Fields;
        V -> [kz_util:uri_encode(V) | Fields]
    end;
format_url_field(JObj, Args, #{<<"const">> := Field}, Fields) ->
    format_url_field(JObj, Args, {'const', Field}, Fields);
format_url_field(_JObj, _Args, {'const', Const}, Fields) ->
    [Const | Fields];
format_url_field(_JObj, _Args, Bin, Fields)
  when is_binary(Bin) ->
    [Bin | Fields].

-spec default_format_url_fields() -> url_fields().
default_format_url_fields() ->
    [{'arg', <<"account_id">>}
    ,{'field', <<"owner_id">>}
    ,{'arg', <<"id">>}
    ,{'arg', <<"attachment">>}
    ].

-spec combined_path(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:api_binary().
combined_path('undefined', 'undefined') -> 'undefined';
combined_path('undefined', Path) -> Path;
combined_path(Path, 'undefined') -> Path;
combined_path(BasePath, OtherPath) ->
    filename:join(BasePath, OtherPath).

-spec path_from_settings(map()) -> kz_term:api_ne_binary().
path_from_settings(#{file := Path}) -> Path;
path_from_settings(#{path := Path}) -> Path;
path_from_settings(Map) ->
    BasePath = maps:get('folder_base_path', Map, 'undefined'),
    OtherPath = maps:get('folder_path', Map, 'undefined'),
    combined_path(BasePath, OtherPath).

-spec headers_as_binaries(kz_term:proplist()) -> kz_term:proplist().
headers_as_binaries(Headers) ->
    [{kz_term:to_binary(K), kz_term:to_binary(V)} || {K,V} <- Headers].
