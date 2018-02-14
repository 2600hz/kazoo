%% @author root
%% @doc @todo Add description to kz_att_util.
-module(kz_att_util).

-export([sha256/1, sha256_mac/2, sha_mac/2
        ,md5/1
        ,format_url/2, format_url/3, format_url/4
        ,combined_path/2
        ,path_from_settings/1
        ,headers_as_binaries/1
        ,encode_multipart/2
        ,handle_http_error_response/3
        ,http_code_to_status_line/1
        ]).

-export_type([format_field/0, format_fields/0]).

-include("kz_att.hrl").

-type format_field() :: map()
                      | {'group', kz_term:proplist()}
                      | {'arg', kz_term:ne_binary()}
                      | {'field', kz_term:ne_binary()}
                      | kz_term:ne_binary().
-type format_fields() :: [format_field()].

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

-spec format_url(map(), attachment_info()) -> kz_term:ne_binary().
format_url(#{file := Path}, _AttInfo) -> Path;
format_url(Map, AttInfo) ->
    format_url(Map, AttInfo, default_format_url_fields()).

-spec format_url(map(), attachment_info(), kz_term:proplist()) -> kz_term:ne_binary().
format_url(#{file := Path}, _AttInfo, _Format) -> Path;
format_url(Map, {DbName, DocId, AName}, Format) ->
    {'ok', JObj} = kz_datamgr:open_cache_doc(DbName, DocId),
    Args = [{<<"attachment">>, AName}
           ,{<<"id">>, DocId}
           ,{<<"account_id">>, kz_doc:account_id(JObj, <<"unknown_account">>)}
           ,{<<"db">>, kz_util:uri_decode(DbName)}
           ],
    format_url(Map, kz_doc:public_fields(JObj), Args, Format).

-spec format_url(map(), kz_json:object(), kz_term:proplist(), kz_term:proplist()) -> kz_term:ne_binary().
format_url(#{file := Path}, _JObj, _Args, _DefaultFields) -> Path;
format_url(Params, JObj, Args, DefaultFields) ->
    Fields = maps:get('field_list', Params, DefaultFields),
    FieldSeparator = maps:get('field_separator', Params, <<"/">>),
    BaseUrl = do_format_url(Fields, JObj, Args, FieldSeparator),
    case path_from_settings(Params) of
        'undefined' -> BaseUrl;
        Path -> list_to_binary([Path, "/", BaseUrl])
    end.

-spec do_format_url(kz_term:proplist(), kz_json:object(), kz_term:proplist(), binary()) -> kz_term:ne_binary().
do_format_url(Fields, JObj, Args, Separator) ->
    FormattedFields = lists:foldl(fun(F, Acc) ->
                                          format_url_field(JObj, Args, F, Acc)
                                  end
                                 ,[]
                                 ,Fields
                                 ),
    Reversed = lists:reverse(FormattedFields),
    kz_binary:join(Reversed, Separator).

-spec format_url_field(kz_json:object(), kz_term:proplist(), format_field(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
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
format_url_field(_JObj, _Args, Field, Fields) ->
    [Field | Fields].

-spec default_format_url_fields() -> kz_term:proplist().
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

-spec encode_multipart([tuple()], binary()) -> binary().
encode_multipart(Parts, Boundary) ->
    encode_multipart(Parts, Boundary, <<>>).

-spec encode_multipart([tuple()], binary(), binary()) -> binary().
encode_multipart([], Boundary, Encoded) ->
    Close = <<"\r\n--" , Boundary/binary, "--">>,
    <<Encoded/binary, Close/binary>>;
encode_multipart([{Body, Headers} | Parts], Boundary, Encoded) ->
    Delimiter = <<"\r\n--" ,Boundary/binary, "\r\n">>,
    H = encode_multipart_headers(Headers),
    Acc = <<Encoded/binary, Delimiter/binary, H/binary, Body/binary>>,
    encode_multipart(Parts, Boundary, Acc).

-spec encode_multipart_headers(kz_term:proplist()) -> binary().
encode_multipart_headers(Headers) ->
    encode_multipart_headers(Headers, <<>>).

-spec encode_multipart_headers(kz_term:proplist(), binary()) -> binary().
encode_multipart_headers([], Encoded) -> <<Encoded/binary, "\r\n">>;
encode_multipart_headers([{K, V} | Headers], Encoded) ->
    Acc = <<Encoded/binary, K/binary, ": ", V/binary, "\r\n">>,
    encode_multipart_headers(Headers, Acc).

%% This function will take care for all the HTTP responses with HTTP code /= `200' and
%% HTTP response errors coming from `kz_http' module.
-spec handle_http_error_response(kz_http:req(), string(), kz_term:ne_binary()) ->
                                        gen_attachment:error_response().
handle_http_error_response({'ok', RespCode, _RespHeaders, RespBody} = _E, Msg, _Url) ->
    lager:error("~p: ~p", [Msg, _E]),
    gen_attachment:error_response(RespCode, RespBody);
handle_http_error_response({'error', {ErrorAtom, ErrorBody}} = _E, Msg, _Url)
  when is_atom(ErrorAtom) ->
    lager:error("~p: ~p", [Msg, _E]),
    Body = kz_binary:join([ErrorAtom, ErrorBody], <<": ">>),
    gen_attachment:error_response(400, Body);
handle_http_error_response(_E, Msg, Url) ->
    lager:error("~p: ~p", [Msg, _E]),
    LowerBinMsg = list_to_binary(string:to_lower(Msg)),
    gen_attachment:error_response(400, <<LowerBinMsg/binary, " from url: ", Url/binary>>).

-spec http_code_to_status_line(atom() | pos_integer()) -> kz_term:ne_binary().
%% 4×× Client Error
http_code_to_status_line(400) -> <<"Bad Request">>;
http_code_to_status_line(401) -> <<"Unauthorized">>;
http_code_to_status_line(402) -> <<"Payment Required">>;
http_code_to_status_line(403) -> <<"Forbidden">>;
http_code_to_status_line(404) -> <<"Not Found">>;
http_code_to_status_line(405) -> <<"Method Not Allowed">>;
http_code_to_status_line(406) -> <<"Not Acceptable">>;
http_code_to_status_line(407) -> <<"Proxy Authentication Required">>;
http_code_to_status_line(408) -> <<"Request Timeout">>;
http_code_to_status_line(409) -> <<"Conflict">>;
http_code_to_status_line(410) -> <<"Gone">>;
http_code_to_status_line(411) -> <<"Length Required">>;
http_code_to_status_line(412) -> <<"Precondition Failed">>;
http_code_to_status_line(413) -> <<"Payload Too Large">>;
http_code_to_status_line(414) -> <<"Request-URI Too Long">>;
http_code_to_status_line(415) -> <<"Unsupported Media Type">>;
http_code_to_status_line(416) -> <<"Requested Range Not Satisfiable">>;
http_code_to_status_line(417) -> <<"Expectation Failed">>;
http_code_to_status_line(418) -> <<"I'm a teapot">>;
http_code_to_status_line(421) -> <<"Misdirected Request">>;
http_code_to_status_line(422) -> <<"Unprocessable Entity">>;
http_code_to_status_line(423) -> <<"Locked">>;
http_code_to_status_line(424) -> <<"Failed Dependency">>;
http_code_to_status_line(426) -> <<"Upgrade Required">>;
http_code_to_status_line(428) -> <<"Precondition Required">>;
http_code_to_status_line(429) -> <<"Too Many Requests">>;
http_code_to_status_line(431) -> <<"Request Header Fields Too Large">>;
http_code_to_status_line(444) -> <<"Connection Closed Without Response">>;
http_code_to_status_line(451) -> <<"Unavailable For Legal Reasons">>;
http_code_to_status_line(499) -> <<"Client Closed Request">>;
%% 5×× Server Error
http_code_to_status_line(500) -> <<"Internal Server Error">>;
http_code_to_status_line(501) -> <<"Not Implemented">>;
http_code_to_status_line(502) -> <<"Bad Gateway">>;
http_code_to_status_line(503) -> <<"Service Unavailable">>;
http_code_to_status_line(504) -> <<"Gateway Timeout">>;
http_code_to_status_line(505) -> <<"HTTP Version Not Supported">>;
http_code_to_status_line(506) -> <<"Variant Also Negotiates">>;
http_code_to_status_line(507) -> <<"Insufficient Storage">>;
http_code_to_status_line(508) -> <<"Loop Detected">>;
http_code_to_status_line(510) -> <<"Not Extended">>;
http_code_to_status_line(511) -> <<"Network Authentication Required">>;
http_code_to_status_line(599) -> <<"Network Connect Timeout Error">>.
