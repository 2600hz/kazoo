%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2020, 2600Hz
%%% @doc S3 Storage for attachments.
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_s3).
-behaviour(gen_attachment).

-include("kz_att.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% gen_attachment behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-export([aws_default_fields/0]).

-define(AMAZON_S3_HOST, <<"s3.amazonaws.com">>).
-define(AMAZON_S3_UPLOAD_TIMEOUT, ?MILLISECONDS_IN_MINUTE * 30).

-define(AMAZON_S3_HTTP_DEFAULT_CLIENT, 'lhttpc').
%% httpc seems broken in otp 21.2.5

-type s3_error() :: {'aws_error'
                    ,{'socket_error', binary()} |
                     {'http_error', pos_integer(), string(), binary()}
                    }.

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
    {Bucket, FilePath, Config} = aws_bpc(Params, {DbName, DocId, AName}),
    case put_object(Bucket, FilePath, Contents, Config) of
        {'ok', RespHeaders} ->
            Metadata = create_metadata(RespHeaders),
            S3Key = encode_retrieval(Params, FilePath),
            {'ok', [{'attachment', [{<<"S3">>, S3Key}
                                   ,{<<"metadata">>, kz_json:from_list(Metadata)}
                                   ]}
                   ,{'headers', RespHeaders}
                   ]};
        {'error', _FilePath, Error} ->
            Routines = [{fun kz_att_error:set_req_url/2, FilePath}
                        | kz_att_error:put_routines(Params, DbName, DocId, AName
                                                   ,<<>>, Options
                                                   )
                       ],
            handle_s3_error(Error, Routines)
    end.

-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(Conn, DbName, DocId, AName) ->
    HandlerProps = kz_json:get_value(<<"handler_props">>, Conn, 'undefined'),
    Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
    case kz_json:get_value(<<"S3">>, Conn) of
        'undefined' ->
            kz_att_error:new('invalid_data', Routines);
        S3 ->
            {Bucket, FilePath, Config} = aws_bpc(S3, HandlerProps, {DbName, DocId, AName}),
            case get_object(Bucket, FilePath, Config) of
                {'ok', RespHeaders} ->
                    {'ok', props:get_value('content', RespHeaders)};
                {'error', FilePath, Error} ->
                    NewRoutines = [{fun kz_att_error:set_req_url/2, FilePath}
                                   | Routines
                                  ],
                    handle_s3_error(Error, NewRoutines)
            end
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bucket(gen_attachment:settings()) -> string().
bucket(#{bucket := Bucket}) -> kz_term:to_list(Bucket).

-spec fix_scheme(kz_term:ne_binary()) -> kz_term:ne_binary().
fix_scheme(<<"https://">> = Scheme) -> Scheme;
fix_scheme(<<"http://">> = Scheme) -> Scheme;
fix_scheme(<<"https">> = Scheme) -> <<Scheme/binary, "://">>;
fix_scheme(<<"http">> = Scheme) -> <<Scheme/binary, "://">>;
fix_scheme(Scheme) -> <<Scheme/binary, "://">>.

-spec aws_default_port(kz_term:ne_binary()) -> inet:port_number().
aws_default_port(<<"https://">>) -> 443;
aws_default_port(_Scheme) -> 80.

-spec aws_region(map()) -> kz_term:api_ne_binary().
aws_region(Map) ->
    case maps:get('region', Map, 'undefined') of
        'undefined' -> 'undefined';
        Bin -> kz_term:to_list(Bin)
    end.

-spec aws_default_host(map()) -> kz_term:ne_binary().
aws_default_host(Map) ->
    case aws_region(Map) of
        'undefined' -> ?AMAZON_S3_HOST;
        Region -> list_to_binary(["s3.", Region, ".amazonaws.com"])
    end.

-spec aws_host(map()) -> kz_term:ne_binary().
aws_host(Map) ->
    maps:get('host', Map,  aws_default_host(Map)).

-spec aws_default_bucket_access(map()) -> atom().
aws_default_bucket_access(Map) ->
    case aws_region(Map) of
        'undefined' -> 'path';
        _Region -> 'auto'
    end.

-spec aws_bucket_access(map()) -> atom().
aws_bucket_access(Map) ->
    kz_term:to_atom(maps:get('bucket_access_method', Map, aws_default_bucket_access(Map)), 'true').

-spec aws_config(gen_attachment:settings()) -> aws_config().
aws_config(#{'key' := Key
            ,'secret' := Secret
            }=Map
          ) ->
    Region = aws_region(Map),
    BucketAfterHost = kz_term:is_true(maps:get('bucket_after_host', Map, 'false')),
    BucketAccess = aws_bucket_access(Map),
    Timeout = kz_term:to_integer(maps:get('upload_timeout', Map, ?AMAZON_S3_UPLOAD_TIMEOUT)),
    HttpClient = kz_term:to_atom(maps:get('http_client', Map, ?AMAZON_S3_HTTP_DEFAULT_CLIENT), 'true'),

    Host = aws_host(Map),
    Scheme = fix_scheme(maps:get('scheme', Map,  <<"https://">>)),
    DefaultPort = aws_default_port(Scheme),
    Port = kz_term:to_integer(maps:get('port', Map,  DefaultPort)),
    #aws_config{access_key_id=kz_term:to_list(Key)
               ,secret_access_key=kz_term:to_list(Secret)
               ,s3_scheme=kz_term:to_list(Scheme)
               ,s3_host=kz_term:to_list(Host)
               ,s3_port=Port
               ,s3_bucket_after_host=BucketAfterHost
               ,s3_bucket_access_method=BucketAccess
               ,s3_follow_redirect=true
               ,s3_follow_redirect_count=3
               ,timeout=Timeout
               ,http_client=HttpClient
               ,aws_region=Region
               }.

-spec aws_default_fields() -> url_fields().
aws_default_fields() ->
    [{'arg', <<"db">>}
    ,{'group', [{'arg', <<"id">>}
               ,{'const', <<"_">>}
               ,{'arg', <<"attachment">>}
               ]}
    ].

-spec aws_format_url(gen_attachment:settings(), attachment_info()) -> kz_term:ne_binary().
aws_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, aws_default_fields()).

-spec merge_params(map() | kz_term:ne_binary(), map() | undefined) -> map().
merge_params(#{bucket := Bucket, host := Host} = M1, #{bucket := Bucket, host := Host} = M2) ->
    kz_maps:merge(M1, M2);
merge_params(#{bucket := Bucket} = M1, #{bucket := Bucket} = M2) ->
    kz_maps:merge(M1, M2);
merge_params(#{}= Map, #{}) ->
    Map;
merge_params(#{}= Map, _M2) ->
    Map;
merge_params(S3, M2)
  when is_binary(S3)->
    M1 = decode_retrieval(S3),
    merge_params(M1, M2).

-spec aws_bpc(gen_attachment:settings(), attachment_info()) -> {string(), kz_term:api_ne_binary(), aws_config()}.
aws_bpc(Map, AttInfo) ->
    {bucket(Map), aws_format_url(Map, AttInfo), aws_config(Map)}.

-spec aws_bpc(kz_term:ne_binary(), map() | 'undefined', attachment_info()) -> {string(), kz_term:api_ne_binary(), aws_config()}.
aws_bpc(S3, Handler, Attinfo) ->
    aws_bpc(merge_params(S3, Handler), Attinfo).

-spec encode_retrieval(map(), kz_term:ne_binary()) -> kz_term:ne_binary().
encode_retrieval(Map, FilePath) ->
    base64:encode(term_to_binary({Map, FilePath})).

-spec decode_retrieval(kz_term:ne_binary()) -> map().
decode_retrieval(S3) ->
    case binary_to_term(base64:decode(S3)) of
        {Key, Secret, Bucket, Path} ->
            #{key => Key
             ,secret => Secret
             ,host => ?AMAZON_S3_HOST
             ,bucket => Bucket
             ,path => Path
             };
        {Key, Secret, {Scheme, Host, Port}, Bucket, Path} ->
            #{key => Key
             ,secret => Secret
             ,host => Host
             ,scheme => Scheme
             ,port => Port
             ,bucket => Bucket
             ,path => Path
             };
        {Key, Secret, Host, Bucket, Path} ->
            #{key => Key
             ,secret => Secret
             ,host => Host
             ,bucket => Bucket
             ,path => Path
             };
        {#{} = Map, FilePath} ->
            Map#{file => FilePath};
        #{} = Map -> Map
    end.

-spec create_metadata(kz_term:proplist()) -> kz_term:proplist().
create_metadata(RespHeaders) ->
    [convert_kv(KV) || KV <- RespHeaders,
                       filter_kv(KV)
    ].

filter_kv({"x-amz" ++ _, _V}) -> 'true';
filter_kv({"etag", _V}) -> 'true';
filter_kv(_KV) -> 'false'.

convert_kv({K, V})
  when is_list(K) ->
    convert_kv({kz_term:to_binary(K), V});
convert_kv({K, V})
  when is_list(V) ->
    convert_kv({K, kz_term:to_binary(V)});
convert_kv({<<"etag">> = K, V}) ->
    {K, binary:replace(V, <<$">>, <<>>, ['global'])};
convert_kv(KV) -> KV.

-spec put_object(string(), string() | kz_term:ne_binary(), binary(), aws_config()) ->
          {'ok', kz_term:proplist()} |
          {'error', string() | kz_term:ne_binary(), s3_error()}.
put_object(Bucket, FilePath, Contents,Config)
  when is_binary(FilePath) ->
    put_object(Bucket, kz_term:to_list(FilePath), Contents,Config);
put_object(Bucket, FilePath, Contents, #aws_config{s3_host=Host} = Config) ->
    lager:debug("storing ~s to ~s", [FilePath, Host]),
    Options = ['return_all_headers'],
    CT = kz_mime:from_filename(FilePath),
    ReqHeaders = [{"content-type", kz_term:to_list(CT)}],
    try erlcloud_s3:put_object(Bucket, FilePath, Contents, Options, ReqHeaders, Config) of
        Headers -> {'ok', Headers}
    catch
        'error':Error -> {'error', FilePath, Error}
    end.

-spec get_object(string(), kz_term:ne_binary(), aws_config()) ->
          {'ok', kz_term:proplist()} |
          {'error', kz_term:ne_binary(), s3_error()}.
get_object(Bucket, FilePath, #aws_config{s3_host=Host} = Config) ->
    lager:debug("retrieving ~s from ~s", [FilePath, Host]),
    Options = [],
    try erlcloud_s3:get_object(Bucket, kz_term:to_list(FilePath), Options, Config) of
        RespHeaders -> {'ok', RespHeaders}
    catch
        'error':Error -> {'error', FilePath, Error}
    end.

%% S3 REST API errors reference: https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingRESTError.html
%% Handle response from `erlcloud_s3:s3_request/8' function, this error objects are built
%% within `erlcloud_aws:request_to_return/1' and the return is also modified at
%% `erlcloud_s3:s3_request2/8'.
-spec handle_s3_error(s3_error(), kz_att_error:update_routines()) -> kz_att_error:error().
handle_s3_error({'aws_error'
                ,{'http_error', RespCode, _RespStatusLine, RespBody}
                } = _E
               ,Routines
               ) ->
    Reason = get_reason(RespCode, RespBody),
    NewRoutines = [{fun kz_att_error:set_resp_code/2, RespCode}
                  ,{fun kz_att_error:set_resp_body/2, RespBody}
                  ,{fun kz_att_error:set_resp_headers/2, []}
                   | Routines
                  ],
    lager:error("S3 error: ~p (code: ~p)", [_E, RespCode]),
    kz_att_error:new(Reason, NewRoutines);
handle_s3_error({'aws_error', {'socket_error', RespBody}} = _E, Routines) ->
    lager:error("S3 request error: ~p", [_E]),
    RespBodyBin = list_to_binary(io_lib:format("~p", [RespBody])),
    Reason = <<"Socket error: ", RespBodyBin/binary>>,
    kz_att_error:new(Reason, Routines);
handle_s3_error(_E, Routines) ->
    lager:error("S3 request error: ~p", [_E]),
    Reason = <<"Unknown Error">>,
    kz_att_error:new(Reason, Routines).

-spec get_reason(atom() | pos_integer(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_reason(RespCode, RespBody) when RespCode >= 400 ->
    %% If the `RespCode' value is >= 400 then the resp_body must contain an error object
    try xmerl_scan:string(binary_to_list(RespBody)) of
        {Xml, _} -> kz_xml:get_value("//Message/text()", Xml)
    catch
        _:_ -> kz_http_util:http_code_to_status_line(RespCode)
    end;
get_reason(RespCode, _RespBody) ->
    kz_http_util:http_code_to_status_line(RespCode).
