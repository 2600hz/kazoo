%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% S3 Storage for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------
-module(kz_att_s3).

-include("kz_att.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([put_attachment/6]).
-export([fetch_attachment/4]).

-define(AMAZON_S3_HOST, <<"s3.amazonaws.com">>).

%% ====================================================================
%% API functions
%% ====================================================================

-spec bucket(map()) -> string().
bucket(#{bucket := Bucket}) -> kz_term:to_list(Bucket).

-spec path(map()) -> api_ne_binary().
path(#{path := Path}) -> Path;
path(Map) ->
    BasePath = maps:get('folder_base_path', Map, 'undefined'),
    OtherPath = maps:get('folder_path', Map, 'undefined'),
    combined_path(BasePath, OtherPath).

-spec fix_scheme(ne_binary()) -> ne_binary().
fix_scheme(<<"https://">> = Scheme) -> Scheme;
fix_scheme(<<"http://">> = Scheme) -> Scheme;
fix_scheme(<<"https">> = Scheme) -> <<Scheme/binary, "://">>;
fix_scheme(<<"http">> = Scheme) -> <<Scheme/binary, "://">>;
fix_scheme(Scheme) -> <<Scheme/binary, "://">>.


-spec aws_config(map()) -> aws_config().
aws_config(#{'key' := Key
            ,'secret' := Secret
            }=Map) ->
    BucketAfterHost = kz_term:is_true(maps:get('bucket_after_host', Map, 'false')),
    BucketAccess = kz_term:to_atom(maps:get('bucket_access_method', Map, 'auto'), 'true'),
    Region = maps:get('region', Map, 'undefined'),

    Host = maps:get('host', Map,  ?AMAZON_S3_HOST),
    Scheme = fix_scheme(maps:get('scheme', Map,  <<"https://">>)),
    DefaultPort = case Scheme of
                      <<"https://">> -> 443;
                      <<"http://">> -> 80;
                      _ -> 80
                  end,
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
               ,aws_region=Region
               }.

-spec merge_params(map() | ne_binary(), map() | undefined) -> map().
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

-spec aws_bpc(map()) -> {string(), api_ne_binary(), aws_config()}.
aws_bpc(Map) ->
    {bucket(Map), path(Map), aws_config(Map)}.

-spec aws_bpc(ne_binary(), map() | undefined) -> {string(), api_ne_binary(), aws_config()}.
aws_bpc(S3, Handler) ->
    aws_bpc(merge_params(S3, Handler)).


-spec encode_retrieval(map()) -> ne_binary().
encode_retrieval(Map) ->
    base64:encode(term_to_binary(Map)).

-spec decode_retrieval(ne_binary()) -> map().
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
        #{} = Map -> Map
    end.

-spec put_attachment(map(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> any().
put_attachment(Params, DbName, DocId, AName, Contents, _Options) ->
    {Bucket, Path, Config} = aws_bpc(Params),
    FilePath = get_file_path(Path, DbName, DocId, AName),
    case put_object(Bucket, FilePath, Contents, Config) of
        {'ok', Props} ->
            props:to_log(Props, <<"AWS HEADERS">>),
            Metadata = [ convert_kv(KV) || KV <- Props, filter_kv(KV)],
            S3Key = encode_retrieval(Params),
            {'ok', [{'attachment', [{<<"S3">>, S3Key}
                                   ,{<<"metadata">>, kz_json:from_list(Metadata)}
                                   ]}
                   ,{'headers', Props}
                   ]};
        _E ->
            lager:debug("error saving attachment to ", [_E]),
            _E
    end.

-spec fetch_attachment(kz_json:object(), ne_binary(), ne_binary(), ne_binary()) ->
                              {'error', 'invalid_data'} |
                              {'ok', binary()}.
fetch_attachment(Conn, DbName, DocId, AName) ->
    HandlerProps = kz_json:get_value(<<"handler_props">>, Conn, 'undefined'),
    case kz_json:get_value(<<"S3">>, Conn) of
        'undefined' -> {'error', 'invalid_data'};
        S3 ->
            {Bucket, Path, Config} = aws_bpc(S3, HandlerProps),
            FilePath = get_file_path(Path, DbName, DocId, AName),
            case get_object(Bucket, FilePath, Config) of
                {'ok', Props} -> {'ok', props:get_value('content', Props)};
                _E -> _E
            end
    end.

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

-spec combined_path(api_binary(), api_binary()) -> api_binary().
combined_path('undefined', 'undefined') -> 'undefined';
combined_path('undefined', Path) -> Path;
combined_path(Path, 'undefined') -> Path;
combined_path(BasePath, OtherPath) ->
    filename:join(BasePath, OtherPath).

-spec get_file_path(api_binary(), ne_binary(), ne_binary(), ne_binary()) -> string().
get_file_path('undefined', DbName, DocId, AName) ->
    kz_term:to_list(list_to_binary([DbName, "/", DocId, "_", AName]));
get_file_path(Path, DbName, DocId, AName) ->
    kz_term:to_list(list_to_binary([Path, "/", DbName, "/", DocId, "_", AName])).

-spec put_object(string(), string(), binary(), aws_config()) -> {ok, kz_proplist()} | {error, any()}.
put_object(Bucket, FilePath, Contents, #aws_config{s3_host=Host} = Config) ->
    lager:debug("storing ~s to ~s", [FilePath, Host]),
    Options = ['return_all_headers'],
    try erlcloud_s3:put_object(Bucket, FilePath, Contents, Options, [], Config) of
        Headers -> {ok, Headers}
    catch
        error : {aws_error, Reason} -> {error, Reason}
    end.

-spec get_object(string(), string(), aws_config()) -> {ok, kz_proplist()} | {error, any()}.
get_object(Bucket, FilePath, #aws_config{s3_host=Host} = Config) ->
    lager:debug("retrieving ~s from ~s", [FilePath, Host]),
    Options = [],
    try erlcloud_s3:get_object(Bucket, FilePath, Options, Config) of
        Headers -> {ok, Headers}
    catch
        error : {aws_error, Reason} -> {error, Reason}
    end.
