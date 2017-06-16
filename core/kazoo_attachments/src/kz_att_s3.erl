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
-include("aws/kz_aws.hrl").

-export([put_attachment/6]).
-export([fetch_attachment/4]).

-define(AMAZON_S3_HOST, <<"s3.amazonaws.com">>).

-type aws_url_parts() :: {ne_binary(), ne_binary(), non_neg_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec aws_config(ne_binary(), ne_binary(), aws_url_parts() | ne_binary(), boolean()) -> aws_config().
aws_config(Key, Secret, {Scheme, Host, Port}, BucketAfterHost) ->
    kz_aws_s3:new(kz_term:to_list(Key)
                 ,kz_term:to_list(Secret)
                 ,kz_term:to_list(Host)
                 ,Port
                 ,kz_term:to_list(Scheme)
                 ,BucketAfterHost
                 );
aws_config(Key, Secret, Host, BucketAfterHost) ->
    aws_config(Key, Secret, {<<"https://">>, Host, 443}, BucketAfterHost).

-spec put_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> any().
put_attachment(Params, DbName, DocId, AName, Contents, _Options) ->
    {Bucket, Key, Secret, Path, Host} = get_map_values(Params),
    BucketAfterHost = kz_term:is_true(maps:get('bucket_after_host', Params, 'false')),
    FilePath = get_file_path(Path, DbName, DocId, AName),
    lager:debug("saving attachment to s3://~p", [Host]),
    Config = aws_config(Key, Secret, Host, BucketAfterHost),
    case kz_aws_s3:put_object(kz_term:to_list(Bucket), FilePath, Contents, Config) of
        {'ok', Props} ->
            Metadata = [ convert_kv(KV) || KV <- Props, filter_kv(KV)],
            S3Key = base64:encode(term_to_binary({Key, Secret, Host, Bucket, Path})),
            {'ok', [{'attachment', [{<<"S3">>, S3Key}
                                   ,{<<"S3-01">>, BucketAfterHost}
                                   ,{<<"metadata">>, kz_json:from_list(Metadata)}
                                   ]}
                   ]};
        _E ->
            lager:debug("error saving attachment to ", [_E]),
            _E
    end.

-spec fetch_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary()) -> any().
fetch_attachment(Conn, DbName, DocId, AName) ->
    HandlerProps = kz_json:get_value(<<"handler_props">>, Conn, 'undefined'),
    case kz_json:get_value(<<"S3">>, Conn) of
        'undefined' -> {'error', 'invalid_data'};
        S3 ->
            {Key, Secret, Host, Bucket, Path} = get_s3_values(S3, HandlerProps),
            FilePath = get_file_path(Path, DbName, DocId, AName),
            BucketAfterHost = kz_json:is_true(<<"S3-01">>, Conn, 'false'),
            Config = aws_config(Key, Secret, Host, BucketAfterHost),
            case kz_aws_s3:get_object(kz_term:to_list(Bucket), FilePath, Config) of
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

-spec get_map_values(kz_data:connection()) ->
                            {ne_binary(), ne_binary(), ne_binary(), api_binary(), aws_url_parts()}.
get_map_values(#{'bucket' := Bucket
                ,'key' := Key
                ,'secret' := Secret
                }=Map) ->
    BasePath = maps:get('folder_base_path', Map, 'undefined'),
    OtherPath = maps:get('folder_path', Map, 'undefined'),
    Host = maps:get('host', Map,  ?AMAZON_S3_HOST),
    Scheme = maps:get('scheme', Map,  <<"https">>),
    DefaultPort = case Scheme of
                      <<"https">> -> 443;
                      <<"http">> -> 80;
                      _ -> 80
                  end,
    Port = kz_term:to_integer(maps:get('port', Map,  DefaultPort)),
    {Bucket, Key, Secret, combined_path(BasePath, OtherPath), {<<Scheme/binary, "://">> , Host, Port}}.

-spec combined_path(api_binary(), api_binary()) -> api_binary().
combined_path('undefined', 'undefined') -> 'undefined';
combined_path('undefined', Path) -> Path;
combined_path(Path, 'undefined') -> Path;
combined_path(BasePath, OtherPath) ->
    filename:join(BasePath, OtherPath).

-spec get_file_path(api_binary(), ne_binary(), ne_binary(), ne_binary()) -> list().
get_file_path('undefined', DbName, DocId, AName) ->
    kz_term:to_list(list_to_binary([DbName, "/", DocId, "_", AName]));
get_file_path(Path, DbName, DocId, AName) ->
    kz_term:to_list(list_to_binary([Path, "/", DbName, "/", DocId, "_", AName])).

-spec get_s3_values(ne_binary(), kz_data:connection()) ->
                           {ne_binary(), ne_binary(), ne_binary() | aws_url_parts(), ne_binary(), api_binary()}.
get_s3_values(S3, 'undefined') ->
    case binary_to_term(base64:decode(S3)) of
        {Key, Secret, Bucket, Path} ->
            {Key, Secret, ?AMAZON_S3_HOST, Bucket, Path};
        {_Key, _Secret, _Host, _Bucket, _Path} = V2 ->
            V2
    end;
get_s3_values(S3, HandlerProps) ->
    Original =  {_, _, Host, Bucket, Path} = get_s3_values(S3, 'undefined'),
    try get_map_values(HandlerProps) of
        {Bucket, Key, Secret, _Path, Host} ->
            {Key, Secret, Host, Bucket, Path};
        _ -> Original
    catch
        _E:_R ->
            lager:info("unable to get s3 cred value using att values (~s): ~p", [_E, _R]),
            Original
    end.
