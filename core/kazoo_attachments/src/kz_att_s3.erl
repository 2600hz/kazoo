%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%% S3 Storage for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------


-module(kz_att_s3).

-include("kz_att.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([put_attachment/6]).
-export([fetch_attachment/4]).


put_attachment(Params, DbName, DocId, AName, Contents, _Options) ->
    #{bucket := Bucket
      ,key := Key
      ,secret := Secret
      ,path := Path
     } = Params,
    FilePath = kz_util:to_list(list_to_binary([Path, "/", DbName, "/", DocId, "_", AName])),
    Config = kz_aws_s3:new(kz_util:to_list(Key), kz_util:to_list(Secret)),
    case kz_aws_s3:put_object(kz_util:to_list(Bucket), FilePath, Contents, Config) of
        {'ok', Props} ->
            Metadata = [ convert_kv(KV) || KV <- Props, filter_kv(KV)],
            S3Key = base64:encode(term_to_binary({Key, Secret, Bucket, Path})),
            {'ok', [{'attachment', [{<<"S3">>, S3Key}
                                   ,{<<"metadata">>, kz_json:from_list(Metadata)}
                                   ]}
                   ]};
        _E -> _E
    end.

fetch_attachment(HandlerProps, DbName, DocId, AName) ->
    case kz_json:get_value(<<"S3">>, HandlerProps) of
        'undefined' -> {'error', 'invalid_data'};
        S3 ->
            {Key, Secret, Bucket, Path} = binary_to_term(base64:decode(S3)),
            FilePath = kz_util:to_list(list_to_binary([Path, "/", DbName, "/", DocId, "_", AName])),
            Config = kz_aws_s3:new(kz_util:to_list(Key), kz_util:to_list(Secret)),
            case kz_aws_s3:get_object(kz_util:to_list(Bucket), FilePath, Config) of
                {'ok', Props} -> {'ok', props:get_value('content', Props)};
                _E -> _E
            end
    end.

filter_kv({"x-amz" ++ _, _V}) -> 'true';
filter_kv({"etag", _V}) -> 'true';
filter_kv(_KV) -> 'false'.

convert_kv({K, V})
  when is_list(K) ->
    convert_kv({kz_util:to_binary(K), V});
convert_kv({K, V})
  when is_list(V) ->
    convert_kv({K, kz_util:to_binary(V)});
convert_kv({<<"etag">> = K, V}) ->
    {K, binary:replace(V, <<$">>, <<>>, ['global'])};
convert_kv(KV) -> KV.
