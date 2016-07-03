%% @author root
%% @doc @todo Add description to kz_s3.
-module(kz_aws_s3).

-export([new/2, new/3, new/4, new/5,
         configure/2, configure/3, configure/4, configure/5,
         create_bucket/1, create_bucket/2, create_bucket/3, create_bucket/4,
         delete_bucket/1, delete_bucket/2,
         get_bucket_attribute/2, get_bucket_attribute/3,
         list_buckets/0, list_buckets/1,
         set_bucket_attribute/3, set_bucket_attribute/4,
         get_bucket_policy/1, get_bucket_policy/2,
         put_bucket_policy/2, put_bucket_policy/3,
         list_objects/1, list_objects/2, list_objects/3,
         list_object_versions/1, list_object_versions/2, list_object_versions/3,
         copy_object/4, copy_object/5, copy_object/6,
         delete_objects_batch/2, delete_objects_batch/3,
         explore_dirstructure/3,
         delete_object/2, delete_object/3,
         delete_object_version/3, delete_object_version/4,
         get_object/2, get_object/3, get_object/4,
         get_object_acl/2, get_object_acl/3, get_object_acl/4,
         get_object_torrent/2, get_object_torrent/3,
         get_object_metadata/2, get_object_metadata/3, get_object_metadata/4,
         put_object/3, put_object/4, put_object/5, put_object/6,
         set_object_acl/3, set_object_acl/4,
         make_link/3, make_link/4,
         make_get_url/3, make_get_url/4,
         start_multipart/2, start_multipart/5,
         upload_part/5, upload_part/7,
         complete_multipart/4, complete_multipart/6,
         abort_multipart/3, abort_multipart/6,
         list_multipart_uploads/1, list_multipart_uploads/2,
         get_object_url/2, get_object_url/3,
         get_bucket_and_key/1,
         extract_metadata/1
        ]).

-include("kz_aws.hrl").

%%% Note that get_bucket_and_key/1 may be used to obtain the Bucket and Key to pass to various
%%%   functions here, from a URL such as https://s3.amazonaws.com/some_bucket/path_to_file

-spec new(string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey) ->
    #aws_config{access_key_id=AccessKeyID
               ,secret_access_key=SecretAccessKey
               }.

-spec new(string(), string(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID
               ,secret_access_key=SecretAccessKey
               ,s3_host=Host
               }.


-spec new(string(), string(), string(), non_neg_integer()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port) ->
    #aws_config{access_key_id=AccessKeyID
               ,secret_access_key=SecretAccessKey
               ,s3_host=Host
               ,s3_port=Port
               }.

-spec new(string(), string(), string(), non_neg_integer(), string()) -> aws_config().

new(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    #aws_config{access_key_id=AccessKeyID
                ,secret_access_key=SecretAccessKey
                ,s3_host=Host
                ,s3_port=Port
                ,s3_scheme=Scheme
               }.

-spec configure(string(), string()) -> 'ok'.

configure(AccessKeyID, SecretAccessKey) ->
    put('aws_config', new(AccessKeyID, SecretAccessKey)),
    'ok'.

-spec configure(string(), string(), string()) -> 'ok'.

configure(AccessKeyID, SecretAccessKey, Host) ->
    put('aws_config', new(AccessKeyID, SecretAccessKey, Host)),
    'ok'.

-spec configure(string(), string(), string(), non_neg_integer()) -> 'ok'.

configure(AccessKeyID, SecretAccessKey, Host, Port) ->
    put('aws_config', new(AccessKeyID, SecretAccessKey, Host, Port)),
    'ok'.

-spec configure(string(), string(), string(), non_neg_integer(), string()) -> 'ok'.

configure(AccessKeyID, SecretAccessKey, Host, Port, Scheme) ->
    put('aws_config', new(AccessKeyID, SecretAccessKey, Host, Port, Scheme)),
    'ok'.

-type s3_bucket_attribute_name() :: 'acl'
                                  | 'location'
                                  | 'logging'
                                  | 'request_payment'
                                  | 'versioning'.

-type s3_bucket_acl() :: 'private'
                       | 'public_read'
                       | 'public_read_write'
                       | 'authenticated_read'
                       | 'bucket_owner_read'
                       | 'bucket_owner_full_control'.

-type s3_location_constraint() :: 'none'
                                | 'us_west_1'
                                | 'eu'.

-define(XMLNS_S3, "http://s3.amazonaws.com/doc/2006-03-01/").

-spec copy_object(string(), string(), string(), string()) ->
                         {'ok', kz_proplist()} |
                         {'error', any()} .

copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName) ->
    copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, []).

-spec copy_object(string(), string(), string(), string(), kz_proplist() | aws_config()) ->
                         {'ok', kz_proplist()} |
                         {'error', any()} .
copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, #aws_config{} = Config) ->
    copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, [], Config);

copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, Options) ->
    copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName
                ,Options, default_config()
               ).

-spec copy_object(string(), string(), string(), string(), kz_proplist(), aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .
copy_object(DestBucketName, DestKeyName, SrcBucketName, SrcKeyName, Options, Config) ->
    SrcVersion = case props:get_value('version_id', Options) of
                     'undefined' -> "";
                     VersionID -> ["?versionId=", VersionID]
                 end,
    RequestHeaders =
        [{"x-amz-copy-source", [SrcBucketName, $/, SrcKeyName, SrcVersion]},
         {"x-amz-metadata-directive", props:get_value('metadata_directive', Options)},
         {"x-amz-copy-source-if-match", props:get_value('if_match', Options)},
         {"x-amz-copy-source-if-none-match", props:get_value('if_none_match', Options)},
         {"x-amz-copy-source-if-unmodified-since", props:get_value('if_unmodified_since', Options)},
         {"x-amz-copy-source-if-modified-since", props:get_value('if_modified_since', Options)},
         {"x-amz-acl", encode_acl(props:get_value('acl', Options))}],
    case s3_request(Config, 'put', DestBucketName, [$/|DestKeyName],"", [], <<>>, RequestHeaders) of
        {'ok', {Headers, _Body}} ->
            Props = props:filter_undefined(
                      [{'copy_source_version_id', props:get_value("x-amz-copy-source-version-id", Headers, "false")}
                      ,{'version_id', props:get_value("x-amz-version-id", Headers, "null")}
                      ]),
            {'ok', Props};
        {'error', _}=E -> E
    end.

-spec create_bucket(string()) -> {'ok', kz_proplist()} | {'error', any()} .


create_bucket(BucketName) ->
    create_bucket(BucketName, 'private').

-spec create_bucket(string(), s3_bucket_acl() | aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .


create_bucket(BucketName, #aws_config{} = Config) ->
    create_bucket(BucketName, private, Config);

create_bucket(BucketName, ACL) ->
    create_bucket(BucketName, ACL, none).

-spec create_bucket(string(), s3_bucket_acl(), s3_location_constraint() | aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .


create_bucket(BucketName, ACL, #aws_config{} = Config) ->
    create_bucket(BucketName, ACL, none, Config);

create_bucket(BucketName, ACL, LocationConstraint) ->
    create_bucket(BucketName, ACL, LocationConstraint, default_config()).

-spec create_bucket(string(), s3_bucket_acl(), s3_location_constraint(), aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .


create_bucket(BucketName, ACL, LocationConstraint, Config)
  when is_list(BucketName), is_atom(ACL), is_atom(LocationConstraint) ->
    Headers = case ACL of
                  private -> [];  %% private is the default
                  _       -> [{"x-amz-acl", encode_acl(ACL)}]
              end,
    POSTData = case LocationConstraint of
                   none -> <<>>;
                   Location when Location =:= eu; Location =:= us_west_1 ->
                       LocationName = case Location of eu -> "EU"; us_west_1 -> "us-west-1" end,
                       XML = {'CreateBucketConfiguration', [{'xmlns:xsi', ?XMLNS_S3}],
                              [{'LocationConstraint', [LocationName]}]},
                       list_to_binary(xmerl:export_simple([XML], xmerl_xml))
               end,
    s3_simple_request(Config, put, BucketName, "/", "", [], POSTData, Headers).

encode_acl(undefined)                 -> undefined;
encode_acl(private)                   -> "private";
encode_acl(public_read)               -> "public-read";
encode_acl(public_read_write)         -> "public-read-write";
encode_acl(authenticated_read)        -> "authenticated-read";
encode_acl(bucket_owner_read)         -> "bucket-owner-read";
encode_acl(bucket_owner_full_control) -> "bucket-owner-full-control".

-spec delete_bucket(string()) -> {'ok', kz_proplist()} | {'error', any()} .

delete_bucket(BucketName) ->
    delete_bucket(BucketName, default_config()).

-spec delete_bucket(string(), aws_config()) -> {'ok', kz_proplist()} | {'error', any()} .

delete_bucket(BucketName, Config)
  when is_list(BucketName) ->
    s3_simple_request(Config, delete, BucketName, "/", "", [], <<>>, []).


-spec delete_objects_batch(string(), list()) -> no_return().
delete_objects_batch(Bucket, KeyList) ->
    delete_objects_batch(Bucket, KeyList, default_config()).

-spec delete_objects_batch(string(), list(), aws_config()) -> no_return().
delete_objects_batch(Bucket, KeyList, Config) ->
    Data = [lists:concat(["<Object><Key>", Item, "</Key></Object>"])
            || Item <- KeyList],
    Payload = unicode:characters_to_list(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Delete>" ++ Data ++ "</Delete>",
                utf8),

    Len = integer_to_list(string:len(Payload)),
    Url = lists:flatten([Config#aws_config.s3_scheme,
                Bucket, ".", Config#aws_config.s3_host, port_spec(Config), "/?delete"]),
    Host = Bucket ++ "." ++ Config#aws_config.s3_host,
    ContentMD5 = base64:encode(kz_att_util:md5(Payload)),
    Headers = [{"host", Host},
               {"content-md5", binary_to_list(ContentMD5)},
               {"content-length", Len}],
    Result = kz_aws_httpc:request(
        Url, "POST", Headers, Payload, delete_objects_batch_timeout(Config), Config),
    kz_aws:http_headers_body(Result).

delete_objects_batch_timeout(#aws_config{timeout = undefined}) ->
    1000;
delete_objects_batch_timeout(#aws_config{timeout = Timeout}) ->
    Timeout.

% returns paths list from AWS S3 root directory, used as input to delete_objects_batch
% example :
%    25> rp(kz_aws_s3:explore_dirstructure("xmppfiledev", ["sailfish/deleteme"], [])).
%    ["sailfish/deleteme/deep/deep1/deep4/ZZZ_1.txt",
%     "sailfish/deleteme/deep/deep1/deep4/ZZZ_0.txt",
%     "sailfish/deleteme/deep/deep1/ZZZ_0.txt",
%     "sailfish/deleteme/deep/ZZZ_0.txt"]
%    'ok'
%
-spec explore_dirstructure(string(), list(), list()) -> list().

explore_dirstructure(_, [], Result) ->
                                    lists:append(Result);
explore_dirstructure(Bucketname, [Branch|Tail], Accum) ->
    ProcessContent = fun(Data)->
            Content = props:get_value(contents, Data),
            lists:foldl(fun(I,Acc)-> R = props:get_value(key, I), [R|Acc] end, [], Content)
            end,

    Data = list_objects(Bucketname,[{prefix, Branch}, {delimiter, "/"}]),
    case props:get_value(common_prefixes, Data) of
        [] -> % it has reached end of the branch
            Files = ProcessContent(Data),
            explore_dirstructure(Bucketname, Tail, [Files|Accum]);
        Sub ->
            Files = ProcessContent(Data),
            List = lists:foldl(fun(I,Acc)-> R = props:get_value(prefix, I), [R|Acc] end, [], Sub),
            Result = explore_dirstructure(Bucketname, List, Accum),
            explore_dirstructure(Bucketname, Tail, [Result, Files|Accum])
    end.

-spec delete_object(string(), string()) -> {'ok', kz_proplist()} | {'error', any()} .

delete_object(BucketName, Key) ->
    delete_object(BucketName, Key, default_config()).

-spec delete_object(string(), string(), aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

delete_object(BucketName, Key, Config)
  when is_list(BucketName), is_list(Key) ->
    case s3_request(Config, delete, BucketName, [$/|Key], "", [], <<>>, []) of
        {'ok', {Headers, _Body}} ->
            Marker = props:get_value("x-amz-delete-marker", Headers, "false"),
            Id = props:get_value("x-amz-version-id", Headers, "null"),
            Props = props:filter_undefined(
                      [{delete_marker, list_to_existing_atom(Marker)}
                       ,{version_id, Id}
                      ]),
            {'ok', Props};
        {'error', _}=E -> E
    end.

-spec delete_object_version(string(), string(), string()) ->
          {'ok', kz_proplist()} | {'error', any()} .

delete_object_version(BucketName, Key, Version) ->
    delete_object_version(BucketName, Key, Version, default_config()).

-spec delete_object_version(string(), string(), string(), aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

delete_object_version(BucketName, Key, Version, Config)
  when is_list(BucketName),
       is_list(Key),
       is_list(Version)->
    case s3_request(Config, delete, BucketName, [$/|Key], ["versionId=", Version], [], <<>>, []) of
        {'ok', {Headers, _Body}} ->
            Marker = props:get_value("x-amz-delete-marker", Headers, "false"),
            Id = props:get_value("x-amz-version-id", Headers, "null"),
            Props = props:filter_undefined(
                      [{delete_marker, list_to_existing_atom(Marker)}
                       ,{version_id, Id}
                      ]),
            {'ok', Props};
        {'error', _}=E -> E
    end.

-spec list_buckets() -> kz_proplist().

list_buckets() ->
    list_buckets(default_config()).

-spec list_buckets(aws_config()) -> kz_proplist().

list_buckets(Config) ->
    Doc = s3_xml_request(Config, get, "", "/", "", [], <<>>, []),
    Buckets = [extract_bucket(Node) || Node <- xmerl_xpath:string("/*/Buckets/Bucket", Doc)],
    [{buckets, Buckets}].

%
% @doc Get S3 bucket policy JSON object
% API Document: http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETacl.html
%
-spec(get_bucket_policy(BucketName::string()) -> 'ok' | {'error', Reason::term()}).
get_bucket_policy(BucketName) ->
    get_bucket_policy(BucketName, default_config()).

%
% Example request: kz_aws_s3:get_bucket_policy("bucket1234", Config).
% Example success repsonse: {ok, "{\"Version\":\"2012-10-17\",\"Statement\": ..........}
% Example error response: {error,{http_error,404,"Not Found",
%                               "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n
%                               <Error>
%                                   <Code>NoSuchBucket</Code>
%                                   <Message>The specified bucket does not exist</Message>
%                                   <BucketName>bucket1234</BucketName>
%                                   <RequestId>DC1EA9456B266EF5</RequestId>
%                                   <HostId>DRtkAB80cAeom+4ffSGU3PFCxS7QvtiW+wxLnPF0dM2nxoaRqQk1SK/z62ZJVHAD</HostId>
%                               </Error>"}}
-spec(get_bucket_policy(BucketName::string(), Config::aws_config()) -> {'ok', Policy::string()} | {'error', Reason::term()}).
get_bucket_policy(BucketName, #aws_config{} = Config) ->
    case s3_request(Config, get, BucketName, "/", "policy", [], <<>>, []) of
	{'ok', {_Headers, Body}} ->
	    {'ok', binary_to_list(Body)};
	Error ->
	    Error
    end.

-spec put_bucket_policy(string(), binary()) ->
          {'ok', kz_proplist()} | {'error', any()} .
put_bucket_policy(BucketName, Policy) ->
    put_bucket_policy(BucketName, Policy, default_config()).

-spec put_bucket_policy(string(), binary(), aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .
put_bucket_policy(BucketName, Policy, #aws_config{} = Config)
  when is_list(BucketName), is_binary(Policy) ->
    s3_simple_request(Config, put, BucketName, "/", "policy", [], Policy, []).


-spec list_objects(string()) -> kz_proplist().

list_objects(BucketName) ->
    list_objects(BucketName, []).

-spec list_objects(string(), kz_proplist() | aws_config()) -> kz_proplist().

list_objects(BucketName, #aws_config{} = Config) ->
    list_objects(BucketName, [], Config);

list_objects(BucketName, Options) ->
    list_objects(BucketName, Options, default_config()).

-spec list_objects(string(), kz_proplist(), aws_config()) -> kz_proplist().

list_objects(BucketName, Options, Config)
  when is_list(BucketName),
       is_list(Options) ->
    Params = [{"delimiter", props:get_value(delimiter, Options)},
              {"marker", props:get_value(marker, Options)},
              {"max-keys", props:get_value(max_keys, Options)},
              {"prefix", props:get_value(prefix, Options)}],
    Doc = s3_xml_request(Config, get, BucketName, "/", "", Params, <<>>, []),
    Attributes = [{name, "Name", text},
                  {prefix, "Prefix", text},
                  {marker, "Marker", text},
                  {delimiter, "Delimiter", text},
                  {max_keys, "MaxKeys", integer},
                  {is_truncated, "IsTruncated", boolean},
                  {common_prefixes, "CommonPrefixes", fun extract_prefixes/1},
                  {contents, "Contents", fun extract_contents/1}],
    kz_aws_xml:decode(Attributes, Doc).

extract_prefixes(Nodes) ->
    Attributes = [{prefix, "Prefix", text}],
    [kz_aws_xml:decode(Attributes, Node) || Node <- Nodes].

extract_contents(Nodes) ->
    Attributes = [{key, "Key", text},
                  {last_modified, "LastModified", time},
                  {etag, "ETag", text},
                  {size, "Size", integer},
                  {storage_class, "StorageClass", text},
                  {owner, "Owner", fun extract_user/1}],
    [kz_aws_xml:decode(Attributes, Node) || Node <- Nodes].

extract_user([]) ->
    [];
extract_user([Node]) ->
    Attributes = [{id, "ID", optional_text},
                  {display_name, "DisplayName", optional_text},
                  {uri, "URI", optional_text}
                 ],
    kz_aws_xml:decode(Attributes, Node).

-spec get_bucket_attribute(string(), s3_bucket_attribute_name()) -> term().

get_bucket_attribute(BucketName, AttributeName) ->
    get_bucket_attribute(BucketName, AttributeName, default_config()).

-spec get_bucket_attribute(string(), s3_bucket_attribute_name(), aws_config()) -> term().

get_bucket_attribute(BucketName, AttributeName, Config)
  when is_list(BucketName), is_atom(AttributeName) ->
    Attr = case AttributeName of
               acl             -> "acl";
               location        -> "location";
               logging         -> "logging";
               request_payment -> "requestPayment";
               versioning      -> "versioning"
           end,
    Doc = s3_xml_request(Config, get, BucketName, "/", Attr, [], <<>>, []),
    case AttributeName of
        acl ->
            Attributes = [{owner, "Owner", fun extract_user/1},
                          {access_control_list, "AccessControlList/Grant", fun extract_acl/1}],
            kz_aws_xml:decode(Attributes, Doc);
        location ->
            case kz_aws_xml:get_text("/LocationConstraint", Doc) of
                %% logic according to http://s3tools.org/s3cmd
                %% s3cmd-1.5.2/S3/S3.py : line 342 (function get_bucket_location)
                [] -> "us-east-1";
                ["US"] -> "us-east-1";
                ["EU"] -> "eu-west-1";
                Loc -> Loc
            end;
        logging ->
            case xmerl_xpath:string("/BucketLoggingStatus/LoggingEnabled", Doc) of
                [] ->
                    {enabled, 'false'};
                [LoggingEnabled] ->
                    Attributes = [{target_bucket, "TargetBucket", text},
                                  {target_prefix, "TargetPrefix", text},
                                  {target_trants, "TargetGrants/Grant", fun extract_acl/1}],
                    [{enabled, 'true'}|kz_aws_xml:decode(Attributes, LoggingEnabled)]
            end;
        request_payment ->
            case kz_aws_xml:get_text("/RequestPaymentConfiguration/Payer", Doc) of
                "Requester" -> requester;
                _           -> bucket_owner
            end;
        versioning ->
            case kz_aws_xml:get_text("/VersioningConfiguration/Status", Doc) of
                "Enabled"   -> enabled;
                "Suspended" -> suspended;
                _           -> disabled
            end
    end.

extract_acl(ACL) ->
    [extract_grant(Item) || Item <- ACL].

extract_grant(Node) ->
    [{grantee, extract_user(xmerl_xpath:string("Grantee", Node))},
     {permission, decode_permission(kz_aws_xml:get_text("Permission", Node))}].

encode_permission(full_control) -> "FULL_CONTROL";
encode_permission(write)        -> "WRITE";
encode_permission(write_acp)    -> "WRITE_ACP";
encode_permission(read)         -> "READ";
encode_permission(read_acp) -> "READ_ACP".

decode_permission("FULL_CONTROL") -> full_control;
decode_permission("WRITE")        -> write;
decode_permission("WRITE_ACP")    -> write_acp;
decode_permission("READ")         -> read;
decode_permission("READ_ACP")     -> read_acp.

-spec get_object(string(), string()) -> {'ok', kz_proplist()} | {'error', any()} .

get_object(BucketName, Key) ->
    get_object(BucketName, Key, []).

-spec get_object(string(), string(), kz_proplist() | aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

get_object(BucketName, Key, #aws_config{} = Config) ->
    get_object(BucketName, Key, [], Config);

get_object(BucketName, Key, Options) ->
    get_object(BucketName, Key, Options, default_config()).

-spec get_object(string(), string(), kz_proplist(), aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

get_object(BucketName, Key, Options, Config) ->
    RequestHeaders = [{"Range", props:get_value(range, Options)},
                      {"If-Modified-Since", props:get_value(if_modified_since, Options)},
                      {"If-Unmodified-Since", props:get_value(if_unmodified_since, Options)},
                      {"If-Match", props:get_value(if_match, Options)},
                      {"If-None-Match", props:get_value(if_none_match, Options)}],
    Subresource = case props:get_value(version_id, Options) of
                      undefined -> "";
                      Version   -> ["versionId=", Version]
                  end,
    case s3_request(Config, get, BucketName, [$/|Key], Subresource, [], <<>>, RequestHeaders) of
        {'ok', {Headers, Body}} ->
            Props = props:filter_undefined(
                      [{etag, props:get_value("etag", Headers)}
                       ,{content_length, props:get_value("content-length", Headers)}
                       ,{content_type, props:get_value("content-type", Headers)}
                       ,{content_encoding, props:get_value("content-encoding", Headers)}
                       ,{delete_marker, list_to_existing_atom(props:get_value("x-amz-delete-marker", Headers, "false"))}
                       ,{version_id, props:get_value("x-amz-version-id", Headers, "null")}
                       ,{headers, Headers}
                       ,{content, Body}
                           | extract_metadata(Headers)
                      ]),
            {'ok', Props};
        {'error', _}=E -> E
    end.

-spec get_object_acl(string(), string()) -> kz_proplist().

get_object_acl(BucketName, Key) ->
    get_object_acl(BucketName, Key, default_config()).

-spec get_object_acl(string(), string(), kz_proplist() | aws_config()) -> kz_proplist().

get_object_acl(BucketName, Key, #aws_config{} = Config) ->
    get_object_acl(BucketName, Key, [], Config);

get_object_acl(BucketName, Key, Options) ->
    get_object_acl(BucketName, Key, Options, default_config()).

-spec get_object_acl(string(), string(), kz_proplist(), aws_config()) -> kz_proplist().

get_object_acl(BucketName, Key, Options, Config)
  when is_list(BucketName), is_list(Key), is_list(Options) ->
    Subresource = case props:get_value(version_id, Options) of
                      undefined -> "";
                      Version   -> ["&versionId=", Version]
                  end,
    Doc = s3_xml_request(Config, get, BucketName, [$/|Key], "acl" ++ Subresource, [], <<>>, []),
    Attributes = [{owner, "Owner", fun extract_user/1},
                  {access_control_list, "AccessControlList/Grant", fun extract_acl/1}],
    kz_aws_xml:decode(Attributes, Doc).

-spec get_object_metadata(string(), string()) ->
          {'ok', kz_proplist()} | {'error', any()} .

get_object_metadata(BucketName, Key) ->
    get_object_metadata(BucketName, Key, []).

-spec get_object_metadata(string(), string(), kz_proplist() | aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

get_object_metadata(BucketName, Key, #aws_config{} = Config) ->
    get_object_metadata(BucketName, Key, [], Config);

get_object_metadata(BucketName, Key, Options) ->
    get_object_metadata(BucketName, Key, Options, default_config()).

-spec get_object_metadata(string(), string(), kz_proplist(), kz_proplist() | aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

get_object_metadata(BucketName, Key, Options, Config) ->
    RequestHeaders = [{"If-Modified-Since", props:get_value(if_modified_since, Options)},
                      {"If-Unmodified-Since", props:get_value(if_unmodified_since, Options)},
                      {"If-Match", props:get_value(if_match, Options)},
                      {"If-None-Match", props:get_value(if_none_match, Options)}],
    Subresource = case props:get_value(version_id, Options) of
                      undefined -> "";
                      Version   -> ["versionId=", Version]
                  end,
    case s3_request(Config, head, BucketName, [$/|Key], Subresource, [], <<>>, RequestHeaders) of
        {'ok', {Headers, _Body}} ->
            Props = props:filter_undefined(
                      [{last_modified, props:get_value("last-modified", Headers)}
                       ,{etag, props:get_value("etag", Headers)}
                       ,{content_length, props:get_value("content-length", Headers)}
                       ,{content_type, props:get_value("content-type", Headers)}
                       ,{content_encoding, props:get_value("content-encoding", Headers)}
                       ,{delete_marker, list_to_existing_atom(props:get_value("x-amz-delete-marker", Headers, "false"))}
                       ,{version_id, props:get_value("x-amz-version-id", Headers, "false")}|extract_metadata(Headers)
                      ]),
            {'ok', Props};
        {'error', _}=E -> E
    end.

extract_metadata(Headers) ->
    [{Key, Value} || {Key = "x-amz-meta-" ++ _, Value} <- Headers].

-spec get_object_torrent(string(), string()) ->
          {'ok', kz_proplist()} | {'error', any()} .

get_object_torrent(BucketName, Key) ->
    get_object_torrent(BucketName, Key, default_config()).

-spec get_object_torrent(string(), string(), aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

get_object_torrent(BucketName, Key, Config) ->
    case s3_request(Config, get, BucketName, [$/|Key], "torrent", [], <<>>, []) of
        {'ok', {Headers, Body}} ->
            Props = props:filter_undefined(
                      [{delete_marker, list_to_existing_atom(props:get_value("x-amz-delete-marker", Headers, "false"))}
                       ,{version_id, props:get_value("x-amz-version-id", Headers, "null")}
                       ,{torrent, Body}
                      ]),
            {'ok', Props};
        {'error', _}=E -> E
    end.

-spec list_object_versions(string()) -> kz_proplist().

list_object_versions(BucketName) ->
    list_object_versions(BucketName, []).

-spec list_object_versions(string(), kz_proplist() | aws_config()) -> kz_proplist().

list_object_versions(BucketName, #aws_config{} = Config) ->
    list_object_versions(BucketName, [], Config);

list_object_versions(BucketName, Options) ->
    list_object_versions(BucketName, Options, default_config()).

-spec list_object_versions(string(), kz_proplist(), aws_config()) -> kz_proplist().

list_object_versions(BucketName, Options, Config)
  when is_list(BucketName), is_list(Options) ->
    Params = [{"delimiter", props:get_value(delimiter, Options)},
              {"key-marker", props:get_value(key_marker, Options)},
              {"max-keys", props:get_value(max_keys, Options)},
              {"prefix", props:get_value(prefix, Options)},
              {"version-id-marker", props:get_value(version_id_marker, Options)}],
    Doc = s3_xml_request(Config, get, BucketName, "/", "versions", Params, <<>>, []),
    Attributes = [{name, "Name", text},
                  {prefix, "Prefix", text},
                  {key_marker, "KeyMarker", text},
                  {next_key_marker, "NextKeyMarker", optional_text},
                  {version_id_marker, "VersionIdMarker", text},
                  {next_version_id_marker, "NextVersionIdMarker", optional_text},
                  {max_keys, "MaxKeys", integer},
                  {is_truncated, "Istruncated", boolean},
                  {versions, "Version", fun extract_versions/1},
                  {delete_markers, "DeleteMarker", fun extract_delete_markers/1}],
    kz_aws_xml:decode(Attributes, Doc).

extract_versions(Nodes) ->
    [extract_version(Node) || Node <- Nodes].

extract_version(Node) ->
    Attributes = [{key, "Key", text},
                  {version_id, "VersionId", text},
                  {is_latest, "IsLatest", boolean},
                  {etag, "ETag", text},
                  {size, "Size", integer},
                  {owner, "Owner", fun extract_user/1},
                  {storage_class, "StorageClass", text},
                  {last_modified, "LastModified", time}],
    kz_aws_xml:decode(Attributes, Node).

extract_delete_markers(Nodes) ->
    [extract_delete_marker(Node) || Node <- Nodes].

extract_delete_marker(Node) ->
    Attributes = [{key, "Key", text},
                  {version_id, "VersionId", text},
                  {is_latest, "IsLatest", boolean},
                  {owner, "Owner", fun extract_user/1}],
    kz_aws_xml:decode(Attributes, Node).

extract_bucket(Node) ->
    kz_aws_xml:decode([{name, "Name", text},
                         {creation_date, "CreationDate", time}],
                        Node).

-spec put_object(string(), string(), iodata()) ->
          {'ok', kz_proplist()} | {'error', any()} .

put_object(BucketName, Key, Value) ->
    put_object(BucketName, Key, Value, []).

-spec put_object(string(), string(), iodata(), kz_proplist() | aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

put_object(BucketName, Key, Value, #aws_config{} = Config) ->
    put_object(BucketName, Key, Value, [], Config);

put_object(BucketName, Key, Value, Options) ->
    put_object(BucketName, Key, Value, Options, default_config()).

-spec put_object(string(), string(), iodata(), kz_proplist(), [{string(), string()}] | aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

put_object(BucketName, Key, Value, Options, #aws_config{} = Config) ->
    put_object(BucketName, Key, Value, Options, [], Config);

put_object(BucketName, Key, Value, Options, HTTPHeaders) ->
    put_object(BucketName, Key, Value, Options, HTTPHeaders, default_config()).

-spec put_object(string(), string(), iodata(), kz_proplist(), [{string(), string()}], aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

put_object(BucketName, Key, Value, Options, HTTPHeaders, Config)
  when is_list(BucketName)
       andalso is_list(Key)
       andalso (is_list(Value)
                orelse is_binary(Value)
               )
       andalso is_list(Options) ->
    RequestHeaders = [{"x-amz-acl", encode_acl(props:get_value('acl', Options))}
                      |HTTPHeaders
                     ]
        ++ [{"x-amz-meta-" ++ string:to_lower(MKey), MValue} ||
               {MKey, MValue} <- props:get_value('meta', Options, [])
           ],
    POSTData = iolist_to_binary(Value),
    case s3_request(Config, 'put', BucketName, [$/|Key], "", [], POSTData, RequestHeaders) of
        {'ok', {Headers, _Body}} -> {'ok', Headers};
        {'error', _}=E -> E
    end.

-spec set_object_acl(string(), string(), kz_proplist()) ->
          {'ok', kz_proplist()} | {'error', any()} .

set_object_acl(BucketName, Key, ACL) ->
    set_object_acl(BucketName, Key, ACL, default_config()).

-spec set_object_acl(string(), string(), kz_proplist(), aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

set_object_acl(BucketName, Key, ACL, Config)
  when is_list(BucketName), is_list(Key), is_list(ACL) ->
    Id = props:get_value(id, props:get_value(owner, ACL)),
    DisplayName = props:get_value(display_name, props:get_value(owner, ACL)),
    ACL1 = props:get_value(access_control_list, ACL),
    XML = {'AccessControlPolicy',
           [{'Owner', [{'ID', [Id]}, {'DisplayName', [DisplayName]}]},
            {'AccessControlList', encode_grants(ACL1)}]},
    XMLText = list_to_binary(xmerl:export_simple([XML], xmerl_xml)),
    s3_simple_request(Config, put, BucketName, [$/|Key], "acl", [], XMLText, [{"content-type", "application/xml"}]).

-spec sign_get(integer(), string(), string(), aws_config()) -> {binary(), string()}.
sign_get(Expire_time, BucketName, Key, Config)
  when is_integer(Expire_time), is_list(BucketName), is_list(Key) ->
    {Mega, Sec, _Micro} = os:timestamp(),
    Datetime = (Mega * 1000000) + Sec,
    Expires = integer_to_list(Expire_time + Datetime),
    SecurityTokenToSign = case Config#aws_config.security_token of
        undefined -> "";
        SecurityToken -> "x-amz-security-token:" ++ SecurityToken ++ "\n"
    end,
    To_sign = lists:flatten(["GET\n\n\n", Expires, "\n", SecurityTokenToSign, "/", BucketName, "/", Key]),
    Sig = base64:encode(kz_att_util:sha_mac(Config#aws_config.secret_access_key, To_sign)),
    {Sig, Expires}.

-spec make_link(integer(), string(), string()) -> {integer(), string(), string()}.

make_link(Expire_time, BucketName, Key) ->
    make_link(Expire_time, BucketName, Key, default_config()).

-spec make_link(integer(), string(), string(), aws_config()) -> {integer(), string(), string()}.

make_link(Expire_time, BucketName, Key, Config) ->
    EncodedKey = kz_aws_http:url_encode_loose(Key),
    {Sig, Expires} = sign_get(Expire_time, BucketName, EncodedKey, Config),
    Host = lists:flatten([Config#aws_config.s3_scheme, BucketName, ".", Config#aws_config.s3_host, port_spec(Config)]),
    SecurityTokenQS = case Config#aws_config.security_token of
        undefined -> "";
        SecurityToken -> "&x-amz-security-token=" ++ kz_aws_http:url_encode(SecurityToken)
    end,
    URI = lists:flatten(["/", EncodedKey, "?AWSAccessKeyId=", kz_aws_http:url_encode(Config#aws_config.access_key_id), "&Signature=", kz_aws_http:url_encode(Sig), "&Expires=", Expires, SecurityTokenQS]),
    {list_to_integer(Expires),
     binary_to_list(erlang:iolist_to_binary(Host)),
     binary_to_list(erlang:iolist_to_binary(URI))}.

-spec get_object_url(string(), string()) -> string().

 get_object_url(BucketName, Key) ->
  get_object_url(BucketName, Key, default_config()).

-spec get_object_url(string(), string(), aws_config()) -> string().

 get_object_url(BucketName, Key, Config) ->
  case Config#aws_config.s3_bucket_after_host of
      'false' -> lists:flatten([Config#aws_config.s3_scheme, BucketName, ".", Config#aws_config.s3_host, port_spec(Config), "/", Key]);
      'true'  -> lists:flatten([Config#aws_config.s3_scheme, Config#aws_config.s3_host, port_spec(Config), "/", BucketName, "/", Key])
  end.

-spec make_get_url(integer(), string(), string()) -> iolist().

make_get_url(Expire_time, BucketName, Key) ->
    make_get_url(Expire_time, BucketName, Key, default_config()).

-spec make_get_url(integer(), string(), string(), aws_config()) -> iolist().

make_get_url(Expire_time, BucketName, Key, Config) ->
    {Sig, Expires} = sign_get(Expire_time, BucketName, kz_aws_http:url_encode_loose(Key), Config),
    SecurityTokenQS = case Config#aws_config.security_token of
        undefined -> "";
        SecurityToken -> "&x-amz-security-token=" ++ kz_aws_http:url_encode(SecurityToken)
    end,
    lists:flatten([get_object_url(BucketName, Key, Config),
     "?AWSAccessKeyId=", kz_aws_http:url_encode(Config#aws_config.access_key_id),
     "&Signature=", kz_aws_http:url_encode(Sig),
     "&Expires=", Expires,
     SecurityTokenQS]).

-spec start_multipart(string(), string()) -> {'ok', kz_proplist()} | {'error', any()}.
start_multipart(BucketName, Key)
  when is_list(BucketName), is_list(Key) ->
    start_multipart(BucketName, Key, [], [], default_config()).

-spec start_multipart(string(), string(), kz_proplist(), [{string(), string()}], aws_config()) -> {'ok', kz_proplist()} | {'error', any()}.
start_multipart(BucketName, Key, Options, HTTPHeaders, #aws_config{} = Config)
  when is_list(BucketName), is_list(Key), is_list(Options), is_list(HTTPHeaders) ->

    RequestHeaders = [{"x-amz-acl", encode_acl(props:get_value(acl, Options))}|HTTPHeaders]
        ++ [{"x-amz-meta-" ++ string:to_lower(MKey), MValue} ||
               {MKey, MValue} <- props:get_value(meta, Options, [])],
    POSTData = <<>>,
    case s3_xml_request2(Config, post, BucketName, [$/|Key], "uploads", [],
                         POSTData, RequestHeaders) of
        {'ok', Doc} ->
            Attributes = [{uploadId, "UploadId", text}],
            {'ok', kz_aws_xml:decode(Attributes, Doc)};

        Error ->
            Error
    end.

-spec upload_part(string(), string(), string(), integer(), iodata()) -> {'ok', kz_proplist()} | {'error', any()}.
upload_part(BucketName, Key, UploadId, PartNumber, Value) ->
    upload_part(BucketName, Key, UploadId, PartNumber, Value, [], default_config()).

-spec upload_part(string(), string(), string(), integer(), iodata(), [{string(), string()}], aws_config()) -> {'ok', kz_proplist()} | {'error', any()}.
upload_part(BucketName, Key, UploadId, PartNumber, Value, HTTPHeaders, #aws_config{} = Config)
  when is_list(BucketName)
       andalso is_list(Key)
       andalso is_list(UploadId)
       andalso is_integer(PartNumber)
       andalso (is_list(Value)
                orelse is_binary(Value)
               )
       andalso is_list(HTTPHeaders) ->
    POSTData = iolist_to_binary(Value),
    case s3_request(Config
                   ,'put'
                   ,BucketName
                   ,[$/|Key]
                   ,[]
                   ,[{"uploadId", UploadId},
                     {"partNumber", integer_to_list(PartNumber)}
                    ]
                   ,POSTData
                   ,HTTPHeaders
                   )
    of
        {'ok', {Headers, _Body}} ->
            {'ok', [{'etag', props:get_value("etag", Headers)}]};
        Error ->
            Error
    end.

-spec complete_multipart(string(), string(), string(), [{integer(), string()}]) -> {'ok', kz_proplist()} | {'error', any()}.
complete_multipart(BucketName, Key, UploadId, ETags)
  when is_list(BucketName), is_list(Key), is_list(UploadId), is_list(ETags) ->
    complete_multipart(BucketName, Key, UploadId, ETags, [], default_config()).

-spec complete_multipart(string(), string(), string(), [{integer(), string()}], [{string(), string()}], aws_config()) -> 'ok' | {'error', any()}.
complete_multipart(BucketName, Key, UploadId, ETags, HTTPHeaders, #aws_config{} = Config)
  when is_list(BucketName), is_list(Key), is_list(UploadId), is_list(ETags), is_list(HTTPHeaders) ->
    POSTData = list_to_binary(xmerl:export_simple([{'CompleteMultipartUpload',
                                                    [{'Part',
                                                      [{'PartNumber', [integer_to_list(Num)]},
                                                       {'ETag', [ETag]}] } || {Num, ETag} <- ETags]}], 'xmerl_xml')),

    case s3_request(Config
                   ,'post'
                   ,BucketName
                   ,[$/|Key]
                   ,[]
                   ,[{"uploadId", UploadId}]
                   ,POSTData
                   ,HTTPHeaders
                   )
    of
        {'ok', {_Headers, _Body}} ->
            'ok';
        Error ->
            Error
    end.

-spec abort_multipart(string(), string(), string()) -> 'ok' | {'error', any()}.
abort_multipart(BucketName, Key, UploadId)
  when is_list(BucketName), is_list(Key), is_list(UploadId) ->
    abort_multipart(BucketName, Key, UploadId, [], [], default_config()).

-spec abort_multipart(string(), string(), string(), kz_proplist(), [{string(), string()}], aws_config()) -> 'ok' | {'error', any()}.
abort_multipart(BucketName, Key, UploadId, Options, HTTPHeaders, #aws_config{} = Config)
  when is_list(BucketName), is_list(Key), is_list(UploadId), is_list(Options), is_list(HTTPHeaders) ->

    case s3_request(Config, delete, BucketName, [$/|Key], [], [{"uploadId", UploadId}],
                     <<>>, HTTPHeaders) of
        {'ok', _} ->
            'ok';
        Error ->
            Error
    end.

-spec list_multipart_uploads(string()) -> {'ok', kz_proplist()} | {'error', any()}.
list_multipart_uploads(BucketName)
  when is_list(BucketName) ->

    list_multipart_uploads(BucketName, [], [], default_config()).

-spec list_multipart_uploads(string(), kz_proplist()) -> {'ok', kz_proplist()} | {'error', any()}.
list_multipart_uploads(BucketName, Options)
  when is_list(BucketName), is_list(Options) ->

    list_multipart_uploads(BucketName, Options, [], default_config()).

-spec list_multipart_uploads(string(), kz_proplist(), [{string(), string()}], aws_config()) ->
                                    {'ok', kz_proplist()} |
                                    {'error', any()}.
list_multipart_uploads(BucketName, Options, HTTPHeaders, #aws_config{} = Config)
  when is_list(BucketName), is_list(Options), is_list(HTTPHeaders) ->

    Params = [{"uploads", ""}
             ,{"delimiter", props:get_value('delimiter', Options)}
             ,{"prefix", props:get_value('prefix', Options)}
             ,{"max-uploads", props:get_value('max_uploads', Options)}
             ,{"key-marker", props:get_value('key_marker', Options)}
             ,{"upload-id-marker", props:get_value('upload_id_marker', Options)}
             ],

    case s3_xml_request2(Config, 'get', BucketName, "/", "", Params, <<>>, HTTPHeaders) of
        {'ok', Xml} ->
            Uploads = [kz_aws_xml:decode([{'key', "Key", 'text'}
                                          ,{'uploadId', "UploadId", 'text'}
                                         ], Node)
                       || Node <- xmerl_xpath:string("/ListMultipartUploadsResult/Upload", Xml)
                      ],

            CommonPrefixes = [kz_aws_xml:get_text("Prefix", Node) || Node <- xmerl_xpath:string("/ListMultipartUploadsResult/CommonPrefixes", Xml)],

            {'ok', [{'uploads', Uploads}
                    ,{'common_prefixes', CommonPrefixes}
                   ]};
        Error ->
            Error
    end.


-spec set_bucket_attribute(string(), atom(), term()) ->
          {'ok', kz_proplist()} | {'error', any()} .

set_bucket_attribute(BucketName, AttributeName, Value) ->
    set_bucket_attribute(BucketName, AttributeName, Value, default_config()).

-spec set_bucket_attribute(string(), atom(), term(), aws_config()) ->
          {'ok', kz_proplist()} | {'error', any()} .

set_bucket_attribute(BucketName, AttributeName, Value, Config)
  when is_list(BucketName) ->
    {Subresource, XML} =
        case AttributeName of
            acl ->
                ACLXML = {'AccessControlPolicy',
                          [{'Owner',
                            [{'ID', [props:get_value('id', props:get_value('owner', Value))]},
                             {'DisplayName', [props:get_value('display_name', props:get_value('owner', Value))]}]},
                           {'AccessControlList', encode_grants(props:get_value('access_control_list', Value))}]},
                {"acl", ACLXML};
            logging ->
                LoggingXML = {'BucketLoggingStatus',
                              [{'xmlns:xsi', ?XMLNS_S3}],
                              case props:get_is_true('enabled', Value) of
                                  'true' ->
                                      [{'LoggingEnabled',
                                        [
                                         {'TargetBucket', [props:get_value('target_bucket', Value)]},
                                         {'TargetPrefix', [props:get_value('target_prefix', Value)]},
                                         {'TargetGrants', encode_grants(props:get_value('target_grants', Value, []))}
                                        ]
                                       }];
                                  'false' ->
                                      []
                              end},
                {"logging", LoggingXML};
            'request_payment' ->
                PayerName = case Value of
                                'requester' -> "Requester";
                                'bucket_owner' -> "BucketOwner"
                            end,
                RPXML = {'RequestPaymentConfiguration', [{'xmlns:xsi', ?XMLNS_S3}],
                         [
                          {'Payer', [PayerName]}
                         ]
                        },
                {"requestPayment", RPXML};
            'versioning' ->
                Status = case props:get_value('status', Value) of
                             'suspended' -> "Suspended";
                             'enabled' -> "Enabled"
                         end,
                MFADelete = case props:get_value('mfa_delete', Value, 'disabled') of
                                'enabled' -> "Enabled";
                                'disabled' -> "Disabled"
                            end,
                VersioningXML = {'VersioningConfiguration', [{'xmlns:xsi', ?XMLNS_S3}],
                                 [{'Status', [Status]},
                                  {'MfaDelete', [MFADelete]}
                                 ]
                                },
                {"versioning", VersioningXML}
        end,
    POSTData = list_to_binary(xmerl:export_simple([XML], 'xmerl_xml')),
    Headers = [{"content-type", "application/xml"}],
    s3_simple_request(Config, 'put', BucketName, "/", Subresource, [], POSTData, Headers).

%%% See http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html and
%%%   http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAPI.html for info on
%%%   addressing
-spec get_bucket_and_key(string()) -> {string(), string()}.

get_bucket_and_key(Uri) ->
  {'ok', Parsed} = http_uri:parse(Uri),
  {Host, Path} = extract_host_and_path(Parsed),
  extract_location_fields(Host, Path).

extract_host_and_path({_Scheme, _UserInfo, Host, _Port, Path, _Query}) ->
  {Host, Path}.

extract_location_fields(Host, Path) ->
  HostTokens = string:tokens(Host, "."),
  extract_bucket_and_key(HostTokens, Path).

extract_bucket_and_key([Bucket, _S3, _AmazonAWS, _Com], [$/ | Key]) ->
  %% Virtual-hosted-style URL
  %% For example: bucket_name.s3.amazonaws.com/path/to/key
  {Bucket, Key};
extract_bucket_and_key([_S3, _AmazonAWS, _Com], [$/ | BucketAndKey]) ->
  %% Path-style URL
  %% For example: s3.amazonaws.com/bucket_name/path/to/key
  [Bucket, Key] = re:split(BucketAndKey, "/", [{'return', 'list'}, {'parts', 2}]),
  {Bucket, Key}.

encode_grants(Grants) ->
    [encode_grant(Grant) || Grant <- Grants].

encode_grant(Grant) ->
    Grantee = props:get_value('grantee', Grant),
    {'Grant',
     [encode_grantee(Grantee),
      {'Permission', [encode_permission(props:get_value('permission', Grant))]}]}.

encode_grantee(Grantee) ->
  case props:get_value('id', Grantee) of
      'undefined' ->
          {'Grantee', [{'xmlns:xsi', ?XMLNS_S3}, {'xsi:type', "Group"}]
           ,[{'URI', [props:get_value('uri', Grantee)]}]
          };
      Id ->
          {'Grantee'
          ,[{'xmlns:xsi', ?XMLNS_S3}, {'xsi:type', "CanonicalUser"}]
          ,[{'ID', [Id]}
            ,{'DisplayName', [props:get_value('display_name', Grantee)]}
           ]
          }
  end.

s3_simple_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) ->
    case s3_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) of
        {'ok', {Headers, <<>>}} -> {'ok', Headers};
        {'ok', {Headers, Body}} ->
            XML = element(1,xmerl_scan:string(binary_to_list(Body))),
            case XML of
                #xmlElement{name='Error'} ->
                    ErrCode = kz_aws_xml:get_text("/Error/Code", XML),
                    ErrMsg = kz_aws_xml:get_text("/Error/Message", XML),
                    {'error', {'s3_error', ErrCode, ErrMsg}};
                _ -> {'ok', [{'body', Body} | Headers]}
            end;
        {'error', _}=E -> E
    end.

s3_xml_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) ->
    case s3_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) of
        {'ok', {_Headers, Body}} ->
            XML = element(1,xmerl_scan:string(binary_to_list(Body))),
            case XML of
                #xmlElement{name='Error'} ->
                    ErrCode = kz_aws_xml:get_text("/Error/Code", XML),
                    ErrMsg = kz_aws_xml:get_text("/Error/Message", XML),
                    {'error', {'s3_error', ErrCode, ErrMsg}};
                _ ->
                    XML
            end;
        {'error', _}=E -> E
    end.

s3_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) ->
    case kz_aws:update_config(Config) of
        {'ok', Config1} ->
            s3_request2_no_update(Config1, Method, Host, Path, Subresource, Params, POSTData, Headers);
        {'error', _Reason}=Error ->
            Error
    end.

s3_xml_request2(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) ->
    case s3_request(Config, Method, Host, Path, Subresource, Params, POSTData, Headers) of
        {'ok', {_Headers, Body}} ->
            XML = element(1,xmerl_scan:string(binary_to_list(Body))),
            case XML of
                #xmlElement{name='Error'} ->
                    ErrCode = kz_aws_xml:get_text("/Error/Code", XML),
                    ErrMsg = kz_aws_xml:get_text("/Error/Message", XML),
                    {'error', {'s3_error', ErrCode, ErrMsg}};
                _ ->
                    {'ok', XML}
            end;
        Error ->
            Error
    end.

s3_request2_no_update(Config, Method, Host, Path, Subresource, Params, Body, Headers0) ->
    ContentType = props:get_value("content-type", Headers0, ""),
    ContentMD5 = case Body of
                     <<>> ->
                         "";
                     _ ->
                         base64:encode(kz_att_util:md5(Body))
                 end,
    Headers = case Config#aws_config.security_token of
                  'undefined' -> Headers0;
                  Token when is_list(Token) -> [{"x-amz-security-token", Token} | Headers0]
              end,
    FHeaders = props:filter_undefined(Headers),
    AmzHeaders = [Header || {"x-amz-" ++ _, _} = Header <- FHeaders],
    Date = httpd_util:rfc1123_date(erlang:localtime()),
    EscapedPath = kz_aws_http:url_encode_loose(Path),
    Authorization = make_authorization(Config, Method, ContentMD5, ContentType
                                       ,Date, AmzHeaders, Host, EscapedPath, Subresource, Params
                                      ),
    RequestHeaders = [{"date", Date}, {"authorization", Authorization}|FHeaders] ++
        case ContentMD5 of
            "" -> [];
            _ -> [{"content-md5", binary_to_list(ContentMD5)}]
        end,
    HostURI = case Config#aws_config.s3_bucket_after_host of
                  'false' ->
                      [case Host of "" -> ""; _ -> [Host, $.] end
                      ,Config#aws_config.s3_host, port_spec(Config)
                      ];
                  'true'  ->
                      [Config#aws_config.s3_host
                      ,port_spec(Config)
                      ,case Host of "" -> ""; _ -> [$/, Host] end
                      ]
              end,
    RequestURI = lists:flatten([Config#aws_config.s3_scheme
                               ,HostURI
                               ,EscapedPath
                               ,case Subresource of "" -> ""; _ -> [$?, Subresource] end
                               ,if
                                    Params =:= [] -> "";
                                    Subresource =:= "" ->
                                        [$?, kz_aws_http:make_query_string(Params, 'no_assignment')];
                                    'true' ->
                                        [$&, kz_aws_http:make_query_string(Params, 'no_assignment')]
                                end
                               ]),

    Request = #aws_request{service = 's3'
                          ,uri = RequestURI
                          ,method = Method
                          },
    Request2 = case Method of
                   M when M =:= 'get'
                          orelse M =:= 'head'
                          orelse M =:= 'delete' ->
                       Request#aws_request{request_headers = RequestHeaders
                                          ,request_body = <<>>
                                          };
                   _ ->
                       Headers2 = case lists:keyfind("content-type", 1, RequestHeaders) of
                                      'false' ->
                                          [{"content-type", ContentType}
                                           | RequestHeaders
                                          ];
                                      _ ->
                                          RequestHeaders
                                  end,
                       Request#aws_request{request_headers = Headers2
                                           ,request_body = Body
                                          }
               end,
    Request3 = kz_aws_retry:request(Config, Request2, fun s3_result_fun/1),
    kz_aws:request_to_return(Request3).

s3_result_fun(#aws_request{response_type = 'ok'} = Request) ->
    Request;
s3_result_fun(#aws_request{response_type = 'error'
                           ,error_type = 'aws'
                           ,response_status = Status
                          } = Request)
  when Status >= 500 ->
    Request#aws_request{should_retry = 'true'};
s3_result_fun(#aws_request{response_type = 'error'
                          ,error_type = 'aws'
                          } = Request) ->
    Request#aws_request{should_retry = 'false'}.

make_authorization(Config, Method, ContentMD5, ContentType, Date, AmzHeaders,
                   Host, Resource, Subresource, Params) ->
    CanonizedAmzHeaders =
        [[Name, $:, Value, $\n] || {Name, Value} <- lists:sort(AmzHeaders)],

    SubResourcesToInclude = ["acl", "lifecycle", "location", "logging", "notification", "partNumber", "policy", "requestPayment", "torrent", "uploadId", "uploads", "versionId", "versioning", "versions", "website"],
    FilteredParams = [NV
                      || {Name, _Value}=NV <- Params,
                         lists:member(Name, SubResourcesToInclude)
                     ],

    ParamsQueryString = kz_aws_http:make_query_string(lists:keysort(1, FilteredParams),
                                                      'no_assignment'
                                                     ),
    StringToSign = [string:to_upper(atom_to_list(Method)), $\n
                    ,ContentMD5, $\n
                    ,ContentType, $\n
                    ,Date, $\n
                    ,CanonizedAmzHeaders
                    ,case Host of "" -> ""; _ -> [$/, Host] end
                    ,Resource
                    ,case Subresource of "" -> ""; _ -> [$?, Subresource] end
                    ,if
                         ParamsQueryString =:= "" -> "";
                         Subresource =:= "" -> [$?, ParamsQueryString];
                         'true' -> [$&, ParamsQueryString]
                     end
                   ],
    Signature = base64:encode(kz_att_util:sha_mac(Config#aws_config.secret_access_key, StringToSign)),
    ["AWS ", Config#aws_config.access_key_id, $:, Signature].

default_config() -> kz_aws:default_config().

port_spec(#aws_config{s3_port=80}) ->
    "";
port_spec(#aws_config{s3_port=Port}) ->
    [":", erlang:integer_to_list(Port)].
