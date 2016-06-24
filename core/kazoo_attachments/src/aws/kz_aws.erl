%% @author root
%% @doc @todo Add description to kz_aws.


-module(kz_aws).

-export([aws_request/5, aws_request/6, aws_request/7, aws_request/8,
         aws_request_xml/5, aws_request_xml/6, aws_request_xml/7, aws_request_xml/8,
         aws_request2/7,
         aws_request_xml2/5, aws_request_xml2/7,
         aws_request4/8,
         aws_request_xml4/6, aws_request_xml4/8,
         aws_region_from_host/1,
         aws_request_form/8,
         param_list/2, default_config/0, update_config/1,
         configure/1, format_timestamp/1,
         http_headers_body/1,
         request_to_return/1,
         sign_v4_headers/5,
         sign_v4/8,
         get_service_status/1]).

-include("kz_aws.hrl").

-record(metadata_credentials,
        {access_key_id :: string(),
         secret_access_key :: string(),
         security_token=undefined :: string(),
         expiration_gregorian_seconds :: integer()
        }).

aws_request_xml(Method, Host, Path, Params, #aws_config{} = Config) ->
    Body = aws_request(Method, Host, Path, Params, Config),
    element(1, xmerl_scan:string(binary_to_list(Body))).
aws_request_xml(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(binary_to_list(Body))).
aws_request_xml(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    Body = aws_request(Method, Protocol, Host, Port, Path, Params, Config),
    element(1, xmerl_scan:string(binary_to_list(Body))).
aws_request_xml(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey) ->
    Body = aws_request(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey),
    element(1, xmerl_scan:string(binary_to_list(Body))).

aws_request_xml2(Method, Host, Path, Params, #aws_config{} = Config) ->
    aws_request_xml2(Method, undefined, Host, undefined, Path, Params, Config).
aws_request_xml2(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    case aws_request2(Method, Protocol, Host, Port, Path, Params, Config) of
        {ok, Body} ->
            {ok, element(1, xmerl_scan:string(binary_to_list(Body)))};
        {error, Reason} ->
            {error, Reason}
    end.

aws_request_xml4(Method, Host, Path, Params, Service, #aws_config{} = Config) ->
    aws_request_xml4(Method, undefined, Host, undefined, Path, Params, Service, Config).
aws_request_xml4(Method, Protocol, Host, Port, Path, Params, Service, #aws_config{} = Config) ->
    case aws_request4(Method, Protocol, Host, Port, Path, Params, Service, Config) of
        {ok, Body} ->
            {ok, element(1, xmerl_scan:string(binary_to_list(Body)))};
        {error, Reason} ->
            {error, Reason}
    end.

aws_request(Method, Host, Path, Params, #aws_config{} = Config) ->
    aws_request(Method, undefined, Host, undefined, Path, Params, Config).
aws_request(Method, Host, Path, Params, AccessKeyID, SecretAccessKey) ->
    aws_request(Method, undefined, Host, undefined, Path, Params, AccessKeyID, SecretAccessKey).
aws_request(Method, Protocol, Host, Port, Path, Params, #aws_config{} = Config) ->
    case aws_request2(Method, Protocol, Host, Port, Path, Params, Config) of
        {ok, Body} ->
            Body;
        {error, Reason} ->
            erlang:error({aws_error, Reason})
    end.
aws_request(Method, Protocol, Host, Port, Path, Params, AccessKeyID, SecretAccessKey) ->
    aws_request(Method, Protocol, Host, Port, Path, Params,
                #aws_config{access_key_id = AccessKeyID, secret_access_key = SecretAccessKey}).

%% aws_request2 returns {ok, Body} or {error, Reason} instead of throwing as aws_request does
%% This is the preferred pattern for new APIs
aws_request2(Method, Protocol, Host, Port, Path, Params, Config) ->
    case update_config(Config) of
        {ok, Config1} ->
            aws_request2_no_update(Method, Protocol, Host, Port, Path, Params, Config1);
        {error, Reason} ->
            {error, Reason}
    end.

aws_request2_no_update(Method, Protocol, Host, Port, Path, Params,
		       #aws_config{access_key_id = AccessKeyId,
				   secret_access_key = SecretAccessKey,
				   security_token = SecurityToken} = Config) ->
    Timestamp = format_timestamp(erlang:universaltime()),
    QParams = lists:sort(
                [{"Timestamp", Timestamp},
                 {"SignatureVersion", "2"},
                 {"SignatureMethod", "HmacSHA1"},
                 {"AWSAccessKeyId", AccessKeyId}|Params] ++
                    case SecurityToken of
                        undefined -> [];
                        Token -> [{"SecurityToken", Token}]
                    end),

    QueryToSign = kz_aws_http:make_query_string(QParams),
    RequestToSign = [string:to_upper(atom_to_list(Method)), $\n,
                     string:to_lower(Host), $\n, Path, $\n, QueryToSign],
    Signature = base64:encode(kz_att_util:sha_mac(SecretAccessKey, RequestToSign)),

    Query = [QueryToSign, "&Signature=", kz_aws_http:url_encode(Signature)],

    aws_request_form(Method, Protocol, Host, Port, Path, Query, [], Config).

aws_region_from_host(Host) ->
    case string:tokens(Host, ".") of
        %% the aws endpoint can vary depending on the region
        %% we need to account for that:
        %%  us-west-2: s3.us-west-2.amazonaws.com
        %%  cn-north-1 (AWS China): s3.cn-north-1.amazonaws.com.cn
        %% it's assumed that the first element is the aws service (s3, ec2, etc),
        %% the second is the region identifier, the rest is ignored
        %% the exception (of course) is the dynamodb streams which follows a different
        %% format
        ["streams", "dynamodb", Value | _Rest] ->
            Value;
        [_, Value, _, _ | _Rest] ->
            Value;
        _ ->
            "us-east-1"
    end.

aws_request4(Method, Protocol, Host, Port, Path, Params, Service, Config) ->
    case update_config(Config) of
        {ok, Config1} ->
            aws_request4_no_update(Method, Protocol, Host, Port, Path, Params, Service, Config1);
        {error, Reason} ->
            {error, Reason}
    end.

aws_request4_no_update(Method, Protocol, Host, Port, Path, Params, Service, #aws_config{} = Config) ->
    Query = kz_aws_http:make_query_string(Params),
    Region = aws_region_from_host(Host),

    SignedHeaders = case Method of
                        post ->
                            sign_v4(Method, Path, Config,
                                    [{"host", Host}], list_to_binary(Query),
                                    Region, Service, []);
                        get ->
                            sign_v4(Method, Path, Config, [{"host", Host}],
                                    <<>>, Region, Service, Params)
                    end,

    aws_request_form(Method, Protocol, Host, Port, Path, Query, SignedHeaders, Config).


-spec aws_request_form(Method :: atom(), Protocol :: undefined | string(), Host :: string(),
                        Port :: undefined | integer() | string(), Path :: string(), Form :: iodata(),
                        Headers :: list(), Config :: aws_config()) -> {ok, binary()} | {error, tuple()}.
aws_request_form(Method, Protocol, Host, Port, Path, Form, Headers, Config) ->
    UProtocol = case Protocol of
        undefined -> "https://";
        _ -> [Protocol, "://"]
    end,

    URL = case Port of
        undefined -> [UProtocol, Host, Path];
        _ -> [UProtocol, Host, $:, port_to_str(Port), Path]
    end,

    %% Note: httpc MUST be used with {timeout, timeout()} option
    %%       Many timeout related failures is observed at prod env
    %%       when library is used in 24/7 manner
    Response =
        case Method of
            get ->
                Req = lists:flatten([URL, $?, Form]),
                kz_aws_httpc:request(
                  Req, get, Headers, <<>>, timeout(Config), Config);
            _ ->
                kz_aws_httpc:request(
                  lists:flatten(URL), Method,
                  [{<<"content-type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>} | Headers],
                  list_to_binary(Form), timeout(Config), Config)
        end,

    http_body(Response).

param_list([], _Key) -> [];
param_list(Values, Key) when is_tuple(Key) ->
    Seq = lists:seq(1, tuple_size(Key)),
    lists:flatten(
      [[{lists:append([element(J, Key), ".", integer_to_list(I)]),
         element(J, Value)} || J <- Seq] ||
          {I, Value} <- lists:zip(lists:seq(1, length(Values)), Values)]
     );
param_list([[{_, _}|_]|_] = Values, Key) ->
    lists:flatten(
      [[{lists:flatten([Key, $., integer_to_list(I), $., SubKey]),
         value_to_string(Value)} || {SubKey, Value} <- SValues] ||
          {I, SValues} <- lists:zip(lists:seq(1, length(Values)), Values)]
     );
param_list(Values, Key) ->
    [{lists:flatten([Key, $., integer_to_list(I)]), Value} ||
        {I, Value} <- lists:zip(lists:seq(1, length(Values)), Values)].

value_to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value_to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value_to_string(Binary) when is_binary(Binary) -> Binary;
value_to_string(String) when is_list(String) -> String;
value_to_string({{_Yr, _Mo, _Da}, {_Hr, _Min, _Sec}} = Timestamp) -> format_timestamp(Timestamp).

format_timestamp({{Yr, Mo, Da}, {H, M, S}}) ->
    lists:flatten(
      io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0bZ",
                    [Yr, Mo, Da, H, M, S])).

default_config() ->
    case get(aws_config) of
        undefined ->
            AccessKeyId = case os:getenv("AWS_ACCESS_KEY_ID") of
                              false -> undefined;
                              AKI -> AKI
                          end,
            SecretAccessKey = case os:getenv("AWS_SECRET_ACCESS_KEY") of
                                  false -> undefined;
                                  SAC -> SAC
                              end,
            #aws_config{access_key_id = AccessKeyId,
                        secret_access_key = SecretAccessKey};
        Config ->
            Config
    end.

-spec update_config(aws_config()) -> {ok, aws_config()} | {error, term()}.
update_config(#aws_config{access_key_id = KeyId} = Config)
  when is_list(KeyId), KeyId /= [] ->
    %% In order to support caching of the aws_config, we could store the expiration_time
    %% and check it here. If it is about to expire (within 5 minutes is what boto uses)
    %% then we should get the new config.
    {ok, Config};
update_config(#aws_config{} = Config) ->
    %% AccessKey is not set. Try to read from role metadata.
    case get_metadata_credentials(Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, Credentials} ->
            {ok, Config#aws_config {
                   access_key_id = Credentials#metadata_credentials.access_key_id,
                   secret_access_key = Credentials#metadata_credentials.secret_access_key,
                   security_token = Credentials#metadata_credentials.security_token}}
    end.

-spec configure(aws_config()) -> {ok, aws_config()}.

configure(#aws_config{} = Config) ->
    put(aws_config, Config),
    {ok, default_config()}.

-spec get_metadata_credentials(aws_config()) -> {ok, #metadata_credentials{}} | {error, term()}.
get_metadata_credentials(Config) ->
    %% See if we have cached credentials
    case application:get_env(kazoo_attachments, metadata_credentials) of
        {ok, #metadata_credentials{expiration_gregorian_seconds = Expiration} = Credentials} ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            %% Get new credentials if these will expire in less than 5 minutes
            case Expiration - Now < 300 of
                true -> get_credentials_from_metadata(Config);
                false -> {ok, Credentials}
            end;
        undefined ->
            get_credentials_from_metadata(Config)
    end.

timestamp_to_gregorian_seconds(Timestamp) ->
    {ok, [Yr, Mo, Da, H, M, S], []} = io_lib:fread("~d-~d-~dT~d:~d:~dZ", binary_to_list(Timestamp)),
    calendar:datetime_to_gregorian_seconds({{Yr, Mo, Da}, {H, M, S}}).

-spec get_credentials_from_metadata(aws_config())
                                   -> {ok, #metadata_credentials{}} | {error, term()}.
get_credentials_from_metadata(Config) ->
    %% TODO this function should retry on errors getting credentials
    %% First get the list of roles
    case http_body(
           kz_aws_httpc:request(
             "http://169.254.169.254/latest/meta-data/iam/security-credentials/",
             get, [], <<>>, timeout(Config), Config)) of
        {error, Reason} ->
            {error, Reason};
        {ok, Body} ->
            %% Always use the first role
            [Role | _] = binary:split(Body, <<$\n>>),
            case http_body(
                   kz_aws_httpc:request(
                     "http://169.254.169.254/latest/meta-data/iam/security-credentials/" ++
                         binary_to_list(Role),
                     get, [], <<>>, timeout(Config), Config)) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Json} ->
                    Creds = kz_json:decode(Json),
                    Record = #metadata_credentials
                        {access_key_id = binary_to_list(proplists:get_value(<<"AccessKeyId">>, Creds)),
                         secret_access_key = binary_to_list(proplists:get_value(<<"SecretAccessKey">>, Creds)),
                         security_token = binary_to_list(proplists:get_value(<<"Token">>, Creds)),
                         expiration_gregorian_seconds = timestamp_to_gregorian_seconds(
                                                          proplists:get_value(<<"Expiration">>, Creds))},
                    application:set_env(kazoo_attachments, metadata_credentials, Record),
                    {ok, Record}
            end
    end.

port_to_str(Port) when is_integer(Port) ->
    integer_to_list(Port);
port_to_str(Port) when is_list(Port) ->
    Port.

-spec http_body({ok, tuple()} | {error, term()})
               -> {ok, binary()} | {error, tuple()}.
%% Extract the body and do error handling on the return of a httpc:request call.
http_body(Return) ->
    case http_headers_body(Return) of
        {ok, {_, Body}} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end.

-type headers() :: [{string(), string()}].
-spec http_headers_body({ok, tuple()} | {error, term()})
                       -> {ok, {headers(), binary()}} | {error, tuple()}.
%% Extract the headers and body and do error handling on the return of a httpc:request call.
http_headers_body({ok, {{OKStatus, _StatusLine}, Headers, Body}})
  when OKStatus >= 200, OKStatus =< 299 ->
    {ok, {Headers, Body}};
http_headers_body({ok, {{Status, StatusLine}, _Headers, Body}}) ->
    {error, {http_error, Status, StatusLine, Body}};
http_headers_body({error, Reason}) ->
    {error, {socket_error, Reason}}.

timeout(#aws_config{timeout = undefined}) ->
    ?DEFAULT_TIMEOUT;
timeout(#aws_config{timeout = Timeout}) ->
    Timeout.

%% Convert an aws_request record to return value as returned by http_headers_body
request_to_return(#aws_request{response_type = ok,
                               response_headers = Headers,
                               response_body = Body}) ->
    {ok, {[ {string:to_lower(H), V} || {H, V} <- Headers ], Body}};
request_to_return(#aws_request{response_type = error,
                               error_type = httpc,
                               httpc_error_reason = Reason}) ->
    {error, {socket_error, Reason}};
request_to_return(#aws_request{response_type = error,
                               error_type = aws,
                               response_status = Status,
                               response_status_line = StatusLine,
                               response_body = Body}) ->
    {error, {http_error, Status, StatusLine, Body}}.

%% http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html
-spec sign_v4_headers(aws_config(), headers(), binary(), string(), string()) -> headers().
sign_v4_headers(Config, Headers, Payload, Region, Service) ->
    sign_v4(post, "/", Config, Headers, Payload, Region, Service, []).

-spec sign_v4(atom(), list(), aws_config(), headers(), binary(), string(), string(), list()) -> headers().
sign_v4(Method, Uri, Config, Headers, Payload, Region, Service, QueryParams) ->
    Date = iso_8601_basic_time(),
    PayloadHash = hash_encode(Payload),
    Headers1 = [{"x-amz-content-sha256", PayloadHash}, {"x-amz-date", Date} | Headers],
    Headers2 = case Config#aws_config.security_token of
                   undefined -> Headers1;
                   Token -> [{"x-amz-security-token", Token} | Headers1]
               end,
    {Request, SignedHeaders} = canonical_request(Method, Uri, QueryParams, Headers2, PayloadHash),
    CredentialScope = credential_scope(Date, Region, Service),
    ToSign = to_sign(Date, CredentialScope, Request),
    SigningKey = signing_key(Config, Date, Region, Service),
    Signature = base16(kz_att_util:sha256_mac( SigningKey, ToSign)),
    Authorization = authorization(Config, CredentialScope, SignedHeaders, Signature),
    [{"Authorization", lists:flatten(Authorization)} | Headers2].

iso_8601_basic_time() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(os:timestamp()),
    lists:flatten(io_lib:format(
                    "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).

canonical_request(Method, CanonicalURI, QParams, Headers, PayloadHash) ->
    {CanonicalHeaders, SignedHeaders} = canonical_headers(Headers),
    CanonicalQueryString = canonical_query_string(QParams),
    {[string:to_upper(atom_to_list(Method)), $\n,
      CanonicalURI, $\n,
      CanonicalQueryString, $\n,
      CanonicalHeaders, $\n,
      SignedHeaders, $\n,
      PayloadHash],
     SignedHeaders}.

canonical_headers(Headers) ->
    Normalized = [{string:to_lower(Name), trimall(Value)} || {Name, Value} <- Headers],
    Sorted = lists:keysort(1, Normalized),
    Canonical = [[Name, $:, Value, $\n] || {Name, Value} <- Sorted],
    Signed = string:join([Name || {Name, _} <- Sorted], ";"),
    {Canonical, Signed}.

%% @doc calculate canonical query string out of query params and according to v4 documentation
canonical_query_string([]) ->
    "";
canonical_query_string(Params) ->
    Normalized = [{kz_aws_http:url_encode(Name), kz_aws_http:url_encode(kz_aws_http:value_to_string(Value))} || {Name, Value} <- Params],
    Sorted = lists:keysort(1, Normalized),
    string:join([case Value of
                     [] -> [Key, "="];
                     _ -> [Key, "=", Value]
                 end
                 || {Key, Value} <- Sorted, Value =/= none, Value =/= undefined], "&").

trimall(Value) ->
    %% TODO - remove excess internal whitespace in header values
    re:replace(Value, "(^\\s+)|(\\s+$)", "", [global]).

hash_encode(Data) ->
    Hash = kz_att_util:sha256( Data),
    base16(Hash).

base16(Data) ->
    io_lib:format("~64.16.0b", [binary:decode_unsigned(Data)]).

credential_scope(Date, Region, Service) ->
    DateOnly = string:left(Date, 8),
    [DateOnly, $/, Region, $/, Service, "/aws4_request"].

to_sign(Date, CredentialScope, Request) ->
    ["AWS4-HMAC-SHA256\n",
     Date, $\n,
     CredentialScope, $\n,
     hash_encode(Request)].

signing_key(Config, Date, Region, Service) ->
    %% TODO cache the signing key so we don't have to recompute for every request
    DateOnly = string:left(Date, 8),
    KDate = kz_att_util:sha256_mac( "AWS4" ++ Config#aws_config.secret_access_key, DateOnly),
    KRegion = kz_att_util:sha256_mac( KDate, Region),
    KService = kz_att_util:sha256_mac( KRegion, Service),
    kz_att_util:sha256_mac( KService, "aws4_request").

authorization(Config, CredentialScope, SignedHeaders, Signature) ->
    ["AWS4-HMAC-SHA256"
     " Credential=", Config#aws_config.access_key_id, $/, CredentialScope, $,,
     " SignedHeaders=", SignedHeaders, $,,
     " Signature=", Signature].

%% This function fetches http://status.aws.amazon.com/data.json
%% and examine "current" section for on going AWS issues/failures.
%% Example of a return status:
%% [{<<"service_name">>,
%%   <<"Amazon Elastic Compute Cloud (Frankfurt)">>},
%%  {<<"summary">>,<<"[RESOLVED] Internet Connectivity ">>},
%%  {<<"date">>,<<"1436972949">>},
%%  {<<"status">>,1},
%%  {<<"details">>,<<>>},
%%  {<<"description">>,
%%   <<"<div><span class=\"yellowfg\"> 8:22 AM PDT</span>&nbsp;Between 7:55 AM PDT and 8:05 AM PDT we experienced Internet connectivity issues for some instances in the EU-CENTRAL-1 Region. The issue has been resolved and the service is operating normally.</div>">>},
%%  {<<"service">>,<<"ec2-eu-central-1">>}]
%%
%% <<"status">> field values are the following:
%%  0 - service is operating normally;
%%  1 - performance issues;
%%  2 - service disruption.
-spec get_service_status(list(string())) -> ok | list().
get_service_status(ServiceNames) when is_list(ServiceNames) ->
    {ok, Json} = aws_request_form(get, "http", "status.aws.amazon.com", undefined,
        "/data.json", "", [], default_config()),

    case get_filtered_statuses(ServiceNames,
            proplists:get_value(<<"current">>, kz_json:decode(Json)))
    of
        [] -> ok;
        ReturnStatuses -> ReturnStatuses
    end.

get_filtered_statuses(ServiceNames, Statuses) ->
    [S || S <- Statuses,
          lists:any(
	    fun(InputService) ->
                    ServiceNameBin = list_to_binary(InputService),
                    ServiceNameLen = byte_size(ServiceNameBin),
                    case proplists:get_value(<<"service">>, S) of
                        <<ServiceNameBin:ServiceNameLen/binary, _/binary>> -> true;
                        _ -> false
                    end
	    end, ServiceNames)].
