%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Moved util functions out of v1_resource so only REST-related calls
%%% are in there.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Jon Blanton
%%%-------------------------------------------------------------------
-module(v1_util).

-export([is_cors_preflight/1
         ,is_cors_request/1
         ,add_cors_headers/2
         ,allow_methods/4
         ,parse_path_tokens/1
         ,get_req_data/2
         ,get_http_verb/2
         ,is_authentic/2
         ,is_permitted/2
         ,is_known_content_type/2
         ,does_resource_exist/1
         ,validate/1
         ,succeeded/1
         ,execute_request/2
         ,finish_request/2
         ,create_push_response/2
         ,set_resp_headers/2
         ,create_resp_content/2
         ,create_pull_response/2
         ,halt/2
         ,content_type_matches/2
         ,ensure_content_type/1
        ]).

-include("crossbar.hrl").

-type cowboy_multipart_response() :: {{'headers', cowboy_http:headers()} |
                                      {'data', binary()} |
                                      'end_of_part' |
                                      'eof'
                                      ,cowboy_req:req()
                                     }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if this is a cross origin resource preflight
%% request
%% @end
%%--------------------------------------------------------------------
-spec is_cors_preflight(cowboy_req:req()) -> {boolean(), cowboy_req:req()}.
is_cors_preflight(Req0) ->
    case is_cors_request(Req0) of
        {'true', Req1} ->
            case cowboy_req:method(Req1) of
                {?HTTP_OPTIONS, Req2} -> {'true', Req2};
                {_M, Req2} -> {'false', Req2}
            end;
        Nope -> Nope
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if this is a cross origin resource sharing
%% request
%% @end
%%--------------------------------------------------------------------
-spec is_cors_request(cowboy_req:req()) -> {boolean(), cowboy_req:req()}.
-spec is_cors_request(cowboy_req:req(), ne_binaries()) -> {boolean(), cowboy_req:req()}.
is_cors_request(Req) ->
    ReqHdrs = [<<"origin">>, <<"access-control-request-method">>, <<"access-control-request-headers">>],

    is_cors_request(Req, ReqHdrs).
is_cors_request(Req, []) -> {'false', Req};
is_cors_request(Req, [ReqHdr|ReqHdrs]) ->
    {Hdr, Req1} = cowboy_req:header(ReqHdr, Req),
    case Hdr of
        'undefined' -> is_cors_request(Req1, ReqHdrs);
        _H -> {'true', Req1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_cors_headers(cowboy_req:req(), cb_context:context()) ->
                              {'ok', cowboy_req:req()}.
add_cors_headers(Req0, #cb_context{allow_methods=Ms}=Context) ->
    {ReqMethod, Req1} = cowboy_req:header(<<"access-control-request-method">>, Req0),

    Methods = [?HTTP_OPTIONS | Ms],
    Allow = case wh_util:is_empty(ReqMethod)
                orelse lists:member(ReqMethod, Methods)
            of
                'false' -> [ReqMethod|Methods];
                'true' -> Methods
            end,

    lists:foldl(fun({H, V}, ReqAcc) ->
                        cowboy_req:set_resp_header(H, V, ReqAcc)
                end, Req1, get_cors_headers(Context#cb_context{allow_methods=Allow})).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_cors_headers(cb_context:context()) -> wh_proplist().
get_cors_headers(#cb_context{allow_methods=Allow}) ->
    [{<<"access-control-allow-origin">>, <<"*">>}
     ,{<<"access-control-allow-methods">>, wh_util:join_binary(Allow, <<", ">>)}
     ,{<<"access-control-allow-headers">>, <<"Content-Type, Depth, User-Agent, X-Http-Method-Override, X-File-Size, X-Requested-With, If-Modified-Since, X-File-Name, Cache-control, X-Auth-Token, If-Match">>}
     ,{<<"access-control-expose-Headers">>, <<"Content-Type, X-Auth-Token, X-Request-ID, Location, Etag, ETag">>}
     ,{<<"access-control-max-age">>, wh_util:to_binary(?SECONDS_IN_DAY)}
    ].

-spec get_req_data(cb_context:context(), cowboy_req:req()) ->
                          {cb_context:context(), cowboy_req:req()} |
                          {'halt', cb_context:context(), cowboy_req:req()}.
-spec get_req_data(cb_context:context(), {content_type(), cowboy_req:req()}, wh_json:object()) ->
                          {cb_context:context(), cowboy_req:req()} |
                          {'halt', cb_context:context(), cowboy_req:req()}.
get_req_data(Context, Req0) ->
    {QS, Req1} = get_query_string_data(Req0),
    get_req_data(Context, get_content_type(Req1), QS).

get_query_string_data(Req0) ->
    {QS0, Req1} = cowboy_req:qs_vals(Req0),    
    get_query_string_data(QS0, Req1).
get_query_string_data([], Req) ->
    {wh_json:new(), Req};
get_query_string_data(QS0, Req) ->
    QS = wh_json:from_list(QS0),
    lager:debug("query string: ~p", [wh_json:encode(QS)]),
    {QS, Req}.

-spec get_content_type(cowboy_req:req()) ->
                              {api_binary(), cowboy_req:req()}.
get_content_type(Req) ->
    get_parsed_content_type(cowboy_req:parse_header(<<"content-type">>, Req)).

-spec get_parsed_content_type({'ok', content_type() | 'undefined', cowboy_req:req()}) ->
                                     {api_binary(), cowboy_req:req()}.
get_parsed_content_type({'ok', 'undefined', Req}) ->
    {'undefined', Req};
get_parsed_content_type({'ok', {Main, Sub, _Opts}, Req}) ->
    {<<Main/binary, "/", Sub/binary>>, Req}.

get_req_data(Context, {'undefined', Req0}, QS) ->
    lager:debug("undefined content type when getting req data, assuming application/json"),
    {JSON, Req1} = get_json_body(Req0),
    {Context#cb_context{req_json=JSON
                        ,req_data=wh_json:get_value(<<"data">>, JSON, wh_json:new())
                        ,query_json=QS
                       }
     ,Req1};
get_req_data(Context, {<<"multipart/form-data">>, Req}, QS) ->
    lager:debug("multipart/form-data content type when getting req data"),
    maybe_extract_multipart(Context#cb_context{query_json=QS}, Req, QS);

%% cURL defaults to this content-type, so check it for JSON if parsing fails
get_req_data(Context, {<<"application/x-www-form-urlencoded">>, Req1}, QS) ->
    lager:debug("application/x-www-form-urlencoded content type when getting req data"),
    maybe_extract_multipart(Context#cb_context{query_json=QS}, Req1, QS);

get_req_data(Context, {<<"application/json">>, Req1}, QS) ->
    lager:debug("application/json content type when getting req data"),
    {JSON, Req2} = get_json_body(Req1),
    {Context#cb_context{req_json=JSON
                        ,req_data=wh_json:get_value(<<"data">>, JSON, wh_json:new())
                        ,query_json=QS
                       }
     ,Req2};
get_req_data(Context, {<<"application/x-json">>, Req1}, QS) ->
    lager:debug("application/x-json content type when getting req data"),
    {JSON, Req2} = get_json_body(Req1),
    {Context#cb_context{req_json=JSON
                        ,req_data=wh_json:get_value(<<"data">>, JSON, wh_json:new())
                        ,query_json=QS
                       }
     ,Req2};
get_req_data(Context, {<<"application/base64">>, Req1}, QS) ->
    lager:debug("application/base64 content type when getting req data"),
    decode_base64(Context#cb_context{query_json=QS}, <<"application/base64">>, Req1);
get_req_data(Context, {<<"application/x-base64">>, Req1}, QS) ->
    lager:debug("application/x-base64 content type when getting req data"),
    decode_base64(Context#cb_context{query_json=QS}, <<"application/base64">>, Req1);
get_req_data(Context, {ContentType, Req1}, QS) ->
    lager:debug("unknown content-type: ~s", [ContentType]),
    extract_file(Context#cb_context{query_json=QS}, ContentType, Req1).

-spec maybe_extract_multipart(cb_context:context(), cowboy_req:req(), wh_json:object()) ->
                                     {cb_context:context(), cowboy_req:req()} |
                                     {'halt', cb_context:context(), cowboy_req:req()}.
maybe_extract_multipart(Context, Req0, QS) ->
    case catch extract_multipart(Context, Req0) of
        {'EXIT', _} ->
            lager:debug("failed to extract multipart"),
            {ReqBody, Req1} = get_request_body(Req0),

            try get_url_encoded_body(ReqBody) of
                JObj ->
                    case wh_json:values(JObj) of
                        {['true'], [JSON]} ->
                            lager:debug("failed to parse url-encoded request body, but we'll give json a go on ~p", [JSON]),
                            try_json(JSON, QS, Context, Req1);
                        _ ->
                            lager:debug("was able to parse request body as url-encoded: ~p", [JObj]),
                            {Context#cb_context{req_json=JObj
                                                ,req_data=wh_json:get_value(<<"data">>, JObj, wh_json:new())
                                                ,query_json=QS
                                               }, Req1}
                    end
            catch
                _:_ ->
                    lager:debug("failed to extract url-encoded request body"),
                    try_json(ReqBody, QS, Context, Req1)
            end;
        Resp -> Resp
    end.

try_json(ReqBody, QS, Context, Req) ->
    try get_json_body(ReqBody, Req) of
        {JObj, Req1} ->
            lager:debug("was able to parse as JSON"),
            {Context#cb_context{req_json=JObj
                                ,req_data=wh_json:get_value(<<"data">>, JObj, wh_json:new())
                                ,query_json=QS
                               }, Req1}
    catch
        'throw':_R ->
            lager:debug("failed to get JSON too: ~p", [_R]),
            {'halt', Context, Req};
        _:_ ->
            {'halt', Context, Req}
    end.

-spec get_url_encoded_body(ne_binary()) -> wh_json:object().
get_url_encoded_body(ReqBody) ->
    wh_json:from_list(cowboy_http:x_www_form_urlencoded(ReqBody)).

-spec extract_multipart(cb_context:context(), cowboy_req:req()) ->
                               {cb_context:context(), cowboy_req:req()}.
extract_multipart(#cb_context{req_files=Files}=Context, Req0) ->
    MPData = cowboy_req:multipart_data(Req0),

    case extract_multipart_content(MPData, wh_json:new()) of
        {'eof', Req1} -> {Context, Req1};
        {'end_of_part', JObj, Req1} ->
            extract_multipart(Context#cb_context{req_files=[JObj|Files]}, Req1)
    end.

-spec extract_multipart_content(cowboy_multipart_response(), wh_json:object()) ->
                                       {'end_of_part', wh_json:object(), cowboy_req:req()} |
                                       {'eof', cowboy_req:req()}.
extract_multipart_content({'eof', _}=EOF, _) -> EOF;
extract_multipart_content({'end_of_part', Req}, JObj) -> {'end_of_part', JObj, Req};
extract_multipart_content({{'headers', Headers}, Req}, JObj) ->
    lager:debug("setting multipart headers: ~p", [Headers]),
    MPData = cowboy_req:multipart_data(Req),
    extract_multipart_content(MPData, wh_json:set_value(<<"headers">>, Headers, JObj));
extract_multipart_content({{'body', Datum}, Req}, JObj) ->
    extract_multipart_content({{'data', Datum}, Req}, JObj);
extract_multipart_content({{'data', Datum}, Req}, JObj) ->
    Data = wh_json:get_value(<<"data">>, JObj, <<>>),
    extract_multipart_content(cowboy_req:multipart_data(Req)
                              ,wh_json:set_value(<<"data">>, <<Data/binary, Datum/binary>>, JObj)
                             ).

-spec extract_file(cb_context:context(), ne_binary(), cowboy_req:req()) ->
                          {cb_context:context(), cowboy_req:req()}.
extract_file(Context, ContentType, Req0) ->
    case cowboy_req:body(Req0) of
        {'error', 'badarg'} -> {Context, Req0};
        {'ok', FileContents, Req1} ->
            %% http://tools.ietf.org/html/rfc2045#page-17
            case cowboy_req:header(<<"content-transfer-encoding">>, Req1) of
                <<"base64">> -> decode_base64(Context, ContentType, Req1);
                _Else ->
                    {ContentLength, Req2} = cowboy_req:header(<<"content-length">>, Req1),
                    Headers = wh_json:from_list([{<<"content_type">>, ContentType}
                                                 ,{<<"content_length">>, ContentLength}
                                                ]),
                    FileJObj = wh_json:from_list([{<<"headers">>, Headers}
                                                  ,{<<"contents">>, FileContents}
                                                 ]),
                    lager:debug("request is a file upload of type: ~s", [ContentType]),
                    FileName = <<"uploaded_file_"
                                 ,(wh_util:to_binary(wh_util:current_tstamp()))/binary>>,
                    {Context#cb_context{req_files=[{FileName, FileJObj}]}, Req2}
            end
    end.

-spec decode_base64(cb_context:context(), ne_binary(), cowboy_req:req()) ->
                           {cb_context:context(), cowboy_req:req()}.
decode_base64(Context, CT, Req0) ->
    case cowboy_req:body(Req0) of
        {'error', _E} ->
            lager:debug("error getting request body: ~p", [_E]),
            {Context, Req0};
        {'ok', Base64Data, Req1} ->
            {EncodedType, FileContents} = decode_base64(Base64Data),
            ContentType = case EncodedType of
                              'undefined' -> CT;
                              <<"application/base64">> -> <<"application/octet-stream">>;
                              Else -> Else
                          end,
            Headers = wh_json:from_list([{<<"content_type">>, ContentType}
                                         ,{<<"content_length">>, wh_util:to_binary(size(FileContents))}
                                        ]),
            FileJObj = wh_json:from_list([{<<"headers">>, Headers}
                                          ,{<<"contents">>, FileContents}
                                         ]),
            lager:debug("request is a base64 file upload of type: ~s", [ContentType]),
            FileName = <<"uploaded_file_"
                         ,(wh_util:to_binary(wh_util:current_tstamp()))/binary>>,
            {Context#cb_context{req_files=[{FileName, FileJObj}]}, Req1}
    end.

-spec decode_base64(ne_binary()) -> {api_binary(), ne_binary()}.
decode_base64(Base64) ->
    case binary:split(Base64, <<",">>) of
        %% http://tools.ietf.org/html/rfc4648
        [Bin] -> {'undefined', corrected_base64_decode(Bin)};
        %% http://tools.ietf.org/rfc/rfc2397.txt
        [<<"data:", CT/binary>>, Bin] ->
            {ContentType, _Opts} = mochiweb_util:parse_header(wh_util:to_list(CT)),
            {wh_util:to_binary(ContentType), corrected_base64_decode(Bin)}
    end.

-spec corrected_base64_decode(ne_binary()) -> ne_binary().
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 3 ->
    base64:mime_decode(<<Base64/bytes, "=">>);
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 2 ->
    base64:mime_decode(<<Base64/bytes, "==">>);
corrected_base64_decode(Base64) ->
    base64:mime_decode(Base64).

-spec get_request_body(cowboy_req:req()) -> {binary(), cowboy_req:req()}.
get_request_body(Req0) ->
    case cowboy_req:body(Req0) of
        {'error', _E} ->
            lager:debug("request body had no payload: ~p", [_E]),
            {<<>>, Req0};
        {'ok', <<>>, Req1} ->
            lager:debug("request body was empty"),
            {<<>>, Req1};
        {'ok', ReqBody, Req1} ->
            {ReqBody, Req1}
    end.

-type get_json_return() :: {wh_json:object(), cowboy_req:req()} |
                           {{'malformed', ne_binary()}, cowboy_req:req()}.
-spec get_json_body(cowboy_req:req()) -> get_json_return().
-spec decode_json_body(ne_binary(), cowboy_req:req()) -> get_json_return().

get_json_body(Req0) ->
    {Body, Req1} = get_request_body(Req0),
    get_json_body(Body, Req1).

get_json_body(<<>>, Req) -> {wh_json:new(), Req};
get_json_body(ReqBody, Req) -> decode_json_body(ReqBody, Req).

decode_json_body(ReqBody, Req) ->
    lager:debug("request has a json payload: ~s", [ReqBody]),
    try wh_json:decode(ReqBody) of
        JObj ->
            case is_valid_request_envelope(JObj) of
                'true' ->
                    lager:debug("request envelope is valid"),
                    {JObj, Req};
                'false' ->
                    lager:debug("invalid request envelope"),
                    {{'malformed', <<"Invalid JSON request envelope">>}, Req}
            end
    catch
        'throw':{'invalid_json',{{'error',{ErrLine, ErrMsg}}, _JSON}} ->
            lager:debug("failed to decode json near ~p: ~s", [ErrLine, ErrMsg]),
            {{'malformed', <<(wh_util:to_binary(ErrMsg))/binary, " (around ", (wh_util:to_binary(ErrLine))/binary>>}, Req}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the request envelope is valid
%% @end
%%--------------------------------------------------------------------
-spec is_valid_request_envelope(wh_json:object()) -> boolean().
is_valid_request_envelope(JSON) ->
    wh_json:get_value([<<"data">>], JSON, 'undefined') =/= 'undefined'.

-spec get_http_verb(http_method(), cb_context:context()) -> ne_binary().
get_http_verb(Method, #cb_context{req_json=ReqJObj
                                  ,query_json=ReqQs
                                 }) ->
    case wh_json:get_value(<<"verb">>, ReqJObj) of
        'undefined' ->
            case wh_json:get_value(<<"verb">>, ReqQs) of
                'undefined' -> Method;
                Verb ->
                    lager:debug("found verb ~s on query string, using instead of ~s", [Verb, Method]),
                    wh_util:to_upper_binary(Verb)
            end;
        Verb ->
            lager:debug("found verb ~s in req data, using instead of ~s", [Verb, Method]),
            wh_util:to_upper_binary(Verb)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will loop over the Tokens in the request path and return
%% a proplist with keys being the module and values a list of parameters
%% supplied to that module.  If the token order is improper a empty list
%% is returned.
%% @end
%%--------------------------------------------------------------------

-type cb_mod_with_tokens() :: {ne_binary(), path_tokens()}.
-type cb_mods_with_tokens() :: [cb_mod_with_tokens(),...] | [].
-spec parse_path_tokens(path_tokens()) -> cb_mods_with_tokens().
parse_path_tokens(Tokens) ->
    parse_path_tokens(Tokens, []).

-spec parse_path_tokens(wh_json:json_strings(), cb_mods_with_tokens()) ->
                               cb_mods_with_tokens().
parse_path_tokens([], Events) -> Events;
parse_path_tokens([<<"schemas">>=Mod|T], Events) ->
    [{Mod, T} | Events];
parse_path_tokens([<<"braintree">>=Mod|T], Events) ->
    [{Mod, T} | Events];
parse_path_tokens([Mod|T], Events) ->
    case is_cb_module(Mod) of
        'false' -> [];
        'true' ->
            {Params, List2} = lists:splitwith(fun(Elem) -> not is_cb_module(Elem) end, T),
            parse_path_tokens(List2, [{Mod, Params} | Events])
    end.

-spec is_cb_module(ne_binary()) -> boolean().
is_cb_module(Elem) ->
    try (wh_util:to_atom(<<"cb_", Elem/binary>>)):module_info('imports') of
        _ -> 'true'
    catch
        'error':'badarg' -> 'false'; %% atom didn't exist already
        _E:_R -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will find the intersection of the allowed methods
%% among event respsonses.  The responses can only veto the list of
%% methods, they can not add.
%%
%% If a client passes a ?verb=(PUT|DELETE) on a POST request, ReqVerb will
%% be ?HTTP_PUT or ?HTTP_DELETE, while HttpVerb is 'POST'. If the allowed
%% methods do not include 'POST', we need to add it if allowed methods include
%% the verb in ReqVerb.
%% So, POSTing a ?HTTP_PUT, and the allowed methods include 'PUT', insert POST
%% as well.
%% POSTing a ?HTTP_DELETE, and 'DELETE' is NOT in the allowed methods, remove
%% 'POST' from the allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allow_methods([http_methods(),...], http_methods(), ne_binary(), http_method()) -> http_methods().
allow_methods(Responses, Available, ReqVerb, HttpVerb) ->
    case crossbar_bindings:succeeded(Responses) of
        [] -> [];
        Succeeded ->
            AllowedSet = lists:foldr(fun(Response, Acc) ->
                                             sets:intersection(Acc, sets:from_list(uppercase_all(Response)))
                                     end, sets:from_list(Available), Succeeded),
            maybe_add_post_method(ReqVerb, HttpVerb, sets:to_list(AllowedSet))
    end.

uppercase_all(L) when is_list(L) ->
    [wh_util:to_upper_binary(wh_util:to_binary(I)) || I <- L].

%% insert 'POST' if Verb is in Allowed; otherwise remove 'POST'.
-spec maybe_add_post_method(ne_binary(), http_method(), http_methods()) -> http_methods().
maybe_add_post_method(Verb, ?HTTP_POST, Allowed) ->
    BigVerb = wh_util:to_upper_binary(Verb),
    case lists:member(BigVerb, Allowed) of
        'true' -> [?HTTP_POST | Allowed];
        'false' -> lists:delete(?HTTP_POST, Allowed)
    end;
maybe_add_post_method(_, _, Allowed) ->
    Allowed.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client has
%% provided a valid authentication token
%% @end
%%--------------------------------------------------------------------
-spec is_authentic(cowboy_req:req(), cb_context:context()) ->
                          {{'false', <<>>} | 'true', cowboy_req:req(), cb_context:context()}.
is_authentic(Req, #cb_context{req_verb = ?HTTP_OPTIONS}=Context) ->
    %% all OPTIONS, they are harmless (I hope) and required for CORS preflight
    {'true', Req, Context};
is_authentic(Req0, Context0) ->
    Event = <<"v1_resource.authenticate">>,
    {Req1, Context1} = get_auth_token(Req0, Context0),
    case crossbar_bindings:succeeded(crossbar_bindings:map(Event, Context1)) of
        [] ->
            lager:debug("failed to authenticate"),
            ?MODULE:halt(Req0, cb_context:add_system_error('invalid_credentials', Context0));
        ['true'|_] ->
            {'true', Req1, Context1};
        [{'true', Context2}|_] ->
            {'true', Req1, Context2};
        [{'halt', Context2}|_] ->
            lager:debug("is_authentic: halt"),
            ?MODULE:halt(Req1, Context2)
    end.

-spec get_auth_token(cowboy_req:req(), cb_context:context()) ->
                            {cowboy_req:req(), cb_context:context()}.
get_auth_token(Req0, #cb_context{req_json=ReqJObj
                                 ,query_json=QSJObj
                                }=Context0) ->
    case cowboy_req:header(<<"x-auth-token">>, Req0) of
        {'undefined', Req1} ->
            case wh_json:get_value(<<"auth_token">>, ReqJObj) of
                'undefined' ->
                    case wh_json:get_value(<<"auth_token">>, QSJObj) of
                        'undefined' ->
                            lager:debug("no auth token found"),
                            {Req1, Context0};
                        Token ->
                            lager:debug("using auth token from query string"),
                            {Req1, Context0#cb_context{auth_token=Token}}
                    end;
                Token ->
                    lager:debug("using auth token from req json"),
                    {Req1, Context0#cb_context{auth_token=Token}}
            end;
        {Token, Req1} ->
            lager:debug("using auth token from header"),
            {Req1, Context0#cb_context{auth_token=Token}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client is
%% authorized for this request
%% @end
%%--------------------------------------------------------------------
-spec is_permitted(cowboy_req:req(), cb_context:context()) ->
                          {'true' | 'halt', cowboy_req:req(), cb_context:context()}.
is_permitted(Req, #cb_context{req_verb = ?HTTP_OPTIONS}=Context) ->
    lager:debug("options requests are permitted by default"),
    %% all all OPTIONS, they are harmless (I hope) and required for CORS preflight
    {'true', Req, Context};
is_permitted(Req0, #cb_context{req_nouns=[{<<"404">>, []}]}=Context0) ->
    ?MODULE:halt(Req0, cb_context:add_system_error('not_found', Context0));
is_permitted(Req0, Context0) ->
    Event = <<"v1_resource.authorize">>,
    case crossbar_bindings:succeeded(crossbar_bindings:map(Event, Context0)) of
        [] ->
            lager:debug("no on authz the request"),
            ?MODULE:halt(Req0, cb_context:add_system_error('forbidden', Context0));
        ['true'|_] ->
            {'true', Req0, Context0};
        [{'true', Context1}|_] ->
            {'true', Req0, Context1};
        [{'halt', Context1}|_] ->
            lager:debug("is_permitted: halt"),
            ?MODULE:halt(Req0, Context1)
    end.

-spec is_known_content_type(cowboy_req:req(), cb_context:context()) ->
                                   {boolean(), cowboy_req:req(), cb_context:context()}.
is_known_content_type(Req, #cb_context{req_verb = ?HTTP_OPTIONS}=Context) ->
    lager:debug("ignore content type for options"),
    {'true', Req, Context};
is_known_content_type(Req, #cb_context{req_verb = ?HTTP_GET}=Context) ->
    lager:debug("ignore content type for get"),
    {'true', Req, Context};
is_known_content_type(Req, #cb_context{req_verb = ?HTTP_DELETE}=Context) ->
    lager:debug("ignore content type for delete"),
    {'true', Req, Context};
is_known_content_type(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    Context1 =
        lists:foldr(fun({Mod, Params}, ContextAcc) ->
                            Event = <<"v1_resource.content_types_accepted.", Mod/binary>>,
                            Payload = [ContextAcc | Params],
                            crossbar_bindings:fold(Event, Payload)
                    end, Context0, Nouns),

    {CT, Req1} = get_content_type(Req0),

    is_known_content_type(Req1, Context1, ensure_content_type(CT)).

-spec is_known_content_type(cowboy_req:req(), cb_context:context(), content_type()) ->
                                   {boolean(), cowboy_req:req(), cb_context:context()}.
is_known_content_type(Req, #cb_context{content_types_accepted=[]}=Context, CT) ->
    is_known_content_type(Req, Context#cb_context{content_types_accepted=?CONTENT_ACCEPTED}, CT);

is_known_content_type(Req, #cb_context{content_types_accepted=CTAs}=Context, CT) ->
    CTA = lists:foldr(fun({_Fun, L}, Acc) ->
                              lists:foldl(fun({Type, Sub}, Acc1) ->
                                                  [{Type, Sub, []} | Acc1]
                                          end, Acc, L);
                         (L, Acc) ->
                              lists:foldl(fun({Type, Sub}, Acc1) ->
                                                  [{Type, Sub, []} | Acc1]
                                          end, Acc, L)
                      end, [], CTAs),

    IsAcceptable = is_acceptable_content_type(CT, CTA),
    lager:debug("is ~s acceptable content type: ~s", [CT, IsAcceptable]),
    {IsAcceptable, Req, Context}.

-spec is_acceptable_content_type(content_type(), [content_type(),...] | []) -> boolean().
is_acceptable_content_type(CTA, CTAs) ->
    [ 'true' || ModCTA <- CTAs, content_type_matches(CTA, ModCTA)] =/= [].

%% (ReqContentType, ModuleContentType)
-spec content_type_matches(content_type(), content_type()) -> boolean().
content_type_matches({Type, _, _}, {Type, <<"*">>, []}) ->
    'true';
content_type_matches({Type, SubType, _}, {Type, SubType, []}) ->
    'true';
content_type_matches({Type, SubType, Opts}, {Type, SubType, ModOpts}) ->
    lists:all(fun({K, V}) ->
                      props:get_value(K, Opts) =:= V
              end, ModOpts);
content_type_matches(CTA, {CT, SubCT, _}) when is_binary(CTA) ->
    CTA =:= <<CT/binary, "/", SubCT/binary>>;
content_type_matches(CTA, CT) when is_binary(CTA), is_binary(CT) ->
    CTA =:= CT;
content_type_matches(_CTA, _CTAs) ->
    lager:debug("cta: ~p, ctas: ~p", [_CTA, _CTAs]),
    'false'.

-spec ensure_content_type(any()) -> content_type().
ensure_content_type('undefined') -> ?CROSSBAR_DEFAULT_CONTENT_TYPE;
ensure_content_type(CT) -> CT.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the target noun
%% (the final module in the chain) accepts this verb parameter pair.
%% @end
%%--------------------------------------------------------------------
-spec does_resource_exist(cb_context:context()) -> boolean().
does_resource_exist(#cb_context{req_nouns=[{Mod, Params}|_]}) ->
    Event = <<"v1_resource.resource_exists.", Mod/binary>>,
    Responses = crossbar_bindings:map(Event, Params),
    crossbar_bindings:all(Responses) and 'true';
does_resource_exist(_Context) ->
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function gives each noun a chance to determine if
%% it is valid and returns the status, and any errors
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(#cb_context{req_nouns=Nouns}=Context0) ->
    Context1 = lists:foldr(fun({Mod, Params}, ContextAcc) ->
                                   Event = <<"v1_resource.validate.", Mod/binary>>,
                                   Payload = [ContextAcc#cb_context{resp_status='fatal'} | Params],
                                   crossbar_bindings:fold(Event, Payload)
                           end
                           ,Context0#cb_context{resp_status='fatal'}
                           ,Nouns),
    case succeeded((C = cb_context:import_errors(Context1))) of
        'true' -> process_billing(C);
        'false' -> C
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client is
%% authorized for this request
%% @end
%%--------------------------------------------------------------------
-spec process_billing(cb_context:context()) -> cb_context:context().
process_billing(Context0)->
    Event = <<"v1_resource.billing">>,
    case crossbar_bindings:fold(Event, Context0) of
        #cb_context{resp_status='success'}=Resp -> lager:debug("billing returned"), Resp;
        #cb_context{}=Resp -> lager:debug("billing failed"), Resp;
        _E -> lager:debug("billing failed: ~p", [_E]), Context0
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the response is of type success
%% @end
%%--------------------------------------------------------------------
-spec succeeded(cb_context:context()) -> boolean().
succeeded(Context) -> cb_context:resp_status(Context) =:='success'.

-spec execute_request(cowboy_req:req(), cb_context:context()) ->
                             {boolean() | 'halt', cowboy_req:req(), cb_context:context()}.
execute_request(Req, #cb_context{req_nouns=[{Mod, Params}|_]
                                 ,req_verb=Verb
                                }=Context) ->
    Event = <<"v1_resource.execute.", (wh_util:to_lower_binary(Verb))/binary, ".", Mod/binary>>,
    Payload = [Context | Params],
    case crossbar_bindings:fold(Event, Payload) of
        #cb_context{resp_status='success'}=Context1 ->
            execute_request_results(Req, Context1);
        #cb_context{}=Context1 ->
            ?MODULE:halt(Req, Context1);
        {'error', _E} ->
            lager:debug("error executing request: ~p", [_E]),
            {'false', Req, Context};
        _E ->
            lager:debug("unexpected return from the fold: ~p", [_E]),
            {'false', Req, Context}
    end;
execute_request(Req, Context) ->
    {'false', Req, Context}.

-spec execute_request_results(cowboy_req:req(), cb_context:context()) ->
                                     {'true' | 'halt', cowboy_req:req(), cb_context:context()}.
execute_request_results(Req, Context) ->
    case succeeded(Context) of
        'false' -> ?MODULE:halt(Req, Context);
        'true' -> {'true', Req, Context}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function runs the request terminated bindings at the conclusion
%% of all requests
%% @end
%%--------------------------------------------------------------------
-spec finish_request(cowboy_req:req(), cb_context:context()) -> 'ok'.
finish_request(_Req, #cb_context{req_nouns=[{Mod, _}|_], req_verb=Verb}=Context) ->
    Event = <<"v1_resource.finish_request.", Verb/binary, ".", Mod/binary>>,
    _ = spawn('crossbar_bindings', 'map', [Event, Context]),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create the content for the response body
%% @end
%%--------------------------------------------------------------------
-spec create_resp_content(cowboy_req:req(), cb_context:context()) ->
                                 {ne_binary() | iolist(), cowboy_req:req()}.
create_resp_content(Req0, Context) ->
    try wh_json:encode(wh_json:from_list(create_resp_envelope(Context))) of
        JSON ->
            case cb_context:req_value(Context, <<"jsonp">>) of
                'undefined' -> {JSON, Req0};
                JsonFun when is_binary(JsonFun) ->
                    lager:debug("jsonp wrapping in ~s: ~p", [JsonFun, JSON]),
                    {[JsonFun, <<"(">>, JSON, <<");">>]
                     ,cowboy_req:set_resp_header(<<"content-type">>, ?JSONP_CONTENT_TYPE, Req0)
                    }
            end
    catch
        'throw':{'json_encode', {'bad_term', _Term}} ->
            lager:debug("json encoding failed on ~p", [_Term]),
            {<<"encoding response failed: bad term">>, Req0};
        _E:_R ->
            lager:debug("failed to encode response: ~s: ~p", [_E, _R]),
            {<<"failure in request, contact support">>, Req0}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pushing data (like PUT)
%% @end
%%--------------------------------------------------------------------
-spec create_push_response(cowboy_req:req(), cb_context:context()) ->
                                  {boolean(), cowboy_req:req(), cb_context:context()}.
create_push_response(Req0, Context) ->
    {Content, Req1} = create_resp_content(Req0, Context),
    Req2 = set_resp_headers(Req1, Context),
    lager:debug("push response content: ~s", [wh_util:to_binary(Content)]),
    {succeeded(Context), cowboy_req:set_resp_body(Content, Req2), Context}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pulling data (like GET)
%% @end
%%--------------------------------------------------------------------
-spec create_pull_response(cowboy_req:req(), cb_context:context()) ->
                                  {text() | 'halt', cowboy_req:req(), cb_context:context()}.
create_pull_response(Req0, Context) ->
    {Content, Req1} = create_resp_content(Req0, Context),
    lager:debug("pull response content: ~s", [wh_util:to_binary(Content)]),
    Req2 = set_resp_headers(Req1, Context),
    case succeeded(Context) of
        'false' -> {'halt', Req2, Context};
        'true' -> {Content, Req2, Context}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function extracts the reponse fields and puts them in a proplist
%% @end
%%--------------------------------------------------------------------
-spec create_resp_envelope(cb_context:context()) -> wh_json:json_proplist(<<_:32,_:_*8>>).
create_resp_envelope(Context) ->
    do_create_resp_envelope(cb_context:import_errors(Context)).

do_create_resp_envelope(#cb_context{auth_token=AuthToken
                                    ,resp_etag=ETag
                                    ,req_id=RequestId
                                   }=Context) ->
    Resp = case cb_context:response(Context) of
               {'ok', RespData} ->
                   [{<<"auth_token">>, AuthToken}
                    ,{<<"status">>, <<"success">>}
                    ,{<<"request_id">>, RequestId}
                    ,{<<"revision">>, wh_util:to_binary(ETag)}
                    ,{<<"data">>, RespData}
                   ];
               {'error', {ErrorCode, ErrorMsg, RespData}} ->
                   lager:debug("generating error ~b ~s response", [ErrorCode, ErrorMsg]),
                   [{<<"auth_token">>, wh_util:to_binary(AuthToken)}
                    ,{<<"request_id">>, RequestId}
                    ,{<<"status">>, <<"error">>}
                    ,{<<"message">>, ErrorMsg}
                    ,{<<"error">>, wh_util:to_binary(ErrorCode)}
                    ,{<<"data">>, RespData}
                   ]
           end,
    props:filter_undefined(Resp).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Iterate through #cb_context.resp_headers, setting the headers specified
%% @end
%%--------------------------------------------------------------------
-spec set_resp_headers(cowboy_req:req(), cb_context:context()) -> cowboy_req:req().
set_resp_headers(Req0, #cb_context{resp_headers=[]}) -> Req0;
set_resp_headers(Req0, #cb_context{resp_headers=Headers}) ->
    lists:foldl(fun({Header, Value}, ReqAcc) ->
                        {H, V} = fix_header(Header, Value, ReqAcc),
                        cowboy_req:set_resp_header(H, V, ReqAcc)
                end, Req0, props:filter_empty(Headers)).

-spec fix_header(text(), text(), cowboy_req:req()) ->
                        {binary(), binary()}.
fix_header(<<"Location">> = H, Path, Req) ->
    {H, crossbar_util:get_path(Req, Path)};
fix_header(H, V, _) ->
    {wh_util:to_binary(H), wh_util:to_binary(V)}.

-spec halt(cowboy_req:req(), cb_context:context()) ->
                  {'halt', cowboy_req:req(), cb_context:context()}.
halt(Req0, #cb_context{resp_error_code=StatusCode}=Context) ->
    lager:debug("halting execution here"),
    {Content, Req1} = create_resp_content(Req0, Context),
    lager:debug("setting resp body: ~s", [Content]),
    Req2 = cowboy_req:set_resp_body(Content, Req1),
    lager:debug("setting status code: ~p", [StatusCode]),
    {'ok', Req3} = cowboy_req:reply(StatusCode, Req2),
    {'halt', Req3, Context}.
