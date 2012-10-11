%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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

-export([is_cors_preflight/1, is_cors_request/1, add_cors_headers/2
         ,allow_methods/4, parse_path_tokens/1
         ,get_req_data/2, get_http_verb/2
         ,is_authentic/2, is_permitted/2
         ,is_known_content_type/2
         ,does_resource_exist/1, validate/1
         ,succeeded/1
         ,execute_request/2
         ,finish_request/2, request_terminated/2
         ,create_push_response/2, set_resp_headers/2
         ,create_resp_content/2, create_pull_response/2
         ,halt/2, content_type_matches/2, ensure_content_type/1
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-type cowboy_multipart_response() :: {{'headers', cowboy_http:headers()} |
                                      {'data', binary()} |
                                      'end_of_part' |
                                      'eof'
                                      ,#http_req{}
                                     }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if this is a cross origin resource preflight
%% request
%% @end
%%--------------------------------------------------------------------
-spec is_cors_preflight/1 :: (#http_req{}) -> {boolean(), #http_req{}}.
is_cors_preflight(Req0) ->
    case is_cors_request(Req0) of
        {true, Req1} ->
            case cowboy_http_req:method(Req1) of
                {'OPTIONS', Req2} -> {true, Req2};
                {_M, Req2} -> {false, Req2}
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
-spec is_cors_request/1 :: (#http_req{}) -> {boolean(), #http_req{}}.
is_cors_request(Req0) ->
    case cowboy_http_req:header(<<"Origin">>, Req0) of
        {undefined, Req1} ->
            case cowboy_http_req:header(<<"Access-Control-Request-Method">>, Req1) of
                {undefined, Req2} ->
                    case cowboy_http_req:header(<<"Access-Control-Request-Headers">>, Req2) of
                        {undefined, Req3} -> {false, Req3};
                        {_, Req3} -> {true, Req3}
                    end;
                {_M, Req2} -> {true, Req2}
            end;
        {_O, Req1} -> {true, Req1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_cors_headers/2 :: (#http_req{}, #cb_context{}) -> {'ok', #http_req{}}.
add_cors_headers(Req0, #cb_context{allow_methods=Ms}=Context) ->
    {ReqM, Req1} = cowboy_http_req:header(<<"Access-Control-Request-Method">>, Req0),

    lists:foldl(fun({H, V}, {ok, ReqAcc}) ->
                        cowboy_http_req:set_resp_header(H, V, ReqAcc)
                end, {ok, Req1}, get_cors_headers(Context#cb_context{allow_methods=[ReqM|Ms]})).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_cors_headers/1 :: (#cb_context{}) -> [{ne_binary(), ne_binary()},...].
get_cors_headers(#cb_context{allow_methods=Allowed}) ->
    Methods = wh_util:join_binary([wh_util:to_binary(A) || A <- Allowed, A =/= undefined], <<", ">>),

    [{<<"Access-Control-Allow-Origin">>, <<"*">>}
     ,{<<"Access-Control-Allow-Methods">>, Methods}
     ,{<<"Access-Control-Allow-Headers">>, <<"Content-Type, Depth, User-Agent, X-File-Size, X-Requested-With, If-Modified-Since, X-File-Name, Cache-Control, X-Auth-Token, If-Match">>}
     ,{<<"Access-Control-Expose-Headers">>, <<"Content-Type, X-Auth-Token, X-Request-ID, Location, Etag, ETag">>}
     ,{<<"Access-Control-Max-Age">>, wh_util:to_binary(?SECONDS_IN_DAY)}
    ].

-spec get_req_data/2 :: (#cb_context{}, #http_req{}) ->
                                {#cb_context{}, #http_req{}} |
                                {'halt', #cb_context{}, #http_req{}}.
-spec get_req_data/3 :: (#cb_context{}, {content_type(), #http_req{}}, wh_json:json_object()) ->
                                {#cb_context{}, #http_req{}} |
                                {'halt', #cb_context{}, #http_req{}}.
get_req_data(Context, Req0) ->
    {QS0, Req1} = cowboy_http_req:qs_vals(Req0),
    QS = wh_json:from_list(QS0),

    get_req_data(Context, cowboy_http_req:parse_header('Content-Type', Req1), QS).

get_req_data(Context, {undefined, Req0}, QS) ->
    lager:debug("undefined content type when getting req data, assuming application/json"),
    {JSON, Req1} = get_json_body(Req0),
    {Context#cb_context{req_json=JSON
                        ,req_data=wh_json:get_value(<<"data">>, JSON, wh_json:new())
                        ,query_json=QS
                       }
     ,Req1};
get_req_data(Context, {{<<"multipart">>, <<"form-data">>, _}, Req}, QS) ->
    lager:debug("multipart/form-data content type when getting req data"),
    maybe_extract_multipart(Context#cb_context{query_json=QS}, Req, QS);

%% cURL defaults to this content-type, so check it for JSON if parsing fails
get_req_data(Context, {{<<"application">>, <<"x-www-form-urlencoded">>, _}, Req1}, QS) ->
    lager:debug("application/x-www-form-urlencoded content type when getting req data"),
    maybe_extract_multipart(Context#cb_context{query_json=QS}, Req1, QS);

get_req_data(Context, {{<<"application">>, <<"json">>, _}, Req1}, QS) ->
    lager:debug("application/json content type when getting req data"),
    {JSON, Req2} = get_json_body(Req1),
    {Context#cb_context{req_json=JSON
                        ,req_data=wh_json:get_value(<<"data">>, JSON, wh_json:new())
                        ,query_json=QS
                       }
     ,Req2};
get_req_data(Context, {{<<"application">>, <<"x-json">>, _}, Req1}, QS) ->
    lager:debug("application/x-json content type when getting req data"),
    {JSON, Req2} = get_json_body(Req1),
    {Context#cb_context{req_json=JSON
                        ,req_data=wh_json:get_value(<<"data">>, JSON, wh_json:new())
                        ,query_json=QS
                       }
     ,Req2};
get_req_data(Context, {{<<"application">>, <<"base64">>, _}, Req1}, QS) ->
    lager:debug("application/base64 content type when getting req data"),
    decode_base64(Context#cb_context{query_json=QS}, <<"application/base64">>, Req1);
get_req_data(Context, {{<<"application">>, <<"x-base64">>, _}, Req1}, QS) ->
    lager:debug("application/x-base64 content type when getting req data"),
    decode_base64(Context#cb_context{query_json=QS}, <<"application/base64">>, Req1);
get_req_data(Context, {{ContentType, ContentSubType, _}, Req1}, QS) ->
    lager:debug("unknown content-type: ~s/~s", [ContentType, ContentSubType]),
    extract_file(Context#cb_context{query_json=QS}, list_to_binary([ContentType, "/", ContentSubType]), Req1).

-spec maybe_extract_multipart/3 :: (#cb_context{}, #http_req{}, wh_json:json_object()) ->
                                           {#cb_context{}, #http_req{}} |
                                           {'halt', #cb_context{}, #http_req{}}.
maybe_extract_multipart(Context, Req0, QS) ->
    case catch extract_multipart(Context, Req0) of
        {'EXIT', _} ->
            lager:debug("failed to extract multipart"),
            try get_json_body(Req0) of
                {JSON, Req1} ->
                    lager:debug("was able to parse as JSON"),
                    lager:debug("was the payload valid: ~s", [wh_json:is_json_object(JSON)]),
                    {Context#cb_context{req_json=JSON
                                        ,req_data=wh_json:get_value(<<"data">>, JSON, wh_json:new())
                                        ,query_json=QS
                                       }, Req1}
            catch
                _:_ ->
                    lager:debug("failed to get JSON too"),
                    {halt, Context, Req0}
            end;
        Resp -> Resp
    end.

-spec extract_multipart/2 :: (#cb_context{}, #http_req{}) -> {#cb_context{}, #http_req{}}.
extract_multipart(#cb_context{req_files=Files}=Context, #http_req{}=Req0) ->
    MPData = cowboy_http_req:multipart_data(Req0),

    case extract_multipart_content(MPData, wh_json:new()) of
        {eof, Req1} -> {Context, Req1};
        {end_of_part, JObj, Req1} ->
            extract_multipart(Context#cb_context{req_files=[JObj|Files]}, Req1)
    end.

-spec extract_multipart_content/2 :: (cowboy_multipart_response(), wh_json:json_object()) ->
                                             {'end_of_part', wh_json:json_object(), #http_req{}} |
                                             {'eof', #http_req{}}.
extract_multipart_content({eof, _}=EOF, _) -> EOF;
extract_multipart_content({end_of_part, Req}, JObj) -> {end_of_part, JObj, Req};
extract_multipart_content({{headers, Headers}, Req}, JObj) ->
    lager:debug("setting multipart headers: ~p", [Headers]),
    MPData = cowboy_http_req:multipart_data(Req),
    extract_multipart_content(MPData, wh_json:set_value(<<"headers">>, Headers, JObj));
extract_multipart_content({{body, Datum}, Req}, JObj) ->
    extract_multipart_content({{data, Datum}, Req}, JObj);
extract_multipart_content({{data, Datum}, Req}, JObj) ->
    Data = wh_json:get_value(<<"data">>, JObj, <<>>),
    extract_multipart_content(cowboy_http_req:multipart_data(Req)
                              ,wh_json:set_value(<<"data">>, <<Data/binary, Datum/binary>>, JObj)
                             ).

-spec extract_file/3 :: (#cb_context{}, ne_binary(), #http_req{}) ->
                                {#cb_context{}, #http_req{}}.
extract_file(Context, ContentType, Req0) ->
    case cowboy_http_req:body(Req0) of
        {error, badarg} -> {Context, Req0};
        {ok, FileContents, Req1} ->
            %% http://tools.ietf.org/html/rfc2045#page-17
            case cowboy_http_req:header(<<"Content-Transfer-Encoding">>, Req1) of
                <<"base64">> -> decode_base64(Context, ContentType, Req1);
                _Else ->
                    {ContentLength, Req2} = cowboy_http_req:header('Content-Length', Req1),
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

-spec decode_base64/3 :: (#cb_context{}, ne_binary(), #http_req{}) -> {#cb_context{}, #http_req{}}.
decode_base64(Context, CT, Req0) ->
    case cowboy_http_req:body(Req0) of
        {error, _E} ->
            lager:debug("error getting request body: ~p", [_E]),
            {Context, Req0};
        {ok, Base64Data, Req1} ->
            {EncodedType, FileContents} = decode_base64(Base64Data),
            ContentType = case EncodedType of
                              undefined -> CT;
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

-spec decode_base64/1 :: (ne_binary()) -> {undefined | ne_binary(), ne_binary()}.
decode_base64(Base64) ->
    case binary:split(Base64, <<",">>) of
        %% http://tools.ietf.org/html/rfc4648
        [Bin] -> {undefined, corrected_base64_decode(Bin)};
        %% http://tools.ietf.org/rfc/rfc2397.txt
        [<<"data:", CT/binary>>, Bin] ->
            {ContentType, _Opts} = mochiweb_util:parse_header(wh_util:to_list(CT)),
            {wh_util:to_binary(ContentType), corrected_base64_decode(Bin)}
    end.

-spec corrected_base64_decode/1 :: (ne_binary()) -> ne_binary().
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 3 ->
    base64:mime_decode(<<Base64/bytes, "=">>);
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 2 ->
    base64:mime_decode(<<Base64/bytes, "==">>);
corrected_base64_decode(Base64) ->
    base64:mime_decode(Base64).

-type get_json_return() :: {wh_json:json_object(), #http_req{}} |
                           {{'malformed', ne_binary()}, #http_req{}}.
-spec get_json_body/1 :: (#http_req{}) -> get_json_return().
-spec get_json_body/2 :: (#http_req{}, ne_binary()) -> get_json_return().

get_json_body(Req0) ->
    case cowboy_http_req:body(Req0) of
        {error, _E} ->
            lager:debug("request had no payload: ~s", [_E]),
            {wh_json:new(), Req0};
        {ok, <<>>, Req1} ->
            lager:debug("request had no payload"),
            {wh_json:new(), Req1};
        {ok, ReqBody, Req1} ->
            get_json_body(Req1, ReqBody)
    end.

get_json_body(Req, ReqBody) ->
    lager:debug("request has a json payload: ~s", [ReqBody]),
    try wh_json:decode(ReqBody) of
        JObj ->
            case is_valid_request_envelope(JObj) of
                true ->
                    lager:debug("request envelope is valid"),
                    {JObj, Req};
                false ->
                    lager:debug("invalid request envelope"),
                    {{malformed, <<"Invalid JSON request envelope">>}, Req}
            end
    catch
        throw:{invalid_json,{{error,{ErrLine, ErrMsg}}, _JSON}} ->
            lager:debug("failed to decode json near ~p: ~s", [ErrLine, ErrMsg]),
            {{malformed, <<(wh_util:to_binary(ErrMsg))/binary, " (around ", (wh_util:to_binary(ErrLine))/binary>>}, Req}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the request envelope is valid
%% @end
%%--------------------------------------------------------------------
-spec is_valid_request_envelope/1 :: (wh_json:json_object()) -> boolean().
is_valid_request_envelope(JSON) ->
    wh_json:get_value([<<"data">>], JSON, undefined) =/= undefined.

-spec get_http_verb/2 :: (http_method(), #cb_context{}) -> ne_binary().
get_http_verb(Method, #cb_context{req_json=ReqJObj, query_json=ReqQs}) ->
    case wh_json:get_value(<<"verb">>, ReqJObj) of
        undefined ->
            case wh_json:get_value(<<"verb">>, ReqQs) of
                undefined ->
                    lager:debug("sticking with method ~s", [Method]),
                    wh_util:to_lower_binary(Method);
                Verb ->
                    lager:debug("found verb ~s on query string, using instead of ~s", [Verb, Method]),
                    wh_util:to_lower_binary(Verb)
            end;
        Verb ->
            lager:debug("found verb ~s in req data, using instead of ~s", [Verb, Method]),
            wh_util:to_lower_binary(Verb)
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
-spec parse_path_tokens/1 :: (path_tokens()) -> [cb_mod_with_tokens(),...] | [].
parse_path_tokens(Tokens) ->
    Ebin = code:lib_dir(crossbar, ebin),

    parse_path_tokens(Tokens, Ebin, []).

-spec parse_path_tokens/3 :: (wh_json:json_strings(), nonempty_string(), [cb_mod_with_tokens(),...] | []) -> [cb_mod_with_tokens(),...] | [].
parse_path_tokens([], _Ebin, Events) ->
    Events;
parse_path_tokens([<<"schemas">>=Mod|T], _, Events) ->
    [{Mod, T} | Events];
parse_path_tokens([Mod|T], Ebin, Events) ->
    case is_cb_module(Mod, Ebin) of
        false ->
            lager:debug("failed to find ~s in loaded cb modules", [Mod]),
            [];
        true ->
            {Params, List2} = lists:splitwith(fun(Elem) -> not is_cb_module(Elem, Ebin) end, T),
            Params1 = [ wh_util:to_binary(P) || P <- Params ],
            lager:debug("mod ~s has params ~p", [Mod, Params1]),
            parse_path_tokens(List2, Ebin, [{Mod, Params1} | Events])
    end.

-spec is_cb_module/2 :: (ne_binary(), nonempty_string()) -> boolean().
is_cb_module(Elem, Ebin) ->
    case code:where_is_file(lists:flatten(["cb_", wh_util:to_list(Elem), ".beam"])) of
        non_existing -> false;
        BeamPath -> lists:prefix(Ebin, BeamPath) =:= true
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will find the intersection of the allowed methods
%% among event respsonses.  The responses can only veto the list of
%% methods, they can not add.
%%
%% If a client passes a ?verb=(PUT|DELETE) on a POST request, ReqVerb will
%% be <<"put">> or <<"delete">>, while HttpVerb is 'POST'. If the allowed
%% methods do not include 'POST', we need to add it if allowed methods include
%% the verb in ReqVerb.
%% So, POSTing a <<"put">>, and the allowed methods include 'PUT', insert POST
%% as well.
%% POSTing a <<"delete">>, and 'DELETE' is NOT in the allowed methods, remove
%% 'POST' from the allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allow_methods/4  :: (http_methods(), http_methods(), ne_binary(), atom()) -> http_methods().
allow_methods(Responses, Available, ReqVerb, HttpVerb) ->
    case crossbar_bindings:succeeded(Responses) of
        [] -> [];
        Succeeded ->
            AllowedSet = lists:foldr(fun(Response, Acc) ->
                                             Set = sets:from_list(Response),
                                             sets:intersection(Acc, Set)
                                     end, sets:from_list(Available), Succeeded),
            maybe_add_post_method(ReqVerb, HttpVerb, sets:to_list(AllowedSet))
    end.

%% insert 'POST' if Verb is in Allowed; otherwise remove 'POST'.
-spec maybe_add_post_method/3 :: (ne_binary(), http_method(), http_methods()) -> http_methods().
maybe_add_post_method(Verb, 'POST', Allowed) ->
    VerbAtom = wh_util:to_atom(wh_util:to_upper_binary(Verb)),
    case lists:member(VerbAtom, Allowed) of
        true -> ['POST' | Allowed];
        false -> lists:delete('POST', Allowed)
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
-spec is_authentic/2 :: (#http_req{}, #cb_context{}) -> {{'false', <<>>} | 'true', #http_req{}, #cb_context{}}.
is_authentic(Req, #cb_context{req_verb = <<"options">>}=Context) ->
    %% all OPTIONS, they are harmless (I hope) and required for CORS preflight
    {true, Req, Context};
is_authentic(Req0, Context0) ->
    Event = <<"v1_resource.authenticate">>,
    {Req1, Context1} = get_auth_token(Req0, Context0),
    case crossbar_bindings:succeeded(crossbar_bindings:map(Event, Context1)) of
        [] ->
            lager:debug("failed to authenticate"),
            Context2 = crossbar_util:response(error, <<"unauthorized">>, 401, Context1),
            {Content, Req1} = create_resp_content(Req1, Context2),
            {ok, Req2} = cowboy_http_req:set_resp_body(Content, Req1),
            {{false, <<>>}, Req2, Context2};
        [true|_] ->
            lager:debug("is_authentic: true"),
            {true, Req1, Context1};
        [{true, Context2}|_] ->
            lager:debug("is_authentic: true"),
            {true, Req1, Context2}
    end.

-spec get_auth_token/2 :: (#http_req{}, #cb_context{}) -> {#http_req{}, #cb_context{}}.
get_auth_token(Req0, #cb_context{req_json=ReqJObj, query_json=QSJObj}=Context0) ->
    case cowboy_http_req:header(<<"X-Auth-Token">>, Req0) of
        {undefined, Req1} ->
            case wh_json:get_value(<<"auth_token">>, ReqJObj) of
                undefined ->
                    case wh_json:get_value(<<"auth_token">>, QSJObj) of
                        undefined ->
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
-spec is_permitted/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
is_permitted(Req, #cb_context{req_verb = <<"options">>}=Context) ->
    lager:debug("options requests are permitted by default"),
    %% all all OPTIONS, they are harmless (I hope) and required for CORS preflight
    {true, Req, Context};
is_permitted(Req0, #cb_context{req_nouns=[{<<"404">>, []}]}=Context0) ->
    Context1 = crossbar_util:response(error, <<"not found">>, 404, Context0),
    {Content, Req0} = create_resp_content(Req0, Context1),
    {ok, Req1} = cowboy_http_req:set_resp_body(Content, Req0),
    {false, Req1, Context1};
is_permitted(Req0, Context0) ->
    Event = <<"v1_resource.authorize">>,
    case crossbar_bindings:succeeded(crossbar_bindings:map(Event, Context0)) of
        [] ->
            lager:debug("no on authz the request"),
            Context1 = crossbar_util:response(error, <<"forbidden">>, 403, Context0),
            {Content, Req0} = create_resp_content(Req0, Context1),
            {ok, Req1} = cowboy_http_req:set_resp_body(Content, Req0),
            {false, Req1, Context1};
        [true|_] ->
            lager:debug("request was authz"),
            {true, Req0, Context0};
        [{true, Context1}|_] ->
            lager:debug("request was authz"),
            {true, Req0, Context1}
    end.

-spec is_known_content_type/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
is_known_content_type(Req, #cb_context{req_verb = <<"options">>}=Context) ->
    lager:debug("ignore content type for options"),
    {true, Req, Context};
is_known_content_type(Req, #cb_context{req_verb = <<"get">>}=Context) ->
    lager:debug("ignore content type for get"),
    {true, Req, Context};
is_known_content_type(Req, #cb_context{req_verb = <<"delete">>}=Context) ->
    lager:debug("ignore content type for delete"),
    {true, Req, Context};
is_known_content_type(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    lager:debug("is_known_content_type"),

    Context1 =
        lists:foldr(fun({Mod, Params}, ContextAcc) ->
                            Event = <<"v1_resource.content_types_accepted.", Mod/binary>>,
                            Payload = [ContextAcc | Params],
                            crossbar_bindings:fold(Event, Payload)
                    end, Context0, Nouns),

    {CT, Req1} = cowboy_http_req:parse_header('Content-Type', Req0),

    is_known_content_type(Req1, Context1, ensure_content_type(CT)).

-spec is_known_content_type/3 :: (#http_req{}, #cb_context{}, content_type()) -> {boolean(), #http_req{}, #cb_context{}}.
is_known_content_type(Req, #cb_context{content_types_accepted=[]}=Context, CT) ->
    lager:debug("no ctas, using defaults"),
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

    lager:debug("is ~p in ~p", [CT, CTA]),

    IsAcceptable = is_acceptable_content_type(CT, CTA),
    lager:debug("is acceptable content type: ~s", [IsAcceptable]),
    {IsAcceptable, Req, Context}.

-spec is_acceptable_content_type/2 :: (content_type(), [content_type(),...] | []) -> boolean().
is_acceptable_content_type(CTA, CTAs) ->
    [ true || ModCTA <- CTAs, content_type_matches(CTA, ModCTA)] =/= [].

%% (ReqContentType, ModuleContentType)
-spec content_type_matches/2 :: (content_type(), content_type()) -> boolean().
content_type_matches({Type, _, _}, {Type, <<"*">>, []}) ->
    true;
content_type_matches({Type, SubType, _}, {Type, SubType, []}) ->
    true;
content_type_matches({Type, SubType, Opts}, {Type, SubType, ModOpts}) ->
    lists:all(fun({K, V}) ->
                      props:get_value(K, Opts) =:= V
              end, ModOpts);
content_type_matches(_CTA, _CTAs) ->
    false.

-spec ensure_content_type/1 :: (any()) -> content_type().
ensure_content_type(undefined) -> ?CROSSBAR_DEFAULT_CONTENT_TYPE;
ensure_content_type(CT) -> CT.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the target noun
%% (the final module in the chain) accepts this verb parameter pair.
%% @end
%%--------------------------------------------------------------------
-spec does_resource_exist/1 :: (#cb_context{}) -> boolean().
does_resource_exist(#cb_context{req_nouns=[{Mod, Params}|_]}) ->
    Event = <<"v1_resource.resource_exists.", Mod/binary>>,
    Responses = crossbar_bindings:map(Event, Params),
    crossbar_bindings:all(Responses) and true;
does_resource_exist(_Context) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function gives each noun a chance to determine if
%% it is valid and returns the status, and any errors
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
validate(#cb_context{req_nouns=Nouns}=Context0) ->
    Context1 = lists:foldr(fun({Mod, Params}, ContextAcc) ->
                                   Event = <<"v1_resource.validate.", Mod/binary>>,
                                   lager:debug("validating against params ~p", [Params]),
                                   Payload = [ContextAcc#cb_context{resp_status=fatal} | Params],
                                   crossbar_bindings:fold(Event, Payload)
                           end, Context0#cb_context{resp_status=fatal}, Nouns),
    C = cb_context:import_errors(Context1),
    case succeeded(C) of
        true -> process_billing(C);
        false -> C
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client is
%% authorized for this request
%% @end
%%--------------------------------------------------------------------
-spec process_billing/1 :: (#cb_context{}) -> #cb_context{}.
process_billing(Context0)->
    Event = <<"v1_resource.billing">>,
    case crossbar_bindings:fold(Event, Context0) of
        #cb_context{resp_status=success}=Resp -> lager:debug("billing returned"), Resp;
        #cb_context{}=Resp -> lager:debug("billing failed"), Resp;
        _E -> lager:debug("billing failed: ~p", [_E]), Context0
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the response is of type success
%% @end
%%--------------------------------------------------------------------
-spec succeeded/1 :: (#cb_context{}) -> boolean().
succeeded(#cb_context{resp_status=success}) -> true;
succeeded(_) -> false.

-spec execute_request/2 :: (#http_req{}, #cb_context{}) -> {boolean() | 'halt', #http_req{}, #cb_context{}}.
execute_request(Req, #cb_context{req_nouns=[{Mod, Params}|_], req_verb=Verb}=Context) ->
    Event = <<"v1_resource.execute.", Verb/binary, ".", Mod/binary>>,
    Payload = [Context | Params],
    case crossbar_bindings:fold(Event, Payload) of
        #cb_context{resp_status=success}=Context1 ->
            lager:debug("execution finished"),
            execute_request_results(Req, Context1);
        #cb_context{}=Context1 ->
            ?MODULE:halt(Req, Context1);
        {error, _E} ->
            lager:debug("error executing request: ~p", [_E]),
            {false, Req, Context};
        _E ->
            lager:debug("unexpected return from the fold: ~p", [_E]),
            {false, Req, Context}
    end;
execute_request(Req, Context) ->
    lager:debug("execute request false end"),
    {false, Req, Context}.

-spec execute_request_results/2 :: (#http_req{}, #cb_context{}) -> {'true' | 'halt', #http_req{}, #cb_context{}}.
execute_request_results(Req, #cb_context{req_nouns=[{Mod, Params}|_], req_verb=Verb}=Context) ->
    case succeeded(Context) of
        false ->
            ?MODULE:halt(Req, Context);
        true ->
            lager:debug("executed ~s request for ~s: ~p", [Verb, Mod, Params]),
            {true, Req, Context}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function runs the request terminated bindings at the conclusion
%% of all requests
%% @end
%%--------------------------------------------------------------------
-spec request_terminated/2 :: (#http_req{}, #cb_context{}) -> 'ok'.
request_terminated(_Req, #cb_context{req_nouns=[{Mod, _}|_], req_verb=Verb}=Context) ->
    Event = <<"v1_resource.request_terminated.", Verb/binary, ".", Mod/binary>>,
    _ = crossbar_bindings:map(Event, Context),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function runs the request terminated bindings at the conclusion
%% of all requests
%% @end
%%--------------------------------------------------------------------
-spec finish_request/2 :: (#http_req{}, #cb_context{}) -> 'ok'.
finish_request(_Req, #cb_context{req_nouns=[{Mod, _}|_], req_verb=Verb}=Context) ->
    Event = <<"v1_resource.finish_request.", Verb/binary, ".", Mod/binary>>,
    _ = crossbar_bindings:map(Event, Context),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create the content for the response body
%% @end
%%--------------------------------------------------------------------
-spec create_resp_content/2 :: (#http_req{}, #cb_context{}) -> {ne_binary() | iolist(), #http_req{}}.
create_resp_content(Req0, #cb_context{req_json=ReqJson}=Context) ->
    try wh_json:encode(wh_json:from_list(create_resp_envelope(Context))) of
        JSON ->
            case wh_json:get_value(<<"jsonp">>, ReqJson) of
                undefined ->
                    {JSON, Req0};
                JsonFun when is_binary(JsonFun) ->
                    lager:debug("jsonp wrapping in ~s: ~p", [JsonFun, JSON]),
                    {[JsonFun, <<"(">>, JSON, <<");">>], Req0}
            end
    catch
        _E:_R ->
            lager:debug("failed to encode response: ~s: ~p", [_E, _R]),
            {[], Req0}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pushing data (like PUT)
%% @end
%%--------------------------------------------------------------------
-spec create_push_response/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
create_push_response(Req0, Context) ->
    lager:debug("create push response"),
    {Content, Req1} = create_resp_content(Req0, Context),
    Req2 = set_resp_headers(Req1, Context),
    lager:debug("content: ~s", [wh_util:to_binary(Content)]),
    {ok, Req3} = cowboy_http_req:set_resp_body(Content, Req2),
    Succeeded = succeeded(Context),
    lager:debug("is successful response: ~p", [Succeeded]),
    {Succeeded, Req3, Context}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pulling data (like GET)
%% @end
%%--------------------------------------------------------------------
-spec create_pull_response/2 :: (#http_req{}, #cb_context{}) -> {ne_binary() | iolist() | 'halt', #http_req{}, #cb_context{}}.
create_pull_response(Req0, Context) ->
    {Content, Req1} = create_resp_content(Req0, Context),
    lager:debug("content: ~s", [wh_util:to_binary(Content)]),
    Req2 = set_resp_headers(Req1, Context),
    case succeeded(Context) of
        false -> {halt, Req2, Context};
        true -> {Content, Req2, Context}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function extracts the reponse fields and puts them in a proplist
%% @end
%%--------------------------------------------------------------------
-spec create_resp_envelope/1 :: (#cb_context{}) -> wh_json:json_proplist(<<_:32,_:_*8>>).
create_resp_envelope(Context) ->
    do_create_resp_envelope(cb_context:import_errors(Context)).

do_create_resp_envelope(#cb_context{resp_data=RespData, resp_status=success, auth_token=AuthToken
                                    ,resp_etag=undefined, req_id=RequestId}) ->
    [{<<"auth_token">>, AuthToken}
     ,{<<"status">>, <<"success">>}
     ,{<<"data">>, RespData}
     ,{<<"request_id">>, RequestId}
    ];
do_create_resp_envelope(#cb_context{resp_data=RespData, resp_status=success, auth_token=AuthToken
                                    ,resp_etag=Etag, req_id=RequestId}) ->
    [{<<"auth_token">>, AuthToken}
     ,{<<"status">>, <<"success">>}
     ,{<<"data">>, RespData}
     ,{<<"request_id">>, RequestId}
     ,{<<"revision">>, wh_util:to_binary(Etag)}
    ];
do_create_resp_envelope(#cb_context{auth_token=AuthToken, resp_data=RespData, resp_status=RespStatus
                                    ,resp_error_code=undefined, resp_error_msg=RespErrorMsg, req_id=RequestId}) ->
    Msg = case RespErrorMsg of
              undefined ->
                  StatusBin = wh_util:to_binary(RespStatus),
                  <<"Unspecified server error: ", StatusBin/binary>>;
              Else ->
                  wh_util:to_binary(Else)
          end,

    [{<<"auth_token">>, wh_util:to_binary(AuthToken)}
     ,{<<"status">>, wh_util:to_binary(RespStatus)}
     ,{<<"message">>, wh_util:to_binary(Msg)}
     ,{<<"error">>, <<"500">>}
     ,{<<"data">>, RespData}
     ,{<<"request_id">>, RequestId}
    ];
do_create_resp_envelope(#cb_context{resp_error_msg=RespErrorMsg, resp_status=RespStatus, resp_error_code=RespErrorCode
                                    ,resp_data=RespData, auth_token=AuthToken, req_id=RequestId}) ->
    Msg = case RespErrorMsg of
              undefined ->
                  StatusBin = wh_util:to_binary(RespStatus),
                  ErrCodeBin = wh_util:to_binary(RespErrorCode),
                  <<"Unspecified server error: ", StatusBin/binary, "(", ErrCodeBin/binary, ")">>;
              Else ->
                  wh_util:to_binary(Else)
          end,
    lager:debug("generating ~s ~b response, ~s", [RespStatus, wh_util:to_integer(RespErrorCode), wh_util:to_binary(Msg)]),
    [{<<"auth_token">>, wh_util:to_binary(AuthToken)}
     ,{<<"status">>, wh_util:to_binary(RespStatus)}
     ,{<<"message">>, wh_util:to_binary(Msg)}
     ,{<<"error">>, wh_util:to_binary(RespErrorCode)}
     ,{<<"data">>, RespData}
     ,{<<"request_id">>, RequestId}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Iterate through #cb_context.resp_headers, setting the headers specified
%% @end
%%--------------------------------------------------------------------
-spec set_resp_headers/2 :: (#http_req{}, #cb_context{}) -> #http_req{}.
set_resp_headers(Req0, #cb_context{resp_headers=[]}) -> Req0;
set_resp_headers(Req0, #cb_context{resp_headers=Headers}) ->
    lists:foldl(fun({Header, Value}, ReqAcc) ->
                        {H, V} = fix_header(Header, Value, ReqAcc),

                        {ok, ReqAcc1} = cowboy_http_req:set_resp_header(H, V, ReqAcc),
                        ReqAcc1
                end, Req0, Headers).

-spec fix_header/3 :: (nonempty_string() | ne_binary(), nonempty_string() | ne_binary(), #http_req{}) -> {binary(), binary()}.
fix_header(<<"Location">> = H, Path, Req) ->
    {H, crossbar_util:get_path(Req, Path)};
fix_header(H, V, _) ->
    {wh_util:to_binary(H), wh_util:to_binary(V)}.

-spec halt/2 :: (#http_req{}, #cb_context{}) -> {'halt', #http_req{}, #cb_context{}}.
halt(Req0, #cb_context{resp_error_code=StatusCode}=Context) ->
    lager:debug("halting execution here"),
    {Content, Req1} = create_resp_content(Req0, Context),
    lager:debug("setting resp body: ~s", [Content]),
    {ok, Req2} = cowboy_http_req:set_resp_body(Content, Req1),
    lager:debug("setting status code: ~p", [StatusCode]),
    {ok, Req3} = cowboy_http_req:reply(StatusCode, Req2),
    {halt, Req3, Context}.
