%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% API resource
%%%
%%%
%%% @end
%%% Created :  05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(v1_resource).

-export([init/1]).
-export([to_json/2, to_binary/2]).
-export([from_json/2, from_form/2, from_binary/2]).
-export([encodings_provided/2, finish_request/2, forbidden/2, allowed_methods/2]).
-export([malformed_request/2, content_types_provided/2, content_types_accepted/2, resource_exists/2]).
-export([allow_missing_post/2, post_is_create/2, create_path/2, options/2]).
-export([expires/2, generate_etag/2]).
-export([process_post/2, delete_resource/2]).

-include("crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(NAME, <<"v1_resource">>).

%%%===================================================================
%%% WebMachine API
%%%===================================================================
init(Opts) ->
    ?TIMER_START("v1.init begin"),
    {Context, _} = crossbar_bindings:fold(<<"v1_resource.init">>, {#cb_context{start=now()}, Opts}),
    ?TIMER_TICK("v1.init end"),
    {ok, Context}.
    %% {{trace, "/tmp"}, Context}.
    %% wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp"). % in your running shell to look at trace files
    %% binds http://host/wmtrace and stores the files in /tmp
    %% wmtrace_resource:remove_dispatch_rules/0 removes the trace rule

allowed_methods(RD, #cb_context{allowed_methods=Methods}=Context) ->
    ?TIMER_TICK("v1.allowed_methods begin"),
    ReqId = couch_mgr:get_uuid(),
    put(callid, ReqId),
    ?LOG_START("recieved new request ~s", [wrq:disp_path(RD)]),
    ?LOG("host: ~s", [wrq:get_req_header("Host", RD)]),
    ?LOG("content type: ~s", [wrq:get_req_header("Content-Type", RD)]),
    ?LOG("accepts: ~s", [wrq:get_req_header("Accept", RD)]),
    ?LOG("method coming in: ~s", [wrq:method(RD)]),
    ?LOG("user-agent: ~s", [wrq:get_req_header("User-Agent", RD)]),

    %% Body = wrq:req_body(RD),
    #cb_context{req_json=ReqJSON}=Context1 = case wrq:get_req_header("Content-Type", RD) of
		   "multipart/form-data" ++ _ ->
		       extract_files_and_params(RD, Context#cb_context{req_id=ReqId});
		   "application/x-www-form-urlencoded" ++ _ ->
		       extract_files_and_params(RD, Context#cb_context{req_id=ReqId});
		   "application/json" ++ _ ->
		       Context#cb_context{req_json=get_json_body(RD), req_id=ReqId};
		   "application/x-json" ++ _ ->
		       Context#cb_context{req_json=get_json_body(RD), req_id=ReqId};
		   %% _ when Body =:= undefined; Body =:= <<>> ->
                   %%     Context#cb_context{req_json=?EMPTY_JSON_OBJECT};
		   _ ->
		       extract_file(RD, Context#cb_context{req_json=?EMPTY_JSON_OBJECT, req_id=ReqId})
	       end,

    Verb = get_http_verb(RD, ReqJSON),
    ?LOG("method using for request: ~s", [Verb]),

    Tokens = lists:map(fun wh_util:to_binary/1, wrq:path_tokens(RD)),

    case parse_path_tokens(Tokens) of
        [{Mod, Params}|_] = Nouns ->
            Responses = crossbar_bindings:map(<<"v1_resource.allowed_methods.", Mod/binary>>, Params),
            Methods1 = allow_methods(Responses, Methods, Verb, wrq:method(RD)),
            case is_cors_preflight(RD) of
                true ->
		    ?TIMER_TICK("v1.allowed_methods end OPTS"),
                    ?LOG("allowing OPTIONS request for CORS preflight"),
                    {['OPTIONS'], RD, Context1#cb_context{req_nouns=Nouns, req_verb=Verb, allow_methods=Methods1}};
                false ->
		    ?TIMER_TICK("v1.allowed_methods end Meth1"),
                    {Methods1
                     ,add_cors_headers(RD, Context1)
                     ,Context1#cb_context{req_nouns=Nouns, req_verb=Verb, allow_methods=Methods1}}
            end;
        [] ->
	    ?TIMER_TICK("v1.allowed_methods end Meths"),
            {Methods
             ,add_cors_headers(RD, Context1)
             ,Context1#cb_context{req_verb=Verb}}
    end.

-spec(malformed_request/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(boolean(), #wm_reqdata{}, #cb_context{})).
malformed_request(RD, #cb_context{req_json={malformed, ErrBin}}=Context) ->
    ?TIMER_TICK("v1.malformed_request w/ bad JSON begin"),
    ?LOG("malformed or invalid request with bad JSON"),
    Context1 = Context#cb_context{
		 resp_status = error
		 ,resp_error_msg = <<"Invalid or malformed content: ", ErrBin/binary>>
		     ,resp_error_code = 400
		},
    Content = create_resp_content(RD, Context1),
    RD1 = wrq:set_resp_body(Content, RD),
    ?TIMER_TICK("v1.malformed_request w/ bad JSON end"),
    {true, RD1, Context1};
malformed_request(RD, #cb_context{req_json=Json, req_verb=Verb}=Context) ->
    ?TIMER_TICK("v1.malformed_request start"),
    Data = wh_json:get_value([<<"data">>], Json),
    Auth = get_auth_token(RD, wh_json:get_value(<<"auth_token">>, Json, <<>>), Verb),
    ?LOG("request is using auth token ~s", [Auth]),
    ?TIMER_TICK("v1.malformed_request end"),
    {false, RD, Context#cb_context{req_json=Json, req_data=Data, auth_token=Auth}}.

forbidden(RD, Context) ->
    ?TIMER_TICK("v1.forbidden start"),
    case is_authentic(RD, Context) of
        {true, RD1, Context1} ->
            ?LOG("the request has been authenticated"),
            case is_permitted(RD1, Context1) of
                {true, RD2, Context2} ->
		    ?TIMER_TICK("v1.forbidden false end"),
                    ?LOG("the request has been authorized"),
                    {false, RD2, Context2};
                false ->
		    ?TIMER_TICK("v1.forbidden true end"),
                    ?LOG("the request is not permitted, returning 403"),
                    {true, RD1, Context1}
            end;
        false ->
	    ?TIMER_TICK("v1.forbidden halt end"),
            ?LOG("the request is not authentic, returning 401"),
            {{halt, 401}, RD, Context}
    end.

resource_exists(RD, #cb_context{req_nouns=[{<<"404">>,_}|_]}=Context) ->
    ?LOG("failed to tokenize request, returning 404"),
    {false, RD, Context};
resource_exists(RD, Context) ->
    ?TIMER_TICK("v1.resource_exists start"),
    case does_resource_exist(RD, Context) of
	true ->
            ?LOG("requested resource exists"),
            {RD1, Context1} = validate(RD, Context),
            case succeeded(Context1) of
                true ->
		    ?LOG("requested resource validated"),
                    {Context1#cb_context.req_verb =/= <<"put">>, RD1, Context1};
                false ->
                    Content = create_resp_content(RD, Context1),
                    RD2 = wrq:append_to_response_body(Content, RD1),
                    ReturnCode = Context1#cb_context.resp_error_code,
		    ?LOG("requested resource did not validate, returning ~w", [ReturnCode]),
		    ?TIMER_TICK("v1.resource_exists halt end"),
                    {{halt, ReturnCode}, wrq:remove_resp_header("Content-Encoding", RD2), Context1}
            end;
	false ->
	    ?TIMER_TICK("v1.resource_exists false end"),
	    ?LOG("requested resource does not exist"),
	    {false, RD, Context}
    end.

options(RD, Context) ->
    {get_cors_headers(Context), RD, Context}.

%% each successive cb module adds/removes the content types they provide (to be matched against the request Accept header)
content_types_provided(RD, #cb_context{req_nouns=Nouns}=Context) ->
    Context1 = lists:foldr(fun({Mod, Params}, Context0) ->
				   Event = <<"v1_resource.content_types_provided.", Mod/binary>>,
				   Payload = {RD, Context0, Params},
				   {_, Context01, _} = crossbar_bindings:fold(Event, Payload),
				   Context01
			   end, Context, Nouns),
    CTP = lists:foldr(fun({Fun, L}, Acc) ->
			      lists:foldr(fun(EncType, Acc1) -> [ {EncType, Fun} | Acc1 ] end, Acc, L)
		      end, [], Context1#cb_context.content_types_provided),
    {CTP, RD, Context1}.

content_types_accepted(RD, #cb_context{req_nouns=Nouns}=Context) ->
    Context1 = lists:foldr(fun({Mod, Params}, Context0) ->
				   Event = <<"v1_resource.content_types_accepted.", Mod/binary>>,
				   Payload = {RD, Context0, Params},
				   {_, Context01, _} = crossbar_bindings:fold(Event, Payload),
				   Context01
			   end, Context, Nouns),
    CTA = lists:foldr(fun({Fun, L}, Acc) ->
			      lists:foldr(fun(EncType, Acc1) -> [ {EncType, Fun} | Acc1 ] end, Acc, L)
		      end, [], Context1#cb_context.content_types_accepted),
    {CTA, RD, Context1}.

generate_etag(RD, Context) ->
    Event = <<"v1_resource.etag">>,
    {RD1, Context1} = crossbar_bindings:fold(Event, {RD, Context}),
    case Context1#cb_context.resp_etag of
        automatic ->
            Tag = mochihex:to_hex(crypto:md5(create_resp_content(RD1, Context1))),
            ?LOG("using automatic etag ~s", [Tag]),
            {Tag, RD1, Context1#cb_context{resp_etag=Tag}};
        undefined ->
            ?LOG("no etag provided, skipping", []),
            {undefined, RD1, Context1#cb_context{resp_etag=undefined}};
        Tag when is_list(Tag) ->
            ?LOG("using etag ~s", [Tag]),
            {Tag, RD1, Context1#cb_context{resp_etag=Tag}}
    end.

encodings_provided(RD, Context) ->
    ?TIMER_TICK("enc_provided s & f"),
    { [ {"identity", fun(X) -> X end} ]
      ,RD, Context}.

expires(RD, #cb_context{resp_expires=Expires}=Context) ->
    Event = <<"v1_resource.expires">>,
    crossbar_bindings:fold(Event, {Expires, RD, Context}).

process_post(RD, Context) ->
    case execute_request(RD, Context) of
        {true, RD1, Context1} ->
            Event = <<"v1_resource.process_post">>,
            _ = crossbar_bindings:map(Event, {RD1, Context1}),
            create_push_response(RD1, Context1);
        Else ->
            Else
    end.

delete_resource(RD, Context) ->
    case execute_request(RD, Context) of
        {true, RD1, Context1} ->
            Event = <<"v1_resource.delete_resource">>,
            _ = crossbar_bindings:map(Event, {RD1, Context1}),
            create_push_response(RD1, Context1);
        Else ->
            Else
    end.

finish_request(RD, #cb_context{start=T1}=Context) ->
    ?TIMER_TICK("v1.finish_request start"),
    Event = <<"v1_resource.finish_request">>,
    {RD1, Context1} = crossbar_bindings:fold(Event, {RD, Context}),
    ?LOG("response body: ~s", [wrq:resp_body(RD1)]),
    ?LOG_END("fulfilled in ~p ms", [timer:now_diff(now(), T1)*0.001]),
    ?TIMER_STOP("v1.finish_request end"),
    {true, set_req_header(RD1, Context1), Context1}.

%%%===================================================================
%%% Content Acceptors
%%%===================================================================
from_json(RD, Context) ->
    case execute_request(RD, Context) of
        {true, RD1, Context1} ->
            Event = <<"v1_resource.from_json">>,
            _ = crossbar_bindings:map(Event, {RD1, Context1}),
            create_push_response(RD1, Context1);
        Else ->
            Else
    end.

from_form(RD, Context) ->
    case execute_request(RD, Context) of
        {true, RD1, Context1} ->
            Event = <<"v1_resource.from_form">>,
            _ = crossbar_bindings:map(Event, {RD1, Context1}),
            create_push_response(RD1, Context1);
        Else ->
            Else
    end.

from_binary(RD, Context) ->
    case execute_request(RD, Context) of
        {true, RD1, Context1} ->
            Event = <<"v1_resource.from_binary">>,
            _ = crossbar_bindings:map(Event, {RD1, Context1}),
            create_push_response(RD1, Context1);
        Else ->
            Else
    end.

%%%===================================================================
%%% Content Providers
%%%===================================================================
-spec(to_json/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(iolist() | tuple(halt, 500), #wm_reqdata{}, #cb_context{})).
to_json(RD, Context) ->
    Event = <<"v1_resource.to_json">>,
    _ = crossbar_bindings:map(Event, {RD, Context}),
    create_pull_response(RD, Context).

-spec(to_binary/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(iolist() | tuple(halt, 500), #wm_reqdata{}, #cb_context{})).
to_binary(RD, #cb_context{resp_data=RespData}=Context) ->
    Event = <<"v1_resource.to_binary">>,
    _ = crossbar_bindings:map(Event, {RD, Context}),
    {RespData, set_resp_headers(RD, Context), Context}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will loop over the Tokens in the request path and return
%% a proplist with keys being the module and values a list of parameters
%% supplied to that module.  If the token order is improper a empty list
%% is returned.
%% @end
%%--------------------------------------------------------------------
-spec parse_path_tokens/1 :: (Tokens) -> [{binary(), [binary(),...]},...] | [] when
      Tokens :: [binary(),...] | [].
parse_path_tokens(Tokens) ->
    Loaded = [ wh_util:to_binary(Mod) || {Mod, _, _, _} <- supervisor:which_children(crossbar_module_sup) ],
    parse_path_tokens(Tokens, Loaded, []).

-spec parse_path_tokens/3 :: (Tokens, Loaded, Events) -> [{binary(), [binary(),...]},...] | [] when
      Tokens :: [binary(),...] | [],
      Loaded :: [binary(),...],
      Events :: [{binary(), [binary(),...]},...] | [].
parse_path_tokens([], _Loaded, Events) ->
    Events;
parse_path_tokens([Mod|T], Loaded, Events) ->
    case lists:member(<<"cb_", (Mod)/binary>>, Loaded) of
        false ->
	    [];
        true ->
            {Params, List2} = lists:splitwith(fun(Elem) -> not lists:member(<<"cb_", (Elem)/binary>>, Loaded) end, T),
            Params1 = [ wh_util:to_binary(P) || P <- Params ],
            parse_path_tokens(List2, Loaded, [{Mod, Params1} | Events])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Addes the request id as a custom header on all responses
%% @end
%%--------------------------------------------------------------------
set_req_header(RD, #cb_context{req_id=ReqId}) ->
    wrq:set_resp_header("X-Request-ID", wh_util:to_list(ReqId), RD).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check whether we need to change the HTTP verb to send to the crossbar
%% modules. Only valid for POST requests, and only to change it from
%% POST to PUT or DELETE.
%% @end
%%--------------------------------------------------------------------
-spec(get_http_verb/2 :: (RD :: #wm_reqdata{}, JSON :: json_object() | malformed) -> binary()).
get_http_verb(RD, {malformed, _}) ->
    wh_util:to_binary(string:to_lower(atom_to_list(wrq:method(RD))));
get_http_verb(RD, JSON) ->
    HttpV = wh_util:to_binary(string:to_lower(atom_to_list(wrq:method(RD)))),
    case override_verb(RD, JSON, HttpV) of
	{true, OverrideV} ->
            ?LOG("override verb, treating request as a ~s", [OverrideV]),
            OverrideV;
	false -> HttpV
    end.

-spec(override_verb/3 :: (RD :: #wm_reqdata{}, JSON :: json_object(), Verb :: binary()) -> tuple(true, binary()) | false).
override_verb(RD, JSON, <<"post">>) ->
    case wh_json:get_value(<<"verb">>, JSON) of
	undefined ->
	    case wrq:get_qs_value("verb", RD) of
		undefined -> false;
		V -> {true, wh_util:to_binary(string:to_lower(V))}
	    end;
	V -> {true, wh_util:to_binary(string:to_lower(binary_to_list(V)))}
    end;
override_verb(RD, _, <<"options">>) ->
    case wrq:get_req_header("Access-Control-Request-Method", RD) of
        undefined -> false;

        V -> {true, wh_util:to_binary(string:to_lower(V))}
    end;
override_verb(_, _, _) -> false.

-spec(get_json_body/1 :: (RD :: #wm_reqdata{}) -> json_object() | tuple(malformed, binary())).
get_json_body(RD) ->
    try
	QS = wh_json:from_list([ {wh_util:to_binary(K), wh_util:to_binary(V)} || {K,V} <- wrq:req_qs(RD)]),
	case wrq:req_body(RD) of
	    <<>> -> QS;
	    ReqBody ->
		JSON = mochijson2:decode(ReqBody),
		case is_valid_request_envelope(JSON) of
		    true -> wh_json:merge_jobjs(JSON, QS);
		    false ->
                        ?LOG("invalid request envelope"),
                        {malformed, <<"Invalid request envelope">>}
		end
	end
    catch
	_:{badmatch, {comma,{decoder,_,S,_,_,_}}} ->
            ?LOG("failed to decode json: comma error around char ~s", [wh_util:to_list(S)]),
	    {malformed, list_to_binary(["Failed to decode: comma error around char ", wh_util:to_list(S)])};
	_:E ->
	    ?LOG("failed to decode json: ~p", [E]),
	    {malformed, <<"JSON failed to validate; check your commas and curlys">>}
    end.

-spec(extract_files_and_params/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> #cb_context{}).
extract_files_and_params(RD, Context) ->
    try
	Boundry = webmachine_multipart:find_boundary(RD),
	?LOG("extracting files with boundry: ~s", [Boundry]),
	StreamReqBody = wrq:stream_req_body(RD, 1024),
	StreamParts = webmachine_multipart:stream_parts(StreamReqBody, Boundry),

	{ReqProp, FilesProp} = get_streamed_body(StreamParts),
	?LOG("extracted request vars(~b) and files(~b)", [length(ReqProp), length(FilesProp)]),
	Context#cb_context{req_json={struct, ReqProp}, req_files=FilesProp}
    catch
	_A:_B ->
	    ?LOG("exception extracting files and params: ~p:~p", [_A, _B]),
	    ?LOG_END("stacktrace: ~p", [erlang:get_stacktrace()]),
	    Context
    end.

-spec(extract_file/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> #cb_context{}).
extract_file(RD, Context) ->
    FileContents = wh_util:to_binary(wrq:req_body(RD)),
    ContentType = wrq:get_req_header("Content-Type", RD),
    ContentSize = wrq:get_req_header("Content-Length", RD),
    ?LOG("extracting file content type ~s", [ContentType]),
    ?LOG("extracting file content size ~s", [ContentSize]),
    Context#cb_context{req_files=[{<<"uploaded_file">>, {struct, [{<<"headers">>, {struct, [{<<"content_type">>, ContentType}
											    ,{<<"content_length">>, ContentSize}
											   ]}}
								  ,{<<"contents">>, FileContents}
								 ]}
				  }]
		      }.

get_streamed_body(StreamReq) ->
    get_streamed_body(StreamReq, [], []).

-spec(get_streamed_body/3 :: (Term :: term(), ReqProp :: proplist(), FilesProp :: proplist()) -> tuple(proplist(), proplist())).
get_streamed_body(done_parts, ReqProp, FilesProp) ->
    ?LOG("Done streaming body"),
    {ReqProp, FilesProp};
get_streamed_body({{_Ignored, {Params, []}, Content}, Next}, ReqProp, FilesProp) ->
    Key = wh_util:to_binary(props:get_value(<<"name">>, Params)),
    Value = binary:replace(wh_util:to_binary(Content), <<$\r,$\n>>, <<>>, [global]),

    ?LOG("streamed query params: ~s: ~s", [Key, Value]),

    get_streamed_body(Next(), [{Key, Value} | ReqProp], FilesProp);
get_streamed_body({{_Ignored, {Params, Hdrs}, Content}, Next}, ReqProp, FilesProp) ->
    Key = wh_util:to_binary(props:get_value(<<"name">>, Params)),
    FileName = wh_util:to_binary(props:get_value(<<"filename">>, Params)),

    Value = wh_util:to_binary(Content),

    ?LOG("streamed file headers ~p", [Hdrs]),
    ?LOG("streamed file name ~s (~s)", [Key, FileName]),
    get_streamed_body(Next(), ReqProp, [{Key, {struct, [{<<"headers">>, wh_json:normalize_jobj({struct, Hdrs})}
							,{<<"contents">>, Value}
							,{<<"filename">>, FileName}
						       ]}}
					| FilesProp]);
get_streamed_body(Term, _, _) ->
    ?LOG("Erro get_streamed_body: ~p", [Term]).


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
-spec(allow_methods/4  :: (Reponses :: list(tuple(term(), term())), Avaliable :: http_methods(), ReqVerb :: binary(), HttpVerb :: atom()) -> http_methods()).
allow_methods(Responses, Available, ReqVerb, HttpVerb) ->
    case crossbar_bindings:succeeded(Responses) of
        [] ->
	    Available;
	Succeeded ->
	    Allowed = lists:foldr(fun({true, Response}, Acc) ->
					  Set1 = sets:from_list(Acc),
					  Set2 = sets:from_list(Response),
					  sets:to_list(sets:intersection(Set1, Set2))
				  end, Available, Succeeded),
            add_post_method(ReqVerb, HttpVerb, Allowed)
    end.

%% insert 'POST' if Verb is in Allowed; otherwise remove 'POST'.
add_post_method(Verb, 'POST', Allowed) ->
    VerbAtom = list_to_atom(string:to_upper(binary_to_list(Verb))),
    case lists:member(VerbAtom, Allowed) of
	true -> ['POST' | Allowed];
	false -> lists:delete('POST', Allowed)
    end;
add_post_method(_, _, Allowed) ->
    Allowed.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will look for the authorization token, first checking the
%% request headers, if not found there it will look either in the HTTP
%% query paramerts (for GET and DELETE) or HTTP content (for POST and PUT)
%% @end
%%--------------------------------------------------------------------
-spec(get_auth_token/3 :: (RD :: #wm_reqdata{}, JsonToken :: binary(), Verb :: binary()) -> binary()).
get_auth_token(RD, JsonToken, Verb) ->
    case wrq:get_req_header("X-Auth-Token", RD) of
        undefined ->
            case Verb of
                <<"get">> ->
                    wh_util:to_binary(props:get_value("auth_token", wrq:req_qs(RD), <<>>));
		_ ->
		    JsonToken
	    end;
        AuthToken ->
            wh_util:to_binary(AuthToken)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the request envelope is valid
%% @end
%%--------------------------------------------------------------------
-spec(is_valid_request_envelope/1 :: (JSON :: json_object()) -> boolean()).
is_valid_request_envelope(JSON) ->
    wh_json:get_value([<<"data">>], JSON, not_found) =/= not_found.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the target noun
%% (the final module in the chain) accepts this verb parameter pair.
%% @end
%%--------------------------------------------------------------------
-spec(does_resource_exist/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> boolean()).
does_resource_exist(_RD, #cb_context{req_nouns=[{Mod, Params}|_]}) ->
    Event = <<"v1_resource.resource_exists.", Mod/binary>>,
    Responses = crossbar_bindings:map(Event, Params),
    crossbar_bindings:all(Responses) and true;
does_resource_exist(_RD, _Context) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client has
%% provided a valid authentication token
%% @end
%%--------------------------------------------------------------------
-spec(is_authentic/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> false | tuple(true, #wm_reqdata{}, #cb_context{})).
is_authentic(RD, Context)->
    case wrq:method(RD) of
        %% all all OPTIONS, they are harmless (I hope) and required for CORS preflight
        'OPTIONS' ->
            {true, RD, Context};
        _ ->
            Event = <<"v1_resource.authenticate">>,
            case crossbar_bindings:succeeded(crossbar_bindings:map(Event, {RD, Context})) of
                [] ->
                    false;
                [{true, {RD1, Context1}}|_] ->
                    {true, RD1, Context1}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client is
%% authorized for this request
%% @end
%%--------------------------------------------------------------------
-spec(is_permitted/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> false | tuple(true, #wm_reqdata{}, #cb_context{})).
is_permitted(RD, Context)->
    case wrq:method(RD) of
        %% all all OPTIONS, they are harmless (I hope) and required for CORS preflight
        'OPTIONS' ->
            {true, RD, Context};
        _ ->
            Event = <<"v1_resource.authorize">>,
            case crossbar_bindings:succeeded(crossbar_bindings:map(Event, {RD, Context})) of
                [] ->
		    false;
                [{true, {RD1, Context1}}|_] ->
                    {true, RD1, Context1}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client is
%% authorized for this request
%% @end
%%--------------------------------------------------------------------
-spec(process_billing/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> false | tuple(true, #wm_reqdata{}, #cb_context{})).
process_billing(RD, Context)->
    case wrq:method(RD) of
        %% all all OPTIONS, they are harmless (I hope) and required for CORS preflight
        'OPTIONS' ->
            {RD, Context};
        _ ->
            Event = <<"v1_resource.billing">>,
            case crossbar_bindings:fold(Event, {RD, Context}) of
                {RD1, Context1} ->
                    {RD1, Context1};
                _ ->
                    RD, Context
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function gives each noun a chance to determine if
%% it is valid and returns the status, and any errors
%% @end
%%--------------------------------------------------------------------
-spec(validate/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(#wm_reqdata{}, #cb_context{})).
validate(RD, #cb_context{req_nouns=Nouns}=Context) ->
    {RD1, Context1} = lists:foldr(fun({Mod, Params}, {RD1, Context1}) ->
                                          Event = <<"v1_resource.validate.", Mod/binary>>,
                                          Payload = [RD1, Context1] ++ Params,
                                          [RD2, Context2 | _] = crossbar_bindings:fold(Event, Payload),
                                          {RD2, Context2}
                                  end, {RD, Context}, Nouns),
    case succeeded(Context1) of
        true ->
            process_billing(RD1, Context1);
        false ->
            {RD1, Context1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will execute the request
%% @end
%%--------------------------------------------------------------------
-spec(execute_request/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(true|tuple(halt, 500), #wm_reqdata{}, #cb_context{})).
execute_request(RD, #cb_context{req_nouns=[{Mod, Params}|_], req_verb=Verb}=Context) ->
    ?TIMER_TICK("v1.execute_request start"),
    Event = <<"v1_resource.execute.", Verb/binary, ".", Mod/binary>>,
    Payload = [RD, Context] ++ Params,
    ?LOG("execute request ~s", [Event]),
    case crossbar_bindings:fold(Event, Payload) of
	[RD1, Context1 | _] ->
	    execute_request_results(RD1, Context1);
	{error, _E} ->
	    ?LOG("error executing request: ~p", [_E]),
	    {{halt, 500}, RD, Context}
    end;
execute_request(RD, Context) ->
    ?TIMER_TICK("v1.execute_request false end"),
    ?LOG("execute request false end"),
    {false, RD, Context}.

execute_request_results(RD, #cb_context{req_nouns=[{Mod, Params}|_], req_verb=Verb}=Context) ->
    case succeeded(Context) of
        false ->
            Content = create_resp_content(RD, Context),
            RD1 = wrq:append_to_response_body(Content, RD),
            ReturnCode = Context#cb_context.resp_error_code,
	    ?TIMER_TICK("v1.execute_request halt end"),
	    ?LOG("failed to execute request, returning ~b", [ReturnCode]),
            {{halt, ReturnCode}, wrq:remove_resp_header("Content-Encoding", RD1), Context};
        true ->
	    ?TIMER_TICK("v1.execute_request verb=/=put end"),
	    ?LOG("executed ~s request for ~s: ~p", [Verb, Mod, Params]),
	    {true, RD, Context}
    end.

%% If we're tunneling PUT through POST, we need to tell webmachine POST is allowed to create a non-existant resource
%% AKA, 201 Created header set
allow_missing_post(RD, Context) ->
    {wrq:method(RD) =:= 'POST', RD, Context}.

%% If allow_missing_post returned true (cause it was a POST) and PUT has been tunnelled,
%% POST is a create
post_is_create(RD, #cb_context{req_verb = <<"put">>}=Context) ->
    {true, RD, Context};
post_is_create(RD, Context) ->
    {false, RD, Context}.

%% whatever (for now)
create_path(RD, Context) ->
    {[], RD, Context}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create the content for the response body
%% @end
%%--------------------------------------------------------------------
-spec(create_resp_content/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> iolist()).
create_resp_content(RD, #cb_context{req_json=ReqJson}=Context) ->
    case get_resp_type(RD) of
	binary ->
	    Context#cb_context.resp_data;
        _ ->
	    Prop = create_resp_envelope(Context),
            JSON = mochijson2:encode({struct, Prop}),
	    case wh_json:get_value(<<"jsonp">>, ReqJson) of
		undefined -> JSON;
		JsonFun when is_binary(JsonFun) ->
		    [JsonFun, "(", JSON, ");"]
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pushing data (like PUT)
%% @end
%%--------------------------------------------------------------------
-spec(create_push_response/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(boolean(), #wm_reqdata{}, #cb_context{})).
create_push_response(RD, Context) ->
    Content = create_resp_content(RD, Context),
    RD1 = set_resp_headers(RD, Context),
    {succeeded(Context), wrq:set_resp_body(Content, RD1), Context}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pulling data (like GET)
%% @end
%%--------------------------------------------------------------------
-spec(create_pull_response/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(iolist() | tuple(halt, 500), #wm_reqdata{}, #cb_context{})).
create_pull_response(RD, Context) ->
    Content = create_resp_content(RD, Context),
    RD1 = set_resp_headers(RD, Context),
    case succeeded(Context) of
        false ->
            {{halt, 500}, wrq:set_resp_body(Content, RD1), Context};
        true ->
            {Content, RD, Context}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the response is of type success
%% @end
%%--------------------------------------------------------------------
-spec(succeeded/1 :: (Context :: #cb_context{}) -> boolean()).
succeeded(#cb_context{resp_status=success}) ->
    true;
succeeded(_) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Iterate through #cb_context.resp_headers, setting the headers specified
%% @end
%%--------------------------------------------------------------------
-spec(set_resp_headers/2 :: (RD0 :: #wm_reqdata{}, Context :: #cb_context{}) -> #wm_reqdata{}).
set_resp_headers(RD0, #cb_context{resp_headers=[]}) -> RD0;
set_resp_headers(RD0, #cb_context{resp_headers=Headers}) ->
    lists:foldl(fun({Header, Value}, RD) ->
			{H, V} = fix_header(RD, Header, Value),
			?LOG("response header: ~s: ~s", [H, V]),
			wrq:set_resp_header(H, V, wrq:remove_resp_header(H, RD))
		end, RD0, Headers).

-spec(fix_header/3 :: (RD :: #wm_reqdata{}, Header :: string(), Value :: string() | binary()) -> tuple(string(), string())).
fix_header(RD, "Location"=H, Url) ->
    %% http://some.host.com:port/"
    Port = case wrq:port(RD) of
	       80 -> "";
	       P -> [":", wh_util:to_list(P)]
	   end,

    Host = ["http://", string:join(lists:reverse(wrq:host_tokens(RD)), "."), Port, "/"],
    ?LOG("host: ~s", [Host]),

    %% /v1/accounts/acct_id/module => [module, acct_id, accounts, v1]
    PathTokensRev = lists:reverse(string:tokens(wrq:path(RD), "/")),
    UrlTokens = string:tokens(wh_util:to_list(Url), "/"),

    Url1 =
	string:join(
	  lists:reverse(
	    lists:foldl(fun("..", []) -> [];
			   ("..", [_ | PathTokens]) -> PathTokens;
			   (".", PathTokens) -> PathTokens;
			   (Segment, PathTokens) -> [Segment | PathTokens]
			end, PathTokensRev, UrlTokens)
	   ), "/"),

    {H, lists:concat([Host | [Url1]])};
fix_header(_, H, V) ->
    {wh_util:to_list(H), wh_util:to_list(V)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if this is a cross origin resource sharing
%% request
%% @end
%%--------------------------------------------------------------------
-spec(is_cors_request/1 :: (RD :: #wm_reqdata{}) -> boolean()).
is_cors_request(RD) ->
    wrq:get_req_header("Origin", RD) =/= 'undefined'
        orelse wrq:get_req_header("Access-Control-Request-Method", RD) =/= 'undefined'
        orelse wrq:get_req_header("Access-Control-Request-Headers", RD) =/= 'undefined'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(add_cors_headers/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> #wm_reqdata{}).
add_cors_headers(RD, Context) ->
    case is_cors_request(RD) of
        true ->
            ?LOG("determined that this request requires CORS headers"),
            wrq:set_resp_headers(get_cors_headers(Context), RD);
        false ->
            RD
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_cors_headers/1 :: (Context) -> [{binary(), binary() | string()},...] when
      Context :: #cb_context{}.
get_cors_headers(#cb_context{allow_methods=Allowed}) ->
    ?LOG("adding CORS headers to response"),
    [
      {<<"Access-Control-Allow-Origin">>, <<"*">>}
     ,{<<"Access-Control-Allow-Methods">>, string:join([wh_util:to_list(A) || A <- Allowed], ", ")}
     ,{<<"Access-Control-Allow-Headers">>, <<"Content-Type, Depth, User-Agent, X-File-Size, X-Requested-With, If-Modified-Since, X-File-Name, Cache-Control, X-Auth-Token, If-Match">>}
     ,{<<"Access-Control-Expose-Headers">>, <<"Content-Type, X-Auth-Token, X-Request-ID, Location, Etag, ETag">>}
     ,{<<"Access-Control-Max-Age">>, wh_util:to_binary(?SECONDS_IN_DAY)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if this is a cross origin resource preflight
%% request
%% @end
%%--------------------------------------------------------------------
-spec(is_cors_preflight/1 :: (RD :: #wm_reqdata{}) -> boolean()).
is_cors_preflight(RD) ->
    is_cors_request(RD) andalso wrq:method(RD) =:= 'OPTIONS'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function extracts the reponse fields and puts them in a proplist
%% @end
%%--------------------------------------------------------------------
-spec create_resp_envelope/1 :: (Context) -> [{binary(), binary() | atom() | json_object() | json_objects()},...] when
      Context :: #cb_context{}.
create_resp_envelope(#cb_context{resp_data=RespData, resp_status=success, auth_token=AuthToken, resp_etag=undefined}) ->
    ?LOG("generating sucessfull response"),
    [{<<"auth_token">>, AuthToken}
     ,{<<"status">>, success}
     ,{<<"data">>, RespData}
    ];
create_resp_envelope(#cb_context{resp_data=RespData, resp_status=success, auth_token=AuthToken, resp_etag=Etag}) ->
    ?LOG("generating sucessfull response"),
    [{<<"auth_token">>, AuthToken}
     ,{<<"status">>, success}
     ,{<<"data">>, RespData}
     ,{<<"revision">>, wh_util:to_binary(Etag)}
    ];
create_resp_envelope(#cb_context{auth_token=AuthToken, resp_data=RespData, resp_status=RespStatus
				 ,resp_error_code=undefined, resp_error_msg=RespErrorMsg}) ->
    Msg = case RespErrorMsg of
	      undefined ->
		  StatusBin = wh_util:to_binary(RespStatus),
		  <<"Unspecified server error: ", StatusBin/binary>>;
	      Else ->
		  wh_util:to_binary(Else)
	  end,
    ?LOG("generating ~s 500 response, ~s", [RespStatus, Msg]),
    [{<<"auth_token">>, AuthToken}
     ,{<<"status">>, RespStatus}
     ,{<<"message">>, Msg}
     ,{<<"error">>, 500}
     ,{<<"data">>, RespData}
    ];
create_resp_envelope(#cb_context{resp_error_msg=RespErrorMsg, resp_status=RespStatus, resp_error_code=RespErrorCode
				 ,resp_data=RespData, auth_token=AuthToken}) ->
    Msg = case RespErrorMsg of
	      undefined ->
		  StatusBin = wh_util:to_binary(RespStatus),
		  ErrCodeBin = wh_util:to_binary(RespErrorCode),
		  <<"Unspecified server error: ", StatusBin/binary, "(", ErrCodeBin/binary, ")">>;
	      Else ->
		  wh_util:to_binary(Else)
	  end,
    ?LOG("generating ~s ~b response, ~s", [RespStatus, wh_util:to_integer(RespErrorCode), Msg]),
    [{<<"auth_token">>, AuthToken}
     ,{<<"status">>, RespStatus}
     ,{<<"message">>, Msg}
     ,{<<"error">>, RespErrorCode}
     ,{<<"data">>, RespData}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine the appropriate content format to return
%% based on the request....
%% @end
%%--------------------------------------------------------------------
-spec(get_resp_type/1 :: (RD :: #wm_reqdata{}) -> json|xml|binary).
get_resp_type(RD) ->
    case wrq:get_resp_header("Content-Type", RD) of
        "application/json" -> json;
        "application/x-json" -> json;
	undefined -> json;
        _Else -> binary
    end.
