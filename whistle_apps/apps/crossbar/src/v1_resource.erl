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

-export([init/3, rest_init/2
         ,known_methods/2
         ,allowed_methods/2
         ,malformed_request/2
         ,is_authorized/2
         ,forbidden/2
         ,valid_content_headers/2
         ,known_content_type/2
         ,valid_entity_length/2
         ,options/2
         ,content_types_provided/2
        ]).

-include("crossbar.hrl").

%%%===================================================================
%%% CowboyHTTPRest API
%%%===================================================================
-spec init/3 :: ({'tcp' | 'ssl', 'http'}, #http_req{}, proplist()) -> {'upgrade', 'protocol', 'cowboy_http_rest'}.
init({tcp, http}, _Req, _Opts) ->
    ?LOG("tcp: upgrading to REST"),
    {upgrade, protocol, cowboy_http_rest};
init({ssl, http}, _Req, _Opts) ->
    ?LOG("ssl: upgrading to REST"),
    {upgrade, protocol, cowboy_http_rest}.

-spec rest_init/2 :: (#http_req{}, proplist()) -> {'ok', #http_req{}, #cb_context{}}.
rest_init(Req, Opts) ->
    ?LOG("rest init: Opts: ~p", [Opts]),
    ReqId = couch_mgr:get_uuid(),
    put(callid, ReqId),

    {Context, _} = crossbar_bindings:fold(<<"v1_resource.init">>, {#cb_context{}, Opts}),
    {ok, Req, Context#cb_context{req_id=ReqId}}.

known_methods(Req, Context) ->
    ?LOG("run: known methods"),
    {?ALLOWED_METHODS, Req, Context}.

allowed_methods(Req0, #cb_context{allowed_methods=Methods}=Context) ->
    ?LOG("run: allowed_methods"),

    {Tokens, Req1} = cowboy_http_req:path_info(Req0),

    case v1_util:parse_path_tokens(Tokens) of
        [{Mod, Params}|_] = Nouns ->
            Responses = crossbar_bindings:map(<<"v1_resource.allowed_methods.", Mod/binary>>, Params),

            %% Because we allow tunneling of verbs through the request,
            %% we have to check and see if we need to override the actual
            %% HTTP method with the tunneled version
            {Method, Req2} = cowboy_http_req:method(Req1),
            {Context1, Req3} = v1_util:get_req_data(Context, Req2),

            Verb = v1_util:get_http_verb(Method, Context1),

            ?LOG("http method: ~s, actual verb to be used: ~s", [Method, Verb]),

            Methods1 = v1_util:allow_methods(Responses, Methods, Verb, Method),
            case v1_util:is_cors_preflight(Req3) of
                {true, Req4} ->
                    ?LOG("allowing OPTIONS request for CORS preflight"),
                    {['OPTIONS'], Req4, Context1#cb_context{allow_methods=Methods1
                                                            ,req_nouns=Nouns
                                                           }};
                {false, Req4} ->
                    ?LOG("not CORS preflight"),
                    {Methods1, Req4, Context1#cb_context{allow_methods=Methods1
                                                         ,req_nouns=Nouns
                                                        }}
            end;
        [] ->
            ?LOG("no path tokens: ~p", [Methods]),
            {Methods, Req1, Context#cb_context{allow_methods=Methods}}
    end.

malformed_request(Req, #cb_context{req_json={malformed, _}}=Context) ->
    ?LOG("request is malformed"),
    {true, Req, Context};
malformed_request(Req, Context) ->
    ?LOG("request is not malformed"),
    {false, Req, Context}.

-spec is_authorized/2 :: (#http_req{}, #cb_context{}) -> {'true' | {'false', proplist()}, #http_req{}, #cb_context{}}.
is_authorized(Req, Context) ->
    ?LOG("run: is_authorized"),
    v1_util:is_authentic(Req, Context).

-spec forbidden/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
forbidden(Req0, Context0) ->
    ?LOG("run: forbidden"),
    {IsPermitted, Req1, Context1} = v1_util:is_permitted(Req0, Context0),
    {not IsPermitted, Req1, Context1}.

-spec valid_content_headers/2 :: (#http_req{}, #cb_context{}) -> {'true', #http_req{}, #cb_context{}}.
valid_content_headers(Req, Context) ->
    ?LOG("run: valid_content_headers"),
    {true, Req, Context}.

-spec known_content_type/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
known_content_type(Req, Context) ->
    ?LOG("run: known_content_type"),
    v1_util:is_known_content_type(Req, Context).

-spec valid_entity_length/2 :: (#http_req{}, #cb_context{}) -> {'true', #http_req{}, #cb_context{}}.
valid_entity_length(Req, Context) ->
    ?LOG("run: valid_entity_length"),
    {true, Req, Context}.

-spec options/2 :: (#http_req{}, #cb_context{}) -> {'ok', #http_req{}, #cb_context{}}.
options(Req0, Context) ->
    ?LOG("run: options"),
    case v1_util:is_cors_request(Req0) of
        {true, Req1} ->
            ?LOG("is CORS request"),
            {ok, Req2} = v1_util:add_cors_headers(Req1, Context),
            {ok, Req2, Context};
        {false, Req1} ->
            ?LOG("is not CORS request"),
            {ok, Req1, Context}
    end.

content_types_provided(Req0, Context0) ->
    
