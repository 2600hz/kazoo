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
         ,terminate/2, rest_terminate/2
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
         ,content_types_accepted/2
         ,languages_provided/2
         ,charsets_provided/2
         ,encodings_provided/2
         ,resource_exists/2
         ,moved_temporarily/2
         ,moved_permanently/2
         ,previously_existed/2
         ,allow_missing_post/2
         ,post_is_create/2
         ,create_path/2
         ,process_post/2
         ,delete_resource/2
         ,is_conflict/2
         ,to_json/2, to_binary/2
         ,from_json/2, from_binary/2, from_form/2
         ,multiple_choices/2
         ,generate_etag/2
         ,expires/2
        ]).

-include("crossbar.hrl").

%%%===================================================================
%%% Startup and shutdown of request
%%%===================================================================
-spec init/3 :: ({'tcp' | 'ssl', 'http'}, #http_req{}, proplist()) -> {'upgrade', 'protocol', 'cowboy_http_rest'}.
init({tcp, http}, _Req, _Opts) ->
    ?LOG("tcp: upgrading to REST"),
    {upgrade, protocol, cowboy_http_rest};
init({ssl, http}, _Req, _Opts) ->
    ?LOG("ssl: upgrading to REST"),
    {upgrade, protocol, cowboy_http_rest}.

-spec rest_init/2 :: (#http_req{}, proplist()) -> {'ok', #http_req{}, #cb_context{}}.
rest_init(Req0, Opts) ->
    ?LOG("rest init: Opts: ~p", [Opts]),
    ReqId = couch_mgr:get_uuid(),
    put(callid, ReqId),

    Req1 = lists:foldl(fun(Prop, ReqAcc) ->
                               case cowboy_http_req:Prop(ReqAcc) of
                                   {Val, ReqAcc1} -> ?LOG("~s: ~s", [Prop, wh_util:to_binary(Val)]);
                                   {ok, Val, ReqAcc1} -> ?LOG("~s: ~s", [Prop, wh_util:to_binary(Val)])
                               end,
                               ReqAcc1
                       end, Req0, [raw_host, port, raw_path, raw_qs, method]),

    {Context, _} = crossbar_bindings:fold(<<"v1_resource.init">>, {#cb_context{}, Opts}),
    {ok, Req1, Context#cb_context{req_id=ReqId, resp_headers=[{<<"X-Request-ID">>, ReqId}]}}.

terminate(_, _) ->
    ?LOG_END("session finished").

rest_terminate(_Req, #cb_context{start=T1}=Context) ->
    crossbar_bindings:map(<<"v1_resource.finish_request">>, Context),
    ?LOG_END("fulfilled in ~p ms", [timer:now_diff(now(), T1) div 1000]).

%%%===================================================================
%%% CowboyHTTPRest API Callbacks
%%%===================================================================
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
                    {ok, Req5} = v1_util:add_cors_headers(Req4, Context),
                    {['OPTIONS'], Req5, Context1#cb_context{allow_methods=Methods1
                                                            ,req_nouns=Nouns
                                                            ,req_verb=Verb
                                                           }};
                {false, Req4} ->
                    ?LOG("not CORS preflight"),
                    {ok, Req5} = v1_util:add_cors_headers(Req4, Context),
                    {Methods1, Req5, Context1#cb_context{allow_methods=Methods1
                                                         ,req_nouns=Nouns
                                                         ,req_verb=Verb
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
            {ok, Req3} = cowboy_http_req:set_resp_body(<<>>, Req2),
            {ok, Req3, Context};
        {false, Req1} ->
            ?LOG("is not CORS request"),
            {ok, Req1, Context}
    end.

-type content_type_callbacks() :: [ {{ne_binary(), ne_binary(), proplist()}, atom()} | {ne_binary(), atom()},...] | [].
-spec content_types_provided/2 :: (#http_req{}, #cb_context{}) -> {content_type_callbacks(), #http_req{}, #cb_context{}}.
content_types_provided(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    ?LOG("run: content_types_provided"),
    {Req1, Context1} = lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
                                           Event = <<"v1_resource.content_types_provided.", Mod/binary>>,
                                           Payload = {ReqAcc, ContextAcc, Params},
                                           {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
                                           {ReqAcc1, ContextAcc1}
                                   end, {Req0, Context0}, Nouns),
    CTP = lists:foldr(fun({Fun, L}, Acc) ->
                              lists:foldr(fun({Type, SubType}, Acc1) ->
                                                  [{{Type, SubType, []}, Fun} | Acc1];
                                             (EncType, Acc1) ->
                                                  [ {EncType, Fun} | Acc1 ]
                                          end, Acc, L)
                      end, [], Context1#cb_context.content_types_provided),

    {CTP, Req1, Context1}.

-spec content_types_accepted/2 :: (#http_req{}, #cb_context{}) -> {content_type_callbacks(), #http_req{}, #cb_context{}}.
content_types_accepted(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    ?LOG("run: content_types_accepted"),
    {Req1, Context1=#cb_context{content_types_accepted=CTAs}} =
        lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
                            Event = <<"v1_resource.content_types_accepted.", Mod/binary>>,
                            Payload = {ReqAcc, ContextAcc, Params},
                            {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
                            {ReqAcc1, ContextAcc1}
                    end, {Req0, Context0}, Nouns),

    CTA = lists:foldr(fun({Fun, L}, Acc) ->
                              lists:foldr(fun({Type, SubType}, Acc1) ->
                                                  [{{Type, SubType, []}, Fun} | Acc1];
                                             (EncType, Acc1) ->
                                                  [ {EncType, Fun} | Acc1 ]
                                          end, Acc, L)
                      end, [], CTAs),
    {CTA, Req1, Context1}.

-spec languages_provided/2 :: (#http_req{}, #cb_context{}) -> {[ne_binary(),...], #http_req{}, #cb_context{}}.
languages_provided(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    ?LOG("run: languages_provided"),

    {Req1, Context1} = 
        lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
                            Event = <<"v1_resource.languages_provided.", Mod/binary>>,
                            Payload = {ReqAcc, ContextAcc, Params},
                            {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
                            {ReqAcc1, ContextAcc1}
                    end, {Req0, Context0}, Nouns),
    {Context1#cb_context.languages_provided, Req1, Context1}.

-spec charsets_provided/2 :: (#http_req{}, #cb_context{}) -> {[ne_binary(),...], #http_req{}, #cb_context{}}.
charsets_provided(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    ?LOG("run: charsets_provided"),

    {Req1, Context1} = 
        lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
                            Event = <<"v1_resource.charsets_provided.", Mod/binary>>,
                            Payload = {ReqAcc, ContextAcc, Params},
                            {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
                            {ReqAcc1, ContextAcc1}
                    end, {Req0, Context0}, Nouns),
    {Context1#cb_context.charsets_provided, Req1, Context1}.

-spec encodings_provided/2 :: (#http_req{}, #cb_context{}) -> {[ne_binary(),...], #http_req{}, #cb_context{}}.
encodings_provided(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    ?LOG("run: encodings_provided"),

    {Req1, Context1} = 
        lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
                            Event = <<"v1_resource.encodings_provided.", Mod/binary>>,
                            Payload = {ReqAcc, ContextAcc, Params},
                            {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
                            {ReqAcc1, ContextAcc1}
                    end, {Req0, Context0}, Nouns),
    {Context1#cb_context.encodings_provided, Req1, Context1}.

-spec resource_exists/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
resource_exists(Req, #cb_context{req_nouns=[{<<"404">>,_}|_]}=Context) ->
    ?LOG("failed to tokenize request, returning 404"),
    {false, Req, Context};
resource_exists(Req0, Context0) ->
    ?LOG("run: resource_exists"),
    case v1_util:does_resource_exist(Context0) of
        true ->
            ?LOG("requested resource exists, validating it"),
            {Req1, Context1} = v1_util:validate(Req0, Context0),
            case v1_util:succeeded(Context1) of
                true ->
                    ?LOG("requested resource validated, but is the request a PUT: ~s", [Context1#cb_context.req_verb]),
                    {Context1#cb_context.req_verb =/= <<"put">>, Req1, Context1};
                false ->
                    ?LOG("failed to validate resource"),
                    {false, Req1, Context1}
            end;
        false ->
            ?LOG("requested resource does not exist"),
            {false, Req0, Context0}
    end.

-spec moved_temporarily/2 :: (#http_req{}, #cb_context{}) -> {false, #http_req{}, #cb_context{}}.
moved_temporarily(Req, Context) ->
    ?LOG("run: moved_temporarily"),
    {false, Req, Context}.

-spec moved_permanently/2 :: (#http_req{}, #cb_context{}) -> {false, #http_req{}, #cb_context{}}.
moved_permanently(Req, Context) ->
    ?LOG("run: moved_permanently"),
    {false, Req, Context}.

-spec previously_existed/2 :: (#http_req{}, #cb_context{}) -> {false, #http_req{}, #cb_context{}}.
previously_existed(Req, State) ->
    ?LOG("run: previously_existed"),
    {false, Req, State}.

%% If we're tunneling PUT through POST, 
%% we need to allow POST to create a non-existent resource
%% AKA, 201 Created header set
-spec allow_missing_post/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
allow_missing_post(Req0, #cb_context{req_verb = <<"put">>}=Context) ->
    ?LOG("run: allow_missing_post when req_verb = put"),
    {Method, Req1} = cowboy_http_req:method(Req0),
    {Method =:= 'POST', Req1, Context};
allow_missing_post(Req, Context) ->
    ?LOG("run: allow_missing_post"),
    {false, Req, Context}.

-spec delete_resource/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
delete_resource(Req, Context) ->
    ?LOG("run: delete_resource"),
    v1_util:execute_request(Req, Context).

%% If allow_missing_post returned true (cause it was a POST) and PUT has been tunnelled,
%% POST is a create
-spec post_is_create/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
post_is_create(Req, #cb_context{req_verb = <<"put">>}=Context) ->
    ?LOG("run: post_is_create: true"),
    {true, Req, Context};
post_is_create(Req, Context) ->
    ?LOG("run: post_is_create: false"),
    {false, Req, Context}.

%% whatever (for now)
-spec create_path/2 :: (#http_req{}, #cb_context{}) -> {[], #http_req{}, #cb_context{}}.
create_path(Req, #cb_context{resp_headers=RespHeaders}=Context) ->
    ?LOG("run: create_path"),
    %% Location header goes here, I believe?

    Path = props:get_value(<<"Location">>, RespHeaders, <<>>),

    {crossbar_util:new_path(Req, Path), Req, Context}.
    

-spec process_post/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
process_post(Req0, Context0) ->
    ?LOG("run: process_post"),
    case v1_util:execute_request(Req0, Context0) of
        {true, Req1, Context1} ->
            Event = <<"v1_resource.process_post">>,
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            v1_util:create_push_response(Req1, Context1);
        Else ->
            Else
    end.

-spec is_conflict/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
is_conflict(Req, #cb_context{resp_error_code=409}=Context) ->
    ?LOG("run: is_conflict: true"),
    {true, Req, Context};
is_conflict(Req, Context) ->
    ?LOG("run: is_conflict: false"),
    {false, Req, Context}.

-spec from_binary/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
from_binary(Req0, Context0) ->
    ?LOG("run: from_binary"),
    case v1_util:execute_request(Req0, Context0) of
        {true, Req1, Context1} ->
            Event = <<"v1_resource.from_binary">>,
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            v1_util:create_push_response(Req1, Context1);
        Else ->
            Else
    end.

-spec from_json/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
from_json(Req0, Context0) ->
    ?LOG("run: from_json"),
    case v1_util:execute_request(Req0, Context0) of
        {true, Req1, Context1} ->
            Event = <<"v1_resource.from_json">>,
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            v1_util:create_push_response(Req1, Context1);
        Else ->
            Else
    end.

-spec from_form/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
from_form(Req0, Context0) ->
    ?LOG("run: from_form"),
    case v1_util:execute_request(Req0, Context0) of
        {true, Req1, Context1} ->
            Event = <<"v1_resource.from_form">>,
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            v1_util:create_push_response(Req1, Context1);
        Else ->
            Else
    end.

-spec to_json/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
to_json(Req, Context) ->
    ?LOG("run: to_json"),
    Event = <<"v1_resource.to_json">>,
    _ = crossbar_bindings:map(Event, {Req, Context}),
    v1_util:create_pull_response(Req, Context).

-spec to_binary/2 :: (#http_req{}, #cb_context{}) -> {boolean(), #http_req{}, #cb_context{}}.
to_binary(Req, #cb_context{resp_data=RespData}=Context) ->
    ?LOG("run: to_binary"),
    Event = <<"v1_resource.to_binary">>,
    _ = crossbar_bindings:map(Event, {Req, Context}),
    {RespData, v1_util:set_resp_headers(Req, Context), Context}.

multiple_choices(Req, Context) ->
    ?LOG("run: multiple_choices"),
    ?LOG("has resp_body: ~s", [cowboy_http_req:has_resp_body(Req)]),
    {false, Req, Context}.

-spec generate_etag/2 :: (#http_req{}, #cb_context{}) -> {ne_binary(), #http_req{}, #cb_context{}}.
generate_etag(Req0, Context0) ->
    ?LOG("run: generate_etag"),

    Event = <<"v1_resource.etag">>,
    {Req1, Context1} = crossbar_bindings:fold(Event, {Req0, Context0}),
    case Context1#cb_context.resp_etag of
        automatic ->
            {Content, _} = v1_util:create_resp_content(Req1, Context1),
            Tag = mochihex:to_hex(crypto:md5(Content)),
            ?LOG("using automatic etag ~s", [Tag]),
            {Tag, Req1, Context1#cb_context{resp_etag=Tag}};
        undefined ->
            ?LOG("no etag provided, skipping", []),
            {undefined, Req1, Context1#cb_context{resp_etag=undefined}};
        Tag ->
            ?LOG("using etag ~s", [Tag]),
            {wh_util:to_binary(Tag), Req1, Context1#cb_context{resp_etag=Tag}}
    end.

-spec expires/2 :: (#http_req{}, #cb_context{}) -> {wh_datetime(), #http_req{}, #cb_context{}}.
expires(Req, #cb_context{resp_expires=Expires}=Context) ->
    ?LOG("run: expires"),
    Event = <<"v1_resource.expires">>,
    crossbar_bindings:fold(Event, {Expires, Req, Context}).
