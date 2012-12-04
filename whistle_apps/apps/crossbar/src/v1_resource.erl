%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% API resource
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Jon Blanton
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
         ,delete_completed/2
         ,is_conflict/2
         ,to_json/2, to_binary/2, to_csv/2
         ,from_json/2, from_binary/2, from_form/2
         ,multiple_choices/2
         ,generate_etag/2
         ,expires/2
        ]).

-include("include/crossbar.hrl").

%%%===================================================================
%%% Startup and shutdown of request
%%%===================================================================
-spec init/3 :: ({'tcp' | 'ssl', 'http'}, #http_req{}, wh_proplist()) ->
                        {'upgrade', 'protocol', 'cowboy_http_rest'}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_rest};
init({ssl, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

-spec rest_init/2 :: (#http_req{}, wh_proplist()) ->
                             {'ok', #http_req{}, cb_context:context()}.
rest_init(Req0, Opts) ->
    ReqId = case cowboy_http_req:header(<<"X-Request-Id">>, Req0) of
                {undefined, _} -> couch_mgr:get_uuid();
                {UserReqId, _} -> wh_util:to_binary(UserReqId)
            end,
    put(callid, ReqId),

    {Host, Req1} = cowboy_http_req:raw_host(Req0),
    {Port, Req2} = cowboy_http_req:port(Req1),
    {Path, Req3} = cowboy_http_req:raw_path(Req2),
    {QS, Req4} = cowboy_http_req:raw_qs(Req3),
    {Method, Req5} = cowboy_http_req:method(Req4),

    lager:debug("~s: ~s?~s", [Method, Path, QS]),

    Context0 = #cb_context{
      req_id = wh_util:to_binary(ReqId)
      ,raw_host = wh_util:to_binary(Host)
      ,port = wh_util:to_integer(Port)
      ,raw_path = wh_util:to_binary(Path)
      ,raw_qs = wh_util:to_binary(QS)
      ,method = wh_util:to_atom(Method)
      ,resp_status = 'fatal'
      ,resp_error_msg = <<"init failed">>
      ,resp_error_code = 500
     },

    {Context1, _} = crossbar_bindings:fold(<<"v1_resource.init">>, {Context0, Opts}),
    {ok, Req6} = cowboy_http_req:set_resp_header(<<"X-Request-ID">>, ReqId, Req5),
    {ok, Req6, Context1}.

terminate(Req, Context) ->
    _ = v1_util:request_terminated(Req, Context),
    lager:debug("session finished").

rest_terminate(Req, #cb_context{start=T1, method='OPTIONS'}=Context) ->
    _ = v1_util:finish_request(Req, Context),
    lager:debug("fulfilled in ~p ms", [timer:now_diff(now(), T1) div 1000]);
rest_terminate(Req, #cb_context{start=T1, resp_status=Status, auth_account_id=AcctId}=Context) ->
    case Status of
        success -> wh_counter:inc(<<"crossbar.requests.successes">>);
        _ -> wh_counter:inc(<<"crossbar.requests.failures">>)
    end,
    wh_counter:inc(<<"crossbar.requests.accounts.", (wh_util:to_binary(AcctId))/binary>>),
    _ = v1_util:finish_request(Req, Context),
    lager:debug("fulfilled in ~p ms", [timer:now_diff(now(), T1) div 1000]).

%%%===================================================================
%%% CowboyHTTPRest API Callbacks
%%%===================================================================
-spec known_methods/2 :: (#http_req{}, cb_context:context()) ->
                                 {http_methods(), #http_req{}, cb_context:context()}.
known_methods(Req, Context) ->
    {?ALLOWED_METHODS, Req, Context#cb_context{allow_methods=?ALLOWED_METHODS
                                               ,allowed_methods=?ALLOWED_METHODS
                                              }}.

-spec allowed_methods/2 :: (#http_req{}, cb_context:context()) ->
                                   {http_methods() | 'halt', #http_req{}, cb_context:context()}.
allowed_methods(Req0, #cb_context{allowed_methods=Methods}=Context) ->
    {Tokens, Req1} = cowboy_http_req:path_info(Req0),
    case v1_util:parse_path_tokens(Tokens) of
        [_|_] = Nouns ->
            %% Because we allow tunneling of verbs through the request,
            %% we have to check and see if we need to override the actual
            %% HTTP method with the tunneled version
            case v1_util:get_req_data(Context, Req1) of
                {halt, Context1, Req2} ->
                    v1_util:halt(Req2, cb_context:add_system_error(parse_error, Context1));
                {Context1, Req2} ->
                    determine_http_verb(Req2, Context1#cb_context{req_nouns=Nouns})
            end;
        [] ->
            lager:debug("no path tokens: ~p", [Methods]),
            {Methods, Req1, Context#cb_context{allow_methods=Methods}}
    end.

-spec determine_http_verb/2 :: (#http_req{}, cb_context:context()) ->
                                       {http_methods() | 'halt', #http_req{}, cb_context:context()}.
determine_http_verb(Req0, Context) ->
    {Method, Req1} = cowboy_http_req:method(Req0),
    find_allowed_methods(Req1, Context#cb_context{req_verb=v1_util:get_http_verb(Method, Context)}).

find_allowed_methods(Req0, #cb_context{allowed_methods=Methods
                                       ,req_verb=Verb
                                       ,req_nouns=[{Mod, Params}|_]
                                      }=Context) ->
    Responses = crossbar_bindings:map(<<"v1_resource.allowed_methods.", Mod/binary>>, Params),
    {Method, Req1} = cowboy_http_req:method(Req0),
    AllowMethods = v1_util:allow_methods(Responses, Methods, Verb, Method),
    maybe_add_cors_headers(Req1, Context#cb_context{allow_methods=AllowMethods}).

-spec maybe_add_cors_headers/2 :: (#http_req{}, cb_context:context()) ->
                                          {http_methods() | 'halt', #http_req{}, cb_context:context()}.
maybe_add_cors_headers(Req0, Context) ->
    case v1_util:is_cors_request(Req0) of
        {true, Req1} ->
            {ok, Req2} = v1_util:add_cors_headers(Req1, Context),
            check_preflight(Req2, Context);
        {false, Req1} ->
            maybe_allow_method(Req1, Context)
    end.

-spec check_preflight/2 :: (#http_req{}, cb_context:context()) ->
                                   {http_methods(), #http_req{}, cb_context:context()}.
check_preflight(Req0, #cb_context{req_verb = <<"options">>}=Context) ->
    lager:debug("allowing OPTIONS request for CORS preflight"),
    {['OPTIONS'], Req0, Context};
check_preflight(Req0, Context) ->
    maybe_allow_method(Req0, Context).

maybe_allow_method(Req0, #cb_context{allow_methods=[]}=Context) ->
    v1_util:halt(Req0, cb_context:add_system_error(not_found, Context));
maybe_allow_method(Req0, #cb_context{allow_methods=Methods, req_verb=Verb}=Context) ->
    VerbAtom = wh_util:to_atom(wh_util:to_upper_binary(Verb)),
    case lists:member(VerbAtom, Methods) of
        true ->
            {Methods, Req0, Context};
        false ->
            v1_util:halt(Req0, cb_context:add_system_error(invalid_method, Context))
    end.

malformed_request(Req, #cb_context{req_json={malformed, _}}=Context) ->
    lager:debug("request is malformed"),
    {true, Req, Context};
malformed_request(Req, #cb_context{req_verb = <<"options">>}=Context) ->
    {false, Req, Context};
malformed_request(Req, #cb_context{req_nouns=Nouns}=Context) ->
    case props:get_value(<<"accounts">>, Nouns) of
        [AcctId] ->
            case cb_accounts:validate(Context, AcctId) of
                #cb_context{resp_status=success}=Context1 ->
                    {false, Req, Context1};
                Context1 ->
                    v1_util:halt(Req, Context1)
            end;
        _Other ->
            lager:debug("other: ~p", [Nouns]),
            {false, Req, Context}
    end.

-spec is_authorized/2 :: (#http_req{}, cb_context:context()) ->
                                 {'true' | {'false', <<>>}, #http_req{}, cb_context:context()}.
is_authorized(Req, Context) ->
    v1_util:is_authentic(Req, Context).

-spec forbidden/2 :: (#http_req{}, cb_context:context()) ->
                             {'false', #http_req{}, cb_context:context()}.
forbidden(Req0, Context0) ->
    case v1_util:is_permitted(Req0, Context0) of
        {halt, _, _}=Reply -> Reply;
        {IsPermitted, Req1, Context1} ->
            {not IsPermitted, Req1, Context1}
    end.

-spec valid_content_headers/2 :: (#http_req{}, cb_context:context()) ->
                                         {'true', #http_req{}, cb_context:context()}.
valid_content_headers(Req, Context) ->
    {true, Req, Context}.

-spec known_content_type/2 :: (#http_req{}, cb_context:context()) ->
                                      {boolean(), #http_req{}, cb_context:context()}.
known_content_type(Req, #cb_context{req_verb = <<"options">>}=Context) ->
    {true, Req, Context};
known_content_type(Req, #cb_context{req_verb = <<"get">>}=Context) ->
    {true, Req, Context};
known_content_type(Req, #cb_context{req_verb = <<"delete">>}=Context) ->
    {true, Req, Context};
known_content_type(Req, Context) ->
    {ok, Req2} = case cowboy_http_req:header('Content-Type', Req) of
                     {undefined, Req1} ->
                         cowboy_http_req:set_resp_header(<<"X-RFC2616">>
                                                             ,<<"Section 14.17 (Try it, you'll like it)">>
                                                             ,Req1);
                     {_, Req1} -> {ok, Req1}
                 end,
    v1_util:is_known_content_type(Req2, Context).

-spec valid_entity_length/2 :: (#http_req{}, cb_context:context()) ->
                                       {'true', #http_req{}, cb_context:context()}.
valid_entity_length(Req, Context) ->
    {true, Req, Context}.

-spec options/2 :: (#http_req{}, cb_context:context()) ->
                           {'ok', #http_req{}, cb_context:context()}.
options(Req0, Context) ->
    case v1_util:is_cors_request(Req0) of
        {true, Req1} ->
            lager:debug("is CORS request"),
            {ok, Req2} = v1_util:add_cors_headers(Req1, Context),
            {ok, Req3} = cowboy_http_req:set_resp_body(<<>>, Req2),
            {ok, Req3, Context};
        {false, Req1} ->
            lager:debug("is not CORS request"),
            {ok, Req1, Context}
    end.

-type content_type_callbacks() :: [{{ne_binary(), ne_binary(), wh_proplist()}, atom()} |
                                   {ne_binary(), atom()}
                                   ,...] | [].
-spec content_types_provided/2 :: (#http_req{}, cb_context:context()) ->
                                          {content_type_callbacks(), #http_req{}, cb_context:context()}.
content_types_provided(Req, #cb_context{req_nouns=Nouns}=Context0) ->
    lager:debug("run: content_types_provided"),
    #cb_context{content_types_provided=CTPs}=Context1 =
        lists:foldr(fun({Mod, Params}, ContextAcc) ->
                            Event = <<"v1_resource.content_types_provided.", Mod/binary>>,
                            Payload = [ContextAcc | Params],
                            crossbar_bindings:fold(Event, Payload)
                    end, Context0, Nouns),

    content_types_provided(Req, Context1, CTPs).

content_types_provided(Req, Context, []) ->
    Def = ?CONTENT_PROVIDED,
    content_types_provided(Req, Context#cb_context{content_types_provided=Def}, Def);
content_types_provided(Req, Context, CTPs) ->
    CTP =
        lists:foldr(fun({Fun, L}, Acc) ->
                            lists:foldr(fun({Type, SubType}, Acc1) ->
                                                [{{Type, SubType, []}, Fun} | Acc1];
                                           ({_,_,_}=EncType, Acc1) ->
                                                [ {EncType, Fun} | Acc1 ]
                                        end, Acc, L)
                    end, [], CTPs),
    lager:debug("ctp: ~p", [CTP]),
    {CTP, Req, Context}.

-spec content_types_accepted/2 :: (#http_req{}, cb_context:context()) ->
                                          {content_type_callbacks(), #http_req{}, cb_context:context()}.
content_types_accepted(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    lager:debug("run: content_types_accepted"),
    Context1 =
        lists:foldr(fun({Mod, Params}, ContextAcc) ->
                            Event = <<"v1_resource.content_types_accepted.", Mod/binary>>,
                            Payload = [ContextAcc | Params],
                            crossbar_bindings:fold(Event, Payload)
                    end, Context0, Nouns),

    case cowboy_http_req:parse_header('Content-Type', Req0) of
        {undefined, Req1} -> default_content_types_accepted(Req1, Context1);
        {CT, Req1} -> content_types_accepted(CT, Req1, Context1)
    end.

-spec default_content_types_accepted/2 :: (#http_req{}, cb_context:context()) ->
                                                  {[{'undefined', atom()},...], #http_req{}, cb_context:context()}.
default_content_types_accepted(Req, #cb_context{content_types_accepted=CTAs}=Context) ->
    CTA = [ {undefined, Fun}
            || {Fun, L} <- CTAs,
               lists:any(fun({Type, SubType}) ->
                                 v1_util:content_type_matches(?CROSSBAR_DEFAULT_CONTENT_TYPE
                                                              ,{Type, SubType, []});
                            ({_,_,_}=ModCT) ->
                                 v1_util:content_type_matches(?CROSSBAR_DEFAULT_CONTENT_TYPE
                                                              ,ModCT)
                         end, L) % check each type against the default
          ],

    {CTA, Req, Context}.

-spec content_types_accepted/3 :: (content_type(), #http_req{}, cb_context:context()) ->
                                          {[{content_type(), atom()},...], #http_req{}, cb_context:context()}.
content_types_accepted(CT, Req, #cb_context{content_types_accepted=CTAs}=Context) ->
    CTA = lists:foldl(fun({Fun, L}, Acc) ->
                              lists:foldl(fun({Type, SubType}, Acc1) ->
                                                  case v1_util:content_type_matches(CT, {Type, SubType, []}) of
                                                      true -> [{CT, Fun} | Acc1];
                                                      false -> Acc1
                                                  end;
                                             ({_,_,_}=EncType, Acc1) ->
                                                  [ {EncType, Fun} | Acc1 ]
                                          end, Acc, L)
                      end, [], CTAs),
    {CTA, Req, Context}.

-spec languages_provided/2 :: (#http_req{}, cb_context:context()) ->
                                      {ne_binaries(), #http_req{}, cb_context:context()}.
languages_provided(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    lager:debug("run: languages_provided"),
    {Req1, #cb_context{languages_provided=LangsProvided}=Context1} = 
        lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
                            Event = <<"v1_resource.languages_provided.", Mod/binary>>,
                            Payload = {ReqAcc, ContextAcc, Params},
                            {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
                            {ReqAcc1, ContextAcc1}
                    end, {Req0, Context0}, Nouns),
    {LangsProvided, Req1, Context1}.

%% -spec charsets_provided/2 :: (#http_req{}, cb_context:context()) -> {[ne_binary(),...], #http_req{}, cb_context:context()}.
%% charsets_provided(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
%%     lager:debug("run: charsets_provided"),
%%     {Req1, #cb_context{charsets_provided=CharsetsProvided}=Context1} = 
%%         lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
%%                             Event = <<"v1_resource.charsets_provided.", Mod/binary>>,
%%                             Payload = {ReqAcc, ContextAcc, Params},
%%                             {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
%%                             {ReqAcc1, ContextAcc1}
%%                     end, {Req0, Context0}, Nouns),
%%     {CharsetsProvided, Req1, Context1}.
charsets_provided(_Req, _Context) ->
    no_call.

-spec encodings_provided/2 :: (#http_req{}, cb_context:context()) ->
                                      {ne_binaries(), #http_req{}, cb_context:context()}.
encodings_provided(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    lager:debug("run: encodings_provided"),
    {Req1, #cb_context{encodings_provided=EncodingsProvided}=Context1} = 
        lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
                            Event = <<"v1_resource.encodings_provided.", Mod/binary>>,
                            Payload = {ReqAcc, ContextAcc, Params},
                            {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
                            {ReqAcc1, ContextAcc1}
                    end, {Req0, Context0}, Nouns),
    {EncodingsProvided, Req1, Context1}.

-spec resource_exists/2 :: (#http_req{}, cb_context:context()) ->
                                   {boolean(), #http_req{}, cb_context:context()}.
resource_exists(Req, #cb_context{req_nouns=[{<<"404">>,_}|_]}=Context) ->
    lager:debug("failed to tokenize request, returning 404"),
    {false, Req, Context};
resource_exists(Req0, Context0) ->
    lager:debug("run: resource_exists"),
    case v1_util:does_resource_exist(Context0) of
        true ->
            lager:debug("requested resource exists, validating it"),
            #cb_context{req_verb=Verb}=Context1 = v1_util:validate(Context0),
            case v1_util:succeeded(Context1) of
                true when Verb =/= <<"put">> ->
                    lager:debug("requested resource update validated"),
                    {true, Req0, Context1};
                true ->
                    lager:debug("requested resource creation validated"),
                    {false, Req0, Context1};
                false ->
                    lager:debug("failed to validate resource"),
                    v1_util:halt(Req0, Context1)
            end;
        false ->
            lager:debug("requested resource does not exist"),
            {false, Req0, Context0}
    end.

-spec moved_temporarily/2 :: (#http_req{}, cb_context:context()) ->
                                     {'false', #http_req{}, cb_context:context()}.
moved_temporarily(Req, Context) ->
    lager:debug("run: moved_temporarily"),
    {false, Req, Context}.

-spec moved_permanently/2 :: (#http_req{}, cb_context:context()) ->
                                     {'false', #http_req{}, cb_context:context()}.
moved_permanently(Req, Context) ->
    lager:debug("run: moved_permanently"),
    {false, Req, Context}.

-spec previously_existed/2 :: (#http_req{}, cb_context:context()) ->
                                      {'false', #http_req{}, cb_context:context()}.
previously_existed(Req, State) ->
    lager:debug("run: previously_existed"),
    {false, Req, State}.

%% If we're tunneling PUT through POST, 
%% we need to allow POST to create a non-existent resource
%% AKA, 201 Created header set
-spec allow_missing_post/2 :: (#http_req{}, cb_context:context()) ->
                                      {boolean(), #http_req{}, cb_context:context()}.
allow_missing_post(Req0, #cb_context{req_verb=_Verb}=Context) ->
    lager:debug("run: allow_missing_post when req_verb = ~s", [_Verb]),
    {Method, Req1} = cowboy_http_req:method(Req0),
    {Method =:= 'POST', Req1, Context}.

-spec delete_resource/2 :: (#http_req{}, cb_context:context()) ->
                                   {boolean() | 'halt', #http_req{}, cb_context:context()}.
delete_resource(Req, Context) ->
    lager:debug("run: delete_resource"),
    v1_util:execute_request(Req, Context).

-spec delete_completed/2 :: (#http_req{}, cb_context:context()) ->
                                    {boolean(), #http_req{}, cb_context:context()}.
delete_completed(Req, Context) ->
    lager:debug("run: delete_completed"),
    v1_util:create_push_response(Req, Context).

%% If allow_missing_post returned true (cause it was a POST) and PUT has been tunnelled,
%% POST is a create
-spec post_is_create/2 :: (#http_req{}, cb_context:context()) ->
                                  {boolean(), #http_req{}, cb_context:context()}.
post_is_create(Req, #cb_context{req_verb = <<"put">>}=Context) ->
    lager:debug("treating post request as a create"),
    {true, Req, Context};
post_is_create(Req, Context) ->
    lager:debug("run: post_is_create: false"),
    {false, Req, Context}.

%% set the location header
-spec create_path/2 :: (#http_req{}, cb_context:context()) ->
                               {ne_binary(), #http_req{}, cb_context:context()}.
create_path(Req, #cb_context{resp_headers=RespHeaders}=Context) ->
    lager:debug("run: create_path"),
    %% Location header goes here, I believe?
    Path = props:get_value(<<"Location">>, RespHeaders, <<>>),
    lager:debug("setting path to: ~s", [Path]),
    {crossbar_util:get_path(Req, Path), Req, Context}.

-spec process_post/2 :: (#http_req{}, cb_context:context()) ->
                                {boolean(), #http_req{}, cb_context:context()}.
process_post(Req0, Context0) ->
    lager:debug("run: process_post"),
    case v1_util:execute_request(Req0, Context0) of
        {true, Req1, Context1} ->
            Event = <<"v1_resource.process_post">>,
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            v1_util:create_push_response(Req1, Context1);
        Else ->
            Else
    end.

-spec is_conflict/2 :: (#http_req{}, cb_context:context()) ->
                               {boolean(), #http_req{}, cb_context:context()}.
is_conflict(Req, #cb_context{resp_error_code=409}=Context) ->
    lager:debug("request resulted in conflict"),
    {true, Req, Context};
is_conflict(Req, Context) ->
    lager:debug("run: is_conflict: false"),
    {false, Req, Context}.

-spec from_binary/2 :: (#http_req{}, cb_context:context()) ->
                               {boolean(), #http_req{}, cb_context:context()}.
from_binary(Req0, Context0) ->
    lager:debug("run: from_binary"),
    case v1_util:execute_request(Req0, Context0) of
        {true, Req1, Context1} ->
            Event = <<"v1_resource.from_binary">>,
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            v1_util:create_push_response(Req1, Context1);
        Else ->
            Else
    end.

-spec from_json/2 :: (#http_req{}, cb_context:context()) ->
                             {boolean(), #http_req{}, cb_context:context()}.
from_json(Req0, Context0) ->
    lager:debug("run: from_json"),
    case v1_util:execute_request(Req0, Context0) of
        {true, Req1, Context1} ->
            Event = <<"v1_resource.from_json">>,
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            v1_util:create_push_response(Req1, Context1);
        Else ->
            Else
    end.

-spec from_form/2 :: (#http_req{}, cb_context:context()) ->
                             {boolean(), #http_req{}, cb_context:context()}.
from_form(Req0, Context0) ->
    lager:debug("run: from_form"),
    case v1_util:execute_request(Req0, Context0) of
        {true, Req1, Context1} ->
            Event = <<"v1_resource.from_form">>,
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            v1_util:create_push_response(Req1, Context1);
        Else ->
            Else
    end.

-spec to_json/2 :: (#http_req{}, cb_context:context()) ->
                           {iolist() | ne_binary() | 'halt', #http_req{}, cb_context:context()}.
to_json(Req, Context) ->
    Event = <<"v1_resource.to_json">>,
    _ = crossbar_bindings:map(Event, {Req, Context}),
    v1_util:create_pull_response(Req, Context).

-spec to_binary/2 :: (#http_req{}, cb_context:context()) ->
                             {binary(), #http_req{}, cb_context:context()}.
to_binary(Req, #cb_context{resp_data=RespData}=Context) ->
    Event = <<"v1_resource.to_binary">>,
    _ = crossbar_bindings:map(Event, {Req, Context}),
    lager:debug("responding to_binary"),
    {RespData, v1_util:set_resp_headers(Req, Context), Context}.

-spec to_csv/2 :: (#http_req{}, cb_context:context()) ->
                          {iolist(), #http_req{}, cb_context:context()}.
to_csv(Req, #cb_context{resp_data=RespData
                        ,resp_headers=RespHeaders
                       }=Context) ->
    RespBody = json_objs_to_csv(RespData),

    RespHeaders1 = [{<<"Content-Type">>, <<"application/octet-stream">>}
                    ,{<<"Content-Length">>, iolist_size(RespBody)}
                    ,{<<"Content-Disposition">>, <<"attachment; filename=\"cdrs.csv\"">>}
                    | RespHeaders
                   ],

    {RespBody, v1_util:set_resp_headers(Req, Context#cb_context{resp_headers=RespHeaders1}), Context}.

-spec json_objs_to_csv/1 :: (wh_json:json_objects()) -> iolist().
json_objs_to_csv([]) -> [];
json_objs_to_csv([J|JObjs]) ->
    [csv_header(J), [json_to_csv(JObj) || JObj <- JObjs]].

-spec csv_header/1 :: (wh_json:json_object()) -> iolist().
csv_header(JObj) ->
    csv_ize(wh_json:get_keys(JObj)).

-spec csv_ize/1 :: (wh_json:keys()) -> iolist().
csv_ize([F|Rest]) ->
    [<<"\"">>, F, <<"\"">>, [[<<",\"">>, V, <<"\"">>] || V <- Rest], <<"\n">>].

-spec json_to_csv/1 :: (wh_json:json_object()) -> iolist().
json_to_csv(JObj) ->
    {Vs, _} = wh_json:get_values(JObj),
    csv_ize(Vs).

-spec multiple_choices/2 :: (#http_req{}, cb_context:context()) ->
                                    {'false', #http_req{}, cb_context:context()}.
multiple_choices(Req, Context) ->
    lager:debug("has resp_body: ~s", [cowboy_http_req:has_resp_body(Req)]),
    {false, Req, Context}.

-spec generate_etag/2 :: (#http_req{}, cb_context:context()) ->
                                 {ne_binary(), #http_req{}, cb_context:context()}.
generate_etag(Req0, Context0) ->
    Event = <<"v1_resource.etag">>,
    {Req1, #cb_context{resp_etag=ETag}=Context1} = crossbar_bindings:fold(Event, {Req0, Context0}),
    case ETag of
        automatic ->
            {Content, _} = v1_util:create_resp_content(Req1, Context1),
            Tag = wh_util:to_hex_binary(crypto:md5(Content)),
            lager:debug("using automatic etag ~s", [Tag]),
            {list_to_binary([$", Tag, $"]), Req1, Context1#cb_context{resp_etag=Tag}};
        undefined ->
            lager:debug("no etag provided, skipping", []),
            {undefined, Req1, Context1#cb_context{resp_etag=undefined}};
        Tag ->
            lager:debug("using etag ~s", [Tag]),
            {list_to_binary([$", Tag, $"]), Req1, Context1#cb_context{resp_etag=Tag}}
    end.

-spec expires/2 :: (#http_req{}, cb_context:context()) ->
                           {wh_datetime(), #http_req{}, cb_context:context()}.
expires(Req, #cb_context{resp_expires=Expires}=Context) ->
    Event = <<"v1_resource.expires">>,
    crossbar_bindings:fold(Event, {Expires, Req, Context}).
