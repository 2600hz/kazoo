%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% API resource
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Jon Blanton
%%%-------------------------------------------------------------------
-module(api_resource).

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

-include("crossbar.hrl").

%%%===================================================================
%%% Startup and shutdown of request
%%%===================================================================
-spec init({'tcp' | 'ssl', 'http'}, cowboy_req:req(), wh_proplist()) ->
                  {'upgrade', 'protocol', 'cowboy_rest'}.
init({'tcp', 'http'}, _Req, _Opts) ->
    {'upgrade', 'protocol', 'cowboy_rest'};
init({'ssl', 'http'}, _Req, _Opts) ->
    {'upgrade', 'protocol', 'cowboy_rest'}.

-spec rest_init(cowboy_req:req(), wh_proplist()) ->
                       {'ok', cowboy_req:req(), cb_context:context()}.
rest_init(Req0, Opts) ->
    ReqId = case cowboy_req:header(<<"x-request-id">>, Req0) of
                {'undefined', _} -> couch_mgr:get_uuid();
                {UserReqId, _} -> wh_util:to_binary(UserReqId)
            end,
    put('callid', ReqId),
    ProfileId = case cowboy_req:header(<<"x-profile-id">>, Req0) of
                    {'undefined', _} -> 'undefined';
                    {ProfId, _} -> wh_util:to_binary(ProfId)
                end,
    {Host, Req1} = cowboy_req:host(Req0),
    {Port, Req2} = cowboy_req:port(Req1),
    {Path, Req3} = cowboy_req:path(Req2),
    {QS, Req4} = cowboy_req:qs(Req3),
    {Method, Req5} = cowboy_req:method(Req4),
    {{Peer, _PeerPort}, Req6} = cowboy_req:peer(Req5),
    {Version, Req7} = cowboy_req:binding('version', Req6),
    ClientIP = wh_network_utils:iptuple_to_binary(Peer),
    Context0 = #cb_context{
                  req_id = ReqId
                  ,raw_host = wh_util:to_binary(Host)
                  ,port = wh_util:to_integer(Port)
                  ,raw_path = wh_util:to_binary(Path)
                  ,raw_qs = wh_util:to_binary(QS)
                  ,method = wh_util:to_binary(Method)
                  ,resp_status = 'fatal'
                  ,resp_error_msg = <<"init failed">>
                  ,resp_error_code = 500
                  ,client_ip = ClientIP
                  ,profile_id = ProfileId
                  ,api_version = Version
                 },
    Event = api_util:create_event_name(Context0, <<"init">>),
    {Context1, _} = crossbar_bindings:fold(Event, {Context0, Opts}),
    lager:info("~s: ~s?~s from ~s", [Method, Path, QS, ClientIP]),
    {'ok', cowboy_req:set_resp_header(<<"x-request-id">>, ReqId, Req7), Context1}.

terminate(_Req, _Context) ->
    lager:debug("session finished").

rest_terminate(Req, #cb_context{start=T1
                                ,method = ?HTTP_OPTIONS
                               }=Context) ->
    lager:info("OPTIONS request fulfilled in ~p ms", [wh_util:elapsed_ms(T1)]),
    _ = api_util:finish_request(Req, Context);
rest_terminate(Req, #cb_context{start=T1
                                ,req_verb=Verb
                               }=Context) ->
    lager:info("~s request fulfilled in ~p ms", [Verb, wh_util:elapsed_ms(T1)]),
    _ = api_util:finish_request(Req, Context).

%%%===================================================================
%%% CowboyHTTPRest API Callbacks
%%%===================================================================
-spec known_methods(cowboy_req:req(), cb_context:context()) ->
                           {http_methods(), cowboy_req:req(), cb_context:context()}.
known_methods(Req, Context) ->
    {?ALLOWED_METHODS, Req, Context#cb_context{allow_methods=?ALLOWED_METHODS
                                               ,allowed_methods=?ALLOWED_METHODS
                                              }}.

-spec allowed_methods(cowboy_req:req(), cb_context:context()) ->
                             {http_methods() | 'halt', cowboy_req:req(), cb_context:context()}.
allowed_methods(Req0, #cb_context{allowed_methods=Methods}=Context) ->
    {Tokens, Req1} = cowboy_req:path_info(Req0),
    case api_util:parse_path_tokens(Tokens) of
        [_|_] = Nouns ->
            %% Because we allow tunneling of verbs through the request,
            %% we have to check and see if we need to override the actual
            %% HTTP method with the tunneled version
            case api_util:get_req_data(Context, Req1) of
                {'halt', Context1, Req2} ->
                    api_util:halt(Req2, cb_context:add_system_error('parse_error', Context1));
                {Context1, Req2} ->
                    determine_http_verb(Req2, Context1#cb_context{req_nouns=Nouns})
            end;
        [] ->
            lager:debug("no path tokens: ~p", [Methods]),
            {Methods, Req1, Context#cb_context{allow_methods=Methods}}
    end.

-spec determine_http_verb(cowboy_req:req(), cb_context:context()) ->
                                 {http_methods() | 'halt', cowboy_req:req(), cb_context:context()}.
determine_http_verb(Req0, Context) ->
    {Method, Req1} = cowboy_req:method(Req0),
    find_allowed_methods(Req1, Context#cb_context{req_verb=api_util:get_http_verb(Method, Context)}).

find_allowed_methods(Req0, #cb_context{allowed_methods=Methods
                                       ,req_verb=Verb
                                       ,req_nouns=[{Mod, Params}|_]
                                      }=Context) ->
    Event = api_util:create_event_name(Context, <<"allowed_methods">>),
    Responses = crossbar_bindings:map(<<Event/binary, ".", Mod/binary>>, Params),
    {Method, Req1} = cowboy_req:method(Req0),
    AllowMethods = api_util:allow_methods(Responses, Methods, Verb, wh_util:to_binary(Method)),
    maybe_add_cors_headers(Req1, Context#cb_context{allow_methods=AllowMethods}).

-spec maybe_add_cors_headers(cowboy_req:req(), cb_context:context()) ->
                                    {http_methods() | 'halt', cowboy_req:req(), cb_context:context()}.
maybe_add_cors_headers(Req0, Context) ->
    case api_util:is_cors_request(Req0) of
        {'true', Req1} ->
            lager:debug("adding cors headers"),
            check_preflight(api_util:add_cors_headers(Req1, Context), Context);
        {'false', Req1} ->
            maybe_allow_method(Req1, Context)
    end.

-spec check_preflight(cowboy_req:req(), cb_context:context()) ->
                             {http_methods(), cowboy_req:req(), cb_context:context()}.
check_preflight(Req0, #cb_context{req_verb = ?HTTP_OPTIONS}=Context) ->
    lager:debug("allowing OPTIONS request for CORS preflight"),
    {[?HTTP_OPTIONS], Req0, Context};
check_preflight(Req0, Context) ->
    maybe_allow_method(Req0, Context).

maybe_allow_method(Req0, #cb_context{allow_methods=[]}=Context) ->
    lager:debug("no allow methods"),
    api_util:halt(Req0, cb_context:add_system_error('not_found', Context));
maybe_allow_method(Req0, #cb_context{allow_methods=[Verb]=Methods
                                     ,req_verb=Verb
                                    }=Context) ->
    {Methods, Req0, Context};
maybe_allow_method(Req0, #cb_context{allow_methods=Methods
                                     ,req_verb=Verb
                                    }=Context) ->
    case lists:member(Verb, Methods) of
        'true' -> {Methods, Req0, Context};
        'false' ->
            api_util:halt(Req0, cb_context:add_system_error('invalid_method', Context))
    end.

malformed_request(Req, #cb_context{req_json={'malformed', _}}=Context) ->
    lager:debug("request is malformed"),
    {'true', Req, Context};
malformed_request(Req, #cb_context{req_verb = ?HTTP_OPTIONS}=Context) ->
    {'false', Req, Context};
malformed_request(Req, #cb_context{req_nouns=Nouns}=Context) ->
    case props:get_value(<<"accounts">>, Nouns) of
        [AcctId] ->
            case cb_accounts:validate(Context, AcctId) of
                #cb_context{resp_status='success'}=Context1 ->
                    {'false', Req, Context1};
                Context1 ->
                    api_util:halt(Req, Context1)
            end;
        _Other ->
            {'false', Req, Context}
    end.

-spec is_authorized(cowboy_req:req(), cb_context:context()) ->
                           {'true' | {'false', <<>>}, cowboy_req:req(), cb_context:context()}.
is_authorized(Req, Context) ->
    api_util:is_authentic(Req, Context).

-spec forbidden(cowboy_req:req(), cb_context:context()) ->
                       {'false', cowboy_req:req(), cb_context:context()}.
forbidden(Req0, Context0) ->
    case api_util:is_permitted(Req0, Context0) of
        {'halt', _, _}=Reply -> Reply;
        {IsPermitted, Req1, Context1} ->
            {not IsPermitted, Req1, Context1}
    end.

-spec valid_content_headers(cowboy_req:req(), cb_context:context()) ->
                                   {'true', cowboy_req:req(), cb_context:context()}.
valid_content_headers(Req, Context) ->
    {'true', Req, Context}.

-spec known_content_type(cowboy_req:req(), cb_context:context()) ->
                                {boolean(), cowboy_req:req(), cb_context:context()}.
known_content_type(Req, #cb_context{req_verb = ?HTTP_OPTIONS}=Context) ->
    {'true', Req, Context};
known_content_type(Req, #cb_context{req_verb = ?HTTP_GET}=Context) ->
    {'true', Req, Context};
known_content_type(Req, #cb_context{req_verb = ?HTTP_DELETE}=Context) ->
    {'true', Req, Context};
known_content_type(Req, Context) ->
    Req2 = case cowboy_req:header(<<"content-type">>, Req) of
               {'undefined', Req1} ->
                   cowboy_req:set_resp_header(<<"X-RFC2616">>
                                              ,<<"Section 14.17 (Try it, you'll like it)">>
                                              ,Req1);
               {_, Req1} -> Req1
           end,
    api_util:is_known_content_type(Req2, Context).

-spec valid_entity_length(cowboy_req:req(), cb_context:context()) ->
                                 {'true', cowboy_req:req(), cb_context:context()}.
valid_entity_length(Req, Context) ->
    {'true', Req, Context}.

-spec options(cowboy_req:req(), cb_context:context()) ->
                     {'ok', cowboy_req:req(), cb_context:context()}.
options(Req0, Context) ->
    case api_util:is_cors_request(Req0) of
        {'true', Req1} ->
            lager:debug("is CORS request"),
            Req2 = api_util:add_cors_headers(Req1, Context),
            Req3 = cowboy_req:set_resp_body(<<>>, Req2),
            {'ok', Req3, Context};
        {'false', Req1} ->
            lager:debug("is not CORS request"),
            {'ok', Req1, Context}
    end.

-type content_type_callbacks() :: [{{ne_binary(), ne_binary(), wh_proplist()}, atom()} |
                                   {ne_binary(), atom()}
                                   ,...] | [].
-spec content_types_provided(cowboy_req:req(), cb_context:context()) ->
                                    {content_type_callbacks(), cowboy_req:req(), cb_context:context()}.
content_types_provided(Req, #cb_context{req_nouns=Nouns}=Context0) ->
    lager:debug("run: content_types_provided"),
    #cb_context{content_types_provided=CTPs}=Context1 =
        lists:foldr(fun({Mod, Params}, ContextAcc) ->
                            Event = api_util:create_event_name(Context0, <<"content_types_provided.", Mod/binary>>),
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
                                                [ {EncType, Fun} | Acc1 ];
                                           (CT, Acc1) when is_binary(CT) ->
                                                [{CT, Fun} | Acc1]
                                        end, Acc, L)
                    end, [], CTPs),
    {CTP, Req, Context}.

-spec content_types_accepted(cowboy_req:req(), cb_context:context()) ->
                                    {content_type_callbacks(), cowboy_req:req(), cb_context:context()}.
content_types_accepted(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    lager:debug("run: content_types_accepted"),
    Context1 =
        lists:foldr(fun({Mod, Params}, ContextAcc) ->
                            Event = api_util:create_event_name(Context0, <<"content_types_accepted.", Mod/binary>>),
                            Payload = [ContextAcc | Params],
                            crossbar_bindings:fold(Event, Payload)
                    end, Context0, Nouns),

    case cowboy_req:parse_header(<<"content-type">>, Req0) of
        {'undefined', 'undefined', Req1} -> default_content_types_accepted(Req1, Context1);
        {'ok', CT, Req1} -> content_types_accepted(CT, Req1, Context1)
    end.

-spec default_content_types_accepted(cowboy_req:req(), cb_context:context()) ->
                                            {[{'undefined', atom()},...], cowboy_req:req(), cb_context:context()}.
default_content_types_accepted(Req, #cb_context{content_types_accepted=CTAs}=Context) ->
    CTA = [ {'*', Fun}
            || {Fun, L} <- CTAs,
               lists:any(fun({Type, SubType}) ->
                                 lager:debug("t: ~p sub: ~p", [Type, SubType]),
                                 api_util:content_type_matches(?CROSSBAR_DEFAULT_CONTENT_TYPE
                                                              ,{Type, SubType, []});
                            ({_,_,_}=ModCT) ->
                                 lager:debug("modct: ~p", [ModCT]),
                                 api_util:content_type_matches(?CROSSBAR_DEFAULT_CONTENT_TYPE, ModCT)
                         end, L) % check each type against the default
          ],
    lager:debug("cta: ~p", [CTA]),
    {CTA, Req, Context}.

-spec content_types_accepted(content_type(), cowboy_req:req(), cb_context:context()) ->
                                    {[{content_type(), atom()},...], cowboy_req:req(), cb_context:context()}.
content_types_accepted(CT, Req, #cb_context{content_types_accepted=CTAs}=Context) ->
    CTA = lists:foldl(fun({Fun, L}, Acc) ->
                              lists:foldl(fun({Type, SubType}, Acc1) ->
                                                  case api_util:content_type_matches(CT, {Type, SubType, []}) of
                                                      'true' -> [{CT, Fun} | Acc1];
                                                      'false' -> Acc1
                                                  end;
                                             ({_,_,_}=EncType, Acc1) ->
                                                  [ {EncType, Fun} | Acc1 ]
                                          end, Acc, L)
                      end, [], CTAs),
    {CTA, Req, Context}.

-spec languages_provided(cowboy_req:req(), cb_context:context()) ->
                                {ne_binaries(), cowboy_req:req(), cb_context:context()}.
languages_provided(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    lager:debug("run: languages_provided"),
    {Req1, #cb_context{languages_provided=LangsProvided}=Context1} =
        lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
                            Event = api_util:create_event_name(Context0, <<"languages_provided.", Mod/binary>>),
                            Payload = {ReqAcc, ContextAcc, Params},
                            {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
                            {ReqAcc1, ContextAcc1}
                    end, {Req0, Context0}, Nouns),
    case cowboy_req:parse_header(<<"accept-language">>, Req1) of
        {'ok', 'undefined', Req2} -> {LangsProvided, Req2, Context1};
        {'ok', [{A,_}|_]=_Accepted, Req2} ->
            lager:debug("adding first accept-lang header language: ~s", [A]),
            {LangsProvided ++ [A], Req2, Context1}
    end.

%% -spec charsets_provided(cowboy_req:req(), cb_context:context()) -> {[ne_binary(),...], cowboy_req:req(), cb_context:context()}.
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
    'no_call'.

-spec encodings_provided(cowboy_req:req(), cb_context:context()) ->
                                {ne_binaries(), cowboy_req:req(), cb_context:context()}.
encodings_provided(Req0, #cb_context{req_nouns=Nouns}=Context0) ->
    lager:debug("run: encodings_provided"),
    {Req1, #cb_context{encodings_provided=EncodingsProvided}=Context1} =
        lists:foldr(fun({Mod, Params}, {ReqAcc, ContextAcc}) ->
                            Event = api_util:create_event_name(Context0, <<"encodings_provided.", Mod/binary>>),
                            Payload = {ReqAcc, ContextAcc, Params},
                            {ReqAcc1, ContextAcc1, _} = crossbar_bindings:fold(Event, Payload),
                            {ReqAcc1, ContextAcc1}
                    end, {Req0, Context0}, Nouns),
    {EncodingsProvided, Req1, Context1}.

-spec resource_exists(cowboy_req:req(), cb_context:context()) ->
                             {boolean(), cowboy_req:req(), cb_context:context()}.
resource_exists(Req, #cb_context{req_nouns=[{<<"404">>,_}|_]}=Context) ->
    lager:debug("failed to tokenize request, returning 404"),
    {'false', Req, Context};
resource_exists(Req0, Context0) ->
    lager:debug("run: resource_exists"),
    case api_util:does_resource_exist(Context0) of
        'true' ->
            lager:debug("requested resource exists, validating it"),
            #cb_context{req_verb=Verb}=Context1 = api_util:validate(Context0),
            case api_util:succeeded(Context1) of
                'true' when Verb =/= ?HTTP_PUT ->
                    lager:debug("requested resource update validated"),
                    {'true', Req0, Context1};
                'true' ->
                    lager:debug("requested resource creation validated"),
                    {'false', Req0, Context1};
                'false' ->
                    lager:debug("failed to validate resource"),
                    api_util:halt(Req0, Context1)
            end;
        'false' ->
            lager:debug("requested resource does not exist"),
            {'false', Req0, Context0}
    end.

-spec moved_temporarily(cowboy_req:req(), cb_context:context()) ->
                               {'false', cowboy_req:req(), cb_context:context()}.
moved_temporarily(Req, Context) ->
    lager:debug("run: moved_temporarily"),
    {'false', Req, Context}.

-spec moved_permanently(cowboy_req:req(), cb_context:context()) ->
                               {'false', cowboy_req:req(), cb_context:context()}.
moved_permanently(Req, Context) ->
    lager:debug("run: moved_permanently"),
    {'false', Req, Context}.

-spec previously_existed(cowboy_req:req(), cb_context:context()) ->
                                {'false', cowboy_req:req(), cb_context:context()}.
previously_existed(Req, State) ->
    lager:debug("run: previously_existed"),
    {'false', Req, State}.

%% If we're tunneling PUT through POST,
%% we need to allow POST to create a non-existent resource
%% AKA, 201 Created header set
-spec allow_missing_post(cowboy_req:req(), cb_context:context()) ->
                                {boolean(), cowboy_req:req(), cb_context:context()}.
allow_missing_post(Req0, #cb_context{req_verb=_Verb}=Context) ->
    lager:debug("run: allow_missing_post when req_verb = ~s", [_Verb]),
    {Method, Req1} = cowboy_req:method(Req0),
    {Method =:= ?HTTP_POST, Req1, Context}.

-spec delete_resource(cowboy_req:req(), cb_context:context()) ->
                             {boolean() | 'halt', cowboy_req:req(), cb_context:context()}.
delete_resource(Req, Context) ->
    lager:debug("run: delete_resource"),
    api_util:execute_request(Req, Context).

-spec delete_completed(cowboy_req:req(), cb_context:context()) ->
                              {boolean(), cowboy_req:req(), cb_context:context()}.
delete_completed(Req, Context) ->
    lager:debug("run: delete_completed"),
    api_util:create_push_response(Req, Context).

%% If allow_missing_post returned true (cause it was a POST) and PUT has been tunnelled,
%% POST is a create
-spec post_is_create(cowboy_req:req(), cb_context:context()) ->
                            {boolean(), cowboy_req:req(), cb_context:context()}.
post_is_create(Req, #cb_context{req_verb = ?HTTP_PUT}=Context) ->
    lager:debug("treating post request as a create"),
    {'true', Req, Context};
post_is_create(Req, Context) ->
    lager:debug("run: post_is_create: false"),
    {'false', Req, Context}.

%% set the location header
-spec create_path(cowboy_req:req(), cb_context:context()) ->
                         {ne_binary(), cowboy_req:req(), cb_context:context()}.
create_path(Req, #cb_context{resp_headers=RespHeaders}=Context) ->
    lager:debug("run: create_path"),
    %% Location header goes here, I believe?
    Path = props:get_value(<<"Location">>, RespHeaders, <<>>),
    lager:debug("setting path to: ~s", [Path]),
    {crossbar_util:get_path(Req, Path), Req, Context}.

-spec process_post(cowboy_req:req(), cb_context:context()) ->
                          {boolean(), cowboy_req:req(), cb_context:context()}.
process_post(Req0, Context0) ->
    lager:debug("run: process_post"),
    case api_util:execute_request(Req0, Context0) of
        {'true', Req1, Context1} ->
            Event = api_util:create_event_name(Context1, <<"process_post">>),
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            api_util:create_push_response(Req1, Context1);
        Else -> Else
    end.

-spec is_conflict(cowboy_req:req(), cb_context:context()) ->
                         {boolean(), cowboy_req:req(), cb_context:context()}.
is_conflict(Req, #cb_context{resp_error_code=409}=Context) ->
    lager:debug("request resulted in conflict"),
    {'true', Req, Context};
is_conflict(Req, Context) ->
    lager:debug("run: is_conflict: false"),
    {'false', Req, Context}.

-spec from_binary(cowboy_req:req(), cb_context:context()) ->
                         {boolean(), cowboy_req:req(), cb_context:context()}.
from_binary(Req0, Context0) ->
    lager:debug("run: from_binary"),
    case api_util:execute_request(Req0, Context0) of
        {'true', Req1, Context1} ->
            Event = api_util:create_event_name(Context1, <<"from_binary">>),
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            api_util:create_push_response(Req1, Context1);
        Else -> Else
    end.

-spec from_json(cowboy_req:req(), cb_context:context()) ->
                       {boolean(), cowboy_req:req(), cb_context:context()}.
from_json(Req0, Context0) ->
    lager:debug("run: from_json"),
    case api_util:execute_request(Req0, Context0) of
        {'true', Req1, Context1} ->
            Event = api_util:create_event_name(Context1, <<"from_json">>),
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            api_util:create_push_response(Req1, Context1);
        Else -> Else
    end.

-spec from_form(cowboy_req:req(), cb_context:context()) ->
                       {boolean(), cowboy_req:req(), cb_context:context()}.
from_form(Req0, Context0) ->
    lager:debug("run: from_form"),
    case api_util:execute_request(Req0, Context0) of
        {'true', Req1, Context1} ->
            Event = api_util:create_event_name(Context1, <<"from_form">>),
            _ = crossbar_bindings:map(Event, {Req1, Context1}),
            api_util:create_push_response(Req1, Context1);
        Else -> Else
    end.

-spec to_json(cowboy_req:req(), cb_context:context()) ->
                     {iolist() | ne_binary() | 'halt', cowboy_req:req(), cb_context:context()}.
to_json(Req, Context) ->
    case is_csv_request(Context) of
        'true' -> to_csv(Req, Context);
        'false' ->
            Event = api_util:create_event_name(Context, <<"to_json">>),
            _ = crossbar_bindings:map(Event, {Req, Context}),
            api_util:create_pull_response(Req, Context)
    end.

-spec to_binary(cowboy_req:req(), cb_context:context()) ->
                       {binary(), cowboy_req:req(), cb_context:context()}.
to_binary(Req, #cb_context{resp_data=RespData}=Context) ->
    Event = api_util:create_event_name(Context, <<"to_binary">>),
    _ = crossbar_bindings:map(Event, {Req, Context}),
    lager:debug("responding to_binary"),
    {RespData, api_util:set_resp_headers(Req, Context), Context}.

-spec to_csv(cowboy_req:req(), cb_context:context()) ->
                    {iolist(), cowboy_req:req(), cb_context:context()}.
to_csv(Req, #cb_context{resp_headers=RespHeaders}=Context) ->
    RespBody = maybe_flatten_jobj(Context),
    RespHeaders1 = [{<<"Content-Type">>, <<"application/octet-stream">>}
                    ,{<<"Content-Length">>, iolist_size(RespBody)}
                    ,{<<"Content-Disposition">>, <<"attachment; filename=\"data.csv\"">>}
                    | RespHeaders
                   ],
    {RespBody, api_util:set_resp_headers(Req, Context#cb_context{resp_headers=RespHeaders1}), Context}.

-spec is_csv_request(cb_context:context()) -> boolean().
is_csv_request(#cb_context{query_json=Query}) ->
    case wh_json:get_value(<<"accept">>, Query, 'false') of
        <<"csv">> ->
            lager:debug("overriding req header to use csv", []),
            'true';
        _ -> 'false'
    end.

maybe_flatten_jobj(#cb_context{resp_data=RespData
                               ,query_json=JsonQuery
                              }) ->
    Query = wh_json:to_proplist(JsonQuery),
    case proplists:get_all_values(<<"identifier">>, Query) of
        [] -> 
            Routines = [fun(J) -> check_integrity(J) end
                        ,fun(J) -> create_csv_header(J) end
                        ,fun(J) -> json_objs_to_csv(J) end
                       ],
            lists:foldl(
              fun(F, J) ->
                      F(J)
              end, RespData, Routines);
        Identifier ->
            Depth = wh_json:get_integer_value(<<"depth">>, JsonQuery, 1),
            JObj = wh_json:flatten(RespData, Depth, Identifier),
            Routines = [fun(J) -> check_integrity(J) end
                        ,fun(J) -> create_csv_header(J) end
                        ,fun(J) -> json_objs_to_csv(J) end
                       ],
            lists:foldl(
              fun(F, J) ->
                      F(J)
              end, JObj, Routines)
    end.

-spec check_integrity(list()) -> wh_json:objects().
check_integrity(JObjs) ->
    Headers = get_headers(JObjs),
    check_integrity(JObjs, Headers, []).

check_integrity([], _, Acc) ->
    lists:reverse(Acc);
check_integrity([JObj|JObjs], Headers, Acc) ->
    NJObj = lists:foldl(
              fun(Header, J) ->
                      case wh_json:get_value(Header, J) of
                          'undefined' ->
                              wh_json:set_value(Header, <<"">>, J);
                          _ -> J
                      end
              end, JObj, Headers),
    NJObj1 = wh_json:from_list(lists:keysort(1, wh_json:to_proplist(NJObj))),
    check_integrity(JObjs, Headers, [NJObj1|Acc]).

-spec get_headers(wh_json:objects()) -> ne_binaries().
get_headers(JObjs) ->
    lists:foldl(
      fun(JObj, Headers) ->
              Keys = proplists:get_keys(wh_json:to_proplist(JObj)),
              lists:foldl(
                fun(Key, Hs) ->
                        case lists:member(Key, Hs) of
                            'false' -> [Key|Hs];
                            'true' -> Hs
                        end
                end, Headers, Keys)
      end, [], JObjs).

-spec create_csv_header(list()) -> wh_json:objects().
create_csv_header([]) -> [];
create_csv_header([JObj|_]=JObjs) -> [JObj|JObjs].

-spec json_objs_to_csv(wh_json:objects()) -> iolist().
json_objs_to_csv([]) -> [];
json_objs_to_csv([J|JObjs]) ->
    [csv_header(J), [json_to_csv(JObj) || JObj <- JObjs]].

-spec csv_header(wh_json:object()) -> iolist().
csv_header(JObj) ->
    csv_ize(wh_json:get_keys(JObj)).

-spec csv_ize(wh_json:keys()) -> iolist().
csv_ize([F|Rest]) ->
    [<<"\"">>, wh_util:to_binary(F), <<"\"">>
     ,[[<<",\"">>, wh_util:to_binary(V), <<"\"">>] || V <- Rest]
     ,<<"\n">>
    ].

-spec json_to_csv(wh_json:object()) -> iolist().
json_to_csv(JObj) ->
    {Vs, _} = wh_json:get_values(correct_jobj(JObj)),
    csv_ize(Vs).

-spec correct_jobj(wh_json:object()) -> wh_json:object().
correct_jobj(JObj) ->
    Prop = wh_json:to_proplist(JObj),
    L = lists:map(fun(X) -> correct_proplist(X) end, Prop),
    wh_json:from_list(L).

correct_proplist({K}) -> {K, <<"">>};
correct_proplist(T) -> T.

-spec multiple_choices(cowboy_req:req(), cb_context:context()) ->
                              {'false', cowboy_req:req(), cb_context:context()}.
multiple_choices(Req, Context) ->
    {'false', Req, Context}.

-spec generate_etag(cowboy_req:req(), cb_context:context()) ->
                           {ne_binary(), cowboy_req:req(), cb_context:context()}.
generate_etag(Req0, Context0) ->
    Event = api_util:create_event_name(Context0, <<"etag">>),
    {Req1, #cb_context{resp_etag=ETag}=Context1} = crossbar_bindings:fold(Event, {Req0, Context0}),
    case ETag of
        'automatic' ->
            {Content, _} = api_util:create_resp_content(Req1, Context1),
            Tag = wh_util:to_hex_binary(crypto:md5(Content)),
            {list_to_binary([$", Tag, $"]), Req1, Context1#cb_context{resp_etag=Tag}};
        'undefined' ->
            {'undefined', Req1, Context1#cb_context{resp_etag='undefined'}};
        Tag ->
            {list_to_binary([$", Tag, $"]), Req1, Context1#cb_context{resp_etag=Tag}}
    end.

-spec expires(cowboy_req:req(), cb_context:context()) ->
                     {wh_datetime(), cowboy_req:req(), cb_context:context()}.
expires(Req, #cb_context{resp_expires=Expires}=Context) ->
    Event = api_util:create_event_name(Context, <<"expires">>),
    crossbar_bindings:fold(Event, {Expires, Req, Context}).
