%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018, Voxter Communications Inc
%%% @doc API operation Context flow
%%% Perform validation of API resources separately from request state
%%%
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(api_flow).

-export([allowed_methods/2
        ,malformed_request/1
        ,is_authorized/1
        ,forbidden/1
        ,content_types_provided/1
        ,languages_provided/2
        ,resource_exists/1
        ]).

-include("crossbar.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods(cb_context:context(), boolean()) ->
                             {http_methods() | 'stop', cb_context:context()}.
allowed_methods(Context, IsCORSRequest) ->
    lager:debug("run: allowed_methods"),

    case api_util:is_early_authentic(Context) of
        {'true', Context1} ->
            authed_allowed_methods(Context1, IsCORSRequest);
        {'stop', Context1} ->
            lager:error("request is not authorized, stopping"),
            {'stop', Context1}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authed_allowed_methods(cb_context:context(), boolean()) ->
                                    {http_methods() | 'stop', cb_context:context()}.
authed_allowed_methods(Context, IsCORSRequest) ->
    lager:debug("run: authed_allowed_methods"),

    Methods = cb_context:allowed_methods(Context),
    Tokens = api_util:path_tokens(Context),

    case api_util:parse_path_tokens(Context, Tokens) of
        [_|_] = Nouns ->
            find_allowed_methods(cb_context:set_req_nouns(Context, Nouns), IsCORSRequest);
        [] ->
            {Methods, cb_context:set_allow_methods(Context, Methods)};
        Result -> Result
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_allowed_methods(cb_context:context(), boolean()) ->
                                  {http_methods() | 'stop', cb_context:context()}.
find_allowed_methods(Context, IsCORSRequest) ->
    [{Mod, Params}|_] = cb_context:req_nouns(Context),

    Event = api_util:create_event_name(Context, <<"allowed_methods">>),
    Responses = crossbar_bindings:map(<<Event/binary, ".", Mod/binary>>, Params),

    Method = cb_context:method(Context),
    AllowMethods = api_util:allow_methods(Responses
                                         ,cb_context:req_verb(Context)
                                         ,kz_term:to_binary(Method)
                                         ),
    maybe_allow_preflight(cb_context:set_allow_methods(Context, AllowMethods), IsCORSRequest).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_allow_preflight(cb_context:context(), boolean()) ->
                                   {http_methods() | 'stop', cb_context:context()}.
maybe_allow_preflight(Context, IsCORSRequest) ->
    case IsCORSRequest of
        'true' ->
            check_preflight(Context);
        'false' ->
            maybe_allow_method(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec check_preflight(cb_context:context()) ->
                             {http_methods() | 'stop', cb_context:context()}.
check_preflight(Context) ->
    check_preflight(Context, cb_context:req_verb(Context)).

check_preflight(Context, ?HTTP_OPTIONS) ->
    lager:debug("allowing OPTIONS request for CORS preflight"),
    {[?HTTP_OPTIONS], Context};
check_preflight(Context, _Verb) ->
    maybe_allow_method(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_allow_method(cb_context:context()) ->
                                {http_methods() | 'stop', cb_context:context()}.
maybe_allow_method(Context) ->
    maybe_allow_method(Context, cb_context:allow_methods(Context), cb_context:req_verb(Context)).

maybe_allow_method(Context, [], _Verb) ->
    lager:debug("no allow methods"),
    {'stop', cb_context:add_system_error('not_found', Context)};
maybe_allow_method(Context, [Verb]=Methods, Verb) ->
    {Methods, Context};
maybe_allow_method(Context, Methods, Verb) ->
    case lists:member(Verb, Methods) of
        'true' -> {Methods, Context};
        'false' -> {'stop', cb_context:add_system_error('invalid_method', Context)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec malformed_request(cb_context:context()) ->
                               {boolean() | 'stop', cb_context:context()}.
malformed_request(Context) ->
    malformed_request(Context, cb_context:req_verb(Context)).

-spec malformed_request(cb_context:context(), http_method()) ->
                               {boolean() | 'stop', cb_context:context()}.
malformed_request(Context, ?HTTP_OPTIONS) ->
    {'false', Context};
malformed_request(Context, _ReqVerb) ->
    case props:get_value(<<"accounts">>, cb_context:req_nouns(Context)) of
        'undefined' ->
            {'false', Context};
        [] ->
            {'false', Context};
        [?MATCH_ACCOUNT_RAW(_) | _] = AccountArgs ->
            Context1 = cb_accounts:validate_resource(Context, AccountArgs),
            case cb_context:resp_status(Context1) of
                'success' -> {'false', Context1};
                _RespStatus -> {'stop', Context1}
                    % api_util:stop(Req, Context1)
            end;
        [<<>> | _] ->
            Error = kz_json:from_list([{<<"message">>, <<"missing account_id">>}]),
            {'stop', cb_context:add_system_error(404, <<"bad identifier">>, Error, Context)};
            % api_util:stop(Req, cb_context:add_system_error(404, <<"bad identifier">>, Error, Context));
        [_Other | _] ->
            Msg = kz_term:to_binary(io_lib:format("invalid account_id '~s'", [_Other])),
            Error = kz_json:from_list([{<<"message">>, Msg}]),
            {'stop', cb_context:add_system_error(404, <<"bad identifier">>, Error, Context)}
            % api_util:stop(Req, cb_context:add_system_error(404, <<"bad identifier">>, Error, Context))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_authorized(cb_context:context()) ->
                           {boolean() | 'stop', cb_context:context()}.
is_authorized(Context) ->
    api_util:is_authentic(Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec forbidden(cb_context:context()) ->
                       {'false' | 'stop', cb_context:context()}.
forbidden(Context0) ->
    case api_util:is_permitted(Context0) of
        {'stop', Context1} -> {'stop', Context1};
        {IsPermitted, Context1} ->
            {not IsPermitted, Context1}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) ->
                                    {content_type_callbacks(), cb_context:context()}.
content_types_provided(Context0) ->
    lager:debug("run: content_types_provided"),

    [{Mod, Params}|_] = cb_context:req_nouns(Context0),
    Event = api_util:create_event_name(Context0, <<"content_types_provided.", Mod/binary>>),
    Payload = [Context0 | Params],

    Context1 = crossbar_bindings:fold(Event, Payload),

    content_types_provided(Context1, cb_context:content_types_provided(Context1)).

content_types_provided(Context, []) ->
    Def = ?CONTENT_PROVIDED,
    content_types_provided(cb_context:set_content_types_provided(Context, Def), Def);
content_types_provided(Context, CTPs) ->
    CTP =
        lists:foldr(fun({Fun, L}, Acc) ->
                            lists:foldr(fun({Type, SubType}, Acc1) ->
                                                [{{Type, SubType, []}, Fun} | Acc1];
                                           ({_,_,_}=EncType, Acc1) ->
                                                [ {EncType, Fun} | Acc1 ];
                                           (CT, Acc1) when is_binary(CT) ->
                                                [{CT, Fun} | Acc1]
                                        end, Acc, L)
                    end
                   ,[]
                   ,CTPs
                   ),
    lager:debug("ctp: ~p", [CTP]),
    {CTP, Context}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec languages_provided(cb_context:context(), kz_term:api_binary()) ->
                                {kz_term:ne_binaries(), cb_context:context()}.
languages_provided(Context0, ExtraAcceptLanguage) ->
    lager:debug("run: languages_provided"),

    [{Mod, Params} | _] = cb_context:req_nouns(Context0),
    Event = api_util:create_event_name(Context0, <<"languages_provided.", Mod/binary>>),
    Payload = [Context0 | Params],
    Context1 = crossbar_bindings:fold(Event, Payload),

    case ExtraAcceptLanguage of
        'undefined' ->
            {cb_context:languages_provided(Context1), Context1};
        _ ->
            {cb_context:languages_provided(Context1) ++ [ExtraAcceptLanguage]
            ,Context1
            }
    end.

    % case cowboy_req:parse_header(<<"accept-language">>, Req0) of
    %     'undefined' ->
    %         {cb_context:languages_provided(Context1), Req0, Context1};
    %     [{A,_}|_]=_Accepted ->
    %         lager:debug("adding first accept-lang header language: ~s", [A]),
    %         {cb_context:languages_provided(Context1) ++ [A], Req0, Context1}
    % end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists(cb_context:context()) ->
                             {boolean() | 'stop', cb_context:context()}.
resource_exists(Context) ->
    resource_exists(Context, cb_context:req_nouns(Context)).

resource_exists(Context, [{<<"404">>,_}|_]) ->
    lager:debug("failed to tokenize request, returning 404"),
    {'false', Context};
resource_exists(Context, _Nouns) ->
    lager:debug("run: resource_exists"),
    case api_util:does_resource_exist(Context) of
        'true' ->
            does_request_validate(Context);
        'false' ->
            lager:debug("requested resource does not exist"),
            {'false', Context}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec does_request_validate(cb_context:context()) ->
                                   {boolean() | 'stop', cb_context:context()}.
does_request_validate(Context0) ->
    lager:debug("requested resource exists, validating it"),
    Context1 = api_util:validate(Context0),
    Verb = cb_context:req_verb(Context1),
    case api_util:succeeded(Context1) of
        'true' when Verb =/= ?HTTP_PUT ->
            lager:debug("requested resource update validated"),
            {'true', Context1};
        'true' ->
            lager:debug("requested resource creation validated"),
            {'false', Context1};
        'false' ->
            lager:debug("failed to validate resource"),
            Msg = case {cb_context:resp_error_msg(Context1)
                       ,cb_context:resp_data(Context1)
                       }
                  of
                      {'undefined', 'undefined'} ->
                          <<"validation failed">>;
                      {'undefined', Data} ->
                          kz_json:get_value(<<"message">>, Data, <<"validation failed">>);
                      {Message, _} -> Message
                  end,
            {'stop', cb_context:set_resp_error_msg(Context1, Msg)}
    end.
