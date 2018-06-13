%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018, Voxter Communications Inc
%%% @doc Multiple operations module
%%% Handle requests to perform crossbar operations on multiple IDs at once or on
%%% the results of a view
%%%
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_collections).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        % ,post/1
        ]).

-include("crossbar.hrl").

-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"*.allowed_methods.collections">>, 'allowed_methods'}
               ,{<<"*.resource_exists.collections">>, 'resource_exists'}
               ,{<<"*.validate.collections">>, 'validate'}
               % ,{<<"*.execute.put.accounts">>, 'put'}
               % ,{<<"*.execute.post.accounts">>, 'post'}
               % ,{<<"*.execute.patch.accounts">>, 'patch'}
               % ,{<<"*.execute.delete.accounts">>, 'delete'}
               ],
    cb_modules_util:bind(?MODULE, Bindings).

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    % [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST].
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Context1 = cb_context:set_resp_data(Context, #{}),
    case create_collection_context(Context1) of
        {'error', Context2} -> Context2;
        CollectionContext -> validate_paths(Context1, CollectionContext)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
% -spec post(cb_context:context()) -> cb_context:context().
% post(Context) ->
%     CollectionContexts = maps:to_list(cb_context:fetch(Context, 'collection_contexts')),
%     post_paths(Context, CollectionContexts).

% -spec post_paths(cb_context:context(), [{binary(), cb_context:context()}]) -> cb_context:context().
% post_paths(Context, []) -> Context;
% post_paths(Context, [{Path, CollectionContext}|CollectionContexts]) ->

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_paths(cb_context:context(), cb_context:context()) -> cb_context:context().
validate_paths(Context, CollectionContext) ->
    Paths = sets:to_list(cb_context:fetch(CollectionContext, 'paths')),
    validate_paths(Context, CollectionContext, Paths).

-spec validate_paths(cb_context:context(), cb_context:context(), kz_term:ne_binaries()) ->
                            cb_context:context().
validate_paths(Context, _, []) ->
    cb_context:set_resp_status(Context, 'success');
validate_paths(Context, CollectionContext, [Path|Paths]) ->
    CollectionContext1 = cb_context:set_raw_path(CollectionContext, Path),
    case api_flow:allowed_methods(CollectionContext1, 'false') of
        {'stop', CollectionContext2} ->
            %% TODO handle better?
            CollectionContext2;
        {Methods, CollectionContext2} ->
            CollectionContext3 = validate_method(CollectionContext2, Methods),
            Context1 = include_collection_result(Context, CollectionContext3),
            %% Re-use unmodified CollectionContext
            validate_paths(Context1, CollectionContext, Paths)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_method(cb_context:context(), http_methods()) -> cb_context:context().
validate_method(CollectionContext, Methods) ->
    case lists:member(cb_context:method(CollectionContext), Methods) of
        'true' -> validate_request_format(CollectionContext);
        'false' -> crossbar_util:response('error', <<"method not allowed">>, 405, CollectionContext)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request_format(cb_context:context()) -> cb_context:context().
validate_request_format(CollectionContext) ->
    case api_flow:malformed_request(CollectionContext) of
        {'stop', CollectionContext1} -> CollectionContext1;
        {_, CollectionContext1} -> validate_authorization(CollectionContext1)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
%% TODO test failing here
-spec validate_authorization(cb_context:context()) -> cb_context:context().
validate_authorization(CollectionContext) ->
    case api_flow:is_authorized(CollectionContext) of
        {'stop', CollectionContext1} -> CollectionContext1;
        {'false', CollectionContext1} -> crossbar_util:response_401(CollectionContext1);
        {'true', CollectionContext1} -> validate_resource_exists(CollectionContext1)
    end.

% -spec validate_content_type_provided(cb_context:context()) -> cb_context:context().
% validate_content_type_provided(CollectionContext) ->

% -spec validate_language_provided(cb_context:context()) -> cb_context().
% validate_language_provided(CollectionContext) ->

%% TODO test failing here w/ something like users or devices
-spec validate_resource_exists(cb_context:context()) -> cb_context:context().
validate_resource_exists(CollectionContext) ->
    case api_flow:resource_exists(CollectionContext) of
        {'stop', CollectionContext1} -> CollectionContext1;
        {_, CollectionContext1} -> CollectionContext1
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec include_collection_result(cb_context:context(), cb_context:context()) -> cb_context:context().
include_collection_result(Context, CollectionContext) ->
    RespData = cb_context:resp_data(Context),
    CollectionPathKey = cb_context:raw_path(CollectionContext),
    Result = collection_result(CollectionContext),
    CollectionContexts = cb_context:fetch(Context, 'collection_contexts', #{}),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, RespData#{CollectionPathKey => Result}}
                       ,{fun cb_context:store/3
                         ,'collection_contexts'
                         ,CollectionContexts#{CollectionPathKey => CollectionContext}
                        }
                       ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec collection_result(cb_context:context()) -> kz_json:object().
collection_result(CollectionContext) ->
    case cb_context:response(CollectionContext) of
        {'ok', RespData} ->
            kz_json:from_list([{<<"data">>, RespData}
                              ,{<<"revision">>, kz_term:to_api_binary(cb_context:resp_etag(CollectionContext))}
                              ,{<<"status">>, <<"success">>}
                              ]);
        {'error', {ErrorCode, ErrorMsg, RespData}} ->
            kz_json:from_list([{<<"data">>, RespData}
                              ,{<<"error">>, kz_term:to_binary(ErrorCode)}
                              ,{<<"message">>, ErrorMsg}
                              ,{<<"status">>, <<"error">>}
                              ])
    end.

%%------------------------------------------------------------------------------
%% @doc Create a cb_context:context() that will be used for requests of the
%% specified path.
%% @end
%%------------------------------------------------------------------------------
-spec create_collection_context(cb_context:context()) -> cb_context:context() | {'error', cb_context:context()}.
create_collection_context(Context) ->
    %% TODO validate using JSONschema
    validate_request_path(Context, cb_context:req_value(Context, <<"path">>)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request_path(cb_context:context(), kz_json:api_json_term()) ->
                                   cb_context:context() | {'error', cb_context:context()}.
validate_request_path(Context, <<_,_/binary>> = Path) ->
    validate_request_arguments(Context, Path, cb_context:req_verb(Context), cb_context:req_value(Context, <<"arguments">>));
validate_request_path(Context, 'undefined') ->
    {'error', cb_context:add_validation_error(<<"path">>, <<"required">>, <<"Field is required but missing">>, Context)};
validate_request_path(Context, _) ->
    {'error', cb_context:add_validation_error(<<"path">>
                                             ,<<"type">>
                                             ,kz_json:from_list([{<<"message">>, <<"Value did not match type(s): string">>}
                                                                ,{<<"target">>, <<"string">>}
                                                                ])
                                             ,Context
                                             )}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request_arguments(cb_context:context(), kz_term:ne_binary(), http_method(), kz_json:api_json_term()) ->
                                        cb_context:context() | {'error', cb_context:context()}.
validate_request_arguments(Context, Path, ?HTTP_GET, Arguments) when is_binary(Arguments) ->
    try kz_json:unsafe_decode(Arguments) of
        JObj -> validate_request_arguments_type(Context, Path, JObj)
    catch
        'throw':{'invalid_json', _, _} ->
            {'error', cb_context:add_validation_error(<<"arguments">>
                                                     ,<<"invalid">>
                                                     ,<<"Field must be valid URL-encoded JSON">>
                                                     ,Context
                                                     )}
    end;
validate_request_arguments(Context, _, ?HTTP_GET, _) ->
    {'error', crossbar_util:response_400(<<"invalid arguments JSON">>, kz_json:new(), Context)};
validate_request_arguments(Context, Path, _, Arguments) ->
    %% TODO more validation
    validate_request_arguments_type(Context, Path, Arguments).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request_arguments_type(cb_context:context(), kz_term:ne_binary(), kz_json:json_term()) ->
                                             cb_context:context() | {'error', cb_context:context()}.
validate_request_arguments_type(Context, Path, Arguments) ->
    %% TODO validate types of each argument
    case kz_json:is_json_object(Arguments) of
        'true' -> do_create_collection_context(Context, Path, Arguments);
        'false' ->
            {'error', crossbar_util:response_400(<<"invalid arguments JSON">>, kz_json:new(), Context)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec do_create_collection_context(cb_context:context(), binary(), kz_json:object()) -> cb_context:context().
do_create_collection_context(Context, Path, Arguments) ->
    Paths = generate_distinct_paths(Path, Arguments),

    Setters = [{fun cb_context:set_method/2, cb_context:method(Context)}
              ,{fun cb_context:set_auth_token/2, cb_context:auth_token(Context)}
              ,{fun cb_context:set_auth_token_type/2, cb_context:auth_token_type(Context)}
              ,{fun cb_context:store/3, 'paths', Paths}
              ],
    cb_context:setters(cb_context:new(), Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec generate_distinct_paths(kz_term:ne_binary(), kz_json:object()) -> sets:set(kz_term:ne_binary()).
generate_distinct_paths(Path, Arguments) ->
    generate_distinct_paths(Path, Arguments, kz_json:get_keys(Arguments), sets:new()).

-spec generate_distinct_paths(kz_term:ne_binary(), kz_json:object(), kz_json:keys(), sets:set(kz_term:ne_binary())) ->
                                     sets:set(kz_term:ne_binary()).
generate_distinct_paths(_, _, [], Acc) -> Acc;
generate_distinct_paths(Path, Arguments, [Key|Keys], Acc) ->
    Value = kz_json:get_value(Key, Arguments),
    Acc1 = apply_path_arguments(Path, Key, Value, Acc),
    generate_distinct_paths(Path, Arguments, Keys, Acc1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec apply_path_arguments(kz_term:ne_binary(), kz_json:path(), kz_json:json_term(), sets:set(kz_term:ne_binary())) ->
                                  sets:set(kz_term:ne_binary()).
apply_path_arguments(Path, Key, Values, Acc) when is_list(Values) ->
    %% TODO test for multiple arguments. Cross product?
    lists:foldl(fun(Value, Acc1) ->
                        Path1 = re:replace(Path, <<"{", Key/binary, "}">>, Value, ['global', {'return', 'binary'}]),
                        sets:add_element(Path1, Acc1)
                end
               ,Acc
               ,Values
               ).
