%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_phone_numbers_v2).

-export([init/0
         ,authenticate/1
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,post/2
         ,put/2 ,put/3
         ,delete/2
        ]).

-include("../crossbar.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(CB_LIST, <<"phone_numbers/crossbar_listing">>).
-define(KNM_NUMBER, <<"numbers">>).

-define(FIND_NUMBER_PREFIX
        ,wh_json:from_list([{<<"required">>, 'true'}
                            ,{<<"type">>, <<"string">>}
                            ,{<<"minLength">>, 3}
                            ,{<<"maxLength">>, 10}
                           ])
       ).
-define(FIND_NUMBER_QUANTITY
        ,wh_json:from_list([{<<"default">>, 1}
                            ,{<<"type">>, <<"integer">>}
                            ,{<<"minimum">>, 1}
                           ])
       ).

-define(FIND_NUMBER_PROPERTIES
        ,wh_json:from_list([{<<"prefix">>, ?FIND_NUMBER_PREFIX}
                            ,{<<"quantity">>, ?FIND_NUMBER_QUANTITY}
                           ])
       ).

-define(FIND_NUMBER_SCHEMA
        ,wh_json:from_list([{<<"$schema">>, <<"http://json-schema.org/draft-03/schema#">>}
                            ,{<<"id">>, <<"find_number">>}
                            ,{<<"properties">>, ?FIND_NUMBER_PROPERTIES}
                           ])
       ).

-define(CLASSIFIERS, <<"classifiers">>).
-define(LOCALITY, <<"locality">>).
-define(FIX, <<"fix">>).
-define(ACTIVATE, <<"activate">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v2_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v2_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v2_resource.allowed_methods.phone_numbers">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v2_resource.resource_exists.phone_numbers">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v2_resource.validate.phone_numbers">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.post.phone_numbers">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.put.phone_numbers">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v2_resource.execute.delete.phone_numbers">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    maybe_authenticate(
        cb_context:req_verb(Context)
        ,cb_context:req_nouns(Context)
    ).

-spec maybe_authenticate(http_method(), req_nouns()) -> boolean().
maybe_authenticate(?HTTP_GET, [{<<"phone_numbers">>, []}]) ->
    'true';
maybe_authenticate(_Verb, _Nouns) ->
    'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    maybe_authorize(
        cb_context:req_verb(Context)
        ,cb_context:req_nouns(Context)
    ).

maybe_authorize(?HTTP_GET, [{<<"phone_numbers">>,[]}]) ->
    'true';
maybe_authorize(_Verb, _Nouns) ->
    'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

allowed_methods(?CLASSIFIERS) ->
    [?HTTP_GET];
allowed_methods(?LOCALITY) ->
    [?HTTP_POST];
allowed_methods(?FIX) ->
    [?HTTP_POST];
allowed_methods(_) ->
    [[?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(?CLASSIFIERS, _Number) ->
    [?HTTP_GET];
allowed_methods(_Number, ?ACTIVATE) ->
    [?HTTP_PUT].


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /phone_numbers => []
%%    /phone_numbers/foo => [<<"foo">>]
%%    /phone_numbers/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists() -> 'true'.

resource_exists(_) -> 'true'.

resource_exists(?CLASSIFIERS, _Number) -> 'true';
resource_exists(_Number, ?ACTIVATE) -> 'true';
resource_exists(_, _) -> 'false'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /phone_numbers mights load a list of skel objects
%% /phone_numbers/123 might load the skel object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_phone_numbers(Context, cb_context:req_verb(Context), cb_context:account_id(Context)).

validate(Context, ?CLASSIFIERS) ->
    load_classifiers(Context);
validate(Context, ?LOCALITY) ->
    validate_locality(Context, cb_context:req_value(Context, <<"numbers">>));
validate(Context, ?FIX) ->
    cb_context:set_resp_status(Context, 'success');
validate(Context, Id) ->
    validate_phone_number(Context, Id, cb_context:req_verb(Context)).

validate(Context, ?CLASSIFIERS, Number) ->
    maybe_classify(Context, Number);
validate(Context, Number, ?ACTIVATE) ->
    cb_context:validate_request_data(?KNM_NUMBER, Context).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?LOCALITY) ->
    case cb_context:resp_status(Context) of
        'success' ->
            locality(Context, cb_context:req_value(Context, <<"numbers">>));
        _ -> Context
    end;
post(Context, ?FIX) ->
    AccountId = cb_context:account_id(Context),
    _ = knm_maintenance:fix_by_account(AccountId),
    summary(Context);
post(Context, Number) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context(), path_token()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token() -> cb_context:context().
put(Context, Num) ->
    ReqJObj = cb_context:req_json(Context),
    DryRun = (not wh_json:is_true(<<"accept_charges">>, ReqJObj)),
    Options = [
        {<<"assigned_to">>, cb_context:account_id(Context)}
        ,{<<"auth_by">>, cb_context:auth_account_id(Context)}
        ,{<<"dry_run">>, DryRun}
    ],
    case knm_number:create(Num, Options) of
        {'error', Reason} ->
            error_return(Context, Num, Reason);
        {'ok', Number} ->
            success_return(Context, Number, DryRun) %TODO
    end.

put(Context, Num, ?ACTIVATE) ->
    ReqJObj = cb_context:req_json(Context),
    DryRun = (not wh_json:is_true(<<"accept_charges">>, ReqJObj)),
    Options = [
        {<<"auth_by">>, cb_context:auth_account_id(Context)}
        ,{<<"dry_run">>, DryRun}
    ],
    case knm_number:buy(Num, cb_context:account_id(Context), Options) of
        {'error', Reason} ->
            error_return(Context, Num, Reason);
        {'ok', Number} ->
            success_return(Context, Number, DryRun) %TODO
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Num) ->
    Options = [
        {<<"auth_by">>, cb_context:auth_account_id(Context)}
    ],
    case knm_number:change_state(Num, ?NUMBER_STATE_RELEASED, Options) of
        {'error', Reason} -> error_return(Context, Num, Reason);
        {'ok', _} ->
            cb_context:set_resp_status(Context, 'success')
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec validate_phone_numbers(cb_context:context(), http_method(), binary()) -> cb_context:context().
validate_phone_numbers(Context, ?HTTP_GET, 'undefined') ->
    find_numbers(Context);
validate_phone_numbers(Context, ?HTTP_GET, _AccountId) ->
    summary(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_phone_number(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_phone_number(Context, Number, ?HTTP_GET) ->
    read(Context, Number)
validate_phone_number(Context, Number, ?HTTP_PUT) ->
    cb_context:validate_request_data(?KNM_NUMBER, Context);
validate_phone_number(Context, Number, ?HTTP_POST) ->
    cb_context:validate_request_data(?KNM_NUMBER, Context);
validate_phone_number(Context, Number, ?HTTP_DELETE) ->
    cb_context:set_doc(
        cb_context:set_resp_status(Context, 'success')
        ,'undefined'
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_locality(cb_context:context(), any()) -> cb_context:context().
validate_locality(Context, 'undefined') ->
    cb_context:add_validation_error(
        <<"numbers">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"list of numbers missing">>}
         ])
        ,Context
    );
validate_locality(Context, []) ->
    cb_context:add_validation_error(
        <<"numbers">>
        ,<<"minimum">>
        ,wh_json:from_list([
            {<<"message">>, <<"minimum 1 number required">>}
         ])
        ,Context
    );
validate_locality(Context, Numbers) when is_list(Numbers) ->
    cb_context:set_resp_status(Context, 'success');
validate_locality(Context, Numbers) ->
    cb_context:add_validation_error(
        <<"numbers">>
        ,<<"type">>
        ,wh_json:from_list([
            {<<"cause">>, Numbers}
            ,{<<"message">>, <<"numbers must be a list">>}
         ])
        ,Context
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_classifiers(cb_context:context()) -> cb_context:context().
load_classifiers(Context) ->
    cb_context:set_resp_data(
        cb_context:set_resp_status(Context, 'success')
        ,knm_converters:available_classifiers()
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_classify(cb_context:context(), path_token()) -> cb_context:context().
maybe_classify(Context, Number) ->
    case knm_converters:classify(Number) of
        'undefined' ->
            unclassified(Context, Number);
        Classifier ->
            classified(Context, Number, Classifier)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unclassified(cb_context:context(), path_token()) -> cb_context:context().
unclassified(Context, Number) ->
    RespData = base_classified(Context, Number),
    cb_context:setters(
        Context
        ,[{fun cb_context:set_resp_data/2, wh_json:from_list(RespData)}
          ,{fun cb_context:set_resp_status/2, 'success'}]
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec classified(cb_context:context(), path_token(), api_binary()) -> cb_context:context().
classified(Context, Number, Classifier) ->
    ClassifierJObj = wh_json:get_value(Classifier, knm_converters:available_classifiers()),
    RespData =
        wh_json:set_values(
            [{<<"name">>, Classifier}
             | base_classified(Context, Number)]
            ,ClassifierJObj
        ),
    cb_context:setters(
        Context
        ,[{fun cb_context:set_resp_data/2, RespData}
          ,{fun cb_context:set_resp_status/2, 'success'}]
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec base_classified(cb_context:context(), ne_binary()) -> wh_proplist().
base_classified(_Context, Number) ->
    Normalized = knm_converters:normalize(Number),
    [{<<"number">>, Number}, {<<"e164">>, Normalized}].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(cb_context:context()) -> cb_context:context().
find_numbers(Context) ->
    Opts = find_numbers_opts(Context),
    Prefix = wh_json:get_ne_value(<<"prefix">>, Opts),
    Quantity = wh_json:get_integer_value(<<"quantity">>, Opts, 1),

    OnSuccess =
        fun(C) ->
            Props = [
                {fun cb_context:set_resp_data/2, knm_carriers:find(Prefix, Quantity, wh_json:to_proplist(Opts))}
                ,{fun cb_context:set_resp_status/2, 'success'}
            ],
            cb_context:setters(C, Props)
        end,

    cb_context:validate_request_data(
        ?FIND_NUMBER_SCHEMA
        ,cb_context:set_req_data(Context, Opts)
        ,OnSuccess
    ).

-spec find_numbers_opts(cb_context:context()) -> wh_json:object().
find_numbers_opts(Context) ->
    JObj = cb_context:query_string(Context),
    AccountId = cb_context:auth_account_id(Context),
    Quantity = wh_util:to_integer(cb_context:req_value(Context, <<"quantity">>, 1)),
    wh_json:set_values([{<<"quantity">>, Quantity}
                       ,{<<"Account-ID">>, AccountId}
                       ], JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context(), ne_binary()) -> cb_context:context().
read(Context, Num) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    case knm_number:get(Num, [{<<"auth_by">>, AuthAccountId}]) of
        {'error', Reason} -> error_return(Context, Num, Reason);
        {'ok', Number} -> success_return(Context, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec locality(cb_context:context(), ne_binaries()) -> cb_context:context().
locality(Context, Numbers) ->
    case knm_locality:fetch(Numbers) of
        {'ok', JObj} ->
            cb_context:set_resp_data(Context, JObj);
        {'error', Reason} ->
            JObj = wh_json:from_list([{<<"message">>, wh_util:to_binary(Reason)}]),
            cb_context:add_system_error('unspecified_fault', JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec error_return(cb_context:context(), ne_binary(), any()) -> cb_context:context().
error_return(Context, Num, Reason) ->
    ErrorJObj = knm_errors:to_json(Reason, Num),
    cb_context:add_system_error(
        wh_json:get_integer_value(<<"code">>, ErrorJObj)
        ,wh_json:get_value(<<"error">>, ErrorJObj)
        ,wh_json:delete_keys([<<"code">>, <<"error">>], ErrorJObj)
        ,Context
    ).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec success_return(cb_context:context(), any()) -> cb_context:context().
-spec success_return(cb_context:context(), any(), boolean()) -> cb_context:context().
success_return(Context, Number) ->
    Props = [
        {fun cb_context:set_resp_data/2, knm_phone_number:to_public_json(Number)}
        ,{fun cb_context:set_resp_status/2, 'success'}
    ],
    cb_context:setters(Context, Props).

success_return(Context, Number, 'false') ->
    success_return(Context, Number);
success_return(Context, Number, 'true') ->
    case knm_phone_number:fetch_storage(Number, <<"services">>) of
        'undefined' -> wh_json:new();
        Services ->
             wh_services:dry_run(Services)
    end.






