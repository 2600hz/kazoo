%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle CRUD operations for functions
%%% @author Ming Luo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_functions).

-export([init/0
        ,authenticate/1
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,validate/1, validate/2
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2, delete_account/1
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"functions/crossbar_listing">>).

-define(PATH_TOKEN_SAMPLES, <<"samples">>).
-define(AVAILABLE_FUNCTIONS, <<"functions/functions_meta_listing">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kz_datamgr:db_create(?KZ_FUNCTIONS_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_SCHEMA_DB, ?APP, <<"schemas/functions.json">>),

    %% TODO: check if DB set up is required at bootstrap, I could have set up manually.
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.functions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.functions">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.functions">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.functions">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.functions">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.functions">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.functions">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.accounts">>, ?MODULE, 'delete_account'),
    _ = crossbar_bindings:bind(<<"*.finish_request">>, ?MODULE, 'finish_request'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Authenticates the incoming request, returning true and context
%% if the requestor is known, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) ->
                          {'true', cb_context:context()} |
                          'false'.
authenticate(Context) ->
    authenticate(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

-spec authenticate(cb_context:context(), http_method(), req_nouns()) ->
                          {'true', cb_context:context()} |
                          'false'.
authenticate(Context, ?HTTP_GET, [{<<"functions">>, []}]) ->
    {'true', Context};
authenticate(Context, ?HTTP_GET, [{<<"functions">>, [?PATH_TOKEN_SAMPLES]}]) ->
    {'true', Context}.

%%------------------------------------------------------------------------------
%% @doc Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(Context
             ,cb_context:req_nouns(Context)
             ,cb_context:req_verb(Context)
             ).

-spec authorize(cb_context:context(), req_nouns(), http_method()) -> boolean().
authorize(_Context, [{<<"functions">>, [?PATH_TOKEN_SAMPLES]}], ?HTTP_GET) ->
    'true';
authorize(_Context, _Nouns, _Verb) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?PATH_TOKEN_SAMPLES) ->
    [?HTTP_GET];
allowed_methods(_FunctionId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% For example:
%%
%% ```
%%    /functions => []
%%    /functions/foo => [<<"foo">>]
%%    /functions/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?PATH_TOKEN_SAMPLES) -> 'true';
resource_exists(_FunctionId) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> boolean().
resource_exists(?PATH_TOKEN_SAMPLES, _SampleId) -> 'true';
resource_exists(_, _) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /functions might load a list of function objects
%% /functions/123 might load the function object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_functions(cb_context:set_account_db(Context, ?KZ_FUNCTIONS_DB), cb_context:req_verb(Context)).

-spec validate_functions(cb_context:context(), http_method()) -> cb_context:context().
validate_functions(Context, ?HTTP_GET) ->
    case cb_context:req_nouns(Context) of
        [{<<"functions">>, []}] -> summary_available(Context);
        _Nouns -> summary(Context)
    end;
validate_functions(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?PATH_TOKEN_SAMPLES) ->
    Path = filename:join(code:priv_dir(?APP), <<"functions_javascript-samples.json">>),
    case file:read_file(Path) of
        {'ok', Bin} ->
            HasFilter = crossbar_filter:is_defined(Context),
            Resp = [JObj || JObj <- kz_json:decode(Bin),
                            crossbar_filter:by_doc(JObj, Context, HasFilter)
                   ],
            crossbar_doc:handle_json_success(Resp, Context);
        {'error', 'enoent'} ->
            crossbar_util:response_bad_identifier(<<"functions_javascript">>, Context);
        {'error', _Reason} ->
            lager:debug("failed to read file ~s: ~p", [Path, _Reason]),
            cb_context:add_system_error('datastore_fault', Context)
    end;
validate(Context, FunctionId) ->
    validate_function(cb_context:set_account_db(Context, ?KZ_FUNCTIONS_DB), FunctionId, cb_context:req_verb(Context)).

-spec validate_function(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_function(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_function(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_function(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, Context);
validate_function(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _ID) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is PATCH, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _) ->
    crossbar_doc:save(Context).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"functions">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(kzd_functions:type())).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(kzd_functions:schema_name(), Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    AccountId = cb_context:account_id(Context),
    crossbar_view:load(Context
                      ,?CB_LIST
                      ,[{'start_keymap',  [AccountId]}
                       ,{'end_keymap', [AccountId, kz_json:new()]}
                       ,{'mapper', fun normalize_view_results/2}
                       ]
                      ).

-spec delete_account(kz_term:ne_binary()) -> 'ok'.
delete_account(Account) ->
    AccountId = kz_util:format_account_id(Account),
    case kz_datamgr:get_results(?KZ_FUNCTIONS_DB
                               ,?CB_LIST
                               ,[{'startkey', [AccountId]}
                                ,{'endkey', [AccountId, <<"\ufff0">>]}
                                ]
                               )
    of
        {'ok', []} -> 'ok';
        {'error', _} -> 'ok';
        {'ok', AccountFunctions} ->
            _ = kz_datamgr:del_docs(?KZ_FUNCTIONS_DB, [kz_doc:id(AccountFun) || AccountFun <- AccountFunctions]),
            lager:info("deleted ~p functions for account ~s", [length(AccountFunctions), AccountId])
    end.

-spec summary_available(cb_context:context()) ->
                               cb_context:context().
summary_available(Context) ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    IsSuperAdmin = cb_context:is_superduper_admin(Context),
    C1 = cb_context:store(Context, 'is_superduper_admin', IsSuperAdmin),
    Options = [{'mapper', fun normalize_available/3}
              ,'include_docs'
              ],
    crossbar_view:load(cb_context:set_account_db(C1, MasterAccountDb), ?AVAILABLE_FUNCTIONS, Options).

-spec normalize_available(cb_context:context(), kz_json:object(), kz_json:objects()) ->
                                 kz_json:objects().
normalize_available(Context, JObj, Acc) ->
    maybe_filter_non_admin_hooks(Context, kz_doc:id(JObj), kz_json:get_json_value(<<"doc">>, JObj), Acc).

-spec maybe_filter_non_admin_hooks(cb_context:context(), kz_term:ne_binary(), kz_json:object(), kz_json:objects()) -> kz_json:objects().
maybe_filter_non_admin_hooks(_, <<"functions_", Id/binary>>, JObj, Acc) ->
    [kz_doc:set_id(JObj, Id) | Acc];
maybe_filter_non_admin_hooks(_, Id, JObj, Acc) ->
    [kz_doc:set_id(JObj, Id) | Acc].
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), kzd_functions:type()));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(kzd_functions:type())).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    Id = kz_doc:id(JObj),
    Value = kz_json:get_json_value(<<"value">>, JObj),
    [kz_json:set_value(<<"id">>, Id, Value)|Acc].
