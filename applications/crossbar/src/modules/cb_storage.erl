%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% storage
%%%
%%% @end
%%% @contributors:
%%%-------------------------------------------------------------------
-module(cb_storage).

-export([init/0
        ,authorize/1
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ,put/1
        ,post/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"storage/crossbar_listing">>).

-define(SYSTEM_DATAPLAN, <<"system">>).

-type scope() :: 'system'
               | {'user', ne_binary(), ne_binary()}
               | {'account', ne_binary()}
               | {'reseller', ne_binary()}.

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
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.storage">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.storage">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.storage">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.storage">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.storage">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.storage">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.storage">>, ?MODULE, 'delete').


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), req_nouns()) -> boolean().
authorize(Context, [{<<"storage">>, []}]) -> cb_context:is_superduper_admin(Context);
authorize(Context, [{<<"storage">>, []}
                   ,{<<"accounts">>, [AccountId]}
                   ]) ->
    cb_context:is_superduper_admin(Context) 
        orelse kz_services:get_reseller_id(AccountId) =:= cb_context:auth_account_id(Context); 
authorize(Context, [{<<"storage">>, []}
                   ,{<<"users">>, [UserId]}
                   ,{<<"accounts">>, [AccountId]}
                   ]) ->
    cb_context:is_superduper_admin(Context) 
        orelse kz_services:get_reseller_id(AccountId) =:= cb_context:auth_account_id(Context)
        orelse ( (AccountId =:= cb_context:auth_account_id(Context)
                  andalso cb_context:is_account_admin(Context)
                 )
                 orelse
                   (AccountId =:= cb_context:auth_account_id(Context)
                    andalso UserId =:= cb_context:auth_user_id(Context)
                   )
               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /storage => []
%%    /storage/foo => [<<"foo">>]
%%    /storage/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /storage mights load a list of storage objects
%% /storage/123 might load the storage object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_storage(Context, cb_context:req_verb(Context)).

-spec validate_storage(cb_context:context(), http_method()) -> cb_context:context().
validate_storage(Context, ?HTTP_GET) ->
    summary(set_scope(Context));
validate_storage(Context, ?HTTP_PUT) ->
    create(Context);
validate_storage(Context, ?HTTP_POST) ->
    update(read(Context));
validate_storage(Context, ?HTTP_DELETE) ->
    read(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"storage">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(cb_context:context()) -> cb_context:context().
read(Context) ->
    Context.
%%    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"storage">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(cb_context:context()) -> cb_context:context().
update(Context) ->
    Context.
%%     OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
%%     cb_context:validate_request_data(<<"storage">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    summary(Context, scope(Context)).

-spec summary(cb_context:context(), scope()) -> cb_context:context().
summary(Context, 'system') ->
    Routines = [{fun cb_context:set_resp_data/2, kzs_plan:fetch_dataplan(?SYSTEM_DATAPLAN)}
               ,{fun cb_context:set_resp_status/2, 'success'}
               ],
    cb_context:setters(Context, Routines);

summary(Context, {'account', AccountId}) ->
    Routines = [{fun cb_context:set_resp_data/2, kzs_plan:fetch_dataplan(AccountId)}
               ,{fun cb_context:set_resp_status/2, 'success'}
               ],
    cb_context:setters(Context, Routines);

summary(Context, {'user', UserId, _AccountId}) ->
    Routines = [{fun cb_context:set_resp_data/2, kzs_plan:fetch_dataplan(UserId)}
               ,{fun cb_context:set_resp_status/2, 'success'}
               ],
    cb_context:setters(Context, Routines).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"storage">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"storage">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
%% -spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
%% normalize_view_results(JObj, Acc) ->
%%     [kz_json:get_value(<<"value">>, JObj)|Acc].

-spec scope(cb_context:context()) -> scope().
scope(Context) ->
    cb_context:fetch(Context, 'scope').

-spec set_scope(cb_context:context()) -> cb_context:context().
set_scope(Context) ->
    set_scope(Context, cb_context:req_nouns(Context)).

-spec set_scope(cb_context:context(), req_nouns()) -> scope().
set_scope(Context, [{<<"storage">>, []}]) ->
    cb_context:store(Context, 'scope', 'system');
set_scope(Context, [{<<"storage">>, []}
                   ,{<<"accounts">>, [AccountId]}
                   ]) ->
    cb_context:store(Context, 'scope', {'account', AccountId});
set_scope(Context, [{<<"storage">>, []}
                   ,{<<"users">>, [UserId]}
                   ,{<<"accounts">>, [AccountId]}
                   ]) ->
    cb_context:store(Context, 'scope', {'user', UserId, AccountId}).
