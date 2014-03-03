%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_contests).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"contests/crossbar_listing">>).

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
    maybe_init_db(),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.contests">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.contests">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.contests">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.contests">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.contests">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.contests">>, ?MODULE, 'delete').

-spec maybe_init_db() -> 'ok'.
maybe_init_db() ->
    case couch_mgr:db_exists(<<"contests">>) of
        'true' -> 'ok';
        'false' -> init_db()
    end.

-spec init_db() -> 'ok'.
init_db() ->
    couch_mgr:db_create(<<"contests">>),
    _ = couch_mgr:revise_doc_from_file(<<"contests">>, 'crossbar', <<"views/contests.json">>),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /contests => []
%%    /contests/foo => [<<"foo">>]
%%    /contests/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /contests mights load a list of contest objects
%% /contests/123 might load the contest object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_contests(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_contest(Context, Id, cb_context:req_verb(Context)).

-spec validate_contests(cb_context:context(), http_method()) -> cb_context:context().
validate_contests(Context, ?HTTP_GET) ->
    summary(Context);
validate_contests(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_contest(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_contest(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_contest(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_contest(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_add_account_to_contests_db(Context1),
            Context1;
        _Status -> Context1
    end.

-spec maybe_add_account_to_contests_db(cb_context:context()) -> 'ok'.
maybe_add_account_to_contests_db(Context) ->
    AccountId = cb_context:account_id(Context),
    case couch_mgr:open_doc(<<"contests">>, AccountId) of
        {'ok', _Doc} ->
            lager:debug("account is already part of the contests aggregate");
        {'error', 'not_found'} ->
            _ = add_account_to_contests_db(AccountId),
            lager:debug("added account ~s to contests aggregate", [AccountId]);
        {'error', _E} ->
            lager:debug("failed to open account doc in contests: ~p", [_E])
    end.

-spec add_account_to_contests_db(ne_binary()) ->
                                        {'ok', wh_json:object()} |
                                        {'error', _}.
add_account_to_contests_db(AccountId) ->
    Doc = wh_doc:update_pvt_parameters(
            wh_json:from_list([{<<"_id">>, AccountId}])
            ,<<"contests">>
            ,[{'account_id', AccountId}
              ,{'type', <<"contest">>}
             ]
           ),
    couch_mgr:ensure_saved(<<"contests">>, Doc).

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
    Context1 = crossbar_doc:delete(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            maybe_remove_account_from_contests_db(Context1),
            Context1;
        _RespStatus -> Context1
    end.

-spec maybe_remove_account_from_contests_db(cb_context:context()) -> 'ok'.
maybe_remove_account_from_contests_db(Context) ->
    Context1 = summary(Context),
    case cb_context:doc(Context1) of
        [] -> remove_account_from_contests_db(cb_context:account_id(Context1));
        _ -> lager:debug("there are contests remaining")
    end.

-spec remove_account_from_contests_db(ne_binary()) -> 'ok'.
remove_account_from_contests_db(AccountId) ->
    _ = couch_mgr:del_doc(<<"contests">>, AccountId),
    lager:debug("removed account ~s from contests aggregate", [AccountId]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"contests">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"contests">>, Context, OnSuccess).

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
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, wh_json:set_value(<<"pvt_type">>, <<"contest">>, cb_context:doc(Context)));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
