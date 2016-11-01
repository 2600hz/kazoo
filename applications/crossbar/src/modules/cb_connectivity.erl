%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for connectivity documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_connectivity).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(CB_LIST, <<"trunkstore/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.connectivity">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.connectivity">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.connectivity">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.connectivity">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.connectivity">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.connectivity">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.connectivity">>, ?MODULE, 'delete'),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_ConnectivityId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().

validate(Context) ->
    validate_connectivity_listing(Context, cb_context:req_verb(Context)).

validate_connectivity_listing(Context, ?HTTP_GET) ->
    summary(Context);
validate_connectivity_listing(Context, ?HTTP_PUT) ->
    create(Context).

validate(Context, Id) ->
    validate_connectivity_pbx(Context, Id, cb_context:req_verb(Context)).

validate_connectivity_pbx(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_connectivity_pbx(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_connectivity_pbx(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, Context);
validate_connectivity_pbx(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            'ok' = track_assignment('post', Context),
            _ = crossbar_util:maybe_refresh_fs_xml('sys_info', Context),
            Context1;
        _Status -> Context1
    end.

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, Id) ->
    post(Context, Id).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            registration_update(Context),
            Context1;
        _Status -> Context1
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    Context1 = crossbar_doc:delete(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            registration_update(Context),
            'ok' = track_assignment('delete', Context),
            _ = crossbar_util:maybe_refresh_fs_xml('sys_info', Context),
            Context1;
        _Status -> Context1
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec registration_update(cb_context:context()) -> 'ok'.
registration_update(Context) ->
    crossbar_util:flush_registrations(
      kz_util:get_account_realm(cb_context:account_id(Context))
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec  track_assignment(atom(), cb_context:context()) -> 'ok'.
track_assignment('post', Context) ->
    OldNums = get_numbers(cb_context:fetch(Context, 'db_doc')),
    NewNums = get_numbers(cb_context:doc(Context)),
    Assigned = [{Num, <<"trunkstore">>}
                || Num <- NewNums,
                   not (lists:member(Num, OldNums))
               ],
    Unassigned = [{Num, 'undefined'}
                  || Num <- OldNums,
                     not (lists:member(Num, NewNums))
                 ],

    Updates = cb_modules_util:apply_assignment_updates(Assigned ++ Unassigned),
    cb_modules_util:log_assignment_updates(Updates);
track_assignment('delete', Context) ->
    Nums = get_numbers(cb_context:doc(Context)),
    Unassigned = [{Num, 'undefined'} || Num <- Nums],

    Updates = cb_modules_util:apply_assignment_updates(Unassigned),
    cb_modules_util:log_assignment_updates(Updates).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec  get_numbers(kz_json:object()) -> ne_binaries().
get_numbers(JObj) ->
    Servers = kz_json:get_value(<<"servers">>, JObj, []),
    lists:foldl(fun get_numbers_fold/2, [], Servers).

-spec get_numbers_fold(kz_json:object(), ne_binaries()) -> ne_binaries().
get_numbers_fold(Server, Acc) ->
    kz_json:get_keys(kz_json:get_value(<<"DIDs">>, Server, kz_json:new())) ++ Acc.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"connectivity">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"sys_info">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"connectivity">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing instance partially with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), <<"sys_info">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"sys_info">>)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST
                          ,[{'reduce', 'false'}]
                          ,Context
                          ,fun normalize_view_results/2
                          ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_doc:id(JObj) | Acc].
