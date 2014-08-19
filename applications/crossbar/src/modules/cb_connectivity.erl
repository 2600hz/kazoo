%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
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
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"trunkstore/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.connectivity">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.connectivity">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.connectivity">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.connectivity">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.connectivity">>, ?MODULE, 'post'),
    crossbar_bindings:bind(<<"*.execute.delete.connectivity">>, ?MODULE, 'delete').

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
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

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
validate_connectivity_pbx(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    'ok' = track_assignment('post', Context),
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    registration_update(Context),
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    registration_update(Context),
    'ok' = track_assignment('delete', Context),
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec registration_update(cb_context:context()) -> 'ok'.
registration_update(Context) ->
    crossbar_util:flush_registrations(
      crossbar_util:get_account_realm(Context)
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec  track_assignment(atom(), cb_context:context()) -> cb_context:context().
track_assignment('post', Context) ->
    OldNums = get_numbers(cb_context:fetch(Context, 'db_doc')),
    NewNums = get_numbers(cb_context:doc(Context)),
    Assigned = [{Num, <<"trunkstore">>} || Num <- NewNums, not (lists:member(Num, OldNums))],
    Unassigned = [{Num, <<>>} || Num <- OldNums, not (lists:member(Num, NewNums))],
    lager:debug("assign ~p, unassign ~p", [Assigned, Unassigned]),
    wh_number_manager:track_assignment(cb_context:account_id(Context), Assigned ++ Unassigned);
track_assignment('delete', Context) ->
    Nums = get_numbers(cb_context:doc(Context)),
    Unassigned = [{Num, <<>>} || Num <- Nums],
    lager:debug("unassign ~p", [Unassigned]),
    wh_number_manager:track_assignment(cb_context:account_id(Context), Unassigned).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec  get_numbers(wh_json:object()) -> ne_binaries().
get_numbers(JObj) ->
    Servers = wh_json:get_value(<<"servers">>, JObj, []),
    lists:foldl(fun get_numbers_fold/2, [], Servers).

-spec get_numbers_fold(wh_json:object(), ne_binaries()) -> ne_binaries().
get_numbers_fold(Server, Acc) ->
    wh_json:get_keys(wh_json:get_value(<<"DIDs">>, Server)) ++ Acc.

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
    crossbar_doc:load(Id, Context).

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
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, wh_json:set_value(<<"pvt_type">>, <<"sys_info">>, cb_context:doc(Context)));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [{'reduce', 'false'}], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"id">>, JObj)|Acc].
