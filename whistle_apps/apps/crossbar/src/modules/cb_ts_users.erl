%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for ts_user documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_ts_users).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("include/crossbar.hrl").

-define(VIEW_FILE, <<"views/ts_users.json">>).
-define(CB_LIST, <<"ts_users/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.ts_users">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.ts_users">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.ts_users">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.ts_users">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.ts_users">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.ts_users">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists() ->
    true.
resource_exists(_) ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    read_ts_user_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_ts_user(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, TSUserId) ->
    read_ts_user(TSUserId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, TSUserId) ->
    update_ts_user(TSUserId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, TSUserId) ->
    read_ts_user(TSUserId, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(Context) ->
    crossbar_doc:save(Context).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new ts_user document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_ts_user/1 :: (#cb_context{}) -> #cb_context{}.
create_ts_user(#cb_context{req_data=JObj}=Context) ->
    JObj1 = wh_json:set_value(<<"_id">>, list_to_binary([<<"user_">>, wh_json:get_binary_value(<<"userID">>, JObj,<<>>)]), JObj),
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, <<"ts_user">>, JObj1)
                       ,resp_status=success
                      }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a ts_user document from the database
%% @end
%%--------------------------------------------------------------------
-spec read_ts_user/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read_ts_user(TSUserId, Context) ->
    case crossbar_doc:load(<<"user_", TSUserId/binary>>, Context) of
        #cb_context{resp_status=success, resp_data=Data1}=Context1 ->
            Id = case wh_json:get_value(<<"id">>, Data1) of
                     <<"user_", Name/binary>> -> Name;
                     Else -> Else
                 end,
            Context1#cb_context{resp_data=wh_json:set_value(<<"id">>, Id, Data1)};
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing ts_user document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_ts_user/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_ts_user(TSUserId, #cb_context{req_data=JObj}=Context) ->
    crossbar_doc:load_merge(<<"user_", TSUserId/binary>>, JObj, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec read_ts_user_summary/1 :: (#cb_context{}) -> #cb_context{}.
read_ts_user_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
