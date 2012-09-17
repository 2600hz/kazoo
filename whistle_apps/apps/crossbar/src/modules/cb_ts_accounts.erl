%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for ts_account documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_ts_accounts).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,authorize/1
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("include/crossbar.hrl").

-define(VIEW_FILE, <<"views/ts_accounts.json">>).
-define(CB_LIST, <<"ts_accounts/crossbar_listing">>).
-define(PVT_TYPE, <<"sip_service">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.ts_accounts">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.ts_accounts">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.ts_accounts">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.ts_accounts">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.ts_accounts">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.ts_accounts">>, ?MODULE, delete).

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
    ['GET', 'POST', 'DELETE', 'HEAD'].

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
%% @end
%%--------------------------------------------------------------------
-spec authorize/1 :: (#cb_context{}) -> boolean().
authorize(#cb_context{auth_doc=AuthDoc, req_nouns=Nouns, req_verb=Verb}) ->
    AccountId = wh_json:get_value(<<"account_id">>, AuthDoc, <<"0000000000">>),

    _ = case props:get_value(<<"ts_accounts">>, Nouns) of
            [] when Verb =:= <<"put">> ->
                lager:debug("authorizing request to create a new trunkstore doc"),
                true;
            [AccountId] ->
                lager:debug("authorizing request to trunkstore doc ~s", [AccountId]),
                true;
            _Args ->
                false
        end.

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
    read_ts_account_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_ts_account(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, TSAccountId) ->
    read_ts_account(TSAccountId, Context#cb_context{account_id=TSAccountId});
validate(#cb_context{req_verb = <<"put">>}=Context, TSAccountId) ->
    check_ts_account(TSAccountId, Context#cb_context{account_id=TSAccountId});
validate(#cb_context{req_verb = <<"post">>}=Context, TSAccountId) ->
    update_ts_account(TSAccountId, Context#cb_context{account_id=TSAccountId});
validate(#cb_context{req_verb = <<"delete">>}=Context, TSAccountId) ->
    read_ts_account(TSAccountId, Context#cb_context{account_id=TSAccountId});
validate(#cb_context{req_verb = <<"head">>}=Context, TSAccountId) ->
    check_ts_account(TSAccountId, Context#cb_context{account_id=TSAccountId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    _ = cb_context:put_reqid(Context),

    #cb_context{doc=Doc} = Context1 = crossbar_doc:save(Context),
    timer:sleep(1000),
    try stepswitch_maintenance:reconcile(wh_json:get_value(<<"_id">>, Doc), true) catch _:_ -> ok end,
    Context1.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(Context) ->
    _ = cb_context:put_reqid(Context),
    #cb_context{doc=Doc} = Context1 = crossbar_doc:save(Context),
    timer:sleep(1000),
    try stepswitch_maintenance:reconcile(wh_json:get_value(<<"_id">>, Doc), true) catch _:_ -> ok end,
    Context1.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _) ->
    _ = cb_context:put_reqid(Context),
    #cb_context{doc=Doc} = Context1 = crossbar_doc:delete(Context),

    %% TODO: THIS IS VERY WRONG! Ties a local crossbar to a LOCAL stepswitch instance... quick and
    %% dirty were the instructions for this module but someone PLEASE fix this later!
    timer:sleep(1000),
    try stepswitch_maintenance:reconcile(wh_json:get_value(<<"_id">>, Doc), true) catch _:_ -> ok end,
    Context1.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new ts_account document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_ts_account/1 :: (#cb_context{}) -> #cb_context{}.
create_ts_account(#cb_context{req_data=Data, account_id=AccountId}=Context) ->
    case wh_json_validator:is_valid(Data, <<"ts_accounts">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Updaters = [fun(J) -> wh_json:set_value(<<"type">>, <<"sys_info">>, J) end
                        ,fun(J) ->
                                 Id = case wh_util:is_empty(AccountId) of
                                          false ->
                                              AccountId;
                                          true ->
                                              AuthRealm = wh_json:get_value([<<"account">>, <<"auth_realm">>], JObj),
                                              {ok, RealmAccountDb} = whapps_util:get_account_by_realm(AuthRealm),
                                              wh_util:format_account_id(RealmAccountDb, raw)
                                      end,
                                 true = is_binary(Id),
                                 wh_json:set_value(<<"_id">>, Id, J)
                         end
                        ,fun(J) -> wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, J) end
                       ],
            Context#cb_context{doc=lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters)
                               ,resp_status=success
                              }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a ts_account document from the database
%% @end
%%--------------------------------------------------------------------
-spec read_ts_account/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read_ts_account(TSAccountId, Context) ->
    crossbar_doc:load(TSAccountId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing ts_account document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_ts_account/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_ts_account(TSAccountId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"ts_accounts">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(TSAccountId, wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj), Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec read_ts_account_summary/1 :: (#cb_context{}) -> #cb_context{}.
read_ts_account_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to determine if the account exists in as light weight as
%% possible, altho just getting here...
%% @end
%%--------------------------------------------------------------------
-spec check_ts_account/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
check_ts_account(TSAccountId, #cb_context{db_name=Db}=Context) ->
    case couch_mgr:lookup_doc_rev(Db, TSAccountId) of
        {ok, Rev} ->
            Context#cb_context{resp_status=success
                               ,resp_data=[]
                               ,resp_etag=wh_util:to_list(Rev)};
        {error, _} ->
            crossbar_util:response_bad_identifier(TSAccountId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
