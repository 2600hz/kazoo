%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Users module
%%%
%%% Handle client requests for user documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_users).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
         ,create_user/1
        ]).

-include("include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(CB_LIST, <<"users/crossbar_listing">>).
-define(LIST_BY_USERNAME, <<"users/list_by_username">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.users">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.users">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.users">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.users">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.users">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.users">>, ?MODULE, delete).

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
-spec allowed_methods/1 :: (path_tokens()) -> http_methods().
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
-spec resource_exists/1 :: (path_tokens()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

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
validate(#cb_context{req_data=ReqData}=Context) ->
    ReqData1 = wh_json:set_value(<<"username">>
                                     ,wh_util:to_lower_binary(wh_json:get_value(<<"username">>, ReqData))
                                 ,ReqData),
    validate_req(Context#cb_context{req_data=ReqData1}).
validate(#cb_context{req_data=ReqData}=Context, Id) ->
    ReqData1 = wh_json:set_value(<<"username">>
                                     ,wh_util:to_lower_binary(wh_json:get_value(<<"username">>, ReqData))
                                 , ReqData),
    validate_req(Context#cb_context{req_data=ReqData1}, Id).

-spec validate_req/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate_req/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate_req(#cb_context{req_verb = <<"get">>}=Context) ->
    load_user_summary(Context);
validate_req(#cb_context{req_verb = <<"put">>}=Context) ->
    create_user(Context).

validate_req(#cb_context{req_verb = <<"get">>}=Context, UserId) ->
    load_user(UserId, Context);
validate_req(#cb_context{req_verb = <<"post">>}=Context, UserId) ->
    update_user(UserId, Context);
validate_req(#cb_context{req_verb = <<"delete">>}=Context, UserId) ->
    load_user(UserId, Context);
validate_req(Context, _UserId) ->
    crossbar_util:response_faulty_request(Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    crossbar_doc:save(hash_password(Context)).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(Context) ->
    crossbar_doc:save(hash_password(Context)).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_user_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_user_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new user document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_user/1 :: (#cb_context{}) -> #cb_context{}.
create_user(#cb_context{req_data=Data}=Context) ->
    UniqueUsername = is_unique_username(undefined, Context),
    case wh_json_validator:is_valid(Data, <<"users">>) of
        {fail, Errors} when UniqueUsername ->
            crossbar_util:response_invalid_data(Errors, Context);
        {fail, Errors} ->
            E = wh_json:set_value([<<"username">>, <<"unique">>], <<"Username is not unique for this account">>, Errors),
            crossbar_util:response_invalid_data(E, Context);
        {pass, _} when not UniqueUsername ->
            E = wh_json:set_value([<<"username">>, <<"unique">>], <<"Username is not unique for this account">>, wh_json:new()),
            crossbar_util:response_invalid_data(E, Context);
        {pass, JObj} ->
            Context#cb_context{
              doc=wh_json:set_value(<<"pvt_type">>, <<"user">>, JObj)
              ,resp_status=success
             }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a user document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_user/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_user(UserId, Context) ->
    crossbar_doc:load(UserId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing user document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_user/2 :: (binary(), #cb_context{}) -> #cb_context{}.
update_user(UserId, #cb_context{req_data=Data}=Context) ->
    UniqueUsername = is_unique_username(UserId, Context),
    case wh_json_validator:is_valid(Data, <<"users">>) of
        {fail, Errors} when UniqueUsername ->
            crossbar_util:response_invalid_data(Errors, Context);
        {fail, Errors} ->
            E = wh_json:set_value([<<"username">>, <<"unique">>], <<"Username is not unique for this account">>, Errors),
            crossbar_util:response_invalid_data(E, Context);
        {pass, _} when not UniqueUsername ->
            E = wh_json:set_value([<<"username">>, <<"unique">>], <<"Username is not unique for this account">>, wh_json:new()),
            crossbar_util:response_invalid_data(E, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(UserId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results/2 :: (Doc :: wh_json:json_object(), Acc :: wh_json:json_objects()) -> wh_json:json_objects()).
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the password parameter is present
%% and if so create the hashes then remove it.
%% @end
%%--------------------------------------------------------------------
-spec hash_password/1 :: (#cb_context{}) -> #cb_context{}.
hash_password(#cb_context{doc=JObj}=Context) ->
    Username = wh_json:get_value(<<"username">>, JObj),
    case wh_json:get_value(<<"password">>, JObj) of
        undefined ->
            Context;
        Password ->
            {MD5, SHA1} = cb_modules_util:pass_hashes(Username, Password),

            JObj1 = wh_json:set_values([{<<"pvt_md5_auth">>, MD5}
                                        ,{<<"pvt_sha1_auth">>, SHA1}
                                       ], JObj),
            Context#cb_context{doc=wh_json:delete_key(<<"password">>, JObj1)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the username in the request is
%% unique or belongs to the request being made
%% @end
%%--------------------------------------------------------------------
-spec is_unique_username/2 :: (ne_binary() | 'undefined', #cb_context{}) -> boolean().
is_unique_username(UserId, #cb_context{req_data=ReqData}=Context) ->
    Username = wh_util:to_lower_binary(wh_json:get_value(<<"username">>, ReqData)),
    JObj = case crossbar_doc:load_view(?LIST_BY_USERNAME, [{<<"key">>, Username}], Context) of
               #cb_context{resp_status=success, doc=[J]} -> J;
               _ -> wh_json:new()
           end,
    case wh_json:get_value(<<"id">>, JObj) of
        undefined ->true;
        UserId -> true;
        _Else -> false
    end.
