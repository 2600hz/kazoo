%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
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
-module(cb_users_v1).

-export([create_user/1]).
-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,authenticate/1
         ,authorize/1
         ,validate/1, validate/2, validate/3, validate/4
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).
-define(CB_LIST, <<"users/crossbar_listing">>).
-define(LIST_BY_USERNAME, <<"users/list_by_username">>).
-define(CHANNELS, <<"channels">>).

%%%===================================================================
%%% API
%%%===================================================================

%% SUPPORT FOR THE DEPRECIATED CB_SIGNUPS...
-spec create_user(cb_context:context()) -> cb_context:context().
create_user(Context) ->
    case validate_request('undefined', Context#cb_context{req_verb = ?HTTP_PUT}) of
        #cb_context{resp_status='success'}=C1 -> ?MODULE:put(C1);
        Else -> Else
    end.

init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.users">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.users">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.users">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.users">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.users">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.users">>, ?MODULE, 'delete').

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
-spec allowed_methods(path_token(), path_token(), path_token()) -> http_methods().

allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_, ?CHANNELS) ->
    [?HTTP_GET].

allowed_methods(_, <<"quickcall">>, _) ->
    [?HTTP_GET].

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
-spec resource_exists(path_token(), path_token(), path_token()) -> 'true'.

resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, ?CHANNELS) -> 'true'.
resource_exists(_, <<"quickcall">>, _) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'true'.
authenticate(#cb_context{req_nouns=?USERS_QCALL_NOUNS, req_verb = ?HTTP_GET}) ->
    lager:debug("authenticating request"),
    'true'.

-spec authorize(cb_context:context()) -> 'true'.
authorize(#cb_context{req_nouns=?USERS_QCALL_NOUNS, req_verb = ?HTTP_GET}) ->
    lager:debug("authorizing request"),
    'true'.

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

validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_user_summary(Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    validate_request(undefined, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, UserId) ->
    load_user(UserId, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context, UserId) ->
    validate_request(UserId, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE}=Context, UserId) ->
    load_user(UserId, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, UserId, ?CHANNELS) ->
    Context1 = load_user(UserId, Context),
    get_channels(Context1).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, UserId, <<"quickcall">>, _) ->
    Context1 = maybe_validate_quickcall(load_user(UserId, Context)),
    case cb_context:has_errors(Context1) of
        'true' -> Context1;
        'false' ->
            cb_modules_util:maybe_originate_quickcall(Context1)
    end.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_channels(cb_context:context()) -> cb_context:context().
get_channels(#cb_context{doc=Doc, account_id=AccountId}=Context) ->
    Realm = crossbar_util:get_account_realm(AccountId),
    Username = wh_json:get_value(<<"username">>, Doc),
    Req = [{<<"Realm">>, Realm}
           ,{<<"Username">>, Username}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_call:publish_query_user_channels_req/1
                                       ,fun wapi_call:query_user_channels_resp_v/1
                                      )
    of
        {'ok', Resp} ->
            Channels = wh_json:get_value(<<"Channels">>, Resp, []),
            crossbar_util:response(Channels, Context);
        {'error', _E} ->
            lager:error("could not reach ecallmgr channels: ~p", [_E]),
            crossbar_util:response('error', <<"could not reach ecallmgr channels">>, Context)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_user_summary(cb_context:context()) -> cb_context:context().
load_user_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a user document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_user(api_binary(), cb_context:context()) -> cb_context:context().
load_user(UserId, Context) -> crossbar_doc:load(UserId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(UserId, Context) ->
    prepare_username(UserId, Context).

prepare_username(UserId, #cb_context{req_data=JObj}=Context) ->
    case wh_json:get_ne_value(<<"username">>, JObj) of
        'undefined' -> check_user_schema(UserId, Context);
        Username ->
            JObj1 = wh_json:set_value(<<"username">>, wh_util:to_lower_binary(Username), JObj),
            check_user_schema(UserId, Context#cb_context{req_data=JObj1})
    end.

check_user_schema(UserId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(UserId, C) end,
    cb_context:validate_request_data(<<"users">>, Context, OnSuccess).

on_successful_validation('undefined', #cb_context{doc=Doc}=Context) ->
    Props = [{<<"pvt_type">>, <<"user">>}],
    maybe_import_credintials('undefined', Context#cb_context{doc=wh_json:set_values(Props, Doc)});
on_successful_validation(UserId, #cb_context{}=Context) ->
    maybe_import_credintials(UserId, crossbar_doc:load_merge(UserId, Context)).

maybe_import_credintials(UserId, #cb_context{doc=JObj}=Context) ->
    case wh_json:get_ne_value(<<"credentials">>, JObj) of
        'undefined' -> maybe_validate_username(UserId, Context);
        Creds ->
            RemoveKeys = [<<"credentials">>, <<"pvt_sha1_auth">>],
            C = Context#cb_context{doc=wh_json:set_value(<<"pvt_md5_auth">>, Creds
                                                         ,wh_json:delete_keys(RemoveKeys, JObj)
                                                        )},
            maybe_validate_username(UserId, C)
    end.

maybe_validate_username(UserId, #cb_context{doc=JObj}=Context) ->
    NewUsername = wh_json:get_ne_value(<<"username">>, JObj),
    CurrentUsername = case cb_context:fetch(Context, 'db_doc') of
                          'undefined' -> NewUsername;
                          CurrentJObj ->
                              wh_json:get_ne_value(<<"username">>, CurrentJObj, NewUsername)
                      end,
    case wh_util:is_empty(NewUsername)
        orelse CurrentUsername =:= NewUsername
        orelse username_doc_id(NewUsername, Context)
    of
        %% username is unchanged
        'true' -> maybe_rehash_creds(UserId, NewUsername, Context);
        %% updated username that doesnt exist
        'undefined' ->
            manditory_rehash_creds(UserId, NewUsername, Context);
        %% updated username to existing, collect any further errors...
        _Else ->
            C = cb_context:add_validation_error(<<"username">>
                                                ,<<"unique">>
                                                ,<<"Username is not unique for this account">>
                                                ,Context
                                               ),
            manditory_rehash_creds(UserId, NewUsername, C)
    end.

maybe_rehash_creds(UserId, Username, Context) ->
    case wh_json:get_ne_value(<<"password">>, cb_context:doc(Context)) of
        %% No username or hash, no creds for you!
        'undefined' when Username =:= 'undefined' ->
            HashKeys = [<<"pvt_md5_auth">>, <<"pvt_sha1_auth">>],
            cb_context:set_doc(Context, wh_json:delete_keys(HashKeys, cb_context:doc(Context)));
        %% Username without password, creds status quo
        'undefined' -> Context;
        %% Got a password, hope you also have a username...
        Password -> rehash_creds(UserId, Username, Password, Context)
    end.

-spec manditory_rehash_creds(ne_binary(), ne_binary(), cb_context:context()) ->
                                    cb_context:context().
manditory_rehash_creds(UserId, Username, Context) ->
    case wh_json:get_ne_value(<<"password">>, cb_context:doc(Context)) of
        'undefined' ->
            cb_context:add_validation_error(<<"password">>
                                            ,<<"required">>
                                            ,<<"The password must be provided when updating the username">>
                                            ,Context
                                           );
        Password -> rehash_creds(UserId, Username, Password, Context)
    end.

-spec rehash_creds(_, api_binary(), ne_binary(), cb_context:context()) ->
                          cb_context:context().
rehash_creds(_, 'undefined', _, Context) ->
    cb_context:add_validation_error(<<"username">>
                                    ,<<"required">>
                                    ,<<"The username must be provided when updating the password">>
                                    ,Context
                                   );
rehash_creds(_, Username, Password, Context) ->
    lager:debug("password set on doc, updating hashes for ~s", [Username]),
    {MD5, SHA1} = cb_modules_util:pass_hashes(Username, Password),
    JObj1 = wh_json:set_values([{<<"pvt_md5_auth">>, MD5}
                                ,{<<"pvt_sha1_auth">>, SHA1}
                               ], cb_context:doc(Context)),
    cb_context:set_doc(Context, wh_json:delete_key(<<"password">>, JObj1)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_validate_quickcall(cb_context:context()) -> cb_context:context().
-spec maybe_validate_quickcall(cb_context:context(), crossbar_status()) -> cb_context:context().
maybe_validate_quickcall(Context) ->
    maybe_validate_quickcall(Context, cb_context:resp_status(Context)).

maybe_validate_quickcall(Context, 'success') ->
    case (not wh_util:is_empty(db_context:auth_token(Context)))
        orelse wh_json:is_true(<<"allow_anoymous_quickcalls">>, cb_context:doc(Context))
    of
        'false' -> cb_context:add_system_error('invalid_credentials', Context);
        'true' -> Context
    end;
maybe_validate_quickcall(Context, _) ->
    cb_context:add_system_error('invalid_credentials', Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the username in the request is
%% unique or belongs to the request being made
%% @end
%%--------------------------------------------------------------------
-spec username_doc_id(api_binary(), cb_context:context()) -> api_binary().
username_doc_id(Username, Context) ->
    username_doc_id(Username, Context, cb_context:account_db(Context)).
username_doc_id(_, _, 'undefined') ->
    'undefined';
username_doc_id(Username, Context, _AccountDb) ->
    Username = wh_util:to_lower_binary(Username),
    Context1 = crossbar_doc:load_view(?LIST_BY_USERNAME, [{'key', Username}], Context),
    case cb_context:resp_status(Context1) =:= 'success'
        andalso cb_context:doc(Context1)
    of
        [JObj] -> wh_json:get_value(<<"id">>, JObj);
        _ -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects()).
normalize_view_results(JObj, Acc) -> [wh_json:get_value(<<"value">>, JObj)|Acc].
