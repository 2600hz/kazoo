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

-export([create_user/1]).
-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/3
         ,authenticate/1
         ,authorize/1
         ,validate/1, validate/2, validate/4
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).

-define(CB_LIST, <<"users/crossbar_listing">>).
-define(LIST_BY_USERNAME, <<"users/list_by_username">>).

%%%===================================================================
%%% API
%%%===================================================================

%% SUPPORT FOR THE DEPRECIATED CB_SIGNUPS...
create_user(Context) ->
    case validate_request('undefined', Context#cb_context{req_verb = ?HTTP_PUT}) of
        #cb_context{resp_status='success'}=C1 -> ?MODULE:put(C1);
        Else -> Else
    end.

init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.users">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.users">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.users">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.users">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.users">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.users">>, ?MODULE, 'delete').

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
    CurrentUsername = case cb_context:fetch('db_doc', Context) of
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
                                                   ,Context),
            manditory_rehash_creds(UserId, NewUsername, C)
    end.

maybe_rehash_creds(UserId, Username, #cb_context{doc=JObj}=Context) ->
    case wh_json:get_ne_value(<<"password">>, JObj) of
        %% No username or hash, no creds for you!
        'undefined' when Username =:= 'undefined' -> 
            HashKeys = [<<"pvt_md5_auth">>, <<"pvt_sha1_auth">>],
            Context#cb_context{doc=wh_json:delete_keys(HashKeys, JObj)};
        %% Username without password, creds status quo
        'undefined' -> Context;
        %% Got a password, hope you also have a username...
        Password -> rehash_creds(UserId, Username, Password, Context)
    end.

manditory_rehash_creds(UserId, Username, #cb_context{doc=JObj}=Context) ->
    case wh_json:get_ne_value(<<"password">>, JObj) of
        'undefined' -> 
            cb_context:add_validation_error(<<"password">>
                                            ,<<"required">>
                                            ,<<"The password must be provided when updating the username">>
                                            ,Context);
        Password -> rehash_creds(UserId, Username, Password, Context)
    end.

rehash_creds(_, 'undefined', _, Context) ->
    cb_context:add_validation_error(<<"username">>
                                    ,<<"required">>
                                    ,<<"The username must be provided when updating the password">>
                                    ,Context);
rehash_creds(_, Username, Password, #cb_context{doc=JObj}=Context) ->
    lager:debug("password set on doc, updating hashes for ~s", [Username]),
    {MD5, SHA1} = cb_modules_util:pass_hashes(Username, Password),
    JObj1 = wh_json:set_values([{<<"pvt_md5_auth">>, MD5}
                                ,{<<"pvt_sha1_auth">>, SHA1}
                               ], JObj),
    Context#cb_context{doc=wh_json:delete_key(<<"password">>, JObj1)}. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
maybe_validate_quickcall(#cb_context{resp_status='success'
                                     ,doc=JObj
                                     ,auth_token=AuthToken
                                    }=Context) ->
    case (not wh_util:is_empty(AuthToken))
          orelse wh_json:is_true(<<"allow_anoymous_quickcalls">>, JObj) 
    of
        'false' -> cb_context:add_system_error('invalid_credentials', Context);
        'true' -> Context
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the username in the request is
%% unique or belongs to the request being made
%% @end
%%--------------------------------------------------------------------
-spec username_doc_id(api_binary(), cb_context:context()) -> api_binary().
username_doc_id(_, #cb_context{db_name='undefined'}) ->
    'undefined';
username_doc_id(Username, Context) ->
    Username = wh_util:to_lower_binary(Username),
    JObj = case crossbar_doc:load_view(?LIST_BY_USERNAME, [{<<"key">>, Username}], Context) of
               #cb_context{resp_status='success', doc=[J]} -> J;
               _ -> wh_json:new()
           end,
    wh_json:get_value(<<"id">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec(normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects()).
normalize_view_results(JObj, Acc) -> [wh_json:get_value(<<"value">>, JObj)|Acc].
