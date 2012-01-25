%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% User auth module
%%%
%%%
%%% @end
%%% Created : 15 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_user_auth).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(ACCT_MD5_LIST, <<"users/creds_by_md5">>).
-define(ACCT_SHA1_LIST, <<"users/creds_by_sha">>).
-define(USERNAME_LIST, <<"users/list_by_username">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, ok, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({binding_fired, Pid, <<"v1_resource.authorize">>
                 ,{RD, #cb_context{req_nouns=[{<<"user_auth">>, _}]
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"user_auth">>, _}]}=Context}}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  ?LOG("authenticating request"),
                  Pid ! {binding_result, true, {RD, Context}}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.user_auth">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.user_auth">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.user_auth">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
         end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.user_auth">>, [RD, Context | [<<"recovery">>]]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = reset_users_password(Context),
                  Pid ! {binding_result, true, [RD, Context1, [<<"recovery">>]]}
         end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.user_auth">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = create_token(RD, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
         end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    couch_mgr:db_create(?TOKEN_DB),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec bind_to_crossbar/0 :: () -> no_return().
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.user_auth">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.user_auth">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.user_auth">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.user_auth">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (path_tokens()) -> {boolean(), http_methods()}.
allowed_methods([]) ->
    {true, ['PUT']};
allowed_methods([<<"recovery">>]) ->
    {true, ['PUT']};
allowed_methods(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/1 :: (path_tokens()) -> {boolean(), []}.
resource_exists([]) ->
    {true, []};
resource_exists([_]) ->
    {true, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/2 :: (list(), #cb_context{}) -> #cb_context{}.
validate([], #cb_context{req_data=Data, req_verb = <<"put">>}=Context) ->
    case wh_json_validator:is_valid(Data, <<"user_auth">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Credentials = wh_json:get_value(<<"credentials">>, JObj),
            Method = wh_json:get_value(<<"method">>, JObj, <<"md5">>),
            AccountName = normalize_account_name(wh_json:get_value(<<"account_name">>, JObj)),
            PhoneNumber = wh_json:get_ne_value(<<"phone_number">>, JObj),
            AccountRealm = wh_json:get_value(<<"account_realm">>, JObj,
                                             wh_json:get_value(<<"realm">>, JObj)),
            case crossbar_util:find_account_db(PhoneNumber, AccountRealm, AccountName) of
                {error, Errors} -> crossbar_util:response_invalid_data(Errors, Context);
                {ok, AccountDb} ->
                    authorize_user(Context, Credentials, Method, AccountDb);
                {multiples, AccountDbs} ->
                    authorize_user(Context, Credentials, Method, AccountDbs)
            end
    end;
validate([<<"recovery">>], #cb_context{req_data=Data, req_verb = <<"put">>}=Context) ->
    case wh_json_validator:is_valid(Data, <<"user_auth_recovery">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            AccountName = normalize_account_name(wh_json:get_value(<<"account_name">>, JObj)),
            PhoneNumber = wh_json:get_ne_value(<<"phone_number">>, JObj),
            AccountRealm = wh_json:get_value(<<"account_realm">>, JObj
                                             ,wh_json:get_value(<<"realm">>, JObj)),
            case crossbar_util:find_account_db(PhoneNumber, AccountRealm, AccountName, false) of
                {error, Errors} -> crossbar_util:response_invalid_data(Errors, Context);
                {ok, AccountDb} ->
                    ?LOG("attempting to load username in db: ~s", [AccountDb]),
                    Username = wh_json:get_value(<<"username">>, JObj),
                    case couch_mgr:get_results(AccountDb, ?USERNAME_LIST, [{<<"key">>, Username}, {<<"include_docs">>, true}]) of
                        {ok, [User]} -> 
                            case wh_json:is_false([<<"doc">>, <<"enabled">>], JObj) of
                                false ->
                                    ?LOG("the username '~s' was found and is not disabled, continue", [Username]),
                                    Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"doc">>, User), db_name=AccountDb};
                                true ->
                                    ?LOG("the username '~s' was found but is disabled", [Username]),
                                    Error = wh_json:set_value([<<"username">>, <<"disabled">>]
                                                              ,<<"The user is disabled">>
                                                              ,wh_json:new()),
                                    crossbar_util:response_invalid_data(Error, Context)
                            end;                    
                        _ ->
                            Error = wh_json:set_value([<<"username">>, <<"not_found">>]
                                                      ,<<"The provided user name was not found">>
                                                          ,wh_json:new()),
                            crossbar_util:response_invalid_data(Error, Context)
                    end
            end
    end;
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalize the account name by converting the name to lower case
%% and then removing all non-alphanumeric characters.
%%
%% This can possibly return an empty binary.
%% @end
%%--------------------------------------------------------------------
-spec normalize_account_name/1 :: (undefined | ne_binary()) -> binary().
normalize_account_name(undefined) ->
    undefined;
normalize_account_name(AccountName) ->
    << <<Char>> || <<Char>> <= wh_util:to_lower_binary(AccountName)
                   ,(Char >= $a andalso Char =< $z) orelse (Char >= $0 andalso Char =< $9) >>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the credentials are valid based on the
%% provided hash method
%%
%% Attempt to lookup and compare the user creds in the provided accounts.
%%
%% Failure here returns 401
%% @end
%%--------------------------------------------------------------------
-spec authorize_user/4 :: (#cb_context{}, ne_binary(), ne_binary(), ne_binary() | [] | [ne_binary(),...] ) -> #cb_context{}.
authorize_user(Context, _, _, []) ->
    ?LOG("no account(s) specified"),
    crossbar_util:response(error, <<"invalid credentials">>, 401, Context);
authorize_user(Context, Credentials, Method, [AccountDb|AccountDbs]) ->
    case authorize_user(Context, Credentials, Method, AccountDb) of
        #cb_context{resp_status=success}=Context1 ->
            Context1;
        _ ->
            authorize_user(Context, Credentials, Method, AccountDbs)
    end;
authorize_user(Context, Credentials, <<"md5">>, AccountDb) ->
    case crossbar_doc:load_view(?ACCT_MD5_LIST, [{<<"key">>, Credentials}], Context#cb_context{db_name=AccountDb}) of
        #cb_context{resp_status=success, doc=[JObj|_]} ->
            ?LOG("found more that one user with MD5 ~s, using ~s", [Credentials, wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        #cb_context{resp_status=success, doc=JObj} when JObj =/= []->
            ?LOG("found MD5 credentials belong to user ~s", [wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        _ ->
            ?LOG("credentials do not belong to any user"),
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
    end;
authorize_user(Context, Credentials, <<"sha">>, AccountDb) ->
    case crossbar_doc:load_view(?ACCT_SHA1_LIST, [{<<"key">>, Credentials}], Context#cb_context{db_name=AccountDb}) of
        #cb_context{resp_status=success, doc=[JObj|_]} ->
            ?LOG("found more that one user with SHA1 ~s, using ~s", [Credentials, wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        #cb_context{resp_status=success, doc=JObj} when JObj =/= []->
            ?LOG("found SHA1 credentials belong to user ~s", [wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        _ ->
            ?LOG("credentials do not belong to any user"),
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
    end;
authorize_user(Context, _, _, _) ->
    crossbar_util:response(error, <<"invalid credentials">>, 401, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token/2 :: (#wm_reqdata{}, #cb_context{}) -> #cb_context{}.
create_token(RD, #cb_context{doc=JObj}=Context) ->
    case wh_json:is_empty(JObj) of
        true ->
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context);
        false ->
            AccountId = wh_json:get_value(<<"account_id">>, JObj, <<>>),
            OwnerId = wh_json:get_value(<<"owner_id">>, JObj, <<>>),
            Token = [{<<"account_id">>, AccountId}
                     ,{<<"owner_id">>, OwnerId}
                     ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                     ,{<<"method">>, wh_util:to_binary(?MODULE)}
                     ,{<<"peer">>, wh_util:to_binary(wrq:peer(RD))}
                     ,{<<"user_agent">>, wh_util:to_binary(wrq:get_req_header("User-Agent", RD))}
                     ,{<<"accept">>, wh_util:to_binary(wrq:get_req_header("Accept", RD))}
                     ,{<<"accept_charset">>, wh_util:to_binary(wrq:get_req_header("Accept-Charset", RD))}
                     ,{<<"accept_endocing">>, wh_util:to_binary(wrq:get_req_header("Accept-Encoding", RD))}
                     ,{<<"accept_language">>, wh_util:to_binary(wrq:get_req_header("Accept-Language", RD))}
                     ,{<<"connection">>, wh_util:to_binary(wrq:get_req_header("Conntection", RD))}
                     ,{<<"keep_alive">>, wh_util:to_binary(wrq:get_req_header("Keep-Alive", RD))}
                    ],
            case couch_mgr:save_doc(?TOKEN_DB, wh_json:from_list(Token)) of
                {ok, Doc} ->
                    AuthToken = wh_json:get_value(<<"_id">>, Doc),
                    ?LOG("created new local auth token ~s", [AuthToken]),
                    crossbar_util:response(wh_json:from_list([{<<"account_id">>, AccountId}
                                                              ,{<<"owner_id">>, OwnerId}])
                                           ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
                {error, R} ->
                    ?LOG("could not create new local auth token, ~p", [R]),
                    crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to generate random strings
%% @end
%%--------------------------------------------------------------------
-spec reset_users_password/1 :: (#cb_context{}) -> #cb_context{}.
reset_users_password(#cb_context{doc=JObj, req_data=Data}=Context) ->
    Password = rand_chars(16),
    {MD5, SHA1} = cb_modules_util:pass_hashes(wh_json:get_value(<<"username">>, JObj), Password),
    Email = wh_json:get_value(<<"email">>, JObj),
    Updaters = [fun(J) -> wh_json:set_value(<<"pvt_md5_auth">>, MD5, J) end
                ,fun(J) -> wh_json:set_value(<<"pvt_sha1_auth">>, SHA1, J) end
                ,fun(J) -> wh_json:set_value(<<"require_password_update">>, true, J) end
               ],
    case crossbar_doc:save(Context#cb_context{doc=lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters), req_verb = <<"post">>}) of
        #cb_context{resp_status=success} -> 
            Notify = [{<<"Email">>, Email}
                      ,{<<"First-Name">>, wh_json:get_value(<<"first_name">>, JObj)}
                      ,{<<"Last-Name">>, wh_json:get_value(<<"last_name">>, JObj)}
                      ,{<<"Password">>, Password}
                      ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
                      ,{<<"Account-DB">>, wh_json:get_value(<<"pvt_account_db">>, JObj)}
                      ,{<<"Request">>, wh_json:delete_key(<<"username">>, Data)}
                      | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
                     ],
            ok = wapi_notifications:publish_pwd_recovery(Notify),
            crossbar_util:response(<<"Password reset, email send to:", Email/binary>>, Context);
        Else -> 
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper function to generate random strings
%% @end
%%--------------------------------------------------------------------
-spec rand_chars/1 :: (pos_integer()) -> ne_binary().
rand_chars(Count) ->
    wh_util:to_binary(wh_util:to_hex(crypto:rand_bytes(Count))).
