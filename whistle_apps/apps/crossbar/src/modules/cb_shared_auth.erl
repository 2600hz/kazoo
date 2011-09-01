%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Shared token auth module, this module validates the token
%%% against a trusted central token server.  If the token
%%% is valid then it will create a local token.  It will
%%% also import the account/user from the central server.
%%%
%%% Used to propagate accounts amongst independent services.
%%%
%%% This is a non-standard module:
%%% * it authenticates and authorizes itself
%%% * it has a completely unique role
%%% * it operates without an account id (or account db)
%%% * it 'proxies' crossbar auth requests to an external URL
%%%
%%% @end
%%% Created : 15 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_shared_auth).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(SERVER, ?MODULE).

-define(TOKEN_DB, <<"token_auth">>).

-define(SHARED_AUTH_CONF, [code:lib_dir(crossbar, priv), "/shared_auth/shared_auth.conf"]).

-record(state, {xbar_url=undefined :: undefined | string()}).

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

reload() ->
    gen_server:cast(?SERVER, {reload}).

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
    {ok, #state{}, 0}.

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
handle_cast({reload}, _) ->
    {noreply, #state{}, 0};

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
                 ,{RD, #cb_context{req_nouns=[{<<"shared_auth">>,[]}]
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"shared_auth">>,[]}]
                                   ,req_verb = <<"put">>
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticating request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.shared_auth">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.shared_auth">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.shared_auth">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context, State),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.shared_auth">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = create_local_token(RD, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, CurState) ->
    bind_to_crossbar(),
    couch_mgr:db_create(?TOKEN_DB),
    State = case file:consult(?SHARED_AUTH_CONF) of
                {ok, Terms} ->
                    ?LOG_SYS("loaded config from ~s", [?SHARED_AUTH_CONF]),
                    #state{xbar_url=props:get_value(authoritative_crossbar, Terms, CurState#state.xbar_url)};
                {error, _} ->
                    ?LOG_SYS("could not read config from ~s", [?SHARED_AUTH_CONF]),
                    #state{}
            end,
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
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.shared_auth">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.shared_auth">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.shared_auth">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.shared_auth">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (Paths) -> tuple(boolean(), http_methods()) when
      Paths :: list().
allowed_methods([]) ->
    {true, ['PUT', 'GET']};
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
-spec resource_exists/1 :: (Paths) -> tuple(boolean(), []) when
      Paths :: list().
resource_exists([]) ->
    {true, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function runs for each side of the shared auth request
%%
%% The Requestor (PUT):
%%     IE: Create (PUT) a local auth token
%% This request bypasses authentication, test the 'shared_token' against our
%% authorative server.  Basicly preform a noraml 'get' to this module with the
%% shared token as the auth token.  If it succeeds we will send 'ourself' the
%% account id, otherwise the token was not known to the auth server.
%%
%% The Authority (GET):
%%     IE: Fetch (GET) the account and user of a shared token
%% If we are validating a 'get' request then we are the authoriative box.  This means
%% another box is using their 'shared_tokens' as our auth token and it validated.
%% So lets figure out what account they belong to and return the complete account
%% definition so they can import it.
%%
%% If the authoriate box is running with noauth[n|z] then just send back a 401 to the
%% requestor because we wont know what account to fetch
%%
%% Failure here returns 400 or 401
%% @end
%%--------------------------------------------------------------------
-spec validate/3 :: (Params, Context, State) -> #cb_context{} when
      Params :: list(),
      Context :: #cb_context{},
      State :: #state{}.
validate([], #cb_context{req_data=JObj, req_verb = <<"put">>}=Context, State) ->
    SharedToken = wh_json:get_value(<<"shared_token">>, JObj),
    case authenticate_shared_token(SharedToken, State) of
        {ok, Payload} ->
            ?LOG("authoritive shared auth request succeeded"),
            RemoteData = wh_json:get_value(<<"data">>, mochijson2:decode(Payload)),
            case import_missing_data(RemoteData) of
                true ->
                    Context#cb_context{resp_status=success, doc=RemoteData, auth_token=SharedToken};
                false ->
                    crossbar_util:response(error, <<"could not import remote account">>, 500, Context)
            end;
        {forbidden, _} ->
            ?LOG("authoritive shared auth request forbidden"),
            crossbar_util:response(error, <<"invalid shared token">>, 401, Context);
        {error, _}=E ->
            ?LOG("authoritive shared auth request error: ~p", [E]),
            crossbar_util:response(error, <<"could not validate shared token">>, 500, Context)
    end;
validate([], #cb_context{auth_doc=undefined, req_verb = <<"get">>}=Context, _) ->
    ?LOG("valid shared auth request received but there is no authorizing doc (noauth running?)"),
    crossbar_util:response(error, <<"authentication information is not avaliable">>, 401, Context);
validate([], #cb_context{auth_doc=JObj, req_verb = <<"get">>}=Context, _) ->
    ?LOG("valid shared auth request received, creating response"),
    AccountId = wh_json:get_value(<<"account_id">>, JObj),
    UserId = wh_json:get_value(<<"owner_id">>, JObj),
    Db = whapps_util:get_db_name(AccountId, encoded),
    case couch_mgr:open_doc(Db, AccountId) of
        {ok, Account} ->
            case couch_mgr:open_doc(Db, UserId) of
                {ok, User} ->
                    Context#cb_context{resp_status=success
                                       ,resp_data={struct, [{<<"account">>, Account}
                                                            ,{<<"user">>, User}]}};
                {error, _} ->
                    crossbar_util:response_db_fatal(Context)
            end;
        {error, _} ->
            crossbar_util:response_db_fatal(Context)
    end;
validate(_, Context, _) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_local_token/2 :: (RD, Context) -> #cb_context{} when
      RD :: #wm_reqdata{},
      Context :: #cb_context{}.
create_local_token(RD, #cb_context{doc=JObj, auth_token=SharedToken}=Context) ->
    AccountId = wh_json:get_value([<<"account">>, <<"_id">>], JObj, <<>>),
    OwnerId = wh_json:get_value([<<"user">>, <<"_id">>], JObj, <<>>),
    Token = {struct, [{<<"account_id">>, AccountId}
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
                      ,{<<"shared_token">>, SharedToken}
                     ]},
    case couch_mgr:save_doc(?TOKEN_DB, Token) of
        {ok, Doc} ->
            AuthToken = wh_json:get_value(<<"_id">>, Doc),
            ?LOG("created new local auth token ~s", [AuthToken]),
            crossbar_util:response({struct, [{<<"account_id">>, AccountId}
                                             ,{<<"owner_id">>, OwnerId}
                                            ]}
                                   ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
        {error, R} ->
            ?LOG("could not create new local auth token, ~p", [R]),
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a crossbar request to the authoriative server to authorize
%% the shared token and get the account/user for the token
%% @end
%%--------------------------------------------------------------------
-spec authenticate_shared_token/2 :: (SharedToken, State) -> tuple(ok, json_object()) | tuple(error, atom()) when
      SharedToken :: undefined | binary(),
      State :: #state{}.
authenticate_shared_token(undefined, _) ->
    {forbidden, missing_shared_token};
authenticate_shared_token(SharedToken, #state{xbar_url=XBarUrl}) ->
    Url = lists:flatten(XBarUrl, "/shared_auth"),
    Headers = [{"Accept", "application/json"}
	       ,{"X-Auth-Token", wh_util:to_list(SharedToken)}
	      ],
    ?LOG("validating shared token ~s via ~s", [SharedToken, Url]),
    case ibrowse:send_req(Url, Headers, get) of
	{ok, "200", _, Resp} ->
            {ok, Resp};
	{ok, "401", _, _} ->
            {forbidden, shared_token_rejected};
	Resp ->
            {error, Resp}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a remote host authenticates the shared token it will return
%% an account and user, ensure those exist locally.
%% @end
%%--------------------------------------------------------------------
-spec import_missing_data/1 :: (RemoteData) -> boolean() when
      RemoteData :: json_object().
import_missing_data(RemoteData) ->
    Account = wh_json:get_value(<<"account">>, RemoteData),
    AccountId = wh_json:get_value(<<"pvt_account_id">>, Account),
    User = wh_json:get_value(<<"user">>, RemoteData),
    UserId = wh_json:get_value(<<"_id">>, User),
    import_missing_account(AccountId, Account) andalso
        import_missing_user(AccountId, UserId, User).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a remote host authenticates the shared token it will return
%% an account and user, ensure the account exists (creating if not)
%% @end
%%--------------------------------------------------------------------
-spec import_missing_account/2 :: (AccountId, Account) -> boolean() when
      AccountId :: undefined | binary(),
      Account :: undefined | json_object().
import_missing_account(undefined, _) ->
    false;
import_missing_account(_, undefined) ->
    false;
import_missing_account(AccountId, Account) ->
    Db = whapps_util:get_db_name(AccountId, encoded),
    case couch_mgr:db_exists(Db) of
        true ->
            ?LOG("remote account ~s alread exists locally", [AccountId]),
            true;
        false ->
            Event = <<"v1_resource.execute.put.accounts">>,
            Doc = wh_json:delete_key(<<"_rev">>, Account),
            Payload = [undefined, #cb_context{doc=Doc}, [[]]],
            case crossbar_bindings:fold(Event, Payload) of
                [_, #cb_context{resp_status=success} | _] ->
                    ?LOG("imported account ~s", [AccountId]),
                    true;
                _ ->
                    ?LOG("could not import account ~s", [AccountId]),
                    false
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a remote host authenticates the shared token it will return
%% an account and user, ensure the user exists locally (creating if not)
%% @end
%%--------------------------------------------------------------------
-spec import_missing_user/3 :: (AccountId, UserId, User) -> boolean() when
      AccountId :: undefined | binary(),
      UserId :: undefined | binary(),
      User :: undefined | json_object().
import_missing_user(_, undefined, _) ->
    false;
import_missing_user(_, _, undefined) ->
    false;
import_missing_user(AccountId, UserId, User) ->
    Db = whapps_util:get_db_name(AccountId, encoded),
    case couch_mgr:lookup_doc_rev(Db, UserId) of
        {ok, _} ->
            ?LOG("remote user ~s already exists locally in account ~s", [UserId, AccountId]),
            true;
        _Else ->
            Event = <<"v1_resource.execute.put.users">>,
            Doc = wh_json:delete_key(<<"_rev">>, User),
            Payload = [undefined, #cb_context{doc=Doc, db_name=Db}, [[]]],
            case crossbar_bindings:fold(Event, Payload) of
                [_, #cb_context{resp_status=success} | _] ->
                    ?LOG("imported user ~s in account ~s", [UserId, AccountId]),
                    true;
                _ ->
                    ?LOG("could not import user ~s in account ~s", [UserId, AccountId]),
                    false
            end
    end.
