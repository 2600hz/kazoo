%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Signup module
%%%
%%% Handle client requests for new account on-boarding.  This is a
%%% special, one-off module because:
%%%
%%% * it authenticates and authorizes itself
%%% * it has a completely unique role
%%% * it operates without an account id (or account db)
%%%
%%% @end
%%% Created : 22 Apr 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_signup).

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

-define(SIGNUP_DB, <<"signups">>).

-define(VIEW_FILE, <<"views/signup.json">>).
-define(VIEW_ACTIVATION_KEYS, <<"signups/listing_by_key">>).
-define(VIEW_ACTIVATION_REALM, <<"signups/listing_by_realm">>).
-define(VIEW_ACTIVATION_CREATED, <<"signups/listing_by_created">>).

-define(SIGNUP_CONF, [code:lib_dir(crossbar, priv), "/signup/signup.conf"]).

-record(state, {cleanup_interval = 18000 :: integer() %% once every 5 hours (in seconds)
                ,signup_lifespan = ?SECONDS_IN_DAY :: integer() %% 24 hours (in seconds)
                ,register_cmd = undefined :: undefined | atom()
                ,activation_email_plain = undefined
                ,activation_email_html = undefined
                ,activation_email_from = undefined
                ,activation_email_subject = undefined
                ,cleanup_timer
               }).

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
init(_) ->
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
handle_cast({reload}, State) ->
    {noreply, State, 0};

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
                 ,{RD, #cb_context{req_nouns=[{<<"signup">>,[]}]
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authorize">>
                 ,{RD, #cb_context{req_nouns=[{<<"signup">>,[_]}]
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"signup">>,[]}]
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticating request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"signup">>,[_]}]
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticating request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.signup">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.signup">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.signup">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context#cb_context{db_name=?SIGNUP_DB}),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.signup">>, [RD, #cb_context{doc=JObj}=Context | [_]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  case activate_signup(JObj) of
                      {ok, Account, User} ->
                          delete_signup(JObj),
                          Context1 = Context#cb_context{resp_status=success
                                                       ,resp_data={struct, [{<<"account">>, Account}
                                                                            ,{<<"user">>, User}]}},
                          Pid ! {binding_result, true, [RD, Context1, Params]};
                      _Else ->
                          Context1 = crossbar_util:response_db_fatal(Context),
                          Pid ! {binding_result, true, [RD, Context1, Params]}
                  end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.signup">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  case crossbar_doc:save(Context#cb_context{db_name=?SIGNUP_DB}) of
                      #cb_context{resp_status=success}=Context1 ->
                          Pid ! {binding_result, true, [RD, Context1#cb_context{resp_data=[]}, Params]},
                          send_activation_email(RD, Context1, State),
                          exec_register_command(RD, Context1, State);
                      _ ->
                          Context1 = crossbar_util:response_db_fatal(Context),
                          Pid ! {binding_result, true, [RD, Context1, Params]}
                  end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(cleanup, State) ->
    cleanup_signups(State),
    {noreply, State};

handle_info(timeout, S) ->
    {ok, State} = code_change(0, S, []),
    {ok, TRef} = timer:send_interval(State#state.cleanup_interval * 1000, cleanup),
    {noreply, State#state{cleanup_timer=TRef}};

handle_info(_Info, State) ->
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
code_change(_OldVsn, #state{cleanup_timer=CurTRef}, _Extra) ->
    timer:cancel(CurTRef),
    bind_to_crossbar(),
    couch_mgr:db_create(?SIGNUP_DB),
    case couch_mgr:update_doc_from_file(?SIGNUP_DB, crossbar, ?VIEW_FILE) of
        {error, _} ->
            couch_mgr:load_doc_from_file(?SIGNUP_DB, crossbar, ?VIEW_FILE);
        {ok, _} -> ok
    end,
    State = case file:consult(?SIGNUP_CONF) of
                {ok, Terms} ->
                    ?LOG_SYS("loaded config from ~s", [?SIGNUP_CONF]),
                    Defaults = #state{},
                    #state{cleanup_interval =
                               props:get_value(cleanup_interval, Terms, Defaults#state.cleanup_interval)
                           ,signup_lifespan =
                               props:get_value(signup_lifespan, Terms, Defaults#state.signup_lifespan)
                           ,register_cmd =
                               compile_template(props:get_value(register_cmd, Terms), cb_signup_register_cmd)
                           ,activation_email_plain =
                               compile_template(props:get_value(activation_email_plain, Terms), cb_signup_email_plain)
                           ,activation_email_html =
                               compile_template(props:get_value(activation_email_html, Terms), cb_signup_email_html)
                           ,activation_email_from =
                               compile_template(props:get_value(activation_email_from, Terms), cb_signup_email_from)
                           ,activation_email_subject =
                               compile_template(props:get_value(activation_email_subject, Terms), cb_signup_email_subject)
                          };
                {error, _} ->
                    ?LOG_SYS("could not read config from ~s", [?SIGNUP_CONF]),
                    #state{}
            end,
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function binds this server to the crossbar bindings server,
%% for the keys we need to consume.
%% @end
%%--------------------------------------------------------------------
-spec bind_to_crossbar/0 :: () ->  no_return().
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.signup">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.signup">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.signup">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.signup">>).

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
    {true, ['PUT']};
allowed_methods([_]) ->
    {true, ['POST']};
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
-spec validate/2 :: (Params, Context) -> #cb_context{} when
      Params :: list(),
      Context :: #cb_context{}.
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    validate_new_signup(Context);
validate([ActivationKey], #cb_context{req_verb = <<"post">>}=Context) ->
    check_activation_key(ActivationKey, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new signup document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec validate_new_signup/1 :: (Context) -> #cb_context{} when
      Context :: #cb_context{}.
validate_new_signup(#cb_context{req_data=JObj}=Context) ->
    {AccountErrors, Account} = validate_account(wh_json:get_value(<<"account">>, JObj), Context),
    {UserErrors, User} = validate_user(wh_json:get_value(<<"user">>, JObj), Context),
    case {AccountErrors, UserErrors} of
        {[], []} ->
            Context#cb_context{doc={struct, [{<<"pvt_user">>, User}
                                             ,{<<"pvt_account">>, Account}
                                             ,{<<"pvt_activation_key">>, create_activation_key()}
                                            ]}
                               ,resp_status=success};
        _Else ->
            crossbar_util:response_invalid_data({struct, [{<<"account">>, AccountErrors}
                                                          ,{<<"user">>, UserErrors}]}, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the account realm is unique and if the account is valid
%% @end
%%--------------------------------------------------------------------
-spec validate_account/2 :: (Account, Context) -> tuple(list(), undefined | json_object()) when
      Account :: undefined | json_object(),
      Context :: #cb_context{}.
validate_account(undefined, _) ->
    ?LOG("signup did not contain an account definition"),
    {[<<"account">>], undefined};
validate_account(Account, Context) ->
    case is_unique_realm(wh_json:get_value(<<"realm">>, Account))
        andalso cb_accounts:create_account(Context#cb_context{req_data=Account}) of
        false ->
            {[<<"duplicate realm">>], undefined};
        #cb_context{resp_status=success, doc=Acct} ->
            ?LOG("signup account is valid"),
            {[], Acct};
        #cb_context{resp_data=Errors} ->
            ?LOG("signup account definition is not valid"),
            {Errors, undefined}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the user object is valid
%% @end
%%--------------------------------------------------------------------
-spec validate_user/2 :: (User, Context) -> tuple(list(), undefined | json_object()) when
      User :: undefined | json_object(),
      Context :: #cb_context{}.
validate_user(undefined, _) ->
    ?LOG("signup did not contain an user definition"),
    {[<<"user">>], undefined};
validate_user(User, Context) ->
    case cb_users:create_user(Context#cb_context{req_data=User}) of
        #cb_context{resp_status=success, doc=Usr} ->
            ?LOG("signup user is valid"),
            {[], Usr};
        #cb_context{resp_data=Errors} ->
            ?LOG("signup user definition is not valid"),
            {Errors, undefined}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% generates a random activation key
%% @end
%%--------------------------------------------------------------------
-spec create_activation_key/0 :: () -> binary().
create_activation_key() ->
    ActivationKey =
        wh_util:to_binary(wh_util:to_hex(crypto:rand_bytes(32))),
    ?LOG("created new activation key ~s", [ActivationKey]),
    ActivationKey.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a signup document from the database
%% @end
%%--------------------------------------------------------------------
-spec check_activation_key/2 :: (ActivationKey, Context) -> #cb_context{} when
      ActivationKey :: binary(),
      Context :: #cb_context{}.
check_activation_key(ActivationKey, Context) ->
    case couch_mgr:get_results(?SIGNUP_DB, ?VIEW_ACTIVATION_KEYS, [{<<"key">>, ActivationKey}
                                                                   ,{<<"include_docs">>, true}]) of
        {ok, []} ->
            ?LOG("activation key not found"),
            crossbar_util:response(error, <<"invalid activation key">>, 403, Context);
        {ok, [JObj|_]} ->
            ?LOG("activation key is valid"),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"doc">>, JObj)};
        _ ->
            ?LOG("db error while looking up activation key"),
            crossbar_util:response(error, <<"invalid activation key">>, 403, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Activate signup document by creating an account and user
%% @end
%%--------------------------------------------------------------------
-spec activate_signup/1 :: (JObj) -> boolean() when
      JObj :: json_object().
activate_signup(JObj) ->
    case activate_account(wh_json:get_value(<<"pvt_account">>, JObj)) of
        {ok, Account} ->
            activate_user(Account, wh_json:get_value(<<"pvt_user">>, JObj));
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create the account defined on the signup document
%% @end
%%--------------------------------------------------------------------
-spec activate_account/1 :: (Account) -> tuple(ok, binary()) | tuple(error, atom()) when
      Account :: undefined | json_object().
activate_account(undefined) ->
    {error, account_undefined};
activate_account(Account) ->
    Event = <<"v1_resource.execute.put.accounts">>,
    Payload = [undefined, #cb_context{doc=Account}, [[]]],
    case crossbar_bindings:fold(Event, Payload) of
        [_, #cb_context{resp_status=success, resp_data=JObj} | _] ->
            AccountId = wh_json:get_value(<<"id">>, JObj),
            ?LOG("created new account ~s", [AccountId]),
            {ok, JObj};
        _ ->
            ?LOG("could not create a new account"),
            {error, creation_failed}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a remote host authenticates the shared token it will return
%% an account and user, ensure the user exists locally (creating if not)
%% @end
%%--------------------------------------------------------------------
-spec activate_user/2 :: (Account, User) -> tuple(ok, binary(), binary())
                                                  | tuple(error, atom()) when
      Account :: json_object(),
      User :: undefined | json_object().
activate_user(_, undefined) ->
    {error, user_undefined};
activate_user(Account, User) ->
    AccountId = wh_json:get_value(<<"id">>, Account),
    Db = whapps_util:get_db_name(AccountId, encoded),
    Event = <<"v1_resource.execute.put.users">>,
    Payload = [undefined, #cb_context{doc=User, db_name=Db}, [[]]],
    case crossbar_bindings:fold(Event, Payload) of
        [_, #cb_context{resp_status=success, resp_data=JObj} | _] ->
            UserId = wh_json:get_value(<<"id">>, JObj),
            ?LOG("created new user ~s in account ~s", [UserId, AccountId]),
            {ok, Account, JObj};
        _ ->
            ?LOG("could not create a new user in account ~s", [AccountId]),
            {error, creation_failed}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% if the system has a register command defined in priv/signup/signup.conf
%% then exectute it now.
%% @end
%%--------------------------------------------------------------------
-spec exec_register_command/3 :: (RD, Context, State) -> no_return() when
      RD :: #wm_reqdata{},
      Context :: #cb_context{},
      State :: #state{}.
exec_register_command(_, _, #state{register_cmd=undefined}) ->
    ok;
exec_register_command(RD, Context, #state{register_cmd=CmdTmpl}) ->
    Props = template_props(RD, Context),
    {ok, Cmd} = CmdTmpl:render(Props),
    ?LOG("executing register command ~s", [Cmd]),
    os:cmd(binary_to_list(iolist_to_binary(Cmd))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_activation_email/3 :: (RD, Context, State) -> no_return() when
      RD :: #wm_reqdata{},
      Context :: #cb_context{},
      State :: #state{}.
send_activation_email(RD, #cb_context{doc=JObj, req_id=ReqId}=Context, #state{activation_email_subject=SubjectTmpl
                                                                ,activation_email_from=FromTmpl}=State) ->
    Props = template_props(RD, Context),
    To = wh_json:get_value([<<"pvt_user">>, <<"email">>], JObj),
    Subject = case SubjectTmpl:render(Props) of
                  {ok, S} -> S;
                  _ -> <<"Confirm your account activation">>
              end,
    From = case FromTmpl:render(Props) of
               {ok, F} -> F;
               _ ->
                   <<"no_reply@", (wh_util:to_binary(net_adm:localhost()))/binary>>
           end,
    Email = {<<"multipart">>, <<"alternative">> %% Content Type / Sub Type
		 ,[ %% Headers
		    {<<"From">>, From},
		    {<<"To">>, To},
		    {<<"Subject">>, Subject}
		  ]
	     ,[] %% Parameters
             ,create_body(State, Props, [])
	    },
    Encoded = mimemail:encode(Email),
    ?LOG("sending activation email to ~s", [To]),
    gen_smtp_client:send({From, [To], Encoded}, [{relay, "localhost"}]
			 ,fun(X) -> ?LOG(ReqId, "sending email to ~s resulted in ~p", [To, X]) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a multipart body from the HTML and plain text templates
%% if they have been provided
%% @end
%%--------------------------------------------------------------------
-spec create_body/3 :: (State, Props, Body) -> list() when
      State :: #state{},
      Props :: proplist(),
      Body :: list().
create_body(#state{activation_email_html=Tmpl}=State, Props, Body) when Tmpl =/= undefined ->
    case Tmpl:render(Props) of
        {ok, Content} ->
            Part = {<<"text">>, <<"html">>
                    ,[{<<"Content-Type">>, <<"text/html">>}]
                    ,[]
                    ,iolist_to_binary(Content)},
             create_body(State#state{activation_email_html=undefined}, Props, [Part|Body]);
        _ ->
             create_body(State#state{activation_email_html=undefined}, Props, Body)
    end;
create_body(#state{activation_email_plain=Tmpl}=State, Props, Body) when Tmpl =/= undefined ->
    case Tmpl:render(Props) of
        {ok, Content} ->
            Part = {<<"text">>, <<"plain">>
                    ,[{<<"Content-Type">>, <<"text/plain">>}]
                    ,[]
                    ,iolist_to_binary(Content)},
             create_body(State#state{activation_email_plain=undefined}, Props, [Part|Body]);
        _ ->
             create_body(State#state{activation_email_plain=undefined}, Props, Body)
    end;
create_body(_, _, Body) ->
    Body.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a proplist to provide to the templates during render
%% @end
%%--------------------------------------------------------------------
-spec template_props/2 :: (RD, Context) -> proplist() when
      RD :: #wm_reqdata{},
      Context :: #cb_context{}.
template_props(RD, #cb_context{doc=JObj, req_data=Data}) ->
    Port = case wrq:port(RD) of
	       80 -> "";
	       P -> [":", wh_util:to_list(P)]
	   end,
    ApiHost = ["http://", string:join(lists:reverse(wrq:host_tokens(RD)), "."), Port, "/"],
    %% remove the redundant request data
    Req1 = wh_json:delete_key(<<"account">>, Data),
    Req2 = wh_json:delete_key(<<"user">>, Req1),
    %% create props to expose to the template
    [{<<"account">>, wh_json:to_proplist(<<"pvt_account">>, JObj)}
     ,{<<"user">>, wh_json:to_proplist(<<"pvt_user">>, JObj)}
     ,{<<"request">>, wh_json:to_proplist(Req2)}
     ,{<<"api_url">>, [{<<"host">>, wh_util:to_binary(ApiHost)}
                       ,{<<"path">>, <<"v1/signup/">>}]}
     ,{<<"host">>, wh_util:to_binary(net_adm:localhost())}
     ,{<<"activation_key">>, wh_json:get_value(<<"pvt_activation_key">>, JObj, <<>>)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determines if the given account realm is unique amoung the existing
%% accounts and completed signups
%% @end
%%--------------------------------------------------------------------
-spec is_unique_realm/1 :: (Realm) -> boolean() when
      Realm :: undefined | binary().
is_unique_realm(undefined) ->
    false;
is_unique_realm(<<>>) ->
    false;
is_unique_realm(Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
        {ok, _} ->
            false;
        {error, _} ->
            case couch_mgr:get_results(?SIGNUP_DB, ?VIEW_ACTIVATION_REALM, [{<<"key">>, Realm}]) of
                {ok, []} ->
                    true;
                _Else ->
                    false
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Periodically runs and cleans expired signups.  The delete function
%% will wait up to 5 minutes randomly, as an experiment to distribute
%% the db load.
%% @end
%%--------------------------------------------------------------------
-spec cleanup_signups/1 :: (State) -> no_return() when
      State :: #state{}.
cleanup_signups(#state{signup_lifespan=Lifespan}) ->
    ?LOG_SYS("cleaning up signups"),
    Expiration = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Lifespan,
    case couch_mgr:get_results(?SIGNUP_DB, ?VIEW_ACTIVATION_CREATED, [{<<"startkey">>, 0}
                                                                      ,{<<"endkey">>, Expiration}
                                                                      ,{<<"include_docs">>, true}]) of
        {ok, Expired} ->
            [spawn(fun() ->
                           timer:sleep(random:uniform(500) * 1000),
                           delete_signup(wh_json:get_value(<<"doc">>, JObj))
                   end)
             || JObj <- Expired];
        _Else ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The helper function spanwed by cleanup_signups to mark a signup
%% as expired.
%% @end
%%--------------------------------------------------------------------
-spec delete_signup/1 :: (JObj) -> no_return() when
      JObj :: undefined | json_object().
delete_signup(undefined) ->
    ok;
delete_signup(JObj) ->
    ?LOG_SYS("removing expired signup ~s", [wh_json:get_value(<<"_id">>, JObj)]),
    crossbar_doc:delete(#cb_context{doc=JObj, db_name=?SIGNUP_DB}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compiles a template string or path, correcting relative paths
%% to the priv directory of this module
%% @end
%%--------------------------------------------------------------------
-spec compile_template/2 :: (Template, Name) -> undefined | atom() when
      Template :: undefined | string() | binary(),
      Name :: atom().
compile_template(undefined, _) ->
    undefined;
compile_template(Template, Name) when not is_binary(Template) ->
    Path = case string:substr(Template, 1, 1) of
               "/" ->
                   Template;
               _ ->
                   BasePath = code:lib_dir(crossbar, priv),
                   lists:concat([BasePath, "/signup/", Template])
           end,
    ?LOG("sourcing template from file at ~s", [Path]),
    do_compile_template(Path, Name);
compile_template(Template, Name) ->
    do_compile_template(Template, Name).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compiles template string or path, normalizing the return
%% @end
%%--------------------------------------------------------------------
-spec do_compile_template/2 :: (Template, Name) -> undefined | atom() when
      Template :: string() | binary(),
      Name :: atom().
do_compile_template(Template, Name) ->
    case erlydtl:compile(Template, Name) of
        {ok, Name} ->
            ?LOG("compiled ~s template", [Name]),
            Name;
        ok ->
            ?LOG("compiled ~s template file", [Name]),
            Name;
        _E ->
            ?LOG("could not compile ~s template, ignoring", [Name]),
            undefined
    end.
