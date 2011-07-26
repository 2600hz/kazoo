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
-module(cb_openid_auth).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(SERVER, ?MODULE).

-define(TOKEN_DB, <<"token_auth">>).

-record(state, {realm = "http://apps002-dev-ord.2600hz.com:8000/v1"}).

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
    ssl:start(),
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
                 ,{RD, #cb_context{req_nouns=[{<<"openid_auth">>,[]}]}=Context}}, State) ->
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"openid_auth">>,[]}]}=Context}}, State) ->
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authorize">>
                 ,{RD, #cb_context{req_nouns=[{<<"openid_auth">>,[<<"checkauth">>, _]}]}=Context}}, State) ->
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"openid_auth">>,[<<"checkauth">>, _]}]}=Context}}, State) ->
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.openid_auth">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.openid_auth">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.openid_auth">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                crossbar_util:binding_heartbeat(Pid),
                Pid ! {binding_result, true, [RD, validate(Params, Context), Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.openid_auth">>, [RD, Context | []=Params]}
            ,#state{realm=Realm}=State) ->
    spawn(fun() ->
                  crossbar_util:binding_heartbeat(Pid),

                  %% Placeholder until we can support other providers
                  {Provider, ProviderUrl} = get_provider(RD, Context),

                  %% we cant just put the UUID on the url, that would defeat the purpose
                  CacheKey = whistle_util:to_binary(whistle_util:to_hex(crypto:rand_bytes(16))),
                  Seed = whistle_util:to_hex(crypto:rand_bytes(32)),
                  wh_cache:store(CacheKey, {Seed, Provider}, 60),

                  case gen_server:call(openid_auth_srv, {prepare, Seed, ProviderUrl}) of
                      {ok, AuthReq} ->
                          Return = lists:flatten([Realm, "/openid_auth/checkauth/", whistle_util:to_list(CacheKey)]),
                          Location = whistle_util:to_list(openid:authentication_url(AuthReq, Return, Realm)),
                          ?LOG("redirecting client to ~s as openid auth ~s", [Location, Seed]),
                          Context1 = Context#cb_context{resp_headers=[{"Location", Location}]
                                                        ,resp_error_code=302
                                                        ,resp_status=error},
                          RD1 = wrq:set_resp_header("Location", Location, RD),
                          Pid ! {binding_result, true, [wrq:do_redirect(true, RD1), Context1, Params]};
                      {error, Error} ->
                          ?LOG("openid auth srv prepare: ~p", [Error]),
                          E = whistle_util:to_binary(Error),
                          Pid ! {binding_result, true, [RD, crossbar_util:response(fatal, E, Context), Params]}
                  end
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.openid_auth">>, [RD, Context | [<<"checkauth">>, CacheKey]=Params]}
            ,#state{realm=Realm}=State) ->
    spawn(fun() ->
                  crossbar_util:binding_heartbeat(Pid),

                  %% get the UUID that we stored when we started this
                  %% NOTE: this restricts the return_to to the same machine the redirected the user (cache is local)
                  {ok, {Seed, Provider}} = wh_cache:fetch(CacheKey),

                  QS = wrq:req_qs(RD),
                  Return = lists:flatten([Realm, "/openid_auth/checkauth/", whistle_util:to_list(CacheKey)]),
                  case gen_server:call(openid_auth_srv, {verify, Seed, Return, QS}) of
                      {ok, IdentityUrl} ->
                          Identity = get_identity(IdentityUrl, Provider, QS),
                          case find_account(Identity, Provider) of
                              {ok, AccountId} ->
                                  ?LOG("determined that ~s id ~s is associated with account ~s", [Provider, Identity, AccountId]),
                                  Context1 = create_token(IdentityUrl, AccountId, RD, Context),
                                  Pid ! {binding_result, true, [RD, Context1, Params]};
                              {error, _} ->
                                  io:format("Attributes ~p~n", [extract_attributes(QS)]),
                                  Context1 = Context#cb_context{resp_data=extract_attributes(QS)
                                                                ,resp_status=error
                                                                ,resp_error_code=400
                                                                ,resp_error_msg = <<"not registered">>
                                                               },
                                  Pid ! {binding_result, true, [RD, Context1, Params]}
                          end;
                      {error, Error} ->
                          ?LOG("openid auth srv verify error: ~p", [Error]),
                          E = whistle_util:to_binary(Error),
                          Pid ! {binding_result, true, [RD, crossbar_util:response(error, E, 400, Context), Params]}
                  end
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    {ok, _} = openid_srv:start_link(openid_auth_srv),
    bind_to_crossbar(),
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
-spec(bind_to_crossbar/0 :: () -> no_return()).
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.openid_auth">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.openid_auth">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.openid_auth">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.openid_auth">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), http_methods())).
allowed_methods([]) ->
    {true, ['GET']};
allowed_methods([<<"checkauth">>, _]) ->
    {true, ['GET']};
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
-spec(resource_exists/1 :: (Paths :: list()) -> tuple(boolean(), [])).
resource_exists([]) ->
    {true, []};
resource_exists([<<"checkauth">>, _]) ->
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
-spec(validate/2 :: (Params :: list(), Context :: #cb_context{}) -> #cb_context{}).
validate([], Context) ->
    Context#cb_context{resp_status=success};
validate([<<"checkauth">>, _], Context) ->
    Context#cb_context{resp_status=success};
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

get_provider(_RD, _Context) ->
    {<<"google">>, "http://google.com/accounts/o8/id"}.

get_identity(IdentityUrl, <<"google">>, _QS) ->
    {_, _, _, IdentityQS, _} = mochiweb_util:urlsplit(IdentityUrl),
    whistle_util:to_binary(props:get_value("id", mochiweb_util:parse_qs(IdentityQS))).

find_account(Identifier, Provider) ->
    case couch_mgr:get_results(<<"accounts">>, {<<"accounts">>, <<"listing_by_openid">>}, [{<<"key">>, [Identifier, Provider]}]) of
        {ok, []} ->
            {error, not_registered};
        {ok, [JObj]} ->
            {ok, wh_json:get_value([<<"value">>, <<"account_id">>], JObj)};
         {error, R}=E ->
            ?LOG("failed to find account for ~s from ~s, ~p", [Identifier, Provider, R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token/4 :: (IdentityUrl, AccountId, RD, Context) -> #cb_context{} when
      IdentityUrl :: binary(),
      AccountId :: binary(),
      RD :: #wm_reqdata{},
      Context :: #cb_context{}.
create_token(IdentityUrl, AccountId, RD, Context) ->
    Token = {struct, [
                       {<<"account_id">>, AccountId}
                      ,{<<"created">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                      ,{<<"modified">>, calendar:datetime_to_gregorian_seconds(calendar:universal_time())}
                      ,{<<"method">>, whistle_util:to_binary(?MODULE)}
                      ,{<<"peer">>, whistle_util:to_binary(wrq:peer(RD))}
                      ,{<<"user_agent">>, whistle_util:to_binary(wrq:get_req_header("User-Agent", RD))}
                      ,{<<"accept">>, whistle_util:to_binary(wrq:get_req_header("Accept", RD))}
                      ,{<<"accept_charset">>, whistle_util:to_binary(wrq:get_req_header("Accept-Charset", RD))}
                      ,{<<"accept_endocing">>, whistle_util:to_binary(wrq:get_req_header("Accept-Encoding", RD))}
                      ,{<<"accept_language">>, whistle_util:to_binary(wrq:get_req_header("Accept-Language", RD))}
                      ,{<<"connection">>, whistle_util:to_binary(wrq:get_req_header("Conntection", RD))}
                      ,{<<"keep_alive">>, whistle_util:to_binary(wrq:get_req_header("Keep-Alive", RD))}
                      ,{<<"openid_identity_url">>, whistle_util:to_binary(IdentityUrl)}
                     ]},
    case couch_mgr:save_doc(?TOKEN_DB, Token) of
        {ok, Doc} ->
            AuthToken = wh_json:get_value(<<"_id">>, Doc),
            crossbar_util:response(
              {struct, [{<<"account_id">>, AccountId}]}
              ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
        {error, _} ->
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
    end.


extract_attributes(QS) ->
    Attributes = [{"http://axschema.org/contact/email", <<"email">>}
                  ,{"http://axschema.org/namePerson/first", <<"first_name">>}
                  ,{"http://axschema.org/namePerson/last", <<"last_name">>}
                  ,{"http://axschema.org/pref/language", <<"lang">>}
                  ,{"http://axschema.org/contact/country/home", <<"country">>}
                 ],
    extract_attributes(Attributes, QS, QS, []).

extract_attributes(_, _, [], Props) ->
    {struct, Props};
extract_attributes(Attributes, QS, [{K, V}|T], Acc) ->
    case props:get_value(V, Attributes) of
        undefined ->
            extract_attributes(Attributes, QS, T, Acc);
        NormalizedName ->
            %% heavy handed approach to namespace, should only operate in "http://openid.net/srv/ax/1.0"
            %% ...getting it done fast
            VKey = re:replace(K, "\\.type\\.", ".value.", [{return, list}]),
            Value = whistle_util:to_binary(props:get_value(VKey, QS, <<>>)),
            extract_attributes(Attributes, QS, T, [{NormalizedName, Value}|Acc])
    end.
