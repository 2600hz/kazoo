%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% OpenID auth module
%%% Handle OpenID authentication of users, this module is non-standard:
%%%
%%% * it authenticates and authorizes itself
%%% * it operates without an account id (or account db)
%%% * it breaks the REST API (prefoming a GETs due to OpenID specs
%%%       and usability for the client)
%%% * it can (and will) redirect the user out of our domain
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

-define(SIGNUP_DB, <<"signups">>).
-define(TOKEN_DB, <<"token_auth">>).

-define(IdPs, [<<"google">>]).

-record(state, {realm = "http://apps002-dev-ord.2600hz.com:8000/v1"
                ,reg_url = "http://apps002-dev-ord.2600hz.com/register.php"
                ,app_url = "http://apps002-dev-ord.2600hz.com/winkstart.php"
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
                 ,{RD, #cb_context{req_nouns=[{<<"openid_auth">>,[_]}], req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"openid_auth">>,[_]}], req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticating request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authorize">>
                 ,{RD, #cb_context{req_nouns=[{<<"openid_auth">>,[<<"checkauth">>, _]}], req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"openid_auth">>,[<<"checkauth">>, _]}], req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticating request", []),
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
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.openid_auth">>, [RD, Context | [Provider]=Params]}
            ,#state{realm=Realm}=State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),

                  %% find the discovery URL of the IdP
                  true = is_list(ProviderUrl = get_provider_url(Provider)),

                  %% if this is a popup then we will not do redirects
                  Popup = wh_util:is_popup(wrq:get_qs_value("popup", RD)),

                  %% we cant just put the UUID on the url, that would defeat the purpose
                  CacheKey = wh_util:to_binary(wh_util:to_hex(crypto:rand_bytes(16))),
                  Seed = wh_util:to_hex(crypto:rand_bytes(32)),
                  wh_cache:store(CacheKey, {Seed, Provider, Popup}, 60),

                  %% build up our URL
                  Return = lists:flatten([Realm, "/openid_auth/checkauth/", wh_util:to_list(CacheKey)]),

                  %% HELO IdP
                  case gen_server:call(openid_auth_srv, {prepare, Seed, ProviderUrl}) of
                      %% Yay! Its friendly.. redirect the user to it
                      {ok, AuthReq} when Popup ->
                          Location = wh_util:to_binary(openid:authentication_url(AuthReq, Return, Realm)),
                          ?LOG("providing redirect location ~s as openid auth ~s", [Location, Seed]),
                          Context1 = Context#cb_context{resp_data={struct, [{"location", Location}]}
                                                        ,resp_status=success},
                          Pid ! {binding_result, true, [RD, Context1, Params]};
                      {ok, AuthReq} ->
                          Location = wh_util:to_list(openid:authentication_url(AuthReq, Return, Realm)),
                          ?LOG("redirecting client to ~s as openid auth ~s", [Location, Seed]),
                          Pid ! redirect_client(Location, RD, Context, Params);
                      %% Must be grumpy today
                      {error, Error} ->
                          ?LOG("openid auth srv prepare: ~p", [Error]),
                          E = wh_util:to_binary(Error),
                          Pid ! {binding_result, true, [RD, crossbar_util:response(fatal, E, Context), Params]}
                  end
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.openid_auth">>, [RD, Context | [<<"checkauth">>, CacheKey]=Params]}
            ,#state{realm=Realm, reg_url=RegUrl, app_url=AppUrl}=State) ->
    CallId = get(callid),
    spawn(fun() ->
                  put(callid, CallId),
                  crossbar_util:binding_heartbeat(Pid),
                  QS = wrq:req_qs(RD),

                  %% get the UUID that we stored when we started this
                  %% NOTE: this restricts the return_to to the same machine the redirected the user (cache is local)
                  {ok, {Seed, Provider, Popup}} = wh_cache:fetch(CacheKey),

                  %% determine the return URL we used
                  Return = lists:flatten([Realm, "/openid_auth/checkauth/", wh_util:to_list(CacheKey)]),

                  %% checkid_setup with the IdP
                  case gen_server:call(openid_auth_srv, {verify, Seed, Return, QS}) of
                      %% GREAT SUCCESS, now do we know who this is...
                      {ok, IdentityUrl} ->
                          Identity = get_identity(IdentityUrl, Provider, QS),
                          case find_account(Identity, Provider) of
                              %% ...we do, we do
                              {ok, AccountId} when Popup->
                                  ?LOG("determined that ~s id ~s is associated with account ~s", [Provider, Identity, AccountId]),
                                  Context1 = create_token(IdentityUrl, AccountId, RD, Context),
                                  Pid ! {binding_result, true, [RD, Context1, Params]};
                              {ok, AccountId} ->
                                  #cb_context{auth_token=AuthToken} = create_token(IdentityUrl, AccountId, RD, Context),
                                  Location = wh_util:to_list(list_to_binary([AppUrl, "?account_id=", AccountId, "&token=", AuthToken])),
                                  ?LOG("redirecting client to web app url: ~s", [Location]),
                                  Pid ! redirect_client(Location, RD, Context, Params);
                              %% ...nope-ish
                              {error, _} when Popup->
                                  JObj = {struct, extract_attributes(QS)},
                                  ?LOG("determined that ~s id ~s (~s) has no associated account", [Provider, Identity, wh_json:get_value(<<"email">>, JObj)]),
                                  Context1 = Context#cb_context{resp_data=JObj
                                                                ,resp_status=error
                                                                ,resp_error_code=400
                                                                ,resp_error_msg = <<"not registered">>
                                                               },
                                  Pid ! {binding_result, true, [RD, Context1, Params]};
                              {error, _}  ->
                                  RespQS = mochiweb_util:urlencode(extract_attributes(QS)),
                                  Location = wh_util:to_list(list_to_binary([RegUrl, "?", RespQS])),
                                  ?LOG("redirecting client to registration url: ~s", [Location]),
                                  Pid ! redirect_client(Location, RD, Context, Params)
                          end;
                      %% bugger
                      {error, Error} ->
                          ?LOG("openid auth srv verify error: ~p", [Error]),
                          E = wh_util:to_binary(Error),
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
-spec bind_to_crossbar/0 :: () -> no_return().
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
-spec allowed_methods/1 :: (Paths) -> tuple(boolean(), http_methods()) when
      Paths :: list().
allowed_methods([_]) ->
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
-spec resource_exists/1 :: (Paths) -> tuple(boolean(), []) when
      Paths :: list().
resource_exists([_]) ->
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
-spec validate/2 :: (Params, Context) -> #cb_context{} when
      Params :: list(),
      Context :: #cb_context{}.
validate([Provider], Context) ->
    case lists:member(Provider, ?IdPs) of
        true ->
            ?LOG("requested openid identity provider ~s is supported", [Provider]),
            Context#cb_context{resp_status=success};
        false ->
            ?LOG("requested openid identity provider ~s is unsupported", [Provider]),
            Context#cb_context{resp_status=error
                               ,resp_error_msg = <<"unsupported openid identity provider">>
                               ,resp_error_code=400
                              }
        end;
validate([<<"checkauth">>, _], Context) ->
    Context#cb_context{resp_status=success};
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is used to identify the IdP from a library of
%% supported providers. Returns the providers discovery URL.
%%
%% TODO: Currently google is hardcoded but this should draw from
%% a list of IdPs
%% @end
%%--------------------------------------------------------------------
-spec get_provider_url/1 :: (Provider) -> string() | undefined when
      Provider :: binary().
get_provider_url(<<"google">>) ->
    "http://google.com/accounts/o8/id";
get_provider_url(_) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% IdP specific identity extractor, this will allow the url to change
%% without breaking our account mappings
%% @end
%%--------------------------------------------------------------------
-spec get_identity/3 :: (IdentityUrl, Provider, QS) -> binary() when
      IdentityUrl :: binary(),
      Provider :: binary(),
      QS :: proplist().
get_identity(IdentityUrl, <<"google">>, _QS) ->
    {_, _, _, IdentityQS, _} = mochiweb_util:urlsplit(IdentityUrl),
    wh_util:to_binary(props:get_value("id", mochiweb_util:parse_qs(IdentityQS))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% find the account id mapping from the IdP unique identifier
%% @end
%%--------------------------------------------------------------------
-spec find_account/2 :: (Identifier, Provider) -> tuple(ok, binary()) | tuple(error, atom()) when
      Identifier :: binary(),
      Provider :: binary().
find_account(Identifier, Provider) ->
    case couch_mgr:get_results(<<"accounts">>, {<<"accounts">>, <<"listing_by_openid">>}, [{<<"key">>, [Identifier, Provider]}]) of
        {ok, []} ->
            {error, not_registered};
        {ok, [JObj]} ->
            AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], JObj),
            ?LOG("found openid ~s belongs to account ~s", [AccountId]),
            {ok, AccountId};
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
    Token = {struct, [{<<"account_id">>, AccountId}
%%                      ,{<<"owner_id">>, OwnerId}
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
                      ,{<<"openid_identity_url">>, wh_util:to_binary(IdentityUrl)}
%%                      ,{<<"openid_provider">>, wh_util:to_binary(IdentityUrl)}
                     ]},
    case couch_mgr:save_doc(?TOKEN_DB, Token) of
        {ok, Doc} ->
            AuthToken = wh_json:get_value(<<"_id">>, Doc),
            ?LOG("created new local auth token ~s", [AuthToken]),
            crossbar_util:response({struct, [{<<"account_id">>, AccountId}
%%                                             ,{<<"owner_id">>, OwnerId}]}
                                             ,{<<"owner_id">>, <<>>}]}
                                   ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
        {error, R} ->
            ?LOG("could not create new local auth token, ~p", [R]),
            crossbar_util:response(error, <<"invalid credentials">>, 401, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract all known attributes that may have been returned by the
%% openid provider
%% @end
%%--------------------------------------------------------------------
-spec extract_attributes/1 :: (QS) -> proplist() when
      QS :: proplist().
extract_attributes(QS) ->
    Attributes = [{"http://axschema.org/contact/email", <<"email">>}
                  ,{"http://axschema.org/namePerson/first", <<"first_name">>}
                  ,{"http://axschema.org/namePerson/last", <<"last_name">>}
                  ,{"http://axschema.org/pref/language", <<"lang">>}
                  ,{"http://axschema.org/contact/country/home", <<"country">>}
                 ],
    extract_attribute(Attributes, QS, QS, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract known attributes that may have been returned by the
%% openid provider and accumulate a proplist (normalizing the names)
%% @end
%%--------------------------------------------------------------------
-spec extract_attribute/4 :: (Attributes, QS, RemainingQS, Accumulator) -> proplist() when
      Attributes :: [{string(), binary()},...],
      QS :: proplist(),
      RemainingQS :: proplist(),
      Accumulator :: proplist().
extract_attribute(_, _, [], Props) ->
    Props;
extract_attribute(Attributes, QS, [{K, V}|T], Acc) ->
    case props:get_value(V, Attributes) of
        undefined ->
            extract_attribute(Attributes, QS, T, Acc);
        NormalizedName ->
            %% heavy handed approach to namespace, should only operate in "http://openid.net/srv/ax/1.0"
            %% ...getting it done fast
            VKey = re:replace(K, "\\.type\\.", ".value.", [{return, list}]),
            Value = wh_util:to_binary(props:get_value(VKey, QS, <<>>)),
            extract_attribute(Attributes, QS, T, [{NormalizedName, Value}|Acc])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the crossbar context to cause the client to be redirected
%% to a given URL
%% @end
%%--------------------------------------------------------------------
-spec redirect_client/4 :: (Location, RD, Context, Params) -> {'binding_result', 'true', list()} when
      Location :: string(),
      RD :: #wm_reqdata{},
      Context :: #cb_context{},
      Params :: list().
redirect_client(Location, RD, Context, Params) ->
    Context1 = Context#cb_context{resp_headers=[{<<"Location">>, Location}]
                                  ,resp_error_code=302
                                  ,resp_status=error},
    RD1 = wrq:set_resp_header("Location", Location, RD),
    {binding_result, true, [wrq:do_redirect(true, RD1), Context1, Params]}.
