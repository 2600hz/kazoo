%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Account API auth module
%%%
%%% This is a non-standard module:
%%% * it authenticates and authorizes itself
%%% * it has a completely unique role
%%% * it operates without an account id (or account db)
%%% * it authorizes an account level cred
%%%
%%% @end
%%% Created : 15 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_api_auth).

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

-define(AGG_DB, <<"accounts">>).
-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_API, <<"accounts/listing_by_api">>).

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
                 ,{RD, #cb_context{req_nouns=[{<<"api_auth">>,[]}]
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authorizing request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>
                 ,{RD, #cb_context{req_nouns=[{<<"api_auth">>,[]}]
                                   ,req_id=ReqId}=Context}}, State) ->
    ?LOG(ReqId, "authenticating request", []),
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.api_auth">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.api_auth">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.api_auth">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.api_auth">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
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
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.api_auth">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.api_auth">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.api_auth">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.api_auth">>).

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
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/2 :: (Params, Context) -> #cb_context{} when
      Params :: list(),
      Context :: #cb_context{}.
validate([], #cb_context{req_data=JObj, req_verb = <<"put">>}=Context) ->
    authorize_api_key(Context, wh_json:get_value(<<"api_key">>, JObj));
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the credentials are valid based on the
%% provided hash method
%%
%% Failure here returns 401
%% @end
%%--------------------------------------------------------------------
-spec authorize_api_key/2 :: (Context, ApiKey) -> #cb_context{} when
      Context :: #cb_context{},
      ApiKey :: binary().
authorize_api_key(Context, ApiKey) when not is_binary(ApiKey) ->
    ?LOG("api key is not the correct format"),
    crossbar_util:response(error, <<"invalid crentials">>, 401, Context);
authorize_api_key(Context, <<"">>) ->
    ?LOG("request has no api key"),
    crossbar_util:response(error, <<"invalid crentials">>, 401, Context);
authorize_api_key(Context, ApiKey) ->
    case crossbar_doc:load_view(?AGG_VIEW_API, [{<<"key">>, ApiKey}], Context#cb_context{db_name=?AGG_DB}) of
        #cb_context{resp_status=success, doc=[JObj|_]}->
            ?LOG("found more account with ~s, using ~s", [ApiKey, wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        #cb_context{resp_status=success, doc=JObj} when JObj =/= ?EMPTY_JSON_OBJECT->
            ?LOG("found API key belongs to account ~s", [wh_json:get_value(<<"id">>, JObj)]),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"value">>, JObj)};
        _ ->
            ?LOG("API key does not belong to any account"),
            crossbar_util:response(error, <<"invalid crentials">>, 401, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to create a token and save it to the token db
%% @end
%%--------------------------------------------------------------------
-spec create_token/2 :: (RD, Context) -> #cb_context{} when
      RD :: #wm_reqdata{},
      Context :: #cb_context{}.
create_token(_, #cb_context{doc=?EMPTY_JSON_OBJECT}=Context) ->
    ?LOG("refusing to create auth token for an empty doc"),
    crossbar_util:response(error, <<"invalid crentials">>, 401, Context);
create_token(RD, #cb_context{doc=JObj}=Context) ->
    AccountId = wh_json:get_value(<<"account_id">>, JObj),
    Token = {struct, [
                       {<<"account_id">>, AccountId}
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
                     ]},
    case couch_mgr:save_doc(?TOKEN_DB, Token) of
        {ok, Doc} ->
            AuthToken = wh_json:get_value(<<"_id">>, Doc),
            ?LOG("created new local auth token ~s", [AuthToken]),
            crossbar_util:response({struct, [{<<"account_id">>, AccountId}]}
                                   ,Context#cb_context{auth_token=AuthToken, auth_doc=Doc});
        {error, R} ->
            ?LOG("could not create new local auth token, ~p", [R]),
            crossbar_util:response(error, <<"invalid crentials">>, 401, Context)
    end.
