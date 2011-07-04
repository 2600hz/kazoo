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

-define(VIEW_FILE, <<"views/user_auth.json">>).
-define(MD5_LIST, <<"user_auth/creds_by_md5">>).
-define(SHA1_LIST, <<"user_auth/creds_by_sha">>).

-define(ACCT_MD5_LIST, <<"users/creds_by_md5">>).
-define(ACCT_SHA1_LIST, <<"users/creds_by_sha">>).

-define(AGG_DB, <<"user_auth">>).
-define(AGG_FILTER, <<"users/export">>).

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
handle_info({binding_fired, Pid, <<"v1_resource.authorize">>, {RD, #cb_context{req_nouns=[{<<"openid_auth">>,[]}]}=Context}}, State) ->
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>, {RD, #cb_context{req_nouns=[{<<"openid_auth">>,[]}]}=Context}}, State) ->
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authorize">>, {RD, #cb_context{req_nouns=[{<<"openid_auth">>,[<<"return">>]}]}=Context}}, State) ->
    Pid ! {binding_result, true, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authenticate">>, {RD, #cb_context{req_nouns=[{<<"openid_auth">>,[<<"return">>]}]}=Context}}, State) ->
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

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.openid_auth">>, [RD, Context | []=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:binding_heartbeat(Pid),
                  case gen_server:call(openid_auth_srv, {prepare, "005e7b40", "http://google.com/accounts/o8/id"}) of
                      {ok, AuthReq} ->
                          io:format("AuthReq ~p~n", [AuthReq]),
                          URL = openid:authentication_url(AuthReq, "http://apps002-dev-ord.2600hz.com:8000/v1/openid_auth/return", "http://apps002-dev-ord.2600hz.com:8000/v1"),
                          io:format("Go to: ~p~n", [URL]),
                          C1 = Context#cb_context{resp_headers=[{"Location", whistle_util:to_list(URL)}], resp_error_code=302, resp_status=error},
                          RD1 = wrq:set_resp_header("Location",  whistle_util:to_list(URL), RD),
                          Pid ! {binding_result, true, [wrq:do_redirect(true, RD1), C1, Params]};
                      {error, Error} ->
                          io:format("Error ~p~n", [Error]),
                          Pid ! {binding_result, true, [RD, Context, Params]};
                      Else ->
                          io:format("Else ~p~n", [Else]),
                          Pid ! {binding_result, true, [RD, Context, Params]}
                  end
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.get.openid_auth">>, [RD, Context | [<<"return">>]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:binding_heartbeat(Pid),
                  case gen_server:call(openid_auth_srv, {verify, "005e7b40", "http://apps002-dev-ord.2600hz.com:8000/v1/openid_auth/return", wrq:req_qs(RD)}) of
                      {ok, ID} ->
                          io:format("return ~p~n", [ID]);
                      {error, Error} ->
                          io:format("error ~p~n", [Error])
                  end,
                  Pid ! {binding_result, true, [RD, Context, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    io:format("started openid_srv ~p~n", [openid_srv:start_link(openid_auth_srv)]),
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
allowed_methods([<<"return">>]) ->
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
resource_exists([<<"return">>]) ->
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
validate([<<"return">>], Context) ->
    Context#cb_context{resp_status=success};
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).
