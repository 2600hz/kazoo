%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Listener for authn_req, reg_success, and reg_query AMQP requests
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(registrar_shared_listener).

-behaviour(gen_listener).

-export([start_link/0, get_queue_name/0, insert_auth_user/1, remove_auth_user/1, get_auth_user/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("reg.hrl").

-define(RESPONDERS, [{'reg_authn_req'
                      ,[{<<"directory">>, <<"authn_req">>}]
                     }
                     ,{'reg_authz_req'
                      ,[{<<"authz">>, <<"authz_req">>}]
                     }
                     ,{{'reg_aaa_resp'
                      ,[{<<"aaa">>, <<"aaa_authn_resp">>}]}
                     }
                     ,{{'reg_route_req', 'handle_route_req'}
                       ,[{<<"dialplan">>, <<"route_req">>}]
                      }
                    ]).
-define(BINDINGS, [{'authn', []}
                   ,{'authz', []}
                   ,{'route', []}
                   ,{'self', []}
                  ]).
-define(SERVER, ?MODULE).
-define(REG_QUEUE_NAME, <<"registrar_listener">>).
-define(REG_QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(REG_CONSUME_OPTIONS, [{'exclusive', 'false'}]).

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
    lager:debug([{'trace', 'true'}], "", []),
    gen_listener:start_link({'local', ?SERVER}, ?MODULE, [{'responders', ?RESPONDERS}
                                                          ,{'bindings', ?BINDINGS}
                                                          ,{'queue_name', ?REG_QUEUE_NAME}
                                                          ,{'queue_options', ?REG_QUEUE_OPTIONS}
                                                          ,{'consume_options', ?REG_CONSUME_OPTIONS}
                                                         ], []).

get_queue_name() ->
    lager:debug([{'trace', 'true'}], "", []),
    ?REG_QUEUE_NAME.

insert_auth_user(AuthUser) ->
    lager:debug([{'trace', 'true'}], "AuthUser=~p~n", [AuthUser]),
    gen_server:call(?MODULE, {'insert_auth_user', AuthUser}).

get_auth_user(MsgId) ->
    lager:debug([{'trace', 'true'}], "MsgId=~p~n", [MsgId]),
    gen_server:call(?MODULE, {'get_auth_user', MsgId}).

remove_auth_user(AuthUser) ->
    lager:debug([{'trace', 'true'}], "AuthUser=~p~n", [AuthUser]),
    gen_server:call(?MODULE, {'remove_auth_user', AuthUser}).

%%%===================================================================
%%% gen_listener callbacks
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
    lager:debug("starting new registrar shared queue server"),
    {'ok', []}.

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
handle_call({'insert_auth_user', AuthUser}, _From, State) ->
    lager:debug([{'trace', 'true'}], "AuthUser=~p~n", [AuthUser]),
    {'reply', 'ok', [AuthUser | State]};
handle_call({'remove_auth_user', AuthUser}, _From, State) ->
    lager:debug([{'trace', 'true'}], "AuthUser=~p~n", [AuthUser]),
    {'reply', 'ok', [J || J <- State, AuthUser =/= J]};
handle_call({'get_auth_user', MsgId}, _From, State) ->
    lager:debug([{'trace', 'true'}], "MsgId=~p~n", [MsgId]),
    [AuthUser] = [AuthUser || {MsgId1, AuthUser} <- State, MsgId == MsgId1],
    {'reply', AuthUser, State};
handle_call(Msg, From, State) ->
    lager:debug([{'trace', 'true'}], "Msg=~p~nFrom=~p~nState=~p~n", [Msg, From, State]),
    {'noreply', State}.

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
    {'noreply', State}.

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
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), term()) -> 'ok'.
terminate(_Reason, _) ->
    lager:debug("registrar shared queue server ~p termination", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
