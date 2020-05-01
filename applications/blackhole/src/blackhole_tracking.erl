%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_tracking).
-behaviour(gen_listener).

-export([start_link/0
        ,handle_req/2

        ,add_socket/1
        ,remove_socket/1
        ,update_socket/1

        ,get_context_by_session_id/1
        ,get_contexts/0
        ,get_contexts_by_account_id/1
        ,get_contexts_by_ip/1

        ,session_count_by_ip/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("blackhole.hrl").

%% {Pid, Reference, SessionId}
-type state() :: [{pid(), reference(), kz_term:ne_binary()}].

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{?MODULE, [{<<"websockets">>, <<"get_req">>}]}]).
-define(BINDINGS, [{'websockets', ['federate']}]).

-define(BLACKHOLE_QUEUE_NAME, <<>>).
-define(BLACKHOLE_QUEUE_OPTIONS, []).
-define(BLACKHOLE_CONSUME_OPTIONS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?BLACKHOLE_QUEUE_NAME}
                            ,{'queue_options', ?BLACKHOLE_QUEUE_OPTIONS}
                            ,{'consume_options', ?BLACKHOLE_CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(GetReq, _Props) ->
    'true' = kapi_websockets:get_req_v(GetReq),
    kz_log:put_callid(GetReq),

    handle_get_req(GetReq, kz_json:get_ne_binary_value(<<"Socket-ID">>, GetReq)).

handle_get_req(GetReq, 'undefined') ->
    get_sockets_by_account(GetReq);
handle_get_req(GetReq, SocketId) ->
    get_sockets_by_id(GetReq, SocketId).

get_sockets_by_id(GetReq, SocketId) ->
    case get_context_by_session_id(SocketId) of
        {'error', 'not_found'} ->
            lager:info("failed to find socket info for ~s", [SocketId]),
            error_resp(GetReq);
        Context ->
            success_resp(GetReq, to_resp_data(Context))
    end.

get_sockets_by_account(GetReq) ->
    AccountId = kz_json:get_ne_binary_value(<<"Account-ID">>, GetReq),
    AuthAccountId = kz_json:get_ne_binary_value(<<"Auth-Account-ID">>, GetReq),

    get_sockets_by_account(GetReq, [AccountId, AuthAccountId], []).

get_sockets_by_account(GetReq, [], Contexts) ->
    success_resp(GetReq, to_resp_data(Contexts));
get_sockets_by_account(GetReq, ['undefined' | IDs], Contexts) ->
    get_sockets_by_account(GetReq, IDs, Contexts);
get_sockets_by_account(GetReq, [AccountId | IDs], Contexts) ->
    case get_contexts_by_account_id(AccountId) of
        {'error', 'not_found'} ->
            lager:info("failed to find contexts for account ~s", [AccountId]),
            get_sockets_by_account(GetReq, IDs, Contexts);
        AccountContexts ->
            UpdatedContexts = lists:foldl(fun maybe_add_context/2, Contexts, AccountContexts),
            get_sockets_by_account(GetReq, IDs, UpdatedContexts)
    end.

%% filter out duplicate (if any) and add context
maybe_add_context(Context, Contexts) ->
    SessionId = bh_context:websocket_session_id(Context),
    [Context |
     [C || C <- Contexts,
           SessionId =/= bh_context:websocket_session_id(C)
     ]
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_resp_data([bh_context:context()] | bh_context:context()) -> kz_json:object() | kz_json:objects().
to_resp_data(Contexts) when is_list(Contexts) ->
    lager:info("found ~p sockets", [length(Contexts)]),
    [to_resp_data(Context) || Context <- Contexts];
to_resp_data(Context) ->
    ToDelete = [<<"account_id">>, <<"auth_token">>, <<"req_id">>, <<"auth_account_id">>],
    kz_json:delete_keys(ToDelete, bh_context:to_json(Context)).

error_resp(GetReq) ->
    Resp = [{<<"Data">>, []}
           ,{<<"Msg-ID">>, kz_api:msg_id(GetReq)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    RespQ = kz_api:server_id(GetReq),
    kapi_websockets:publish_get_resp(RespQ, Resp).

-spec success_resp(kz_json:object(), kz_json:object() | kz_json:objects()) -> 'ok'.
success_resp(GetReq, Data) ->
    RespQ = kz_api:server_id(GetReq),
    Resp = [{<<"Data">>, Data}
           ,{<<"Msg-ID">>, kz_api:msg_id(GetReq)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_websockets:publish_get_resp(RespQ, Resp).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_socket(bh_context:context()) -> bh_context:context().
add_socket(Context) ->
    'true' = ets:insert(?SERVER, Context),
    gen_listener:cast(?SERVER, {'monitor', bh_context:websocket_session_id(Context), bh_context:websocket_pid(Context)}),
    Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_socket(bh_context:context() | kz_term:ne_binary()) -> bh_context:context() | 'true'.
remove_socket(<<SessionId/binary>>) ->
    'true' = ets:delete(?SERVER, SessionId);
remove_socket(Context) ->
    'true' = ets:delete(?SERVER, bh_context:websocket_session_id(Context)),
    gen_listener:cast(?SERVER, {'demonitor', bh_context:websocket_pid(Context)}),
    Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_socket(bh_context:context()) -> bh_context:context().
update_socket(Context) ->
    'true' = ets:insert(?SERVER, Context),
    Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_contexts() -> [bh_context:context()].
get_contexts() ->
    ets:tab2list(?SERVER).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_contexts_by_account_id(kz_term:ne_binary()) ->
          [bh_context:context(),...] |
          {'error', 'not_found'}.
get_contexts_by_account_id(<<AccountId/binary>>) ->
    Pattern = bh_context:match_auth_account_id(AccountId),
    case ets:match_object(?SERVER, Pattern) of
        [] -> {'error', 'not_found'};
        Contexts -> Contexts
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_context_by_session_id(kz_term:ne_binary()) ->
          bh_context:context() |
          {'error', 'not_found'}.
get_context_by_session_id(<<SessionId/binary>>) ->
    case ets:lookup(?SERVER, SessionId) of
        [Context] -> Context;
        _ -> {'error', 'not_found'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_contexts_by_ip(kz_term:ne_binary() | inet:ip_addresS()) ->
          [bh_context:context(),...] |
          {'error', 'not_found'}.
get_contexts_by_ip(<<IP/binary>>) ->
    Pattern = bh_context:match_source(IP),
    case ets:match_object(?SERVER, Pattern) of
        [] -> {'error', 'not_found'};
        Contexts -> Contexts
    end;
get_contexts_by_ip(IPAddr) ->
    get_contexts_by_ip(kz_network_utils:iptuple_to_binary(IPAddr)).

-spec session_count_by_ip(kz_term:ne_binary() | inet:ip_address()) -> non_neg_integer().
session_count_by_ip(<<IP/binary>>) ->
    Pattern = [{bh_context:match_source(IP), [], ['true']}],
    ets:select_count(?SERVER, Pattern);
session_count_by_ip(IPAddr) ->
    session_count_by_ip(kz_network_utils:iptuple_to_binary(IPAddr)).

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    lager:debug("starting new ~s server", [?SERVER]),
    _ = ets:new(?SERVER, ['set'
                         ,'public'
                         ,'named_table'
                         ,{'keypos', bh_context:id_position()}
                         ]),
    _ = blackhole_bindings:bind(<<"blackhole.session.open">>, ?MODULE, 'add_socket'),
    _ = blackhole_bindings:bind(<<"blackhole.session.close">>, ?MODULE, 'remove_socket'),
    _ = blackhole_bindings:bind(<<"blackhole.finish.*">>, ?MODULE, 'update_socket'),
    {'ok', []}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'monitor', SessionId, Pid}, State) ->
    Ref = monitor('process', Pid),
    {'noreply', [{Pid, Ref, SessionId} | State]};
handle_cast({'demonitor', Pid}, State) ->
    {'noreply', maybe_update_state(Pid, 1, State)};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', Ref, 'process', _Pid, _Reason}, State) ->
    lager:debug("process ~p down: ~p", [_Pid, _Reason]),
    {'noreply', maybe_update_state(Ref, 2, State)};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

maybe_update_state(Value, Position, State) ->
    case lists:keytake(Value, Position, State) of
        'false' -> State;
        {'value', {_Pid, Ref, SessionId}, NewState} ->
            _ = remove_socket(SessionId),
            demonitor(Ref, ['flush']),
            NewState
    end.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    blackhole_bindings:unbind(<<"blackhole.session.open">>, ?MODULE, 'add_socket'),
    blackhole_bindings:unbind(<<"blackhole.session.close">>, ?MODULE, 'remove_socket'),
    blackhole_bindings:unbind(<<"blackhole.finish.*">>, ?MODULE, 'update_socket'),
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
