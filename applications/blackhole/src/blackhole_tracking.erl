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
        ,get_sockets/1
        ,get_socket/1
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

-type state() :: ets:tid() | atom().

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
handle_req(ApiJObj, _Props) ->
    'true' = kapi_websockets:get_req_v(ApiJObj),
    kz_log:put_callid(ApiJObj),

    Node = kz_json:get_binary_value(<<"Node">>, ApiJObj),
    RespData =
        handle_get_req_data(kz_json:get_value(<<"Account-ID">>, ApiJObj)
                           ,kz_json:get_value(<<"Socket-ID">>, ApiJObj)
                           ,Node
                           ),
    case RespData of
        'ok' -> 'ok';
        RespData ->
            RespQ = kz_json:get_value(<<"Server-ID">>, ApiJObj),
            Resp = [{<<"Data">>, RespData}
                   ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, ApiJObj)}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            lager:debug("sending reply ~p to ~s",[RespData, Node]),
            kapi_websockets:publish_get_resp(RespQ, Resp)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_socket(bh_context:context()) -> 'ok'.
add_socket(Context) ->
    gen_server:cast(?SERVER, {'add_socket', Context}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_socket(bh_context:context()) -> 'ok'.
remove_socket(Context) ->
    gen_server:cast(?SERVER, {'remove_socket', Context}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_socket(bh_context:context()) -> 'ok'.
update_socket(Context) ->
    gen_server:cast(?SERVER, {'update_socket', Context}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_sockets(kz_term:ne_binary()) -> [bh_context:context(), ...] | {'error', 'not_found'}.
get_sockets(AccountId) ->
    gen_server:call(?SERVER, {'get_sockets', AccountId}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_socket(kz_term:ne_binary()) -> {'ok', bh_context:context()} | {'error', 'not_found'}.
get_socket(Id) ->
    gen_server:call(?SERVER, {'get_socket', Id}).

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    process_flag('trap_exit', 'true'),
    lager:debug("starting new ~s server", [?SERVER]),
    Tab = ets:new(?SERVER, ['set'
                           ,'protected'
                           ,'named_table'
                           ,{'keypos', #bh_context.websocket_session_id}
                           ]),
    _ = blackhole_bindings:bind(<<"blackhole.session.open">>, ?MODULE, 'add_socket'),
    _ = blackhole_bindings:bind(<<"blackhole.session.close">>, ?MODULE, 'remove_socket'),
    _ = blackhole_bindings:bind(<<"blackhole.finish.*">>, ?MODULE, 'update_socket'),
    {'ok', Tab}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'get_sockets', AccountId}, _From, State) ->
    Pattern = #bh_context{auth_account_id=AccountId, _='_'},
    Result =
        case ets:match_object(State, Pattern) of
            [] -> {'error', 'not_found'};
            Contexts -> Contexts
        end,
    {'reply', Result, State};
handle_call({'get_socket', Id}, _From, State) ->
    Pattern = #bh_context{websocket_session_id=Id, _='_'},
    Result =
        case ets:match_object(State, Pattern) of
            [] -> {'error', 'not_found'};
            [Context] ->
                {'ok', Context}
        end,
    {'reply', Result, State};
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'add_socket', Context}, State) ->
    _ = ets:insert(State, Context),
    {noreply, State};
handle_cast({'remove_socket', Context}, State) ->
    _ = ets:delete_object(State, Context),
    {noreply, State};
handle_cast({'update_socket', Context}, State) ->
    _ = ets:insert(State, Context),
    {noreply, State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_get_req_data(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) -> any().
handle_get_req_data('undefined', 'undefined', Node) ->
    lager:warning("received undefined blackhole get req ~s", [Node]);
handle_get_req_data(AccountId, 'undefined', Node) ->
    lager:debug("received blackhole get for account:~s from ~s", [AccountId, Node]),
    case get_sockets(AccountId) of
        {'error', 'not_found'} ->
            lager:debug("no sockets found for ~s", [AccountId]),
            [];
        Contexts ->
            ToDelete = [<<"account_id">>, <<"auth_token">>, <<"req_id">>, <<"auth_account_id">>],
            [kz_json:delete_keys(ToDelete, bh_context:to_json(Context))
             || Context <- Contexts]
    end;
handle_get_req_data('undefined', SocketId, Node) ->
    lager:debug("received blackhole get for socket:~s from ~s", [SocketId, Node]),
    case get_socket(SocketId) of
        {'error', 'not_found'} ->
            lager:debug("socket ~s not found", [SocketId]);
        {'ok', Context} ->
            bh_context:to_json(Context)
    end.
