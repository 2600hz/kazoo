%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_parser_hep).

-behaviour(gen_server).

-include("../call_inspector.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {socket :: gen_udp:socket()
               ,listen_ip :: ne_binary()
               ,listen_port :: pos_integer()
               }
       ).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(term()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    ServerName = ci_parsers_util:make_name(Args),
    gen_server:start_link({'local', ServerName}, ?MODULE, Args, []).

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
init({'parser_args', IP, Port}) ->
    {'ok', Socket} = gen_udp:open(Port, ['binary'
                                         ,{'active', 'true'}
                                        ]),
    State = #state{socket = Socket
                  ,listen_ip = IP
                  ,listen_port = Port},
    {'ok', State}.

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
-spec handle_call(atom(), _, state()) -> handle_call_ret().
handle_call(_Request, _From, State) ->
    lager:debug("unhandled handle_call executed ~p~p", [_Request, _From]),
    Reply = 'ok',
    {'reply', Reply, State}.

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
    lager:debug("unhandled handle_cast ~p", [_Msg]),
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
handle_info({'udp', _Socket, _IPTuple, _InPortNo, Packet}, State) ->
    {'ok', Hep} = hep:decode(Packet),
    make_and_store_chunk(Hep),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminate
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket = Socket}) ->
    'ok' = gen_udp:close(Socket),
    lager:debug("call inspector kamailio parser terminated: ~p", [_Reason]).

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

make_and_store_chunk(Hep) ->
    Data = binary:split(hep:payload(Hep), <<"\r\n">>, ['global', 'trim']),
    Chunk =
        ci_chunk:setters(ci_chunk:new()
                        , [ {fun ci_chunk:set_data/2, Data}
                          , {fun ci_chunk:set_call_id/2, ci_parser_freeswitch:call_id(Data)}
                          , {fun ci_chunk:set_timestamp/2, ci_parsers_util:timestamp(hep:timestamp(Hep))}
                          , {fun ci_chunk:set_parser/2, ?MODULE}
                          , {fun ci_chunk:set_label/2, hd(Data)}
                          , {fun ci_chunk:set_from/2, ip(hep:src_ip(Hep))}
                          , {fun ci_chunk:set_to/2, ip(hep:dst_ip(Hep))}
                          ]
                        ),
    lager:debug("parsed chunk ~s (~s)", [ci_chunk:call_id(Chunk), ci_parsers_sup:child(self())]),
    ci_datastore:store_chunk(Chunk).

ip(IP) ->
    wh_network_utils:iptuple_to_binary(IP).
