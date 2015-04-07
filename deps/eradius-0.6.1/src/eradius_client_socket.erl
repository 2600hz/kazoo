
-module(eradius_client_socket).

-behaviour(gen_server).

-export([start/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {client, socket, pending, mode, counter}).

start(SocketIP, Client, PortIdx) ->
    gen_server:start_link(?MODULE, [SocketIP, Client, PortIdx], []).

init([SocketIP, Client, PortIdx]) ->
    Client ! {PortIdx, self()},
    case SocketIP of
        undefined ->
            ExtraOptions = [];
        SocketIP when is_tuple(SocketIP) ->
            ExtraOptions = [{ip, SocketIP}]
    end,
    {ok, Socket} = gen_udp:open(0, [{active, once}, binary | ExtraOptions]),
    Pending = dict:new(),
    {ok, #state{client = Client, socket = Socket, pending = Pending, mode = active, counter = 0}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({SenderPid, send_request, {IP, Port}, ReqId, EncRequest},
        State = #state{socket = Socket, pending = Pending, counter = Counter}) ->
    case gen_udp:send(Socket, IP, Port, EncRequest) of
        ok ->
            ReqKey = {IP, Port, ReqId},
            NPending = dict:store(ReqKey, SenderPid, Pending),
            {noreply, State#state{pending = NPending, counter = Counter+1}};
        {error, Reason} ->
            SenderPid ! {error, Reason},
            {noreply, State}
    end;

handle_info({udp, Socket, FromIP, FromPort, EncRequest},
        State = #state{socket = Socket, pending = Pending, mode = Mode, counter = Counter}) ->
    case eradius_lib:decode_request_id(EncRequest) of
        {ReqId, EncRequest} ->
            case dict:find({FromIP, FromPort, ReqId}, Pending) of
                error ->
                    %% discard reply because we didn't expect it
                    inet:setopts(Socket, [{active, once}]),
                    {noreply, State};
                {ok, WaitingSender} ->
                    WaitingSender ! {self(), response, ReqId, EncRequest},
                    inet:setopts(Socket, [{active, once}]),
                    NPending = dict:erase({FromIP, FromPort, ReqId}, Pending),
                    NState = State#state{pending = NPending, counter = Counter-1},
                    case {Mode, Counter-1} of
                        {inactive, 0}   -> {stop, normal, NState};
                        _               -> {noreply, NState}
                    end
            end;
        bad_pdu ->
            %% discard reply because it was malformed
            inet:setopts(Socket, [{active, once}]),
            {noreply, State}
    end;

handle_info(close, State = #state{counter = Counter}) ->
    case Counter of
        0   -> {stop, normal, State};
        _   -> {noreply, State#state{mode = inactive}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

