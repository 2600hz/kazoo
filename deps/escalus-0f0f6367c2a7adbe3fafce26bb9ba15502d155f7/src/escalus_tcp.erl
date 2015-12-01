%%%===================================================================
%%% @copyright (C) 2011-2012, Erlang Solutions Ltd.
%%% @doc Module abstracting TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(escalus_tcp).
-behaviour(gen_server).
-behaviour(escalus_connection).

-include_lib("exml/include/exml_stream.hrl").
-include("escalus.hrl").

%% API exports
-export([connect/1,
         send/2,
         is_connected/1,
         upgrade_to_tls/2,
         use_zlib/2,
         get_transport/1,
         reset_parser/1,
         get_sm_h/1,
         set_sm_h/2,
         set_filter_predicate/2,
         stop/1,
         kill/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Low level API
-export([get_active/1,
         set_active/2,
         recv/1]).

-ifdef(EUNIT_TEST).
-compile(export_all).
-endif.

%% Stream management automation
%%               :: {Auto-ack?,                H,         counting Hs?}.
-type sm_state() :: {boolean(), non_neg_integer(), 'active'|'inactive'}.

-export_type([sm_state/0]).

-define(WAIT_FOR_SOCKET_CLOSE_TIMEOUT, 200).
-define(SERVER, ?MODULE).
-include("escalus_tcp.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec connect([proplists:property()]) -> {ok, #client{}}.
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Args, self()], []),
    Transport = gen_server:call(Pid, get_transport),
    {ok, Transport}.

send(#client{rcv_pid = Pid} = Client, Elem) ->
    gen_server:cast(Pid, {send, Client, Elem}).

is_connected(#client{rcv_pid = Pid}) ->
    erlang:is_process_alive(Pid).

reset_parser(#client{rcv_pid = Pid}) ->
    gen_server:cast(Pid, reset_parser).

get_sm_h(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_sm_h).

set_sm_h(#client{rcv_pid = Pid}, H) ->
    gen_server:call(Pid, {set_sm_h, H}).

-spec set_filter_predicate(escalus_connection:client(),
                           escalus_connection:filter_pred()) -> ok.
set_filter_predicate(#client{rcv_pid = Pid}, Pred) ->
    gen_server:call(Pid, {set_filter_pred, Pred}).

stop(#client{rcv_pid = Pid}) ->
    try
        gen_server:call(Pid, stop)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            already_stopped
    end.

kill(#client{rcv_pid = Pid}) ->
    %% Use `kill_connection` to avoid confusion with exit reason `kill`.
    gen_server:call(Pid, kill_connection).

upgrade_to_tls(#client{socket = Socket, rcv_pid = Pid} = Client, Props) ->
    Starttls = escalus_stanza:starttls(),
    gen_tcp:send(Socket, exml:to_iolist(Starttls)),
    escalus_connection:get_stanza(Client, proceed),
    SSLOpts = proplists:get_value(ssl_opts, Props, []),
    case gen_server:call(Pid, {upgrade_to_tls, SSLOpts}) of
        {error, Error} ->
            error(Error);
        _ ->
            Client2 = get_transport(Client),
            {Props2, _} = escalus_session:start_stream(Client2, Props),
            {Client2, Props2}
    end.

use_zlib(#client{rcv_pid = Pid} = Client, Props) ->
    escalus_connection:send(Client, escalus_stanza:compress(<<"zlib">>)),
    Compressed = escalus_connection:get_stanza(Client, compressed),
    escalus:assert(is_compressed, Compressed),
    gen_server:call(Pid, use_zlib),
    Client1 = get_transport(Client),
    {Props2, _} = escalus_session:start_stream(Client1, Props),
    {Client1, Props2}.

get_transport(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_transport).

%%%===================================================================
%%% Low level API
%%%===================================================================

get_active(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, get_active).

set_active(#client{rcv_pid = Pid}, Active) ->
    gen_server:call(Pid, {set_active, Active}).

-spec recv(#client{}) -> exml_stream:element() | empty.
recv(#client{rcv_pid = Pid}) ->
    gen_server:call(Pid, recv).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Args, Owner]) ->
    Host = proplists:get_value(host, Args, <<"localhost">>),
    Port = proplists:get_value(port, Args, 5222),

    Address = host_to_inet(Host),
    EventClient = proplists:get_value(event_client, Args),
    Interface = proplists:get_value(iface, Args),
    IsSSLConnection = proplists:get_value(ssl, Args, false),

    OnReplyFun = proplists:get_value(on_reply, Args, fun(_) -> ok end),
    OnRequestFun = proplists:get_value(on_request, Args, fun(_) -> ok end),
    OnConnectFun = proplists:get_value(on_connect, Args, fun(_) -> ok end),

    SM = case {proplists:get_value(stream_management, Args, false),
               proplists:get_value(manual_ack, Args, false)}
         of
             {false,_}          -> {false, 0, inactive};
             {_, true}          -> {false, 0, inactive};
             {true,false}       -> {true, 0, inactive}
         end,


    BasicOpts = [binary, {active, once}],
    SocketOpts = case Interface of
                     undefined -> BasicOpts;
                     _         -> [{ip, iface_to_ip_address(Interface)}] ++ BasicOpts
                 end,

    {ok, Socket} = do_connect(IsSSLConnection, Address, Port, Args,
                              SocketOpts, OnConnectFun),
    {ok, Parser} = exml_stream:new_parser(),
    {ok, #state{owner = Owner,
                socket = Socket,
                parser = Parser,
                ssl = IsSSLConnection,
                sm_state = SM,
                event_client = EventClient,
                on_reply = OnReplyFun,
                on_request = OnRequestFun}}.

handle_call(get_sm_h, _From, #state{sm_state = {_, H, _}} = State) ->
    {reply, H, State};
handle_call({set_sm_h, H}, _From, #state{sm_state = {A, _OldH, S}} = State) ->
    NewState = State#state{sm_state={A, H, S}},
    {reply, {ok, H}, NewState};
handle_call(get_transport, _From, State) ->
    {reply, transport(State), State};
handle_call({upgrade_to_tls, SSLOpts}, _From, #state{socket = Socket} = State) ->
    SSLOpts1 = [{reuse_sessions, true}],
    SSLOpts2 = lists:keymerge(1, lists:keysort(1, SSLOpts),
                              lists:keysort(1, SSLOpts1)),
    case ssl:connect(Socket, SSLOpts2) of
        {ok, Socket2} ->
            {ok, Parser} = exml_stream:new_parser(),
            {reply, Socket2,
             State#state{socket = Socket2, parser = Parser, ssl=true}};
        {error, closed} = E ->
            {reply, E, State}
    end;
handle_call(use_zlib, _, #state{parser = Parser, socket = Socket} = State) ->
    Zin = zlib:open(),
    Zout = zlib:open(),
    ok = zlib:inflateInit(Zin),
    ok = zlib:deflateInit(Zout),
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {reply, Socket, State#state{parser = NewParser,
                                compress = {zlib, {Zin,Zout}}}};
handle_call(get_active, _From, #state{active = Active} = State) ->
    {reply, Active, State};
handle_call({set_active, Active}, _From, State) ->
    {reply, ok, State#state{active = Active}};
handle_call({set_filter_pred, Pred}, _From, State) ->
    {reply, ok, State#state{filter_pred = Pred}};
handle_call(recv, _From, State) ->
    {Reply, NS} = handle_recv(State),
    {reply, Reply, NS};
handle_call(kill_connection, _, #state{socket = Socket } = S) ->
    gen_tcp:close(Socket),
    close_compression_streams(S#state.compress),
    {stop, normal, ok, S};
handle_call(stop, _From, #state{} = S) ->
    send_stream_end(S),
    close_compression_streams(S#state.compress),
    wait_until_closed(S#state.socket),
    {stop, normal, ok, S}.

handle_cast({send, #client{socket = Socket, ssl = Ssl, compress = Compress},
             Elem}, #state{on_request = OnRequestFun} = State) ->
    Reply = case {Ssl, Compress} of
                {true, _} ->
                    ssl:send(Socket, exml:to_iolist(Elem));
                {false, {zlib, {_,Zout}}} ->
                    Deflated = zlib:deflate(Zout, exml:to_iolist(Elem), sync),
                    gen_tcp:send(State#state.socket, Deflated);
                {false, false} ->
                    gen_tcp:send(Socket, exml:to_iolist(Elem))
            end,
    OnRequestFun(Reply),
    {noreply, State};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, Data}, State) ->
    inet:setopts(Socket, [{active, once}]),
    NewState = handle_data(Socket, Data, State),
    {noreply, NewState};
handle_info({ssl, Socket, Data}, State) ->
    ssl:setopts(Socket, [{active, once}]),
    NewState = handle_data(Socket, Data, State),
    {noreply, NewState};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket, ssl = true} = State) ->
    common_terminate(_Reason, State),
    ssl:close(Socket);
terminate(_Reason, #state{socket = Socket} = State) ->
    common_terminate(_Reason, State),
    gen_tcp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Helpers
%%%===================================================================
handle_data(Socket, Data, #state{parser = Parser,
                                 socket = Socket,
                                 compress = Compress,
                                 on_reply = OnReplyFun} = State) ->
    OnReplyFun({erlang:byte_size(Data)}),
    {ok, NewParser, Stanzas} =
        case Compress of
            false ->
                exml_stream:parse(Parser, Data);
            {zlib, {Zin,_}} ->
                Decompressed = iolist_to_binary(zlib:inflate(Zin, Data)),
                exml_stream:parse(Parser, Decompressed)
        end,
    NewState = State#state{parser = NewParser},
    case State#state.active of
        true ->
            escalus_connection:maybe_forward_to_owner(NewState#state.filter_pred,
                                                      NewState, Stanzas,
                                                      fun forward_to_owner/2);
        false ->
            store_reply(Stanzas, NewState)
    end.


forward_to_owner(Stanzas0, #state{owner = Owner,
                                  sm_state = SM0,
                                  event_client = EventClient} = State) ->
    {SM1, AckRequests, StanzasNoRs} = separate_ack_requests(SM0, Stanzas0),
    reply_to_ack_requests(SM1, AckRequests, State),
    NewState = State#state{sm_state=SM1},

    lists:foreach(fun(Stanza) ->
        escalus_event:incoming_stanza(EventClient, Stanza),
        Owner ! {stanza, transport(NewState), Stanza}
    end, StanzasNoRs),

    case lists:keyfind(xmlstreamend, 1, StanzasNoRs) of
        false -> ok;
        _     -> gen_server:cast(self(), stop)
    end,
    NewState#state{replies = StanzasNoRs}.

store_reply(Stanzas, #state{replies = Replies} = S) ->
    S#state{replies = Replies ++ Stanzas}.

%% @doc this looks like it's only used in esl/escalus_tests bosh_SUITE
%% Maybe it can be removed?
handle_recv(#state{replies = []} = S) ->
    {empty, S};
handle_recv(#state{replies = [Reply | Replies]} = S) ->
    case Reply of
        #xmlstreamend{} ->
            gen_server:cast(self(), stop);
        _ -> ok
    end,
    {Reply, S#state{replies = Replies}}.

separate_ack_requests({false, H0, A}, Stanzas) ->
    %% Don't keep track of H
    {{false, H0, A}, [], Stanzas};
separate_ack_requests({true, H0, inactive}, Stanzas) ->
    Enabled = [ S || S <- Stanzas, escalus_pred:is_sm_enabled(S)],
    Resumed = [ S || S <- Stanzas, escalus_pred:is_sm_resumed(S)],

    case {length(Enabled),length(Resumed)} of
        %% Enabled SM: set the H param to 0 and activate counter.
        {1,0} -> {{true, 0, active}, [], Stanzas};

        %% Resumed SM: keep the H param and activate counter.
        {0,1} -> {{true, H0, active}, [], Stanzas};

        %% No new SM state: continue as usual
        {0,0} -> {{true, H0, inactive}, [], Stanzas}
    end;
separate_ack_requests({true, H0, active}, Stanzas) ->
    %% Count H and construct appropriate acks
    F = fun(Stanza, {H, Acks, NonAckRequests}) ->
                case escalus_pred:is_sm_ack_request(Stanza) of
                    true -> {H, [make_ack(H)|Acks], NonAckRequests};
                    false -> {H+1, Acks, [Stanza|NonAckRequests]}
                end
        end,
    {H, Acks, Others} = lists:foldl(F, {H0, [], []}, Stanzas),
    {{true, H, active}, lists:reverse(Acks), lists:reverse(Others)}.

make_ack(H) -> {escalus_stanza:sm_ack(H), H}.

reply_to_ack_requests({false,H,A}, _, _) -> {false, H, A};
reply_to_ack_requests({true,H,inactive}, _, _) -> {true, H, inactive};
reply_to_ack_requests({true, H0, active}, Acks, State) ->
    {true,
     lists:foldl(fun({Ack,H}, _) -> raw_send(State, Ack), H end,
                 H0, Acks),
     active}.

raw_send(#state{socket=Socket, ssl=true}, Elem) ->
    ssl:send(Socket, exml:to_iolist(Elem));
raw_send(#state{socket=_Socket, compress=true}, _Elem) ->
    throw({escalus_tcp, auto_ack_not_implemented_for_compressed_streams});
raw_send(#state{socket=Socket}, Elem) ->
    gen_tcp:send(Socket, exml:to_iolist(Elem)).

common_terminate(_Reason, #state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

transport(#state{socket = Socket,
                 ssl = Ssl,
                 compress = Compress,
                 event_client = EventClient}) ->
    #client{module = ?MODULE,
               rcv_pid = self(),
               socket = Socket,
               ssl = Ssl,
               compress = Compress,
               event_client = EventClient}.

wait_until_closed(Socket) ->
    receive
        {tcp_closed, Socket} ->
            ok
    after ?WAIT_FOR_SOCKET_CLOSE_TIMEOUT ->
            ok
    end.

-spec host_to_inet(tuple() | atom() | list() | binary())
    -> inet:ip_address() | inet:hostname().
host_to_inet({_,_,_,_} = IP4) -> IP4;
host_to_inet({_,_,_,_,_,_,_,_} = IP6) -> IP6;
host_to_inet(Address) when is_list(Address) orelse is_atom(Address) -> Address;
host_to_inet(BAddress) when is_binary(BAddress) -> binary_to_list(BAddress).

iface_to_ip_address({_,_,_,_} = IP4) -> IP4;
iface_to_ip_address({_,_,_,_,_,_,_,_} = IP6) -> IP6.

close_compression_streams(false) ->
    ok;
close_compression_streams({zlib, {Zin, Zout}}) ->
    try
        ok = zlib:inflateEnd(Zin),
        ok = zlib:deflateEnd(Zout)
    catch
        error:data_error -> ok
    after
        ok = zlib:close(Zin),
        ok = zlib:close(Zout)
    end.

send_stream_end(#state{socket = Socket, ssl = Ssl, compress = Compress}) ->
    StreamEnd = escalus_stanza:stream_end(),
    case {Ssl, Compress} of
        {true, _} ->
            ssl:send(Socket, exml:to_iolist(StreamEnd));
        {false, {zlib, {_, Zout}}} ->
            gen_tcp:send(Socket, zlib:deflate(Zout,
                                              exml:to_iolist(StreamEnd),
                                              finish));
        {false, false} ->
            gen_tcp:send(Socket, exml:to_iolist(StreamEnd))
    end.

do_connect(IsSSLConnection, Address, Port, Args, SocketOpts, OnConnectFun) ->
    TimeB = os:timestamp(),
    Reply = maybe_ssl_connection(IsSSLConnection, Address, Port, SocketOpts, Args),
    TimeA = os:timestamp(),
    ConnectionTime = timer:now_diff(TimeA, TimeB),
    case Reply of
        {ok, Socket} ->
            OnConnectFun({ok, Socket, ConnectionTime});
        {error, _} ->
            OnConnectFun(Reply)
    end,
    Reply.

maybe_ssl_connection(true, Address, Port, Opts, Args) ->
    SSLOpts = proplists:get_value(ssl_opts, Args, []),
    ssl:connect(Address, Port, Opts ++ SSLOpts);
maybe_ssl_connection(_, Address, Port, Opts, _) ->
    gen_tcp:connect(Address, Port, Opts).
