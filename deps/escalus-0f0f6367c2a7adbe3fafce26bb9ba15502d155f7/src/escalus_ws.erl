%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Module abstracting Websockets over TCP connection to XMPP server
%%% @end
%%%===================================================================

-module(escalus_ws).
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
         stop/1,
         kill/1,
         set_filter_predicate/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(WAIT_FOR_SOCKET_CLOSE_TIMEOUT, 200).
-define(HANDSHAKE_TIMEOUT, 3000).
-define(SERVER, ?MODULE).

-record(state, {owner, socket, parser, legacy_ws, compress = false,
                event_client, filter_pred}).

%%%===================================================================
%%% API
%%%===================================================================

-spec connect([proplists:property()]) -> {ok, escalus:client()}.
connect(Args) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Args, self()], []),
    Transport = gen_server:call(Pid, get_transport),
    {ok, Transport}.

send(#client{rcv_pid = Pid, compress = {zlib, {_, Zout}}}, Elem) ->
    gen_server:cast(Pid, {send_compressed, Zout, Elem});
send(#client{rcv_pid = Pid}, Elem) ->
    gen_server:cast(Pid, {send, exml:to_iolist(Elem)}).

is_connected(#client{rcv_pid = Pid}) ->
    erlang:is_process_alive(Pid).

reset_parser(#client{rcv_pid = Pid}) ->
    gen_server:cast(Pid, reset_parser).

stop(#client{rcv_pid = Pid}) ->
    try
        gen_server:call(Pid, stop)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            already_stopped
    end.

kill(Transport) ->
    error({not_implemented_for, ?MODULE}, [Transport]).

-spec set_filter_predicate(escalus_connection:client(),
    escalus_connection:filter_pred()) -> ok.
set_filter_predicate(#client{rcv_pid = Pid}, Pred) ->
    gen_server:call(Pid, {set_filter_pred, Pred}).


upgrade_to_tls(_, _) ->
    throw(starttls_not_supported).

%% TODO: this is en exact duplicate of escalus_tcp:use_zlib/2, DRY!
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
%%% gen_server callbacks
%%%===================================================================

init([Args, Owner]) ->
    Host = get_host(Args, "localhost"),
    Port = get_port(Args, 5280),
    Resource = get_resource(Args, "/ws-xmpp"),
    LegacyWS = get_legacy_ws(Args, false),
    EventClient = proplists:get_value(event_client, Args),
    SSL = proplists:get_value(ssl, Args, false),
    WSOptions = [{ssl, SSL}],
    {ok, Socket} = wsecli:start(Host, Port, Resource, WSOptions),
    Pid = self(),
    wsecli:on_open(Socket, fun() -> Pid ! opened end),
    wsecli:on_error(Socket, fun(Reason) -> Pid ! {error, Reason} end),
    wsecli:on_message(Socket, fun(Type, Data) -> Pid ! {Type, Data} end),
    wsecli:on_close(Socket, fun(_) -> Pid ! tcp_closed end),
    wait_for_socket_start(),
    ParserOpts = if
                     LegacyWS -> [];
                     true -> [{infinite_stream, true}, {autoreset, true}]
                 end,
    {ok, Parser} = exml_stream:new_parser(ParserOpts),
    {ok, #state{owner = Owner,
                socket = Socket,
                parser = Parser,
                legacy_ws = LegacyWS,
                event_client = EventClient}}.

handle_call(get_transport, _From, State) ->
    {reply, transport(State), State};
handle_call(use_zlib, _, #state{parser = Parser, socket = Socket} = State) ->
    Zin = zlib:open(),
    Zout = zlib:open(),
    ok = zlib:inflateInit(Zin),
    ok = zlib:deflateInit(Zout),
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {reply, Socket, State#state{parser = NewParser,
                                compress = {zlib, {Zin,Zout}}}};
handle_call({set_filter_pred, Pred}, _From, State) ->
    {reply, ok, State#state{filter_pred = Pred}};
handle_call(stop, _From, #state{socket = Socket,
                                compress = Compress} = State) ->
    StreamEnd = if
                    State#state.legacy_ws -> escalus_stanza:stream_end();
                    true -> escalus_stanza:ws_close()
                end,
    case Compress of
        {zlib, {Zin, Zout}} ->
            try
                ok = zlib:inflateEnd(Zin)
            catch
                error:data_error -> ok
            end,
            ok = zlib:close(Zin),
            wsecli:send(Socket, zlib:deflate(Zout,
                                             exml:to_iolist(StreamEnd),
                                             finish)),
            ok = zlib:deflateEnd(Zout),
            ok = zlib:close(Zout);
        false ->
            wsecli:send(Socket, exml:to_iolist(StreamEnd))
    end,
    wait_until_closed(),
    {stop, normal, ok, State}.

handle_cast({send_compressed, Zout, Elem}, State) ->
    wsecli:send(State#state.socket, zlib:deflate(Zout, exml:to_iolist(Elem), sync)),
    {noreply, State};
handle_cast({send, Data}, State) ->
    wsecli:send(State#state.socket, Data),
    {noreply, State};
handle_cast(reset_parser, #state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {noreply, State#state{parser = NewParser}}.

handle_info(tcp_closed, State) ->
    {stop, normal, State};
handle_info({error, Reason}, State) ->
    {stop, Reason, State};
handle_info({text, Data}, State) ->
    handle_data(list_to_binary(lists:flatten(Data)), State);
handle_info({binary, Data}, State) ->
    handle_data(Data, State);
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket} = State) ->
    common_terminate(_Reason, State),
    wsecli:stop(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Helpers
%%%===================================================================

handle_data(Data, State = #state{parser = Parser,
                                 compress = Compress}) ->
    {ok, NewParser, Stanzas} =
        case Compress of
            false ->
                exml_stream:parse(Parser, Data);
            {zlib, {Zin,_}} ->
                Decompressed = iolist_to_binary(zlib:inflate(Zin, Data)),
                exml_stream:parse(Parser, Decompressed)
        end,
    NewState = State#state{parser = NewParser},
    escalus_connection:maybe_forward_to_owner(NewState#state.filter_pred,
                                              NewState,
                                              Stanzas,
                                              fun forward_to_owner/2),
    case lists:filter(fun is_stream_end/1, Stanzas) of
        [] -> {noreply, NewState};
        _ -> {stop, normal, NewState}
    end.

-spec is_stream_end(exml_stream:element()) -> boolean().
is_stream_end(#xmlstreamend{}) -> true;
is_stream_end(_) -> false.

forward_to_owner(Stanzas, #state{owner = Owner,
                                 event_client = EventClient} = NewState) ->
    lists:foreach(fun(Stanza) ->
                          escalus_event:incoming_stanza(EventClient, Stanza),
                          Owner ! {stanza, transport(NewState), Stanza}
                  end, Stanzas).

common_terminate(_Reason, #state{parser = Parser}) ->
    exml_stream:free_parser(Parser).

transport(#state{socket = Socket,
                 compress = Compress,
                 event_client = EventClient}) ->
    #client{module = ?MODULE,
               socket = Socket,
               ssl = undefined,
               compress = Compress,
               rcv_pid = self(),
               event_client = EventClient}.

wait_until_closed() ->
    receive
        tcp_closed ->
            ok
    after ?WAIT_FOR_SOCKET_CLOSE_TIMEOUT ->
            ok
    end.

wait_for_socket_start() ->
    receive
        opened ->
            ok
    after ?HANDSHAKE_TIMEOUT ->
            throw(handshake_timeout)
    end.

-spec get_port(list(), inet:port_number()) -> inet:port_number().
get_port(Args, Default) ->
    get_option(port, Args, Default).

-spec get_host(list(), string()) -> string().
get_host(Args, Default) ->
    maybe_binary_to_list(get_option(host, Args, Default)).

-spec get_resource(list(), string()) -> string().
get_resource(Args, Default) ->
    maybe_binary_to_list(get_option(wspath, Args, Default)).

-spec get_legacy_ws(list(), boolean()) -> boolean().
get_legacy_ws(Args, Default) ->
    get_option(wslegacy, Args, Default).

-spec maybe_binary_to_list(binary() | string()) -> string().
maybe_binary_to_list(B) when is_binary(B) -> binary_to_list(B);
maybe_binary_to_list(S) when is_list(S) -> S.

-spec get_option(any(), list(), any()) -> any().
get_option(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        false -> Default;
        {Key, Value} -> Value
    end.
