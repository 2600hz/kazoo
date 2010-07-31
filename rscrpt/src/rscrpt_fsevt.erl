%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Reports the freeSWITCH resource status to a queue.
%%% Listens on the fS Event Socket for events, parses them, and sends
%%% a well-formed payload to the resources queue.
%%% @end
%%% Created : 26 Jul 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(rscrpt_fsevt).

-behaviour(gen_server).

-compile(export_all).

-import(rscrpt_logger, [log/2, format_log/3]).

%% API
-export([start_link/0, stop/0, send_cmd/1, add_event/1, del_event/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_EVENTS, ["HEARTBEAT"]).

-record(state, {fs_sock, events=[], active_channels=0}).

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

stop() ->
    gen_server:cast(?MODULE, stop).

add_event(Evt) ->
    gen_server:call(?MODULE, {add_event, Evt}).

del_event(Evt) ->
    gen_server:call(?MODULE, {del_event, Evt}).

send_cmd(Cmd) ->
    gen_server:call(?MODULE, {send_cmd, Cmd}).

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
    log(info, "Starting FS Event Socket Listener"),
    Hostname = "localhost",
    Port = 8021,
    Auth = "ClueCon",

    Socket = case gen_tcp:connect(Hostname, Port, [list, {active, false}]) of
		 {ok, FsSock} ->
		     inet:setopts(FsSock, [{packet, line}]),
		     log(info, ["Opened FreeSWITCH event socket to ", Hostname]),
		     ok = gen_tcp:send(FsSock, lists:concat(["auth ", Auth, "\n\n"])),
		     ok = gen_tcp:send(FsSock, "events plain all\n\n"),
		     lists:foreach(fun(Evt) ->
					   gen_tcp:send(FsSock, lists:concat(["filter Event-Name ", Evt, "\n\n"]))
				   end, ?DEFAULT_EVENTS),
		     spawn(fun() -> reader_loop(FsSock, [], 0) end),
		     FsSock;
		 {error, Reason} ->
		     format_log(error, "Unable to open socket: ~p~n", [Reason]),
		     undefined
	     end,
    {ok, #state{fs_sock=Socket, events=?DEFAULT_EVENTS}}.

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
handle_call(_Call, _From, #state{fs_sock=undefined}=State) ->
    {reply, socket_not_up, State};
handle_call({add_event, Evt}, _From, #state{events=Es}=State) ->
    rscrpt_fsevt:send_cmd(lists:concat(["filter Event-Name ", Evt])),
    {reply, ok, State#state{events=[Evt|Es]}};
handle_call({del_event, Evt}, _From, #state{events=Es}=State) ->
    rscrpt_fsevt:send_cmd(lists:concat(["filter delete Event-Name ", Evt])),
    {reply, ok, State#state{events=lists:delete(Evt,Es)}};
handle_call({send_cmd, Cmd}, _From, #state{fs_sock=FsSock}=State) ->
    format_log(info, "Sending cmd: ~p~n", [Cmd]),
    ok = gen_tcp:send(FsSock, lists:concat([Cmd, "\n\n"])),
    {reply, ok, State};
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
handle_cast({process_event, Evt}, #state{events=Es, active_channels=Active}=State) ->
    State1 = State#state{active_channels=update_active(Active, Evt)},
    case lists:member(proplists:get_value(event_name, Evt), Es) of
	true -> rscrpt_reporter:send_report(
		  [{active_channels, State1#state.active_channels}
		   ,{resource_types, [message]}
		   | Evt]
		 );
	false -> ok
    end,
    {noreply, State1};
handle_cast(stop, #state{fs_sock=FsSock}=State) ->
    gen_tcp:close(FsSock),
    {stop, normal, State};
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
handle_info(_Info, State) ->
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
terminate(_Reason, #state{fs_sock=FsSock}) ->
    gen_tcp:close(FsSock),
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
reader_loop(Socket, Headers, ContentLength) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, "\n"} ->
            %% End of message. Parse
            %log(info, ["COMPLETE HEADER: ", Headers]),

            case ContentLength > 0 of
		true ->
		    inet:setopts(Socket, [{packet, raw}]),
		    {ok, Body} = gen_tcp:recv(Socket, ContentLength),
		    inet:setopts(Socket, [{packet, line}]),
		    %log(info, [" COMPLETE DATA: ", Body]),
		    parse_data(proplists:get_value("Content-Type", Headers), Headers, Body);
		_ -> ok
            end,

            reader_loop(Socket, [], 0);
        {ok, Data} ->
            log(info, ["Received a line: ", Data]),

            %% Parse the line
	    KV = split(Data),
	    {K, V} = KV,

            %% Is this a content-length string? If so, we'll need to gather extra data later
            case K =:= "Content-Length" of
		true ->
		    Length = list_to_integer(V);
		_ ->
		    Length = ContentLength
            end,

            reader_loop(Socket, [KV | Headers], Length);
        {error, closed} ->
            ok
    end.

parse_data("text/event-plain", _Headers, Body) ->
    process_event(event_to_proplist(Body));
parse_data("command/reply", Headers, Body) ->
    format_log(info, "Command reply: ~p~nCommand data: ~p~n", [Headers, Body]);
parse_data("api/response", Headers, Body) ->
    format_log(info, "API response: ~p~nAPI data: ~p~n", [Headers, Body]);
parse_data(ContentType, _Headers, _Body) ->
    format_log(warning, "Unused Content-Type: ~p~n", [ContentType]).


event_to_proplist(Str) ->
    L = string:tokens(Str, "\n"),
    lists:map(fun(S) -> [K, V0] = string:tokens(S, ":"),
			V1 = string:strip(V0, left, $ ),
			{V2, []} = rscrpt_mochiweb_util:parse_qs_value(V1),
			{K, V2}
	      end, L).

process_event(Evt) ->
    format_log(info, "Event-Name: ~p~n", [proplists:get_value("Event-Name", Evt)]),
    gen_server:cast(?MODULE, {process_event, format_event(Evt, [])}).

%% make keys we want atoms
format_event([], Msg) -> lists:reverse(Msg);
format_event([{"Event-Name", V} | KVs], Msg) ->
    format_event(KVs, [{event_name, V} | Msg]);
format_event([{"Core-UUID", V} | KVs], Msg) ->
    format_event(KVs, [{core_uuid, V} | Msg]);
format_event([{"Event-Date-GMT", V} | KVs], Msg) ->
    format_event(KVs, [{event_date_gmt, V} | Msg]);
format_event([{"Caller-Username", V} | KVs], Msg) ->
    format_event(KVs, [{caller_username, V} | Msg]);
format_event([_|KVs], Msg) ->
    format_event(KVs, Msg).

%% Takes K: V\n and returns {K, V}
split(Data) ->
    [K | V] = string:tokens(Data, ": "),
    V1 = case length(V) of
	     0 -> "";
	     1 -> string:strip(hd(V), right, $\n);
	     _ -> lists:map(fun(S) -> string:strip(S, right, $\n) end, V)
	 end,
    {K, V1}.

update_active(Active, Evt) ->
    update_count(proplists:get_value(event_name, Evt), Active).

update_count("CHANNEL_CREATE", Active) -> Active+1;
update_count("CHANNEL_DESTROY", Active) -> Active-1;
update_count(_, Active) -> Active.
