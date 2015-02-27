%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_parser_fs).

-behaviour(gen_server).

-include("../call_inspector.hrl").

%% API
-export([start_link/0
         ,open_logfile/1
         ,start_parsing/0
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-record(state, {logfile :: file:name()
               ,iodevice :: file:io_device()}).
-type state() :: #state{}.

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================


open_logfile(Filename) ->
    gen_server:cast(?MODULE, {open_logfile, Filename}).


start_parsing() ->
    gen_server:cast(?MODULE, start_parsing).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
    {'ok', #state{}}.

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
-spec handle_call(atom(), any(), state()) -> handle_call_ret().
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
handle_cast({open_logfile, LogFile}, State) ->
    {ok, IoDevice} = file:open(LogFile, [read,raw,binary,read_ahead]),%read+append??
    NewState = State#state{logfile = LogFile, iodevice = IoDevice},
    {noreply, NewState};
handle_cast(start_parsing, State=#state{iodevice = IoDevice}) ->
    case extract_chunk(IoDevice) of
        [] -> ok;
        Data ->
            Chunk = ci_chunk:set_data(ci_chunk:new(), Data),
            CallId = extract_callid(Data),
            ci_datastore:put(CallId, Chunk)
    end,
    {noreply, State};
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
terminate(_Reason, #state{iodevice = IoDevice}) ->
    ok = file:close(IoDevice),
    lager:debug("call inspector freeswitch parser terminated: ~p", [_Reason]),
    'ok'.

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

extract_chunk(Dev) ->
    extract_chunk(Dev, []).

extract_chunk(Dev, Buffer) ->
    case file:read_line(Dev) of
        eof -> [];
        {ok, Line} ->
            case {Line,Buffer} of
                {<<"recv ", _/binary>>
                ,[]} ->
                    %% Start of a new chunk
                    extract_chunk(Dev, [Line]);
                {<<"send ", _/binary>>
                ,[]} ->
                    %% Start of a new chunk
                    extract_chunk(Dev, [Line]);
                {<<"   ------------------------------------------------------------------------\n">>
                ,Acc=[_]} ->
                    %% Second line of a chunk (special case given end of chunk)
                    extract_chunk(Dev, Acc);
                {<<"   ------------------------------------------------------------------------\n">>
                ,_} ->
                    %% End of current chunk
                    lists:reverse(Buffer);
                {_
                ,Acc} when Acc =/= [] ->
                    %% Between start and end of chunk
                    extract_chunk(Dev, [Line|Acc]);
                {_
                ,_} ->
                    %% Skip over the rest
                    extract_chunk(Dev, Buffer)
            end
    end.


extract_callid([Data|Rest]) ->
    case Data of
        <<"   Call-ID: ", CallId/binary>> ->
            CallId;
        _ ->
            extract_callid(Rest)
    end.
