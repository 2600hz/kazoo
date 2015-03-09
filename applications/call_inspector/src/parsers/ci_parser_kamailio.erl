%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_parser_kamailio).

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

-record(state, {logfile :: file:name()
               ,iodevice :: file:io_device()
               ,logip :: ne_binary()
               ,timer :: reference()
               ,counter :: pos_integer()
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
init({'parser_args', LogFile, LogIP}) ->
    NewDev = ci_parsers_util:open_file(LogFile),
    State = #state{logfile = LogFile
                  ,iodevice = NewDev
                  ,logip = LogIP
                  ,counter = 1},
    self() ! 'start_parsing',
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
handle_info('start_parsing', State=#state{iodevice = IoDevice
                                         ,logip = LogIP
                                         ,timer = OldTimer
                                         ,counter = Counter}) ->
    case OldTimer of
        'undefined' -> 'ok';
        _ -> erlang:cancel_timer(OldTimer)
    end,
    NewCounter = extract_chunks(IoDevice, LogIP, Counter),
    NewTimer = erlang:send_after(ci_parsers_util:parse_interval()
                                , self(), 'start_parsing'),
    {'noreply', State#state{timer = NewTimer
                           ,counter = NewCounter}};
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
    'ok' = file:close(IoDevice),
    lager:debug("call inspector kamailio parser terminated: ~p", [_Reason]),
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

extract_chunks(Dev, LogIP, Counter) ->
    case extract_chunk(Dev) of
        [] -> Counter;
        {{'callid',Callid}, Data0} ->
            NewCounter = make_and_store_chunk(LogIP, Callid, Counter, Data0),
            extract_chunks(Dev, LogIP, NewCounter);
        {'buffers', Buffers} ->
            StoreEach =
                fun ({{'callid',Callid}, Data0}, ACounter) ->
                        make_and_store_chunk(LogIP, Callid, ACounter, Data0)
                end,
            lists:foldl(StoreEach, Counter, Buffers)
    end.

make_and_store_chunk(LogIP, Callid, Counter, Data0) ->
    Apply = fun (Fun, Arg) -> Fun(Arg) end,
    {Data, Ts} = cleanse_data_and_get_timestamp(Data0),
    %% Counter is a fallback time ID (for old logfile format)
    {NewCounter, Timestamp} = case Ts of
                                  'undefined' -> {Counter+1, Counter};
                                  _Ts -> {Counter, Ts}
                              end,
    Setters = [fun (C) -> ci_chunk:set_data(C, Data) end
              ,fun (C) -> ci_chunk:set_call_id(C, Callid) end
              ,fun (C) -> ci_chunk:set_timestamp(C, Timestamp) end
              ,fun (C) -> ci_chunk:set_parser(C, ?MODULE) end
              ,fun (C) -> ci_chunk:set_label(C, label(hd(Data))) end
              ,fun (C) -> ci_chunk:set_from(C, from(lists:reverse(Data0),LogIP)) end
              ,fun (C) -> ci_chunk:set_to(C, to(lists:reverse(Data0),LogIP)) end
              ],
    Chunk = lists:foldl(Apply, ci_chunk:new(), Setters),
    lager:debug("parsed kamailio chunk ~s", [ci_chunk:call_id(Chunk)]),
    ci_datastore:store_chunk(Chunk),
    NewCounter.

extract_chunk(Dev) ->
    case file:read_line(Dev) of
        'eof' ->
            dump_buffers();
        {'ok', Line} ->
            case binary:split(Line, <<"|">>) of
                [RawTimestamp, Logged] ->
                    Key = {'callid',callid(Line)},
                    Buffer = get_buffer(Key),
                    acc(rm_newline(Logged), [{RawTimestamp}|Buffer], Dev, Key);
                _Ignore ->
                    extract_chunk(Dev)
            end
    end.

acc(<<"start|",_/binary>>=Logged, Buffer, Dev, Key) ->
    put(Key, [Logged]),
    case Buffer of
        [_RawTimestamp] ->
            %% This is a new chunk, keep buffering
            extract_chunk(Dev);
        _ ->
            %% Return buffered chunk
            {Key, Buffer}
    end;
acc(<<"log|external ",_/binary>>=Logged, Buffer, _Dev, Key) ->
    %% Turn into chunk to make sure consecutive "external ..." don't get ignored
    erase(Key),
    {Key, [Logged|Buffer]};
acc(<<"log|",_/binary>>=Logged, Buffer, Dev, Key) ->
    put(Key, [Logged|Buffer]),
    extract_chunk(Dev);
acc(<<"pass|",_/binary>>=Logged, Buffer, _Dev, Key) ->
    erase(Key),
    {Key, [Logged|Buffer]};
acc(<<"end|",_/binary>>=Logged, Buffer, _Dev, Key) ->
    erase(Key),
    {Key, [Logged|Buffer]};
acc(<<"stop|",_/binary>>=Logged, Buffer, _Dev, Key) ->
    erase(Key),
    {Key, [Logged|Buffer]}.

cleanse_data_and_get_timestamp(Data0) ->
    F =
        fun ({RawTimestamp}, {Acc, TS}) ->
                case ci_parsers_util:timestamp(RawTimestamp) of
                    Ts when Ts < TS ->
                        {Acc, Ts};
                    _Ts ->
                        {Acc, TS}
                end;
            (Bin, {Acc, TS}) ->
                {[unwrap(Bin)|Acc], TS}
        end,
    lists:foldl(F, {[], 'undefined'}, Data0).

get_buffer(Key) ->
    case get(Key) of
        'undefined' -> [];
        Buffer -> Buffer
    end.

dump_buffers() ->
    Buffers = [{Key, Buff} || {{'callid',_}=Key,Buff} <- get()],
    case Buffers of
        [] -> [];
        _ ->
            RmFromProcDict = fun ({Key, _Buffer}) ->
                                     erase(Key)
                             end,
            lists:foreach(RmFromProcDict, Buffers),
            {'buffers', Buffers}
    end.

unwrap(Bin0) ->
    case binary:split(Bin0, <<"|">>) of
        [_Tag, Bin] -> Bin;
        [RawTimestamp] -> RawTimestamp
    end.

rm_newline(Line0) ->
    [Line, <<>>] = binary:split(Line0, <<"\n">>),
    Line.


callid(Line) ->
    {'match', [Callid]} =
        re:run(Line
              ,"<script>: ([^\\s\\|]+)\\|"
              ,[{'capture','all_but_first','binary'}]),
    Callid.

label(<<"recieved internal reply ", Label/binary>>) -> Label;
label(<<"recieved ", _Protocol:3/binary, " request ", Label/binary>>) -> Label;
label(<<"external reply ", Label/binary>>) -> Label;
label(<<"received failure reply ", Label/binary>>) -> Label;
label(<<"recieved ", Label/binary>>) -> Label;
label(_Other) -> 'undefined'.

from([], LogIP) -> LogIP;
from([<<"start|recieved internal reply", _/binary>>|_Data], LogIP) -> LogIP;
from([<<"log|external reply", _/binary>>|_Data], LogIP) -> LogIP;
from([<<"log|source ", From/binary>>|_Data], _LogIP) ->
    ip(From);
from([_Line|Lines], LogIP) ->
    from(Lines, LogIP).

ip(RawIP) ->
    [IP, _Port] = binary:split(RawIP, <<":">>),
    IP.

to([], LogIP) -> LogIP;
to([<<"start|recieved internal reply",_/binary>>|Data], LogIP) ->
    to(Data, LogIP);
to([<<"start|",_/binary>>|_Data], LogIP) -> LogIP;
to([<<"pass|",To/binary>>|_Data], _LogIP) ->
    ip(To);
to([_Datum|Data], LogIP) ->
    to(Data, LogIP).
