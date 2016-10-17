%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_parser_kamailio).

-behaviour(gen_server).

-include("call_inspector.hrl").

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

-record(state, {parser_id :: atom()
               ,logfile :: file:name()
               ,iodevice :: file:io_device()
               ,logip :: ne_binary()
               ,logport :: pos_integer()
               ,timer :: reference()
               ,counter :: pos_integer()
               }
       ).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(ne_binary() | list()) -> startlink_ret().
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
-spec init({'parser_args', ne_binary(), ne_binary(), pos_integer()}) -> {'ok', state()}.
init({'parser_args', LogFile, LogIP, LogPort} = Args) ->
    ParserId = ci_parsers_util:make_name(Args),
    _ = kz_util:put_callid(ParserId),
    NewDev = ci_parsers_util:open_file(LogFile),
    State = #state{parser_id = ParserId
                  ,logfile = LogFile
                  ,iodevice = NewDev
                  ,logip = LogIP
                  ,logport = LogPort
                  ,counter = 1
                  },
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info('start_parsing', State=#state{parser_id = ParserId
                                         ,iodevice = IoDevice
                                         ,logip = LogIP
                                         ,logport = LogPort
                                         ,timer = OldTimer
                                         ,counter = Counter
                                         }) ->
    _ = case OldTimer of
            'undefined' -> 'ok';
            _ -> erlang:cancel_timer(OldTimer)
        end,
    NewCounter = extract_chunks(ParserId, IoDevice, LogIP, LogPort, Counter),
    NewTimer = erlang:send_after(ci_parsers_util:parse_interval()
                                ,self()
                                ,'start_parsing'
                                ),
    {'noreply', State#state{timer = NewTimer
                           ,counter = NewCounter
                           }};
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{iodevice = IoDevice}) ->
    'ok' = file:close(IoDevice),
    lager:debug("call inspector kamailio parser terminated: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec extract_chunks(atom(), file:io_device(), ne_binary(), pos_integer(), pos_integer()) -> pos_integer().
extract_chunks(ParserId, Dev, LogIP, LogPort, Counter) ->
    case extract_chunk(Dev) of
        [] -> Counter;
        {{'callid',Callid}, Data0} ->
            NewCounter = make_and_store_chunk(ParserId, LogIP, LogPort, Callid, Counter, Data0),
            extract_chunks(ParserId, Dev, LogIP, LogPort, NewCounter);
        {'buffers', Buffers} ->
            StoreEach =
                fun ({{'callid',Callid}, Data0}, ACounter) ->
                        make_and_store_chunk(ParserId, LogIP, LogPort, Callid, ACounter, Data0)
                end,
            lists:foldl(StoreEach, Counter, Buffers)
    end.

-type key() :: {'callid', ne_binary()}.
-type datum() :: ne_binary() | {'timestamp', ne_binary()}.
-type data() :: [datum()].

-spec make_and_store_chunk(atom(), ne_binary(), pos_integer(), ne_binary(), pos_integer(), data()) ->
                                  pos_integer().
make_and_store_chunk(ParserId, LogIP, LogPort, Callid, Counter, Data0) ->
    {Data, Ts} = cleanse_data_and_get_timestamp(Data0),
    %% Counter is a fallback time ID (for old logfile format)
    {NewCounter, Timestamp} = case Ts of
                                  'undefined' -> {Counter+1, Counter};
                                  _Ts -> {Counter, Ts}
                              end,
    ReversedData0 = lists:reverse(Data0),
    Chunk =
        ci_chunk:setters(ci_chunk:new()
                        ,[{fun ci_chunk:data/2, Data}
                         ,{fun ci_chunk:call_id/2, Callid}
                         ,{fun ci_chunk:timestamp/2, Timestamp}
                         ,{fun ci_chunk:parser/2, ParserId}
                         ,{fun ci_chunk:label/2, label(hd(Data))}
                         ,{fun ci_chunk:src_ip/2, from(ReversedData0,LogIP)}
                         ,{fun ci_chunk:dst_ip/2, to(ReversedData0,LogIP)}
                         ,{fun ci_chunk:src_port/2, from_port(ReversedData0,LogPort)}
                         ,{fun ci_chunk:dst_port/2, to_port(ReversedData0,LogPort)}
                         ,{fun ci_chunk:c_seq/2, c_seq(Data)}
                         ]
                        ),
    lager:debug("parsed chunk ~s", [ci_chunk:call_id(Chunk)]),
    ci_datastore:store_chunk(Chunk),
    NewCounter.

-type buffer() :: {key(), data()}.
-spec extract_chunk(file:io_device()) ->
                           buffer() |
                           [] |
                           {'buffers', [buffer()]}.
extract_chunk(Dev) ->
    case file:read_line(Dev) of
        'eof' ->
            dump_buffers();
        {'ok', Line} ->
            %% Expected Line looks like: RawTimestamp […] <script>: CallId | Tag | Log \n
            %% <<"2015-03-03T23:44:07.812917+00:00 kamailio[19136]: INFO: <script>: OTBjNGY4NmVlZDAyYTQ5M2NlMTVkOWQ5ZDQ4YmFlNmI.|log|from sip:user_3331@webdev.realm;transport=UDP\n">>
            case {binary:matches(Line, <<"|">>), binary:split(Line, <<"<script>: ">>)} of
                {[_,_], [RawTimestamp, CallIdAndLogPart]} ->
                    [CallId, LogPart] = binary:split(CallIdAndLogPart, <<"|">>),
                    Key = {'callid', CallId},
                    Buffer = get_buffer(Key),
                    acc(rm_newline(LogPart), [{'timestamp', RawTimestamp}|Buffer], Dev, Key);
                _Ignore ->
                    extract_chunk(Dev)
            end
    end.

-spec acc(ne_binary(), data(), file:io_device(), key()) -> {key(), data()}.
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

-type cleanse_acc() :: {ne_binaries(), api_number()}.

-spec cleanse_data_and_get_timestamp(data()) -> cleanse_acc().
cleanse_data_and_get_timestamp(Data0) ->
    lists:foldl(fun cleanse_data_fold/2
               ,{[], 'undefined'}
               ,Data0
               ).

-spec cleanse_data_fold(datum()
                       ,cleanse_acc()
                       ) -> cleanse_acc().
cleanse_data_fold({'timestamp', RawTimestamp}, {Acc, TS}) ->
    case ci_parsers_util:timestamp(RawTimestamp) of
        Ts when Ts < TS ->
            {Acc, Ts};
        _Ts ->
            {Acc, TS}
    end;
cleanse_data_fold(Bin, {Acc, TS}) ->
    {[unwrap(Bin)|Acc], TS}.

-spec get_buffer(key()) -> data().
get_buffer(Key) ->
    case get(Key) of
        'undefined' -> [];
        Buffer -> Buffer
    end.

-spec dump_buffers() -> [] | {'buffers', [buffer()]}.
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

-spec unwrap(ne_binary()) -> ne_binary().
unwrap(Bin0) ->
    case binary:split(Bin0, <<"|">>) of
        [_Tag, Bin] -> Bin;
        [RawTimestamp] -> RawTimestamp
    end.

-spec rm_newline(ne_binary()) -> ne_binary().
rm_newline(Line0) ->
    [Line, <<>>] = binary:split(Line0, <<"\n">>),
    Line.

-spec label(ne_binary()) -> api_binary().
label(<<"received internal reply ", Label/binary>>) -> Label;
label(<<"received ", _Protocol:3/binary, " request ", Label/binary>>) -> Label;
label(<<"external reply ", Label/binary>>) -> Label;
label(<<"received failure reply ", Label/binary>>) -> Label;
label(<<"received ", Label/binary>>) -> Label;
label(_Other) -> 'undefined'.

-spec from(ne_binaries(), Default) -> ne_binary() | Default.
from([], Default) -> Default;
from([<<"start|received internal reply", _/binary>>|_Data], Default) -> Default;
from([<<"log|external reply", _/binary>>|_Data], Default) -> Default;
from([<<"log|source ", From/binary>>|_Data], Default) ->
    get_ip(From, Default);
from([_Line|Lines], Default) ->
    from(Lines, Default).

-spec get_ip(ne_binary(), Default) -> ne_binary() | Default.
get_ip(Bin, Default) ->
    case binary:split(Bin, <<":">>) of
        [IP, _Port] -> IP;
        _Else -> Default  %% Unexpected case
    end.

-spec to(ne_binaries(), Default) -> ne_binary() | Default.
to([], Default) -> Default;
to([<<"start|received internal reply",_/binary>>|Data], Default) ->
    to(Data, Default);
to([<<"start|",_/binary>>|_Data], Default) -> Default;
to([<<"pass|",To/binary>>|_Data], Default) ->
    get_ip(To, Default);
to([_Datum|Data], Default) ->
    to(Data, Default).

-spec from_port(ne_binaries(), Default) -> ne_binary() | Default.
from_port([], Default) -> Default;
from_port([<<"start|received internal reply", _/binary>>|_Data], Default) -> Default;
from_port([<<"log|external reply", _/binary>>|_Data], Default) -> Default;
from_port([<<"log|source ", From/binary>>|_Data], Default) ->
    get_port(From, Default);
from_port([_Line|Lines], Default) ->
    from_port(Lines, Default).

-spec get_port(ne_binary(), Default) -> ne_binary() | Default.
get_port(Bin, Default) ->
    case binary:split(Bin, <<":">>) of
        [_IP, Port] -> Port;
        _Else -> Default  %% Unexpected case
    end.

-spec to_port(ne_binaries(), Default) -> ne_binary() | Default.
to_port([], Default) -> Default;
to_port([<<"start|received internal reply",_/binary>>|Data], Default) ->
    to_port(Data, Default);
to_port([<<"start|",_/binary>>|_Data], Default) -> Default;
to_port([<<"pass|",To/binary>>|_Data], Default) ->
    get_port(To, Default);
to_port([_Datum|Data], Default) ->
    to(Data, Default).


-spec c_seq(ne_binaries()) -> api_binary().
c_seq([<<"cseq ", CSeq/binary>>|_Data]) -> CSeq;
c_seq([]) -> 'undefined';
c_seq([_Datum|Data]) -> c_seq(Data).
