%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_parser_freeswitch).

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
-spec start_link(list()) -> startlink_ret().
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
terminate(_Reason, #state{iodevice = IoDevice}) ->
    'ok' = file:close(IoDevice),
    lager:debug("call inspector freeswitch parser terminated: ~p", [_Reason]).

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

-spec extract_chunks(atom(), file:io_device(), ne_binary(), pos_integer(), pos_integer()) -> pos_integer().
extract_chunks(ParserId, Dev, LogIP, LogPort, Counter) ->
    case extract_chunk(Dev, buffer()) of
        [] -> Counter;
        Data0 ->
            NewCounter = make_and_store_chunk(ParserId, LogIP, LogPort, Counter, Data0),
            extract_chunks(ParserId, Dev, LogIP, LogPort, NewCounter)
    end.

-type buffer() :: [binary() | {'timestamp', maybe(number())}].

-spec make_and_store_chunk(atom(), ne_binary(), pos_integer(), pos_integer(), buffer()) -> pos_integer().
make_and_store_chunk(ParserId, LogIP, LogPort, Counter, Data00) ->
    Apply = fun (Fun, Arg) -> Fun(Arg) end,
    {Timestamp, Data0, NewCounter} =
        case lists:keytake('timestamp', 1, Data00) of
            {'value', {'timestamp',TS}, D0} -> {TS, D0, Counter};
            'false' ->                         {Counter, Data00, Counter+1}
        end,
    Cleansers = [fun remove_whitespace_lines/1
                 ,fun remove_unrelated_lines/1 %% MUST be called before unwrap_lines/1
                 ,fun unwrap_lines/1
                 ,fun strip_truncating_pieces/1
                 ,fun remove_dashes/1
                ],
    Data = lists:foldl(Apply, Data0, Cleansers),
    Chunk =
        ci_chunk:setters(set_legs(LogIP, LogPort, ci_chunk:new(), Data)
                         ,[{fun ci_chunk:data/2, Data}
                           ,{fun ci_chunk:call_id/2, ci_parsers_util:call_id(Data)}
                           ,{fun ci_chunk:timestamp/2, Timestamp}
                           ,{fun ci_chunk:parser/2, ParserId}
                           ,{fun ci_chunk:label/2, label(Data)}
                           ,{fun ci_chunk:c_seq/2, ci_parsers_util:c_seq(Data)}
                          ]
                        ),
    lager:debug("parsed chunk ~s", [ci_chunk:call_id(Chunk)]),
    ci_datastore:store_chunk(Chunk),
    NewCounter.

-spec extract_chunk(file:io_device(), buffer()) -> buffer().
extract_chunk(Dev, Buffer) ->
    case file:read_line(Dev) of
        'eof' -> buffer(Buffer);
        {'ok', Line} ->
            case binary:split(Line, <<":  ">>) of
                %% Keep log's timestamp from chunks' beginnings
                [RawTimestamp, <<"send ",_/binary>>=Logged0] ->
                    acc(Logged0, [{'timestamp',ci_parsers_util:timestamp(RawTimestamp)}|Buffer], Dev);
                [RawTimestamp, <<"recv ",_/binary>>=Logged0] ->
                    acc(Logged0, [{'timestamp',ci_parsers_util:timestamp(RawTimestamp)}|Buffer], Dev);
                [_Timestamp, Logged0] ->
                    acc(Logged0, Buffer, Dev);
                [Line] ->
                    acc(Line, Buffer, Dev)
            end
    end.

-spec acc(binary(), buffer(), file:io_device()) -> buffer().
acc(<<"recv ", _/binary>>=Line, Buffer, Dev)
  when Buffer == [] ->
    %% Start of a new chunk
    extract_chunk(Dev, [Line]);
acc(<<"send ", _/binary>>=Line, Buffer, Dev)
  when Buffer == [] ->
    %% Start of a new chunk
    extract_chunk(Dev, [Line]);
acc(<<"   ------------------------------------------------------------------------\n">>, [_]=Buffer, Dev) ->
    %% Second line of a chunk (special case given end of chunk)
    extract_chunk(Dev, Buffer);
acc(<<"   ------------------------------------------------------------------------\n">>, Buffer, _Dev)
  when Buffer =/= [] ->
    %% End of current chunk
    lists:reverse(Buffer);
acc(Line, Buffer, Dev)
  when Buffer =/= [] ->
    %% Between start and end of chunk
    extract_chunk(Dev, [Line|Buffer]);
acc(_Line, Buffer, Dev) ->
    %% Skip over the rest
    extract_chunk(Dev, Buffer).

-spec buffer() -> [].
buffer() ->
    case get('buffer') of
        'undefined' -> [];
        Buffer -> Buffer
    end,
    [].

-spec buffer(buffer()) -> [].
buffer(Buffer) ->
    _OldBuffer = put('buffer', Buffer),
    [].


-spec set_legs(ne_binary(), pos_integer(), ci_chunk:chunk(), [ne_binary()]) ->
                      ci_chunk:chunk().
set_legs(LogIP, LogPort, Chunk, [FirstLine|_Lines]) ->
    case FirstLine of
        <<"send ", _/binary>> ->
            FromIP = LogIP,
            ToIP   = ip(FirstLine),
            FromPort = LogPort,
            ToPort   = get_port(FirstLine);
        <<"recv ", _/binary>> ->
            FromIP = ip(FirstLine),
            ToIP   = LogIP,
            FromPort = get_port(FirstLine),
            ToPort   = LogPort
    end,
    ci_chunk:setters(Chunk
                     ,[{fun ci_chunk:src_ip/2, FromIP}
                       ,{fun ci_chunk:dst_ip/2, ToIP}
                       ,{fun ci_chunk:src_port/2, FromPort}
                       ,{fun ci_chunk:dst_port/2, ToPort}
                      ]
                    ).

-spec ip(ne_binary()) -> ne_binary().
ip(Bin) ->
    %% 15 = Look ahead inside longest IPv4 possible
    extract_ahead(<<"/[">>, 4*3+3, <<"]:">>, Bin).

-spec get_port(ne_binary()) -> ne_binary().
get_port(Bin) ->
    extract_ahead(<<"]:">>, 6, <<" at ">>, Bin).

-spec extract_ahead(ne_binary(), pos_integer(), ne_binary(), binary()) -> maybe(binary()).
extract_ahead(Lhs, Span, Rhs, Bin) ->
    case binary:match(Bin, Lhs) of
        {StartS, StartP} ->
            Start = StartS + StartP,
            LookAhead = {Start, Span+byte_size(Rhs)},
            case binary:match(Bin, Rhs, [{'scope',LookAhead}]) of
                {End, _} ->  binary:part(Bin, Start, End-Start);
                'nomatch' -> 'undefined'
            end;
        'nomatch' ->
            'undefined'
    end.

-spec label([ne_binary()]) -> ne_binary().
label(Data) ->
    lists:nth(2, Data).

-spec remove_whitespace_lines([binary()]) -> [ne_binary()].
remove_whitespace_lines(Data) ->
    [Line || Line <- Data, not all_whitespace(Line)].

-spec all_whitespace(binary()) -> boolean().
all_whitespace(<<$\s, Rest/binary>>) ->
    all_whitespace(Rest);
all_whitespace(<<$\n, Rest/binary>>) ->
    all_whitespace(Rest);
all_whitespace(<<>>) -> 'true';
all_whitespace(_) -> 'false'.

-spec strip_truncating_pieces([ne_binary()]) -> [ne_binary()].
strip_truncating_pieces(Data) ->
    [case re:run(Line, "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{6} \\[[A-Z]+\\] )") of
         {'match', [{Offset,_}|_]} -> kz_util:truncate_right_binary(Line, Offset);
         'nomatch' -> Line
     end
     || Line <- Data
    ].

-spec remove_unrelated_lines([ne_binary()]) -> [ne_binary()].
remove_unrelated_lines([FirstLine|Lines]) ->
    [FirstLine | do_remove_unrelated_lines(Lines)].

-spec do_remove_unrelated_lines([ne_binary()]) -> [ne_binary()].
do_remove_unrelated_lines([]) -> [];
do_remove_unrelated_lines([<<"   ", _/binary>>=Line|Lines]) ->
    [Line | do_remove_unrelated_lines(Lines)];
do_remove_unrelated_lines([_|Lines]) ->
    do_remove_unrelated_lines(Lines).

-spec unwrap_lines([ne_binary()]) -> [ne_binary()].
unwrap_lines([FirstLine|Lines]) ->
    [unwrap_first_line(FirstLine)] ++ [unwrap(Line) || Line <- Lines].

-spec unwrap_first_line(ne_binary()) -> ne_binary().
unwrap_first_line(FirstLine) ->
    Rm = length(":\n"),
    Sz = byte_size(FirstLine) - Rm,
    <<Line:Sz/binary, _:Rm/binary>> = FirstLine,
    Line.

-spec unwrap(ne_binary()) -> ne_binary().
unwrap(Line) ->
    Sz = byte_size(Line) - length("   ") - length("\n"),
    <<"   ", Data:Sz/binary, _:1/binary>> = Line,
    Data.

-spec remove_dashes([ne_binary()]) -> [ne_binary()].
remove_dashes([]) -> [];
remove_dashes([Line|Lines]) ->
    case binary:split(Line, <<"#012   --">>) of
        [Good, _Bad] -> Good;
        [Good] -> Good
    end,
    [Good | remove_dashes(Lines)].
