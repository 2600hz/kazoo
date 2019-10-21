%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
               ,logip :: kz_term:ne_binary()
               ,logport :: pos_integer()
               ,timer :: kz_term:api_reference()
               ,counter :: pos_integer()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link([ci_parsers_util:parser_args()]) -> kz_types:startlink_ret().
start_link([Arg]=Args) ->
    ServerName = ci_parsers_util:make_name(Arg),
    gen_server:start_link({'local', ServerName}, ?MODULE, Args, []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init({'parser_args', file:filename_all(), kz_term:ne_binary(), pos_integer()}) ->
                  {'ok', state()} |
                  {'stop', any()}.
init({'parser_args', LogFile, LogIP, LogPort} = Args) ->
    ParserId = ci_parsers_util:make_name(Args),
    _ = kz_log:put_callid(ParserId),
    case ci_parsers_util:open_file_for_read(LogFile) of
        {'ok', IoDevice} ->
            State = #state{parser_id = ParserId
                          ,logfile = LogFile
                          ,iodevice = IoDevice
                          ,logip = LogIP
                          ,logport = LogPort
                          ,counter = 1
                          ,timer = 'undefined'
                          },
            self() ! 'start_parsing',
            {'ok', State};
        {'error', Error} ->
            {'stop', Error}
    end.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(atom(), any(), state()) -> kz_types:handle_call_ret().
handle_call(_Request, _From, State) ->
    lager:debug("unhandled handle_call executed ~p~p", [_Request, _From]),
    Reply = 'ok',
    {'reply', Reply, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    lager:debug("unhandled handle_cast ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the gen_server terminate
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{iodevice = IoDevice}) ->
    'ok' = file:close(IoDevice),
    lager:debug("call inspector freeswitch parser terminated: ~p", [_Reason]).

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
-spec extract_chunks(atom(), file:io_device(), kz_term:ne_binary(), pos_integer(), pos_integer()) -> pos_integer().
extract_chunks(ParserId, Dev, LogIP, LogPort, Counter) ->
    case extract_chunk(Dev, buffer()) of
        [] -> Counter;
        Data0 ->
            NewCounter = make_and_store_chunk(ParserId, LogIP, LogPort, Counter, Data0),
            extract_chunks(ParserId, Dev, LogIP, LogPort, NewCounter)
    end.

-type buffer() :: [binary() | {'timestamp', kz_term:api_number()}].

-spec make_and_store_chunk(atom(), kz_term:ne_binary(), pos_integer(), pos_integer(), buffer()) -> pos_integer().
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
acc(<<"recv ", _/binary>>=Line, [], Dev) ->
    %% Start of a new chunk
    extract_chunk(Dev, [Line]);
acc(<<"send ", _/binary>>=Line, [], Dev) ->
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


-spec set_legs(kz_term:ne_binary(), pos_integer(), ci_chunk:chunk(), [kz_term:ne_binary()]) ->
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

-spec ip(kz_term:ne_binary()) -> kz_term:ne_binary().
ip(Bin) ->
    %% 15 = Look ahead inside longest IPv4 possible
    extract_ahead(<<"/[">>, 4*3+3, <<"]:">>, Bin).

-spec get_port(kz_term:ne_binary()) -> kz_term:ne_binary().
get_port(Bin) ->
    extract_ahead(<<"]:">>, 6, <<" at ">>, Bin).

-spec extract_ahead(kz_term:ne_binary(), pos_integer(), kz_term:ne_binary(), binary()) -> kz_term:api_binary().
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

-spec label([kz_term:ne_binary()]) -> kz_term:ne_binary().
label(Data) ->
    lists:nth(2, Data).

-spec remove_whitespace_lines([binary()]) -> [kz_term:ne_binary()].
remove_whitespace_lines(Data) ->
    [Line || Line <- Data, not all_whitespace(Line)].

-spec all_whitespace(binary()) -> boolean().
all_whitespace(<<$\s, Rest/binary>>) ->
    all_whitespace(Rest);
all_whitespace(<<$\n, Rest/binary>>) ->
    all_whitespace(Rest);
all_whitespace(<<>>) -> 'true';
all_whitespace(_) -> 'false'.

-spec strip_truncating_pieces([kz_term:ne_binary()]) -> [kz_term:ne_binary()].
strip_truncating_pieces(Data) ->
    [case re:run(Line, "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{6} \\[[A-Z]+\\] )") of
         {'match', [{Offset,_}|_]} -> kz_binary:truncate_right(Line, Offset);
         'nomatch' -> Line
     end
     || Line <- Data
    ].

-spec remove_unrelated_lines([kz_term:ne_binary()]) -> [kz_term:ne_binary()].
remove_unrelated_lines([FirstLine|Lines]) ->
    [FirstLine | do_remove_unrelated_lines(Lines)].

-spec do_remove_unrelated_lines([kz_term:ne_binary()]) -> [kz_term:ne_binary()].
do_remove_unrelated_lines([]) -> [];
do_remove_unrelated_lines([<<"   ", _/binary>>=Line|Lines]) ->
    [Line | do_remove_unrelated_lines(Lines)];
do_remove_unrelated_lines([_|Lines]) ->
    do_remove_unrelated_lines(Lines).

-spec unwrap_lines([kz_term:ne_binary()]) -> [kz_term:ne_binary()].
unwrap_lines([FirstLine|Lines]) ->
    [unwrap_first_line(FirstLine)] ++ [unwrap(Line) || Line <- Lines].

-spec unwrap_first_line(kz_term:ne_binary()) -> kz_term:ne_binary().
unwrap_first_line(FirstLine) ->
    Rm = length(":\n"),
    Sz = byte_size(FirstLine) - Rm,
    <<Line:Sz/binary, _:Rm/binary>> = FirstLine,
    Line.

-spec unwrap(kz_term:ne_binary()) -> kz_term:ne_binary().
unwrap(Line) ->
    Sz = byte_size(Line) - length("   ") - length("\n"),
    <<"   ", Data:Sz/binary, _:1/binary>> = Line,
    Data.

-spec remove_dashes([kz_term:ne_binary()]) -> [kz_term:ne_binary()].
remove_dashes([]) -> [];
remove_dashes([Line|Lines]) ->
    case binary:split(Line, <<"#012   --">>) of
        [Good, _Bad] -> Good;
        [Good] -> Good
    end,
    [Good | remove_dashes(Lines)].
