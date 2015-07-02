%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_parser_freeswitch).

-behaviour(gen_server).

-include("../call_inspector.hrl").

%% API
-export([start_link/1]).
-export([call_id/1]).

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

extract_chunks(Dev, LogIP, Counter) ->
    case extract_chunk(Dev, buffer()) of
        [] -> Counter;
        Data0 ->
            NewCounter = make_and_store_chunk(LogIP, Counter, Data0),
            extract_chunks(Dev, LogIP, NewCounter)
    end.

make_and_store_chunk(LogIP, Counter, Data00) ->
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
                ,fun remove_dashes/1],
    Data = lists:foldl(Apply, Data0, Cleansers),
    Chunk =
        ci_chunk:setters(set_legs(LogIP, ci_chunk:new(), Data)
                        , [ {fun ci_chunk:set_data/2, Data}
                          , {fun ci_chunk:set_call_id/2, call_id(Data)}
                          , {fun ci_chunk:set_timestamp/2, Timestamp}
                          , {fun ci_chunk:set_parser/2, ?MODULE}
                          , {fun ci_chunk:set_label/2, label(Data)}
                          ]
                        ),
    lager:debug("parsed chunk ~s (~s)", [ci_chunk:call_id(Chunk), ci_parsers_sup:child(self())]),
    ci_datastore:store_chunk(Chunk),
    NewCounter.

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

buffer() ->
    case get('buffer') of
        'undefined' -> [];
        Buffer -> Buffer
    end,
    [].

buffer(Buffer) ->
    _OldBuffer = put('buffer', Buffer),
    [].


set_legs(LogIP, Chunk, [FirstLine|_Lines]) ->
    case FirstLine of
        <<"send ", _/binary>> ->
            From = LogIP,
            To   = ip(FirstLine);
        <<"recv ", _/binary>> ->
            From = ip(FirstLine),
            To   = LogIP
    end,
    ci_chunk:set_to(ci_chunk:set_from(Chunk,From), To).

ip('undefined') -> 'undefined';
ip(Bin) ->
    case re:run(Bin, "/\\[([\\d\\.]+)\\]:", [{'capture','all_but_first','binary'}]) of
        {'match', [IP]} -> IP;
        'nomatch' -> 'undefined'
    end.


label(Data) ->
    lists:nth(2, Data).

call_id(Data) ->
    get_field([<<"Call-ID">>, <<"i">>], Data).


-spec get_field(ne_binaries(), ne_binaries()) -> api_binary().
get_field(_Fields, []) ->
    'undefined';
get_field(Fields, [Data|Rest]) ->
    F = fun (Field) -> try_all(Data, Field) end,
    case filtermap(F, Fields) of
        [] ->
            get_field(Fields, Rest);
        [Value] ->
            Value
    end.

filtermap(Fun, List1) ->
    lists:foldr(fun(Elem, Acc) ->
                        case Fun(Elem) of
                            'false' -> Acc;
                            'true' -> [Elem|Acc];
                            {'true',Value} -> [Value|Acc]
                        end
                end, [], List1).

-spec try_all(ne_binary(), ne_binary()) -> 'false' | {'true', ne_binary()}.
try_all(Data, Field) ->
    FieldSz = byte_size(Field),
    case Data of
        <<Field:FieldSz/binary, _/binary>> ->
            case binary:split(Data, <<": ">>) of
                [_Key, Value0] ->
                    Value = binary:part(Value0, {0, byte_size(Value0)}),
                    {'true', Value};
                _ ->
                    'false'
            end;
        _ ->
            'false'
    end.


remove_whitespace_lines(Data) ->
    [Line || Line <- Data, not all_whitespace(Line)].

all_whitespace(<<$\s, Rest/binary>>) ->
    all_whitespace(Rest);
all_whitespace(<<$\n, Rest/binary>>) ->
    all_whitespace(Rest);
all_whitespace(<<>>) -> 'true';
all_whitespace(_) -> 'false'.


strip_truncating_pieces([]) -> [];
strip_truncating_pieces([Data|Rest]) ->
    case re:run(Data, "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{6} \\[[A-Z]+\\] )") of
        'nomatch' ->
            [Data | strip_truncating_pieces(Rest)];
        {'match', [{Offset,_}|_]} ->
            [binary:part(Data, {0,Offset}) | strip_truncating_pieces(Rest)]
    end.


remove_unrelated_lines([FirstLine|Lines]) ->
    [FirstLine | do_remove_unrelated_lines(Lines)].

do_remove_unrelated_lines([]) -> [];
do_remove_unrelated_lines([<<"   ", _/binary>>=Line|Lines]) ->
    [Line | do_remove_unrelated_lines(Lines)];
do_remove_unrelated_lines([_|Lines]) ->
    do_remove_unrelated_lines(Lines).


unwrap_lines([FirstLine|Lines]) ->
    [unwrap_first_line(FirstLine)] ++ [unwrap(Line) || Line <- Lines].

unwrap_first_line(FirstLine) ->
    Rm = length(":\n"),
    Sz = byte_size(FirstLine) - Rm,
    <<Line:Sz/binary, _:Rm/binary>> = FirstLine,
    Line.

unwrap(Line) ->
    Sz = byte_size(Line) - length("   ") - length("\n"),
    <<"   ", Data:Sz/binary, _:1/binary>> = Line,
    Data.

remove_dashes([]) -> [];
remove_dashes([Line|Lines]) ->
    case binary:split(Line, <<"#012   --">>) of
        [Good, _Bad] -> Good;
        [Good] -> Good
    end,
    [Good | remove_dashes(Lines)].
