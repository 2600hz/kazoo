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
               ,iodevice :: file:io_device()
               }
       ).
-type state() :: #state{}.

-define(SERVER, ?MODULE).

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
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

open_logfile(Filename) ->
    gen_server:cast(?MODULE, {'open_logfile', Filename}).

start_parsing() ->
    gen_server:cast(?MODULE, 'start_parsing').

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
handle_cast({'open_logfile', LogFile}, State) ->
    {'ok', IoDevice} = file:open(LogFile, ['read','raw','binary','read_ahead']),%read+append??
    NewState = State#state{logfile = LogFile
                          ,iodevice = IoDevice},
    {'noreply', NewState};
handle_cast('start_parsing', State=#state{iodevice = IoDevice}) ->
    'ok' = extract_chunks(IoDevice),
    {'noreply', State};
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
    'ok' = file:close(IoDevice),
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

extract_chunks(Dev) ->
    extract_chunks(Dev, 1).
extract_chunks(Dev, Counter) ->
    case extract_chunk(Dev) of
        [] -> 'ok';
        {{'callid',Callid}, Data} ->
            make_and_store_chunk(Counter, Callid, Data),
            extract_chunks(Dev, Counter+1);
        {'buffers', Buffers} ->
            StoreEach =
                fun ({{'callid',Callid}, Data}, Count) ->
                        make_and_store_chunk(Counter, Callid, Data),
                        Count + 1
                end,
            lists:foldl(StoreEach, Counter, Buffers),
            'ok'
    end.

make_and_store_chunk(Counter, Callid, Data) ->
    Apply = fun (Fun, Arg) -> Fun(Arg) end,
    Setters = [fun (C) -> ci_chunk:set_data(C, Data) end
              ,fun (C) -> ci_chunk:set_call_id(C, Callid) end
              ,fun (C) -> ci_chunk:set_timestamp(C, Counter) end
              ,fun (C) -> ci_chunk:set_parser(C, ?MODULE) end
              ,fun (C) -> ci_chunk:set_label(C, label(hd(Data))) end
              ,fun (C) -> ci_chunk:set_from(C, source(Data)) end
              ,fun (C) -> ci_chunk:set_to(C, to(Data)) end
               %% from to (legs)
              ],
    Chunk = lists:foldl(Apply, ci_chunk:new(), Setters),
    io:format("~p\n",[Chunk]),
    ci_datastore:store_chunk(Chunk).

extract_chunk(Dev) ->
    case file:read_line(Dev) of
        'eof' ->
            dump_buffers();
        {'ok', Line} ->
            case binary:split(Line, <<"|">>) of
                [_Timestamp, Logged0] ->
                    [Logged, <<>>] = binary:split(Logged0, <<"\n">>),
                    Key = {'callid',callid(Line)},
                    Buffer = get_buffer(Key),
                    acc(Logged, Buffer, Key, Dev);
                _Ignore ->
                    extract_chunk(Dev)
            end
    end.

acc(<<"start|",_/binary>>=Logged, Buffer, Key, Dev)
  when Buffer == [] ->
    put(Key, [Logged]),
    extract_chunk(Dev);
acc(<<"start|",_/binary>>=Logged, Buffer, Key, _Dev)
  when Buffer =/= [] ->
    put(Key, [Logged]),
    {Key, lists:reverse(Buffer)};
acc(<<"log|external ",_/binary>>=Logged, Buffer, Key, _Dev) ->
    put(Key, []),
    {Key, lists:reverse([Logged|Buffer])};
acc(<<"log|",_/binary>>=Logged, Buffer, Key, Dev) ->
    put(Key, [Logged|Buffer]),
    extract_chunk(Dev);
acc(<<"pass|",_/binary>>=Logged, Buffer, Key, _Dev) ->
    put(Key, []),
    {Key, lists:reverse([Logged|Buffer])};
acc(<<"end|",_/binary>>=Logged, Buffer, Key, _Dev) ->
    put(Key, []),
    {Key, lists:reverse([Logged|Buffer])};
acc(Logged, Buffer, Key, Dev) ->
    io:format("Unhandled acc/4 case:\n\tLogged=~p\n\tBuffer=~p\n\tKey=~p\n", [Logged,Buffer,Key]),
    extract_chunk(Dev).

get_buffer(Key) ->
    case get(Key) of
        'undefined' -> [];
        Buffer -> Buffer
    end.

dump_buffers() ->
    Buffers = [{Key, lists:reverse(Buff)}
               || {{'callid',_}=Key,Buff} <- get(),
                  Buff =/= []],
    case Buffers of
        [] -> [];
        _ -> {'buffers', Buffers}
    end.


callid(Line) ->
    {'match', [Callid]} =
        re:run(Line
              ,"<script>: ([^\\s\\|]+)\\|"
              ,[{'capture','all_but_first','binary'}]),
    Callid.

label(<<"start|recieved internal reply ", Label/binary>>) -> Label;
label(<<"start|recieved UDP request ", Label/binary>>) -> Label;
label(<<"start|recieved udp request ", Label/binary>>) -> Label;
label(<<"log|external reply ", Label/binary>>) -> Label;
label(<<"start|received failure reply ", Label/binary>>) -> Label;
label(_Other) -> io:format("??LABEL?? ~p\n",[_Other]), 'undefined'.

source([]) -> 'undefined';
source([<<"log|source ", Source0/binary>>|_]) ->
    [Source, _Port] = binary:split(Source0, <<":">>),
    Source;
source([_Line|Lines]) ->
    source(Lines).

to([]) -> 'undefined';
to([<<"log|to ", _/binary>>=Line|_]) ->
    case re:run(Line, "(?:@([^@;]+);|@([^@$]+)$|:([^:]+):|:([^:$]+)$)"
               ,[{'capture','all_but_first','binary'}]) of
        {'match', Matches} ->
            [Host] = [Match || Match <- Matches, Match =/= <<>>],
            Host;
        'nomatch' -> 'undefined'
    end;
to([_Line|Lines]) ->
    to(Lines).
