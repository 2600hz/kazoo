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
         ,open_logfile/2
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
               ,kamailioIP :: ne_binary()
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

open_logfile(Filename, KamailioIP) ->
    gen_server:cast(?MODULE, {'open_logfile', Filename, KamailioIP}).

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
handle_cast({'open_logfile', LogFile, KamailioIP}, State) ->
    {'ok', IoDevice} = file:open(LogFile, ['read','raw','binary','read_ahead']),%read+append??
    NewState = State#state{logfile = LogFile
                          ,iodevice = IoDevice
                          ,kamailioIP = KamailioIP},
    {'noreply', NewState};
handle_cast('start_parsing', State=#state{iodevice = IoDevice
                                         ,kamailioIP = KamailioIP}) ->
    'ok' = extract_chunks(IoDevice, KamailioIP),
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

extract_chunks(Dev, KamailioIP) ->
    case extract_chunk(Dev) of
        [] -> 'ok';
        {{'callid',Callid}, Data0} ->
            make_and_store_chunk(KamailioIP, Callid, Data0),
            extract_chunks(Dev, KamailioIP);
        {'buffers', Buffers} ->
            StoreEach =
                fun ({{'callid',Callid}, Data0}) ->
                        make_and_store_chunk(KamailioIP, Callid, Data0)
                end,
            lists:foreach(StoreEach, Buffers)
    end.

make_and_store_chunk(KamailioIP, Callid, Data0) ->
    Apply = fun (Fun, Arg) -> Fun(Arg) end,
    {Data, Timestamp} = cleanse_data_and_get_timestamp(Data0),
    Setters = [fun (C) -> ci_chunk:set_data(C, Data) end
              ,fun (C) -> ci_chunk:set_call_id(C, Callid) end
              ,fun (C) -> ci_chunk:set_timestamp(C, Timestamp) end
              ,fun (C) -> ci_chunk:set_parser(C, ?MODULE) end
              ,fun (C) -> ci_chunk:set_label(C, label(hd(Data))) end
              ,fun (C) -> ci_chunk:set_from(C, source(Data)) end
              ,fun (C) -> ci_chunk:set_to(C, KamailioIP) end
              ],
    Chunk = lists:foldl(Apply, ci_chunk:new(), Setters),
    ci_datastore:store_chunk(Chunk).

extract_chunk(Dev) ->
    case file:read_line(Dev) of
        'eof' ->
            dump_buffers();
        {'ok', Line} ->
            case binary:split(Line, <<"|">>) of
                [RawTimestamp, Logged0] ->
                    [Logged, <<>>] = binary:split(Logged0, <<"\n">>),
                    Key = {'callid',callid(Line)},
                    Buffer = get_buffer(Key),
                    acc(Logged, [RawTimestamp|Buffer], Dev, Key);
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
    put(Key, []),
    {Key, [Logged] ++ Buffer};
acc(<<"log|",_/binary>>=Logged, Buffer, Dev, Key) ->
    put(Key, [Logged|Buffer]),
    extract_chunk(Dev);
acc(<<"pass|",_/binary>>=Logged, Buffer, _Dev, Key) ->
    put(Key, []),
    {Key, [Logged|Buffer]};
acc(<<"end|",_/binary>>=Logged, Buffer, _Dev, Key) ->
    put(Key, []),
    {Key, [Logged|Buffer]};
acc(<<"stop|",_/binary>>=Logged, Buffer, _Dev, Key) ->
    put(Key, []),
    {Key, [Logged|Buffer]}.

cleanse_data_and_get_timestamp(Data0) ->
    F =
        fun (Bin, {Acc, TS}) ->
                case ci_parsers_util:timestamp(Bin) of
                    'undefined' ->
                        {[Bin|Acc], TS};
                    Ts when Ts < TS ->
                        {Acc, Ts};
                    _Ts ->
                        {Acc, TS}
                end
        end,
    lists:foldl(F, {[], 1.0e100}, Data0).

get_buffer(Key) ->
    case get(Key) of
        'undefined' -> [];
        Buffer -> Buffer
    end.

dump_buffers() ->
    Buffers = [{Key, Buff} || {{'callid',_}=Key,Buff} <- get()
                                  , Buff =/= []],
    case Buffers of
        [] -> [];
        _ ->
            RmFromProcDict = fun ({Key, _Buffer}) ->
                                     put(Key, [])
                             end,
            lists:foreach(RmFromProcDict, Buffers),
            {'buffers', Buffers}
    end.


callid(Line) ->
    {'match', [Callid]} =
        re:run(Line
              ,"<script>: ([^\\s\\|]+)\\|"
              ,[{'capture','all_but_first','binary'}]),
    Callid.

label(<<"start|recieved internal reply ", Label/binary>>) -> Label;
label(<<"start|recieved ", _Protocol:3/binary, " request ", Label/binary>>) -> Label;
label(<<"log|external reply ", Label/binary>>) -> Label;
label(<<"start|received failure reply ", Label/binary>>) -> Label;
label(Other) ->
    case binary:split(Other, <<"|">>) of
        [_Tag, <<"recieved ">>=Line] ->
            Line;
        _ ->
            'undefined'
    end.

source([]) -> 'undefined';
source([<<"log|source ", Source0/binary>>|_]) ->
    [Source, _Port] = binary:split(Source0, <<":">>),
    Source;
source([_Line|Lines]) ->
    source(Lines).
