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

open_logfile(Filename) when is_list(Filename) ->
    gen_server:cast(?MODULE, {'open_logfile', Filename});
open_logfile(Filename) ->
    open_logfile(wh_util:to_list(Filename)).

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
    NewState = State#state{logfile = LogFile, iodevice = IoDevice},
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

extract_chunks(Dev) ->
    case extract_chunk(Dev, []) of
        [] -> 'ok';
        Data0 ->
            Cleansers = [fun remove_whitespace_lines/1
                        ,fun strip_pieces/1],
            Data = lists:foldl(fun (F,A) -> F(A) end, Data0, Cleansers),
            Setters = [fun (C) -> ci_chunk:set_data(C, Data) end
                      ,fun (C) -> ci_chunk:set_call_id(C, extract_call_id(Data)) end
                      ,fun (C) -> ci_chunk:set_timestamp(C, extract_timestamp(Data)) end
                      ,fun (C) -> ci_chunk:set_to(C, get_field([<<"To">>, <<"t">>],Data)) end
                      ,fun (C) -> ci_chunk:set_from(C, get_field([<<"From">>, <<"f">>],Data)) end
                      ,fun (C) -> ci_chunk:set_parser(C, ?MODULE) end
                      ],
            Chunk = lists:foldl(fun (F,A) -> F(A) end, ci_chunk:new(), Setters),
            ci_datastore:store_chunk(Chunk),
            extract_chunks(Dev)
    end.

extract_chunk(Dev, Buffer) ->
    case file:read_line(Dev) of
        'eof' -> [];
        {'ok', Line} ->
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
                ,Acc} when Acc =/= [] ->
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


extract_timestamp([]) -> 'undefined';
extract_timestamp([Data|Rest]) ->
    case Data of
        <<"send ", _/binary>> ->
            do_extract_timestamp(Data, Rest);
        <<"recv ", _/binary>> ->
            do_extract_timestamp(Data, Rest);
        _ ->
            extract_timestamp(Rest)
    end.

do_extract_timestamp(Line, Rest) ->
    case binary:split(Line, <<" at ">>) of
        [_IpPort, Data] ->
            RmLastTwo = byte_size(Data) - 2,
            <<Timestamp:RmLastTwo/binary, ":\n">> = Data,
            Timestamp;
        _ ->
            extract_timestamp(Rest)
    end.


extract_call_id(Data) ->
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
                            false ->
                                Acc;
                            true -> [Elem|Acc];
                            {true,Value} -> [Value|Acc]
                        end
                end, [], List1).

-spec try_all(ne_binary(), ne_binary()) -> 'false' | {'true', ne_binary()}.
try_all(Data, Field) ->
    FieldSz = byte_size(Field),
    case Data of
        <<"   ", Field:FieldSz/binary, _/binary>> ->
            case binary:split(Data, <<": ">>) of
                [_Key, Value0] ->
                    RmLastCharacter = byte_size(Value0) -1,
                    Value = binary:part(Value0, {0, RmLastCharacter}),
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


strip_pieces([]) -> [];
strip_pieces([Data|Rest]) ->
    case re:run(Data, "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\.\\d{6} \\[[A-Z]+\\] )") of
        nomatch ->
            [Data | strip_pieces(Rest)];
        {match, [{Offset,_}|_]} ->
            [binary:part(Data, {0,Offset}) | strip_pieces(Rest)]
    end.
