%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Log messages in a way to make importing to WebSequenceDiagrams.com
%%% easier
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(webseq).

-behaviour(gen_server).

-export([start_link/0
         ,evt/3
         ,title/1
         ,note/3
         ,trunc/0
         ,rotate/0
         ,process_pid/1
         ,reg_who/2
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,code_change/3
         ,terminate/2
        ]).

-include_lib("whistle/include/wh_types.hrl").

-define(WEBSEQNAME, "/tmp/webseq.txt").

-record(state, {io_device :: file:io_device()
               ,who_registry :: dict()
               }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-type who() :: pid() | ne_binary().

-spec title/1 :: (ne_binary()) -> 'ok'.
title(Title) ->
    gen_server:cast(?MODULE, {write, "title ~s~n", [what(Title)]}).

-spec evt/3 :: (who(), who(), ne_binary()) -> 'ok'.
evt(From, To, Desc) ->
    gen_server:cast(?MODULE, {write, "~s->~s: ~s~n", [who(From), who(To), what(Desc)]}).

-spec note/3 :: (who(), 'right' | 'left', ne_binary()) -> 'ok'.
note(Who, Dir, Note) ->
    gen_server:cast(?MODULE, {write, "note ~s of ~s: ~s~n", [Dir, who(Who), what(Note)]}).

trunc() -> gen_server:cast(?MODULE, trunc).
rotate() -> gen_server:cast(?MODULE, rotate).
process_pid(P) ->
    ProcId = wh_json:get_value(<<"Process-ID">>, P),
    case re:run(ProcId, <<".*(\<.*\>)">>, [{capture, [1], binary}]) of
        {match, [M]} -> M;
        {match, M} -> iolist_to_binary(M);
        _ -> ProcId
    end.

reg_who(P, W) -> gen_server:cast(?MODULE, {reg_who, P, W}).

who(P) ->
    case gen_server:call(?MODULE, {who, P}) of
        undefined when is_pid(P) -> pid_to_list(P);
        undefined -> P;
        W -> W
    end.

what(B) when is_binary(B) -> B;
what(IO) when is_list(IO) -> iolist_to_binary(IO).


init(_) ->
    {ok, IO} = file:open(?WEBSEQNAME, [append, raw, delayed_write]),
    {ok, #state{io_device=IO
                ,who_registry=dict:new()
               }}.


handle_call({who, P}, _, #state{who_registry=Who}=State) when is_pid(P) ->
    PBin = wh_util:to_binary(pid_to_list(P)),
    case dict:find(PBin, Who) of
        {ok, V} -> {reply, V, State};
        error -> {reply, P, State}
    end;
handle_call({who, P}, _, #state{who_registry=Who}=State) ->
    case dict:find(P, Who) of
        {ok, V} -> {reply, V, State};
        error -> {reply, P, State}
    end;
handle_call(_,_,S) ->
    {reply, ok, S}.

handle_cast({write, Str, Args}, #state{io_device=IO}=State) ->
    catch file:write(IO, io_lib:format(Str, Args)),
    {noreply, State};
handle_cast(trunc, #state{io_device=IO}=State) ->
    catch file:truncate(IO),
    {noreply, State};
handle_cast(rotate, #state{io_device=OldIO}=State) ->
    file:close(OldIO),
    file:rename(?WEBSEQNAME, iolist_to_binary([?WEBSEQNAME, ".", wh_util:to_binary(wh_util:current_tstamp())])),
    {ok, IO} = file:open(?WEBSEQNAME, [append, raw, delayed_write]),
    {noreply, State#state{io_device=IO}};
handle_cast({reg_who, P, W}, #state{who_registry=Who}=State) when is_pid(P) ->
    PBin = wh_util:to_binary(pid_to_list(P)),
    {noreply, State#state{who_registry=dict:store(PBin, W, Who)}};
    
handle_cast(_,S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, S}.

code_change(_, S, _) ->
    {ok, S}.

terminate(_Reason, #state{io_device=IO}) ->
    file:close(IO),
    lager:debug("webseq terminating: ~p", [_Reason]).
