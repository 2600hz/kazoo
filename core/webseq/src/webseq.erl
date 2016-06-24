%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%% Log messages in a way to make importing to WebSequenceDiagrams.com
%%% easier
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(webseq).

-export([start/1
         ,stop/0, stop/1
         ,running/0

         ,evt/4
         ,title/2
         ,note/4
         ,trunc/1
         ,rotate/1
         ,process_pid/1
         ,reg_who/3
        ]).

-include_lib("webseq/src/webseq.hrl").

-spec what(what()) -> ne_binary().
what(B) when is_binary(B) -> B;
what(IO) when is_list(IO) -> iolist_to_binary(IO).

-define(GPROC_KEY(Type), {'n', 'l', type_key(Type)}).

-type type_key() :: {?MODULE, ne_binary() | '_' | '$1'}.

-spec type_key(diagram_type() | ne_binary() | atom()) -> type_key().
type_key({'file', Filename}) -> type_key(Filename);
type_key({'file', Name, _Filename}) -> type_key(Name);
type_key({'db', Database}) -> type_key(Database);
type_key({'db', Name, _Database}) -> type_key(Name);
type_key(<<_/binary>>=Name) -> {?MODULE, Name};
type_key(A) when is_atom(A) -> {?MODULE, A}.

-type webseq_srv() :: server_ref() | diagram_type() | ne_binary().

-spec start(diagram_type()) ->
                   {'ok', server_ref()} |
                   {'error', 'already_started', server_ref()}.
start(Type) ->
    case server_ref(Type) of
        'undefined' -> start_srv(Type);
        Pid -> {'error', 'already_started', Pid}
    end.

-spec stop() -> 'ok'.
-spec stop(diagram_type()) -> 'ok'.
stop() ->
    %% {{'n', 'l', Key}, PidToMatch, ValueToMatch}
    MatchHead = {?GPROC_KEY('$2'), '_', '$1'},
    Guard = [],
    Result = ['$1', '$2'],

    case gproc:select('n', [{MatchHead, Guard, [Result]}]) of
        [] -> 'ok';
        Pids ->
            _ = [stop_pid(Pid) || Pid <- Pids],
            'ok'
    end.

stop(Type) ->
    case server_ref(Type) of
        'undefined' -> 'ok';
        Pid -> stop_pid([Pid, Type])
    end.

-spec stop_pid([atom() | diagram_type()]) -> 'ok'.
stop_pid([Pid, Type]) ->
    case erlang:is_process_alive(Pid) of
        'true' -> webseq_diagram_srv:stop(Pid);
        'false' -> 'ok'
    end,
    catch gproc:unreg(?GPROC_KEY(Type)),
    'ok'.

-spec running() -> [{pid(), type_key()}].
running() ->
    %% {{'n', 'l', Key}, PidToMatch, ValueToMatch}
    MatchHead = {?GPROC_KEY('$2'), '_', '$1'},
    Guard = [],
    Result = ['$1', '$2'],

    Running = gproc:select('n', [{MatchHead, Guard, [Result]}]),
    lager:debug("running: ~p", [Running]),
    [{Pid, Type} || [Pid, Type] <- Running, erlang:is_process_alive(Pid)].

-spec server_ref(webseq_srv()) -> api_pid().
server_ref(Pid) when is_pid(Pid) -> Pid;
server_ref(Type) ->
    try gproc:lookup_value(?GPROC_KEY(Type)) of
        Pid -> Pid
    catch
        _:_ -> 'undefined'
    end.

-spec start_srv(diagram_type()) -> startlink_ret().
start_srv(Type) ->
    case webseq_diagram_srv:start(Type) of
        {'error', _E}=E ->
            lager:debug("failed to start server: ~p", [_E]),
            E;
        {'ok', Pid} ->
            lager:debug("started server for ~p: ~p", [?GPROC_KEY(Type), Pid]),
            gproc:reg(?GPROC_KEY(Type), Pid),
            {'ok', Pid}
    end.

-spec title(webseq_srv(), what()) -> 'ok'.
title(Srv, Title) ->
    gen_server:cast(server_ref(Srv), {'write', "title ~s~n", [what(Title)]}).

-spec evt(webseq_srv(), who(), who(), what()) -> 'ok'.
evt(Ref, From, To, Desc) ->
    Srv = server_ref(Ref),
    gen_server:cast(Srv, {'write', "~s->~s: ~s~n", [who(Srv, From), who(Srv, To), what(Desc)]}).

-spec note(webseq_srv(), who(), 'right' | 'left', what()) -> 'ok'.
note(Ref, Who, Dir, Note) ->
    Srv = server_ref(Ref),
    gen_server:cast(Srv, {'write', "note ~s of ~s: ~s~n", [Dir, who(Srv, Who), what(Note)]}).

trunc(Srv) -> gen_server:cast(server_ref(Srv), 'trunc').
rotate(Srv) -> gen_server:cast(server_ref(Srv), 'rotate').

process_pid(P) ->
    ProcId = kz_json:get_value(<<"Process-ID">>, P),
    case re:run(ProcId, <<".*(<.*>)">>, [{'capture', [1], 'binary'}]) of
        {'match', [M]} -> M;
        {'match', M} -> iolist_to_binary(M);
        _ -> ProcId
    end.

reg_who(Srv, P, W) -> gen_server:cast(server_ref(Srv), {'reg_who', P, W}).

who(Srv, P) ->
    case catch gen_server:call(server_ref(Srv), {'who', P}) of
        {'EXIT', _} when is_pid(P) -> pid_to_list(P);
        {'EXIT', _} -> P;
        'undefined' when is_pid(P) -> pid_to_list(P);
        'undefined' -> P;
        W -> W
    end.
