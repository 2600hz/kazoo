%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Log messages in a way to make importing to WebSequenceDiagrams.com
%%% easier
%%%
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-include("webseq.hrl").

-spec what(what()) -> kz_term:ne_binary().
what(B) when is_binary(B) -> B;
what(IO) when is_list(IO) -> iolist_to_binary(IO).

-define(GPROC_KEY(Type), {'n', 'l', type_key(Type)}).

-type type_key() :: {?MODULE, kz_term:ne_binary() | '_' | '$1'}.

-spec type_key(diagram_type() | kz_term:ne_binary() | atom()) -> type_key().
type_key({'file', Filename}) -> type_key(Filename);
type_key({'file', Name, _Filename}) -> type_key(Name);
type_key({'db', Database}) -> type_key(Database);
type_key({'db', Name, _Database}) -> type_key(Name);
type_key(<<_/binary>>=Name) -> {?MODULE, Name};
type_key(A) when is_atom(A) -> {?MODULE, A}.

-type webseq_srv() :: kz_types:server_ref() | diagram_type() | kz_term:ne_binary().

-spec start(diagram_type()) ->
          {'ok', kz_types:server_ref()} |
          {'error', 'already_started', kz_types:server_ref()}.
start(Type) ->
    case get_server_ref(Type) of
        'undefined' -> start_srv(Type);
        Pid -> {'error', 'already_started', Pid}
    end.

-spec stop() -> 'ok'.
stop() ->
    %% {{'n', 'l', Key}, PidToMatch, ValueToMatch}
    MatchHead = {?GPROC_KEY('$2'), '_', '$1'},
    Guard = [],
    Result = ['$1', '$2'],

    case gproc:select('n', [{MatchHead, Guard, [Result]}]) of
        [] -> 'ok';
        Pids -> lists:foreach(fun stop_pid/1, Pids)
    end.

-spec stop(diagram_type()) -> 'ok'.
stop(Type) ->
    case get_server_ref(Type) of
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

-spec get_server_ref(webseq_srv()) -> kz_term:api_pid().
get_server_ref(Pid) when is_pid(Pid) -> Pid;
get_server_ref(Type) ->
    try gproc:lookup_value(?GPROC_KEY(Type)) of
        Pid -> Pid
    catch
        _:_ -> 'undefined'
    end.

-spec start_srv(diagram_type()) -> kz_types:startlink_ret().
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
    gen_server:cast(get_server_ref(Srv), {'write', "title ~s~n", [what(Title)]}).

-spec evt(webseq_srv(), who(), who(), what()) -> 'ok'.
evt(Ref, From, To, Desc) ->
    Srv = get_server_ref(Ref),
    gen_server:cast(Srv, {'write', "~s->~s: ~s~n", [who(Srv, From), who(Srv, To), what(Desc)]}).

-spec note(webseq_srv(), who(), 'right' | 'left', what()) -> 'ok'.
note(Ref, Who, Dir, Note) ->
    Srv = get_server_ref(Ref),
    gen_server:cast(Srv, {'write', "note ~s of ~s: ~s~n", [Dir, who(Srv, Who), what(Note)]}).

-spec trunc(webseq_srv()) -> 'ok'.
trunc(Srv) -> gen_server:cast(get_server_ref(Srv), 'trunc').

-spec rotate(webseq_srv()) -> 'ok'.
rotate(Srv) -> gen_server:cast(get_server_ref(Srv), 'rotate').

-spec process_pid(kz_json:object()) -> kz_term:ne_binary().
process_pid(P) ->
    ProcId = kz_json:get_value(<<"Process-ID">>, P),
    case re:run(ProcId, <<".*(<.*>)">>, [{'capture', [1], 'binary'}]) of
        {'match', [M]} -> M;
        {'match', M} -> iolist_to_binary(M);
        _ -> ProcId
    end.

-spec reg_who(webseq_srv(), pid(), kz_term:ne_binary()) -> 'ok'.
reg_who(Srv, P, W) -> gen_server:cast(get_server_ref(Srv), {'reg_who', P, W}).

-spec who(webseq_srv(), kz_term:ne_binary() | pid()) -> kz_term:ne_binary().
who(Srv, P) ->
    case catch gen_server:call(get_server_ref(Srv), {'who', P}) of
        {'EXIT', _} when is_pid(P) -> pid_to_list(P);
        {'EXIT', _} -> P;
        'undefined' when is_pid(P) -> pid_to_list(P);
        'undefined' -> P;
        W -> W
    end.
