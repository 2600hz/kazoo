%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%% Log messages in a way to make importing to WebSequenceDiagrams.com
%%% easier
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(webseq_diagram_srv).

-behaviour(gen_server).

-export([start/1
         ,stop/1
         ,evt/4
         ,title/2
         ,note/4
         ,trunc/1
         ,rotate/1
         ,process_pid/1
         ,reg_who/3
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,code_change/3
         ,terminate/2
        ]).

-include("webseq.hrl").

-record(state, {type :: diagram_type()
                ,name :: ne_binary()
                ,io_device :: 'undefined' | file:io_device()
                ,who_registry :: dict()
               }).
-type state() :: #state{}.

-spec start(diagram_type()) -> startlink_ret().
start(Type) ->
    gen_server:start(?MODULE, [Type], []).

-spec stop(server_ref()) -> 'ok'.
stop(Pid) ->
    gen_server:call(Pid, 'stop').

-spec title(server_ref(), what()) -> 'ok'.
title(Srv, Title) ->
    gen_server:cast(Srv, {'write', "title ~s~n", [what(Title)]}).

-spec evt(server_ref(), who(), who(), what()) -> 'ok'.
evt(Srv, From, To, Desc) ->
    gen_server:cast(Srv, {'write', "~s->~s: ~s~n", [who(Srv, From), who(Srv, To), what(Desc)]}).

-spec note(server_ref(), who(), 'right' | 'left', what()) -> 'ok'.
note(Srv, Who, Dir, Note) ->
    gen_server:cast(Srv, {'write', "note ~s of ~s: ~s~n", [Dir, who(Srv, Who), what(Note)]}).

-spec trunc(server_ref()) -> 'ok'.
trunc(Srv) -> gen_server:cast(Srv, 'trunc').

-spec rotate(server_ref()) -> 'ok'.
rotate(Srv) -> gen_server:cast(Srv, 'rotate').

-spec process_pid(wh_json:object()) -> api_binary().
process_pid(P) ->
    ProcId = wh_json:get_value(<<"Process-ID">>, P),
    case re:run(ProcId, <<".*(\<.*\>)">>, [{'capture', [1], 'binary'}]) of
        {'match', [M]} -> M;
        {'match', M} -> iolist_to_binary(M);
        _ -> ProcId
    end.

-spec reg_who(server_ref(), pid(), ne_binary()) -> 'ok'.
reg_who(Srv, P, W) -> gen_server:cast(Srv, {'reg_who', P, W}).

-spec who(server_ref(), ne_binary() | pid()) -> ne_binary().
who(Srv, P) ->
    case catch gen_server:call(Srv, {'who', P}) of
        {'EXIT', _} when is_pid(P) -> wh_util:to_binary(pid_to_list(P));
        {'EXIT', _} -> P;
        'undefined' when is_pid(P) -> wh_util:to_binary(pid_to_list(P));
        'undefined' -> P;
        W -> W
    end.

-spec what(what()) -> ne_binary().
what(B) when is_binary(B) -> B;
what(IO) when is_list(IO) -> iolist_to_binary(IO).

-spec init(diagram_type() | [diagram_type()]) ->
                  {'ok', state()} |
                  {'stop', _}.
init([Type]) ->
    init(Type);
init({'file', <<_/binary>>=Filename}) ->
    init({'file', Filename, Filename});
init({'file', Name, PreFilename}=Type) ->
    wh_util:put_callid(Name),

    Filename = create_filename(PreFilename),

    case start_file(Filename) of
        {'ok', IO} ->
            lager:debug("webseq tracing ~s to file: ~s", [Name, Filename]),
            {'ok', #state{io_device=IO
                          ,name=Name
                          ,type=Type
                          ,who_registry=dict:new()
                         }};
        {'error', 'eaccess'} ->
            lager:info("failed to open ~s, eaccess error - check permissions", [Filename]),
            {'stop', 'eaccess'};
        {'error', E} ->
            lager:info("failed to open ~s, error: ~s", [Filename, E]),
            {'stop', E}
    end;
init({'db', Database}) ->
    init({'db', wh_util:rand_hex_binary(4), Database});
init({'db', Name, Database}=Type) ->
    wh_util:put_callid(Name),

    case couch_mgr:db_exists(Database) of
        'true' ->
            lager:debug("webseq tracing ~s to db: ~s", [Name, Database]),
            {'ok', #state{name=Name
                          ,type=Type
                          ,who_registry=dict:new()
                         }};
        'false' ->
            lager:debug("database ~s not found", [Database]),
            {'error', 'not_found'}
    end.

handle_call('stop', _, State) ->
    {'stop', 'normal', 'ok', State};
handle_call({'who', P}, _, #state{who_registry=Who}=State) when is_pid(P) ->
    PBin = wh_util:to_binary(pid_to_list(P)),
    case dict:find(PBin, Who) of
        {'ok', V} -> {'reply', V, State};
        'error' -> {'reply', P, State}
    end;
handle_call({'who', P}, _, #state{who_registry=Who}=State) ->
    case dict:find(P, Who) of
        {'ok', V} -> {'reply', V, State};
        'error' -> {'reply', P, State}
    end;
handle_call(_,_,S) ->
    {'reply', 'ok', S}.

handle_cast({'write', Str, Args}, #state{type={'file', _Name, _Filename}
                                         ,io_device=IO
                                        }=State) ->
    catch file:write(IO, io_lib:format(Str, Args)),
    {'noreply', State};
handle_cast({'write', Str, Args}, #state{type={'db', Name, Database}}=State) ->
    write_to_db(Database, Name, Str, Args),
    {'noreply', State};

handle_cast('trunc', #state{io_device=IO
                            ,type={'file', _Name, _Filename}
                           }=State) ->
    catch file:truncate(IO),
    {'noreply', State};
handle_cast('trunc', #state{type={'db', Name, Database}}=State) ->
    trunc_database(Database, Name),
    {'noreply', State};

handle_cast('rotate', #state{io_device=OldIO
                             ,type={'file', _Name, Filename}
                            }=State) ->
    _ = file:close(OldIO),
    {'ok', IO} = start_file(Filename),
    lager:debug("rotated ~s", [Filename]),
    {'noreply', State#state{io_device=IO}};
handle_cast('rotate', #state{type={'db', Name, Database}}=State) ->
    rotate_db(Database, Name),
    {'noreply', State};

handle_cast({'reg_who', P, W}, #state{who_registry=Who}=State) when is_pid(P) ->
    PBin = wh_util:to_binary(pid_to_list(P)),
    {'noreply', State#state{who_registry=dict:store(PBin, W, Who)}};
handle_cast(_Msg, S) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', S}.

handle_info(_Info, S) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', S}.

code_change(_, S, _) ->
    {'ok', S}.

terminate(_Reason, #state{io_device='undefined'}) ->
    gproc:goodbye(),
    lager:debug("webseq terminating: ~p", [_Reason]);
terminate(_Reason, #state{io_device=IO}) ->
    _ = file:close(IO),
    gproc:goodbye(),
    lager:debug("webseq terminating: ~p", [_Reason]).

-spec webseq_doc(ne_binary(), text(), text()) -> wh_json:object().
webseq_doc(Name, Str, Args) ->
    Line = iolist_to_binary(io_lib:format(Str, Args)),
    wh_json:from_list([{<<"line">>, Line}
                       ,{<<"name">>, Name}
                      ]).

-spec write_to_db(ne_binary(), ne_binary(), text(), text()) -> 'ok'.
write_to_db(Database, Name, Str, Args) ->
    Doc = wh_doc:update_pvt_parameters(
            webseq_doc(Name, Str, Args)
            ,Database
            ,[{'type', <<"webseq">>}]
           ),
    case couch_mgr:save_doc(Database, Doc) of
        {'ok', _} -> 'ok';
        {'error', E} ->
            lager:debug("failed to write ~s with ~s: ~p", [Str, Args, E]),
            throw(E)
    end.

-spec start_file(ne_binary()) ->
                        {'ok', file:io_device()} |
                        {'error', _}.
start_file(Filename) ->
    _ = file:rename(Filename, iolist_to_binary([Filename, ".", wh_util:to_binary(wh_util:current_tstamp())])),
    file:open(Filename, ['append', 'raw', 'delayed_write']).

-spec trunc_database(ne_binary(), ne_binary()) -> 'ok'.
trunc_database(Database, Name) ->
    case get_docs_by_name(Database, Name) of
        {'ok', []} -> 'ok';
        {'ok', Docs} ->
            lager:debug("deleting docs for ~s in ~s", [Name, Database]),
            {'ok', _} = couch_mgr:del_docs(Database, Docs),
            'ok';
        {'error', 'not_found'} ->
            init_db(Database),
            trunc_database(Database, Name);
        {'error', E} ->
            lager:debug("failed to delete docs for ~s in ~s: ~p", [Name, Database, E]),
            throw(E)
    end.

-spec get_docs_by_name(ne_binary(), ne_binary()) ->
                              {'ok', wh_json:objects()} |
                              {'error', _}.
get_docs_by_name(Database, Name) ->
    get_docs_by_name(Database, Name, []).
get_docs_by_name(Database, Name, Opts) ->
    Options = props:insert_value('key', Name, Opts),
    couch_mgr:get_results(Database, <<"webseq/listing_by_name">>, Options).

-spec rotate_db(ne_binary(), ne_binary()) -> 'ok'.
rotate_db(Database, Name) ->
    case get_docs_by_name(Database, Name, ['include_docs']) of
        {'ok', []} -> 'ok';
        {'ok', Docs} ->
            _ = rotate_db(Database, Name, Docs),
            lager:debug("rotated ~s in ~s", [Name, Database]);
        {'error', 'not_found'} ->
            init_db(Database),
            rotate_db(Database, Name);
        {'error', E} ->
            lager:debug("failed to rotate ~s in ~s: ~p", [Name, Database, E]),
            throw(E)
    end.

-spec rotate_db(ne_binary(), ne_binary(), wh_json:objects()) -> {'ok', wh_json:objects()}.
rotate_db(Database, Name, Docs) ->
    RotatedName = <<Name/binary, ".", (wh_util:rand_hex_binary(3))/binary>>,
    Rotated = [rotate_doc(RotatedName, wh_json:get_value(<<"doc">>, Doc)) || Doc <- Docs],
    {'ok', _} = couch_mgr:save_docs(Database, Rotated).

-spec rotate_doc(ne_binary(), wh_json:object()) -> wh_json:object().
rotate_doc(RotatedName, Doc) ->
    wh_json:set_value(<<"name">>, RotatedName
                      ,wh_doc:update_pvt_parameters(Doc, 'undefined')
                     ).

-spec init_db(ne_binary()) -> 'ok'.
init_db(Database) ->
    lager:debug("refreshing ~s", [Database]),
    Views = whapps_util:get_views_json('webseq', "views"),
    _ = whapps_util:update_views(Database, Views, 'true'),
    lager:debug("refreshed ~s", [Database]).

create_filename(<<"/", _/binary>> = Filename) ->
    Filename;
create_filename(<<"~/", _/binary>> = Filename) ->
    Filename;
create_filename(PreFilename) ->
    <<"/tmp/", PreFilename/binary, ".wsd">>.
