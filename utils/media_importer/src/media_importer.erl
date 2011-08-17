%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 24 April 2011 by Karl Anderson <karl@2600hz.org>
%%%------------------------------------------------------------------
-module(media_importer).

-export([main/1]).

-import(proplists, [is_defined/2, get_value/2]).

-type proplist() :: list(tuple(binary() | atom(), term())) | [].
-type argv() :: string() | undefined.
-type json_string() :: atom() | binary().
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].
-type json_object() :: {struct, [{json_string(), json_term()}]}.
-type json_iolist() :: {json, iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist().

-spec(main/1 :: (Args :: string()) -> no_return()).
main(Args) ->   
    error_logger:tty(false),
    {ok, Options, Paths} = parse_args(Args),
    application:start(whistle_couch),
    try 
        ok = set_couch_host(Options),
        Db = get_value(db, Options),
        couch_mgr:db_create(Db),
        [to_couch(Dir, Db) || Dir <- Paths]
    catch
        _:_=E ->
            io:format("~n~nERROR: unable to connect to datastore: ~p~n", [E])
    end,
    erlang:halt().    

-spec(set_couch_host/1 :: (Options :: proplist()) -> ok | tuple(error, term())).
set_couch_host(Options) ->
    conn_to_couch(
       get_value(host, Options)
      ,get_value(port, Options)
      ,get_value(user, Options)
      ,get_value(pwd, Options)
     ).

-spec(conn_to_couch/4 :: (Host :: argv(), Port :: argv(), User :: argv(), Pwd :: argv()) 
                         -> ok | tuple(error, term())).
conn_to_couch(undefined, _, _, _) ->
    {error, no_host};
conn_to_couch(Host, undefined, _, undefined) ->
    couch_mgr:set_host(Host);
conn_to_couch(Host, undefined, User, Pwd) ->
    couch_mgr:set_host(Host, User, Pwd);
conn_to_couch(Host, Port, _, undefined) ->
    couch_mgr:set_host(Host, Port);
conn_to_couch(Host, Port, User, Pwd) ->
    couch_mgr:set_host(Host, Port, User, Pwd).

-spec(to_couch/2 :: (Dir :: string(), Db :: string()) -> no_return()).
to_couch(Dir, Db) ->
    [import_file(F, Db) || F <- filelib:wildcard(Dir)].

-spec(import_file/2 :: (Path :: string(), Db :: string()) -> no_return()).
import_file(Path, Db) ->
    io:format("Import file ~p . . . ", [Path]),
    {File, Id} = parse_path(wh_util:to_binary(Path)),
    case couch_mgr:lookup_doc_rev(Db, Id) of
        {ok, Rev} ->
            {ok, Content} = file:read_file(Path),                 
            couch_mgr:put_attachment(Db, Id, File, Content, [{'content_type', "audio/x-wav"}, {'rev', Rev}]),
            io:format("UPDATED~n", []);
        _ ->            
            case couch_mgr:save_doc(Db, create_doc(File, Id)) of
                {ok, _} ->
                    {ok, Rev} = couch_mgr:lookup_doc_rev(Db, Id),
                    {ok, Content} = file:read_file(Path),                 
                    couch_mgr:put_attachment(Db, Id, File, Content, [{'content_type', "audio/x-wav"}, {'rev', Rev}]),
                    io:format("SUCCESS~n", []);
                _Else ->
                    io:format("FAILED ~p~n", [_Else])
            end            
    end.

-spec(create_doc/2 :: (File :: binary(), Id :: binary()) -> json_object()).
create_doc(File, Id) ->
    {struct, [
               {<<"_id">>, Id}
              ,{<<"pvt_type">>, <<"media">>}
              ,{<<"display_name">>, File}
              ,{<<"description">>, <<"">>}
             ]}.

-spec(parse_path/1 :: (Path :: string()) -> tuple(binary(), binary())).
parse_path(Path) ->
    PTokens = binary:split(Path, <<"/">>, [global, trim]),
    File = lists:last(PTokens),
    {File, hd(binary:split(File, <<".">>))}.

-spec(parse_args/1 :: (Args :: string()) -> tuple(ok, proplist(), list()) | tuple(error, atom())).    
parse_args(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, _}} when not is_list(Options) ->
            display_help(),
            {error, invalid_args};
        {ok, {_, []}} ->            
            display_help(),
            {error, no_paths};        
        {ok, {Options, Paths}} ->
            is_defined(help, Options) andalso display_help(),
            {ok, Options, Paths};         
        {error, {Reason, _}} ->
            display_help(),
            {error, Reason}
    end.

-spec(display_help/0 :: () -> no_return()).                             
display_help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "media_importer", "<path_to_fs_sounds> ..."),
    erlang:halt().

-spec(option_spec_list/0 :: () -> proplist()).
option_spec_list() ->
    [
     {help,    $?,    "help",    undefined,                     "Show the program options"},
     {host,    $h,    "host",    {string, net_adm:localhost()}, "Database server host"},
     {port,    $P,    "port",    integer,                       "Database server port"},
     {pwd,     $p,    "pwd",     string,                        "Database server password"},
     {user,    $u,    "user",    {string, "admin"},             "Database server username"},
     {db,      $d,    "db",      {string, "system_media"},      "Database name to import to"}
    ].
