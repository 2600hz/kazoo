%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 24 April 2011 by Karl Anderson <karl@2600hz.org>
%%%------------------------------------------------------------------
-module(media_importer).

-include_lib("couchbeam/include/couchbeam.hrl").

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

-spec main/1 :: (Args) -> no_return() when
      Args :: string().
main(Args) ->
    ensure_started(sasl),
    ensure_started(crypto),
%%    ensure_started(riak_err),
    ensure_started(ibrowse),
    ensure_started(couchbeam),

    {ok, Options, Paths} = parse_args(Args),

    Server = conn_to_server(Options),

    DbName = get_value(db, Options),
    {ok, Db} = couchbeam:open_db(Server, DbName),
    io:format("Open database ~-78s", [DbName]),
    case couchbeam:db_exists(Server, DbName) of
        true ->
            io:format("SUCCESS~n", []);
        false ->
            case couchbeam:create_db(Server, DbName) of
                {ok, _} ->
                    io:format("CREATED~n", []);
                Error ->
                    io:format("FAILED~nerror cause: ~p~n", [Error]),
                    erlang:halt(1)
            end
    end,

    [to_couch(Dir, Db) || Dir <- Paths],

    erlang:halt(0).

-spec conn_to_server/1 :: (Options) -> #server{} when
      Options :: proplist().
conn_to_server(Options) ->
    Host = get_value(host, Options),
    Port = get_value(port, Options),
    Log = io_lib:format("Attempting to connect to ~s:~p, ", [Host, Port]),
    {Opts, CredLog} = case {get_value(user, Options), get_value(pwd, Options)} of
                          {_, ""} ->
                              {[], "without creds"};
                          {"", _} ->
                              {[], "without creds"};
                          Creds ->
                              {[{basic_auth, Creds}], "with creds"}
                      end,
    io:format("~-92s", [Log ++ CredLog]),
    Server = couchbeam:server_connection(Host, wh_util:to_list(Port), "", Opts),
    case couchbeam:server_info(Server) of
        {'ok', ServerData}  ->
            CouchVersion = wh_json:get_value(<<"version">>, ServerData),
            BigCouchVersion = wh_json:get_value(<<"bigcouch">>, ServerData),
            io:format("SUCCESS~n", []),
            io:format("CouchDB version ~76s~s~n", [" ", CouchVersion]),
            is_binary(BigCouchVersion) andalso io:format("BigCouch version ~75s~s~n", [" ", BigCouchVersion]),
            Server;
        Error ->
            io:format("FAILED~nerror cause: ~p~n", [Error]),
            erlang:halt(1)
    end.

-spec to_couch/2 :: (Dir, Db) -> no_return() when
      Dir :: string(),
      Db :: #db{}.
to_couch(Dir, Db) ->
    [import_file(F, Db) || F <- filelib:wildcard(Dir)].

-spec import_file/2 :: (Path, Db) -> no_return() when
      Path :: string(),
      Db :: #db{}.
import_file(Path, Db) ->
    io:format("Import file ~-80s", [Path]),
    {File, Id} = parse_path(wh_util:to_binary(Path)),
    case couchbeam:lookup_doc_rev(Db, binary_to_list(Id)) of
        {ok, Rev} ->
            {ok, Content} = file:read_file(Path),
            couchbeam:put_attachment(Db, Id, File, Content, [{'content_type', "audio/x-wav"}, {'rev', Rev}]),
            io:format("UPDATED~n", []);
        _ ->
            case couchbeam:save_doc(Db, create_doc(File, Id)) of
                {ok, _} ->
                    {ok, Rev} = couchbeam:lookup_doc_rev(Db, Id),
                    {ok, Content} = file:read_file(Path),
                    couchbeam:put_attachment(Db, Id, File, Content, [{'content_type', "audio/x-wav"}, {'rev', Rev}]),
                    io:format("SUCCESS~n", []);
                _Else ->
                    io:format("FAILED~nerror cause: ~p~n", [_Else]),
                    erlang:halt(1)
            end
    end.

-spec create_doc/2 :: (File, Id) -> json_object() when
      File :: binary(),
      Id :: binary().
create_doc(File, Id) ->
    {struct, [
               {<<"_id">>, Id}
              ,{<<"pvt_type">>, <<"media">>}
              ,{<<"display_name">>, File}
              ,{<<"description">>, <<"">>}
             ]}.

-spec parse_path/1 :: (Path) -> tuple(binary(), binary()) when
      Path :: string().
parse_path(Path) ->
    PTokens = binary:split(Path, <<"/">>, [global, trim]),
    File = lists:last(PTokens),
    {File, hd(binary:split(File, <<".">>))}.

-spec parse_args/1 :: (Args) -> tuple(ok, proplist(), list()) | tuple(error, atom()) when
      Args :: string().
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

-spec display_help/0 :: () -> no_return().
display_help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "media_importer", "<path_to_fs_sounds> ..."),
    erlang:halt().

-spec option_spec_list/0 :: () -> proplist().
option_spec_list() ->
    [
     {help,    $?,    "help",    undefined,                     "Show the program options"},
     {host,    $h,    "host",    {string, net_adm:localhost()}, "Database server host"},
     {port,    $P,    "port",    {integer, 5984},                "Database server port"},
     {pwd,     $p,    "pwd",     {string, ""},                   "Database server password"},
     {user,    $u,    "user",    {string, ""},                  "Database server username"},
     {db,      $d,    "db",      {string, "system_media"},      "Database name to import to"}
    ].

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
