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
-include_lib("whistle/include/wh_types.hrl").

-export([main/1]).

-import(proplists, [is_defined/2, get_value/2]).

-spec main/1 :: (string()) -> none().
main(Args) ->
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
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
                    timer:sleep(100),
                    io:format("CREATED~n", []);
                Error ->
                    io:format("FAILED~n~p: error cause: ~p~n", [?LINE, Error]),
                    erlang:halt(1)
            end
    end,

    try [to_couch(Dir, Db) || Dir <- Paths] of
        _ -> ok
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            io:format("~nERROR: ~p:~p~n", [_E, _R]),
            [io:format("~p~n", [S]) || S <- ST]
    end,

    erlang:halt(0).

-spec conn_to_server/1 :: (wh_proplist()) -> #server{}.
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
            io:format("FAILED~n~p: error cause: ~p~n", [?LINE, Error]),
            erlang:halt(1)
    end.

-spec to_couch/2 :: (string(), #db{}) -> any().
to_couch(Dir, Db) ->
    [import_file(F, Db) || F <- filelib:wildcard(Dir)].

-spec import_file/2 :: (string(), #db{}) -> any().
import_file(Path, Db) ->
    io:format("Import file ~-80s", [Path]),
    erlang:garbage_collect(),
    timer:sleep(100),

    {File, Id} = parse_path(wh_util:to_binary(Path)),
    case couchbeam:lookup_doc_rev(Db, binary_to_list(Id)) of
        {error, not_found} ->
            case couchbeam:save_doc(Db, create_doc(File, Id)) of
                {ok, Saved} ->
                    {ok, Content} = file:read_file(Path),
                    Opts = [{'content_type', "audio/x-wav"}
                            ,{'content_length', byte_size(Content)}
                            ,{'rev', wh_json:get_string_value(<<"_rev">>, Saved)}
                           ],
                    save_attachment(Db, Id, File, Content, Opts, "CREATED");
                {error, conflict} ->
                    io:format("EXISTS~n", []);
                {error, retry_later} ->
                    timer:sleep(100),
                    import_file(Path, Db);
                _Else ->
                    io:format("FAILED~n~p: error cause: ~p~n", [?LINE, _Else]),
                    erlang:halt(1)
            end;
        {error, retry_later} ->
            timer:sleep(250),
            import_file(Path, Db);
        {error, _E} ->
            io:format("REV ERROR: ~p~n", [_E]);
        Rev when is_binary(Rev) ->
            {ok, Content} = file:read_file(Path),
            Opts = [{'content_type', "audio/x-wav"}
                    ,{'content_length', byte_size(Content)}
                    ,{'rev', wh_util:to_list(Rev)}
                   ],
            save_attachment(Db, Id, File, Content, Opts, "UPDATED")
    end.

save_attachment(Db, Id, File, Content, Opts, Success) ->
    case couchbeam:put_attachment(Db, Id, File, Content, Opts) of
        {error, {ok, Err, _, _Body}} ->
            io:format("DB ERROR ~s: ~s~n", [Err, _Body]);
        {error, retry_later} ->
            io:format("DB ERROR: retry later~n", []);
        {error, conflict} ->
            io:format("DB ERROR: conflict ~p~n", [props:get_value(rev, Opts)]);
        {error, _E} ->
            io:format("DB ERROR: ~p~n", [_E]);
        _OK ->
            io:format("~s~n", [Success])
    end.

-spec create_doc/2 :: (ne_binary(), ne_binary()) -> wh_json:object().
create_doc(File, Id) ->
    wh_json:from_list([{<<"_id">>, Id}
                       ,{<<"pvt_type">>, <<"media">>}
                       ,{<<"display_name">>, File}
                       ,{<<"description">>, <<"">>}
                      ]).

-spec parse_path/1 :: (ne_binary()) -> {ne_binary(), ne_binary()}.
parse_path(Path) ->
    PTokens = binary:split(Path, <<"/">>, [global, trim]),
    File = lists:last(PTokens),
    {File, hd(binary:split(File, <<".">>))}.

-spec parse_args/1 :: (string()) -> {'ok', wh_proplist(), list()} |
                                    {'error', atom()}.
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

-spec display_help/0 :: () -> none().
display_help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "media_importer", "<path_to_fs_sounds> ..."),
    erlang:halt().

-spec option_spec_list/0 :: () -> [{atom(), byte(), string(), 'undefined' | tuple(), string()},...].
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
