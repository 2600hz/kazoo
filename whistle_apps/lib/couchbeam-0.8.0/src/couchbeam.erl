%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include_lib("couchbeam/include/couchbeam.hrl").

-define(TIMEOUT, infinity).

% generic functions
-export([start/0, stop/0, version/0]).

%% utilities urls
-export([server_url/1, uuids_url/1, db_url/1, doc_url/2, make_url/3]).
-export([db_request/4, db_request/5, db_request/6]).

%% API urls
-export([server_connection/0, server_connection/2, server_connection/4,
        server_info/1,
        get_uuid/1, get_uuids/2,
        replicate/2, replicate/3, replicate/4,
        all_dbs/1, db_exists/2,
        create_db/2, create_db/3, create_db/4,
        open_db/2, open_db/3,
        open_or_create_db/2, open_or_create_db/3, open_or_create_db/4,
        delete_db/1, delete_db/2,
        db_info/1, design_info/2,
        save_doc/2, save_doc/3,
        doc_exists/2,
        open_doc/2, open_doc/3,
        delete_doc/2, delete_doc/3,
        save_docs/2, save_docs/3, delete_docs/2, delete_docs/3,
        lookup_doc_rev/2, lookup_doc_rev/3,
        fetch_attachment/3, fetch_attachment/4, fetch_attachment/5,
        stream_fetch_attachment/4, stream_fetch_attachment/5,
        stream_fetch_attachment/6, delete_attachment/3,
        delete_attachment/4, put_attachment/4, put_attachment/5,
        all_docs/1, all_docs/2, view/2, view/3,
        ensure_full_commit/1, ensure_full_commit/2,
        compact/1, compact/2, view_cleanup/1
        ]).


%% --------------------------------------------------------------------
%% Generic utilities.
%% --------------------------------------------------------------------

%% @doc Start the couchbeam process. Useful when testing using the shell.
start() ->
    couchbeam_deps:ensure(),
    _ = application:load(couchbeam),
    couchbeam_util:start_app_deps(couchbeam),
    application:start(couchbeam).

%% @doc Stop the couchbeam process. Useful when testing using the shell.
stop() ->
    application:stop(couchbeam).

%% @spec () -> Version
%%     Version = string()
%% @doc Return the version of the application.
version() ->
    {ok, Version} = application:get_key(couchbeam, vsn),
    Version.

%% --------------------------------------------------------------------
%% API functins.
%% --------------------------------------------------------------------

%% @doc Create a server for connectiong to a CouchDB node
%% @equiv server_connection("127.0.0.1", 5984, "", [], false)
server_connection() ->
    #server{host="127.0.0.1", port=5984, prefix="", options=[]}.

%% @doc Create a server for connectiong to a CouchDB node
%% @equiv server_connection(Host, Port, "", [])

server_connection(Host, Port) ->
    server_connection(Host, Port, "", []).

%% @doc Create a server for connectiong to a CouchDB node
%%
%%      Connections are made to:
%%      ```http://Host:PortPrefix'''
%%
%%      If ssl is set https is used.
%%
%%      For a description of SSL Options, look in the <a href="http://www.erlang.org/doc/apps/ssl/index.html">ssl</a> manpage.
%%
%% @spec server_connection(Host::string(), Port::integer(),
%%                        Prefix::string(), Options::optionList())
%%                        -> Server::server()
%% optionList() = [option()]
%% option() =
%%          {is_ssl, boolean()}                |
%%          {ssl_options, [SSLOpt]}            |
%%          {pool_name, atom()}                |
%%          {proxy_host, string()}             |
%%          {proxy_port, integer()}            |
%%          {proxy_user, string()}             |
%%          {proxy_password, string()}         |
%%          {basic_auth, {username(), password()}} |
%%          {cookie, string()}                 |
%%          {oauth, oauthOptions()}
%%
%% username() = string()
%% password() = string()
%% SSLOpt = term()
%% oauthOptions() = [oauth()]
%% oauth() =
%%          {consumer_key, string()} |
%%          {token, string()} |
%%          {token_secret, string()} |
%%          {consumer_secret, string()} |
%%          {signature_method, string()}
%%
server_connection(Host, Port, Prefix, Options) when is_binary(Port) ->
    server_connection(Host, binary_to_list(Port), Prefix, Options);
server_connection(Host, Port, Prefix, Options) when is_list(Port) ->
    server_connection(Host, list_to_integer(Port), Prefix, Options);
server_connection(Host, Port, Prefix, Options) when is_integer(Port), Port =:=443 ->
    Options1 = case proplists:get_value(ssl_options, Options) of
        undefined ->
            [{is_ssl, true}, {ssl_options, []}|Options];
        _ ->
            [{is_ssl, true}|Options]
    end,

    #server{host=Host, port=Port, prefix=Prefix, options=Options1};
server_connection(Host, Port, Prefix, Options) ->
    #server{host=Host, port=Port, prefix=Prefix, options=Options}.

%% @doc Get Information from the server
%% @spec server_info(server()) -> {ok, iolist()}
server_info(#server{options=IbrowseOpts}=Server) ->
    Url = binary_to_list(iolist_to_binary(server_url(Server))),
    case couchbeam_httpc:request(get, Url, ["200"], IbrowseOpts) of
        {ok, _Status, _Headers, Body} ->
            Version = ejson:decode(Body),
            {ok, Version};
        Error -> Error
    end.

%% @doc Get one uuid from the server
%% @spec get_uuid(server()) -> lists()
get_uuid(Server) ->
    couchbeam_uuids:get_uuids(Server, 1).

%% @doc Get a list of uuids from the server
%% @spec get_uuids(server(), integer()) -> lists()
get_uuids(Server, Count) ->
    couchbeam_uuids:get_uuids(Server, Count).


%% @doc Handle replication. Pass an object containting all informations
%% It allows to pass for example an authentication info
%% ```
%% RepObj = {[
%% {<<"source">>, <<"sourcedb">>},
%% {<<"target">>, <<"targetdb">>},
%% {<<"create_target">>, true}
%% ]}
%% replicate(Server, RepObj).
%% '''
%%
%% @spec replicate(Server::server(), RepObj::{list()})
%%          -> {ok, Result}|{error, Error}
replicate(#server{options=IbrowseOpts}=Server, RepObj) ->
    Url = make_url(Server, "_replicate", []),
    Headers = [{"Content-Type", "application/json"}],
    JsonObj = ejson:encode(RepObj),

    case couchbeam_httpc:request_stream({self(), once}, post, Url, IbrowseOpts, Headers,
            JsonObj) of
        {ok, ReqId} ->
            couchbeam_changes:wait_for_change(ReqId);
        {error, Error} -> {error, Error}
    end.

%% @doc Handle replication.
%% @spec replicate(Server::server(), Source::string(), Target::target())
%%          ->  {ok, Result}|{error, Error}
replicate(Server, Source, Target) ->
    replicate(Server, Source, Target, {[]}).

%% @doc handle Replication. Allows to pass options with source and
%% target.  Options is a Json object.
%% ex:
%% ```
%% Options = {[{<<"create_target">>, true}]},
%% couchbeam:replicate(S, "testdb", "testdb2", Options).
%% '''
replicate(Server, Source, Target, {Prop}) ->
    RepProp = [
        {<<"source">>, couchbeam_util:to_binary(Source)},
        {<<"target">>, couchbeam_util:to_binary(Target)} |Prop
    ],

    replicate(Server, {RepProp}).



%% @doc get list of databases on a CouchDB node
%% @spec all_dbs(server()) -> {ok, iolist()}
all_dbs(#server{options=IbrowseOpts}=Server) ->
    Url = make_url(Server, "_all_dbs", []),
    case couchbeam_httpc:request(get, Url, ["200"], IbrowseOpts) of
        {ok, _, _, Body} ->
            AllDbs = ejson:decode(Body),
            {ok, AllDbs};
        Error ->
            Error
    end.

%% @doc test if db with dbname exists on the CouchDB node
%% @spec db_exists(server(), string()) -> boolean()
db_exists(#server{options=IbrowseOpts}=Server, DbName) ->
    Url = make_url(Server, DbName, []),
    case couchbeam_httpc:request(head, Url, ["200"], IbrowseOpts) of
        {ok, _, _, _} -> true;
        _Error -> false
    end.

%% @doc Create a database and a client for connectiong to it.
%% @equiv create_db(Server, DbName, [], [])
create_db(Server, DbName) ->
    create_db(Server, DbName, [], []).

%% @doc Create a database and a client for connectiong to it.
%% @equiv create_db(Server, DbName, Options, [])
create_db(Server, DbName, Options) ->
    create_db(Server, DbName, Options, []).

%% @doc Create a database and a client for connectiong to it.
%%
%%      Connections are made to:
%%      ```http://Host:PortPrefix/DbName'''
%%
%% If ssl is set https is used. See server_connections for options.
%% Params is a list of optionnal query argument you want to pass to the
%% db. Useful for bigcouch for example.
%%
%% @spec create_db(Server::server(), DbName::string(),
%%                 Options::optionList(), Params::list()) -> {ok, db()|{error, Error}}
create_db(#server{options=IbrowseOpts}=Server, DbName, Options, Params) ->
    Options1 = couchbeam_util:propmerge1(Options, IbrowseOpts),
    Url = make_url(Server, dbname(DbName), Params),
    case couchbeam_httpc:request(put, Url, ["201", "202"], Options1) of
        {ok, _Status, _Headers, _Body} ->
            {ok, #db{server=Server, name=DbName, options=Options1}};
        {error, {ok, "412", _, _}} ->
            {error, db_exists};
       Error ->
          Error
    end.

%% @doc Create a client for connection to a database
%% @equiv open_db(Server, DbName, [])
open_db(Server, DbName) ->
    open_db(Server, DbName, []).

%% @doc Create a client for connection to a database
%% @spec open_db(Server::server(), DbName::string(), Options::optionList())
%%              -> {ok, db()}
open_db(#server{options=IbrowseOpts}=Server, DbName, Options) ->
    Options1 = couchbeam_util:propmerge1(Options, IbrowseOpts),
    {ok, #db{server=Server, name=dbname(DbName), options=Options1}}.


%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @equiv open_or_create_db(Server, DbName, [], [])
open_or_create_db(Server, DbName) ->
    open_or_create_db(Server, DbName, [], []).

%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @equiv open_or_create_db(Server, DbName, Options, [])
open_or_create_db(Server, DbName, Options) ->
    open_or_create_db(Server, DbName, Options, []).

%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @spec open_or_create_db(server(), string(), list(), list()) -> {ok, db()|{error, Error}}
open_or_create_db(#server{options=IbrowseOpts}=Server, DbName, Options, Params) ->
    Url = make_url(Server, DbName, []),
    IbrowseOpts1 = couchbeam_util:propmerge1(Options, IbrowseOpts),
    case couchbeam_httpc:request(get, Url, ["200"], IbrowseOpts1) of
        {ok, _, _, _} ->
            open_db(Server, DbName, Options);
        {error, {ok, "404", _, _}} ->
            create_db(Server, DbName, Options, Params);
        Error ->
            Error
    end.

%% @doc delete database
%% @equiv delete_db(Server, DbName)
delete_db(#db{server=Server, name=DbName}) ->
    delete_db(Server, DbName).

%% @doc delete database
%% @spec delete_db(server(), DbName) -> {ok, iolist()|{error, Error}}
delete_db(#server{options=IbrowseOpts}=Server, DbName) ->
    Url = make_url(Server, dbname(DbName), []),
    case couchbeam_httpc:request(delete, Url, ["200"], IbrowseOpts) of
        {ok, _, _, Body} ->
            {ok, ejson:decode(Body)};
        Error ->
            Error
    end.

%% @doc get database info
%% @spec db_info(db()) -> {ok, iolist()|{error, Error}}
db_info(#db{server=Server, name=DbName, options=IbrowseOpts}) ->
    Url = make_url(Server, DbName, []),
    case couchbeam_httpc:request(get, Url, ["200"], IbrowseOpts) of
        {ok, _Status, _Headers, Body} ->
            Infos = ejson:decode(Body),
            {ok, Infos};
        {error, {ok, "404", _, _}} ->
            {error, db_not_found};
       Error ->
          Error
    end.

design_info(#db{server=Server, options=IbrowseOpts}=Db, DesignDoc) ->
    Url = make_url(Server, [db_url(Db), "/_design/", DesignDoc, "/_info"], []),
    case couchbeam_httpc:request(get, Url, ["200"], IbrowseOpts) of
        {ok, _Status, _Headers, Body} ->
            {ok, ejson:decode(Body)}; 
        {error, {ok, "404", _, _}} ->
            {error, design_not_found};
        Error ->
            Error
    end.

%% @doc test if doc with uuid exists in the given db
%% @spec doc_exists(db(), string()) -> boolean()
doc_exists(#db{server=Server, options=IbrowseOpts}=Db, DocId) ->
    DocId1 = couchbeam_util:encode_docid(DocId),
    Url = make_url(Server, doc_url(Db, DocId1), []),
    case couchbeam_httpc:request(head, Url, ["200"], IbrowseOpts) of
        {ok, _, _, _} -> true;
        _Error -> false
    end.

%% @doc open a document
%% @equiv open_doc(Db, DocId, [])
open_doc(Db, DocId) ->
    open_doc(Db, DocId, []).

%% @doc open a document
%% Params is a list of query argument. Have a look in CouchDb API
%% @spec open_doc(Db::db(), DocId::string(), Params::list())
%%          -> {ok, Doc}|{error, Error}
open_doc(#db{server=Server, options=IbrowseOpts}=Db, DocId, Params) ->
    DocId1 = couchbeam_util:encode_docid(DocId),
    Url = make_url(Server, doc_url(Db, DocId1), Params),
    case db_request(get, Url, ["200", "201"], IbrowseOpts) of
        {ok, _, _, Body} ->
            {ok, ejson:decode(Body)};
        Error ->
            Error
    end.

%% @doc save a document
%% @equiv save_doc(Db, Doc, [])
save_doc(Db, Doc) ->
    save_doc(Db, Doc, []).

%% @doc save a document
%% A document is a Json object like this one:
%%
%%      ```{[
%%          {<<"_id">>, <<"myid">>},
%%          {<<"title">>, <<"test">>}
%%      ]}'''
%%
%% Options are arguments passed to the request. This function return a
%% new document with last revision and a docid. If _id isn't specified in
%% document it will be created. Id is created by extracting an uuid from
%% the couchdb node.
%%
%% @spec save_doc(Db::db(), Doc, Options::list()) -> {ok, Doc1}|{error, Error}
save_doc(#db{server=Server, options=IbrowseOpts}=Db, {Props}=Doc, Options) ->
    DocId = case couchbeam_util:get_value(<<"_id">>, Props) of
        undefined ->
            [Id] = get_uuid(Server),
            Id;
        DocId1 ->
            couchbeam_util:encode_docid(DocId1)
    end,
    Url = make_url(Server, doc_url(Db, DocId), Options),
    Body = ejson:encode(Doc),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(put, Url, ["201", "202"], IbrowseOpts, Headers, Body) of
        {ok, _, _, RespBody} ->
            {JsonProp} = ejson:decode(RespBody),
            NewRev = couchbeam_util:get_value(<<"rev">>, JsonProp),
            NewDocId = couchbeam_util:get_value(<<"id">>, JsonProp),
            Doc1 = couchbeam_doc:set_value(<<"_rev">>, NewRev,
                couchbeam_doc:set_value(<<"_id">>, NewDocId, Doc)),
            {ok, Doc1};
        Error ->
            Error
    end.

%% @doc delete a document
%% @equiv delete_doc(Db, Doc, [])
delete_doc(Db, Doc) ->
    delete_doc(Db, Doc, []).

%% @doc delete a document
%% if you want to make sure the doc it emptied on delete, use the option
%% {empty_on_delete,  true} or pass a doc with just _id and _rev
%% members.
%% @spec delete_doc(Db, Doc, Options) -> {ok,Result}|{error,Error}
delete_doc(Db, Doc, Options) ->
     delete_docs(Db, [Doc], Options).

%% @doc delete a list of documents
%% @equiv delete_docs(Db, Docs, [])
delete_docs(Db, Docs) ->
    delete_docs(Db, Docs, []).

%% @doc delete a list of documents
%% if you want to make sure the doc it emptied on delete, use the option
%% {empty_on_delete,  true} or pass a doc with just _id and _rev
%% members.
%% @spec delete_docs(Db::db(), Docs::list(),Options::list()) -> {ok, Result}|{error, Error}
delete_docs(Db, Docs, Options) ->
    Empty = couchbeam_util:get_value("empty_on_delete", Options,
        false),

    {FinalDocs, FinalOptions} = case Empty of
        true ->
            Docs1 = lists:map(fun(Doc)->
                        {[{<<"_id">>, couchbeam_doc:get_id(Doc)},
                         {<<"_rev">>, couchbeam_doc:get_rev(Doc)},
                         {<<"_deleted">>, true}]}
                 end, Docs),
             {Docs1, proplists:delete("all_or_nothing", Options)};
         _ ->
            Docs1 = lists:map(fun({DocProps})->
                        {[{<<"_deleted">>, true}|DocProps]}
                end, Docs),
            {Docs1, Options}
    end,
    save_docs(Db, FinalDocs, FinalOptions).

%% @doc save a list of documents
%% @equiv save_docs(Db, Docs, [])
save_docs(Db, Docs) ->
    save_docs(Db, Docs, []).

%% @doc save a list of documents
%% @spec save_docs(Db::db(), Docs::list(),Options::list()) -> {ok, Result}|{error, Error}
save_docs(#db{server=Server, options=IbrowseOpts}=Db, Docs, Options) ->
    Docs1 = [maybe_docid(Server, Doc) || Doc <- Docs],
    Options1 = couchbeam_util:parse_options(Options),
    {Options2, Body} = case couchbeam_util:get_value("all_or_nothing",
            Options1, false) of
        true ->
            Body1 = ejson:encode({[
                {<<"all_or_nothing">>, true},
                {<<"docs">>, Docs1}
            ]}),

            {proplists:delete("all_or_nothing", Options1), Body1};
        _ ->
            Body1 = ejson:encode({[{<<"docs">>, Docs1}]}),
            {Options1, Body1}
        end,
    Url = make_url(Server, [db_url(Db), "/", "_bulk_docs"], Options2),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(post, Url, ["201", "202"], IbrowseOpts, Headers, Body) of
        {ok, _, _, RespBody} ->
            {ok, ejson:decode(RespBody)};
        Error ->
            Error
        end.

lookup_doc_rev(Db, DocId) ->
    lookup_doc_rev(Db, DocId, []).

lookup_doc_rev(#db{server=Server, options=IbrowseOpts}=Db, DocId, Params) ->
    DocId1 = couchbeam_util:encode_docid(DocId),
    Url = make_url(Server, doc_url(Db, DocId1), Params),
    case db_request(head, Url, ["200"], IbrowseOpts) of
        {ok, _, Headers, _} ->
            MHeaders = mochiweb_headers:make(Headers),
            re:replace(mochiweb_headers:get_value("etag", MHeaders),
                <<"\"">>, <<>>, [global, {return, binary}]);
        Error ->
            Error
    end.

%% @doc fetch a document attachment
%% @equiv fetch_attachment(Db, DocId, Name, [], DEFAULT_TIMEOUT)
fetch_attachment(Db, DocId, Name) ->
    fetch_attachment(Db, DocId, Name, [], ?DEFAULT_TIMEOUT).

%% @doc fetch a document attachment
%% @equiv fetch_attachment(Db, DocId, Name, Options, DEFAULT_TIMEOUT)
fetch_attachment(Db, DocId, Name, Options) ->
    fetch_attachment(Db, DocId, Name, Options, ?DEFAULT_TIMEOUT).

%% @doc fetch a document attachment
%% @spec fetch_attachment(db(), string(), string(),
%%                        list(), infinity|integer()) -> {ok, binary()}
fetch_attachment(Db, DocId, Name, Options, Timeout) ->
    {ok, ReqId} = stream_fetch_attachment(Db, DocId, Name, self(), Options,
        Timeout),
    couchbeam_attachments:wait_for_attachment(ReqId, Timeout).

%% @doc stream fetch attachment to a Pid.
%% @equiv stream_fetch_attachment(Db, DocId, Name, ClientPid, [], DEFAULT_TIMEOUT)
stream_fetch_attachment(Db, DocId, Name, ClientPid) ->
    stream_fetch_attachment(Db, DocId, Name, ClientPid, [], ?DEFAULT_TIMEOUT).


%% @doc stream fetch attachment to a Pid.
%% @equiv stream_fetch_attachment(Db, DocId, Name, ClientPid, Options, DEFAULT_TIMEOUT)
stream_fetch_attachment(Db, DocId, Name, ClientPid, Options) ->
     stream_fetch_attachment(Db, DocId, Name, ClientPid, Options, ?DEFAULT_TIMEOUT).

%% @doc stream fetch attachment to a Pid. Messages sent to the Pid
%%      will be of the form `{reference(), message()}',
%%      where `message()' is one of:
%%      <dl>
%%          <dt>done</dt>
%%              <dd>You got all the attachment</dd>
%%          <dt>{ok, binary()}</dt>
%%              <dd>Part of the attachment</dd>
%%          <dt>{error, term()}</dt>
%%              <dd>n error occurred</dd>
%%      </dl>
%% @spec stream_fetch_attachment(Db::db(), DocId::string(), Name::string(),
%%                               ClientPid::pid(), Options::list(), Timeout::integer())
%%          -> {ok, reference()}|{error, term()}
stream_fetch_attachment(#db{server=Server, options=IbrowseOpts}=Db, DocId, Name, ClientPid,
        Options, Timeout) ->
    Options1 = couchbeam_util:parse_options(Options),
    %% custom headers. Allows us to manage Range.
    {Options2, Headers} = case couchbeam_util:get_value("headers", Options1) of
        undefined ->
            {Options1, []};
        Headers1 ->
            {proplists:delete("headers", Options1), Headers1}
    end,
    DocId1 = couchbeam_util:encode_docid(DocId),
    Url = make_url(Server, [db_url(Db), "/", DocId1, "/", Name], Options2),
    StartRef = make_ref(),
    Pid = spawn(couchbeam_attachments, attachment_acceptor, [ClientPid,
            StartRef, Timeout]),
    case couchbeam_httpc:request_stream(Pid, get, Url, IbrowseOpts, Headers) of
        {ok, ReqId}    ->
            Pid ! {ibrowse_req_id, StartRef, ReqId},
            {ok, StartRef};
        {error, Error} -> {error, Error}
    end.

%% @doc put an attachment
%% @equiv put_attachment(Db, DocId, Name, Body, [])
put_attachment(Db, DocId, Name, Body)->
    put_attachment(Db, DocId, Name, Body, []).

%% @doc put an attachment
%% @spec put_attachment(Db::db(), DocId::string(), Name::string(),
%%                      Body::body(), Option::optionList()) -> {ok, iolist()}
%%       optionList() = [option()]
%%       option() = {rev, string()} |
%%                  {content_type, string()} |
%%                  {content_length, string()}
%%       body() = [] | string() | binary() | fun_arity_0() | {fun_arity_1(), initial_state()}
%%       initial_state() = term()
put_attachment(#db{server=Server, options=IbrowseOpts}=Db, DocId, Name, Body, Options) ->
    QueryArgs = case couchbeam_util:get_value(rev, Options) of
        undefined -> [];
        Rev -> [{"rev", couchbeam_util:to_list(Rev)}]
    end,

    Headers = couchbeam_util:get_value(headers, Options, []),

    FinalHeaders = lists:foldl(fun(Option, Acc) ->
                case Option of
                    {content_length, V} -> [{"Content-Length", V}|Acc];
                    {content_type, V} -> [{"Content-Type", V}|Acc];
                    _ -> Acc
                end
        end, Headers, Options),

    Url = make_url(Server, [db_url(Db), "/",
            couchbeam_util:encode_docid(DocId), "/",
            couchbeam_util:encode_att_name(Name)], QueryArgs),

    case db_request(put, Url, ["201", "202"], IbrowseOpts, FinalHeaders, Body) of
        {ok, _, _, RespBody} ->
            {[{<<"ok">>, true}|R]} = ejson:decode(RespBody),
            {ok, {R}};
        Error ->
            Error
    end.

%% @doc delete a document attachment
%% @equiv delete_attachment(Db, Doc, Name, [])
delete_attachment(Db, Doc, Name) ->
    delete_attachment(Db, Doc, Name, []).

%% @doc delete a document attachment
%% @spec(db(), string()|list(), string(), list() -> {ok, Result} | {error, Error}
delete_attachment(#db{server=Server, options=IbrowseOpts}=Db, DocOrDocId, Name, Options) ->
    Options1 = couchbeam_util:parse_options(Options),
    {Rev, DocId} = case DocOrDocId of
        {Props} ->
            Rev1 = couchbeam_util:get_value(<<"_rev">>, Props),
            DocId1 = couchbeam_util:get_value(<<"_id">>, Props),
            {Rev1, DocId1};
        DocId1 ->
            Rev1 = couchbeam_util:get_value("rev", Options1),
            {Rev1, DocId1}
    end,
    case Rev of
        undefined ->
           {error, rev_undefined};
        _ ->
            Options2 = case couchbeam_util:get_value("rev", Options1) of
                undefined ->
                    [{"rev", Rev}|Options1];
                _ ->
                    Options1
            end,
            Url = make_url(Server, [db_url(Db), "/", DocId, "/", Name], Options2),
            case db_request(delete, Url, ["200", "202"], IbrowseOpts) of
            {ok, _, _, RespBody} ->
                {[{<<"ok">>,true}|R]} = ejson:decode(RespBody),
                {ok, {R}};

            Error ->
                Error
            end
    end.

%% @doc get all documents from a CouchDB database.
%% @equiv all_docs(Db, [])
%% @deprecated use new api in {@link couch_view}.
all_docs(Db) ->
    all_docs(Db, []).

%% @doc get all documents from a CouchDB database. It return a
%% #view{} record that you can use with couchbeam_oldview functions:
%% <ul>
%%      <li>{@link couchbeam_oldview:count/1. couchbeam_oldview:count/1}</li>
%%      <li>{@link couchbeam_oldview:fetch/1. couchbeam_oldview:fetch/1}</li>
%%      <li>{@link couchbeam_oldview:first/1. couchbeam_oldview:first/1}</li>
%%      <li>{@link couchbeam_oldview:fold/1. couchbeam_oldview:fold/1}</li>
%%      <li>{@link couchbeam_oldview:foreach/1. couchbeam_oldview:foreach/1}</li>
%% </ul>
%%
%% @spec all_docs(Db::db(), Options::list())
%%              -> {ok, View::view()}|{error, term()}
%% @deprecated use new api in {@link couch_view}.
all_docs(Db, Options) ->
    view(Db, "_all_docs", Options).

%% @doc  get view results from database
%% @equiv view(Db, ViewName, [])
%% @deprecated use new api in {@link couch_view}.
view(Db, ViewName) ->
    view(Db, ViewName, []).

%% @doc get view results from database. viewname is generally
%% a tupple like {DesignName::string(), ViewName::string()} or string
%% like "designname/viewname". It return a #view{} record  that you can use
%% with couchbeam_oldview functions:
%%
%% <ul>
%%      <li>{@link couchbeam_oldview:count/1. couchbeam_oldview:count/1}</li>
%%      <li>{@link couchbeam_oldview:fetch/1. couchbeam_oldview:fetch/1}</li>
%%      <li>{@link couchbeam_oldview:first/1. couchbeam_oldview:first/1}</li>
%%      <li>{@link couchbeam_oldview:fold/1. couchbeam_oldview:fold/1}</li>
%%      <li>{@link couchbeam_oldview:foreach/1. couchbeam_oldview:foreach/1}</li>
%% </ul>
%%
%% Options are CouchDB view parameters
%%
%% See [http://wiki.apache.org/couchdb/HTTP_view_API] for more informations.
%%
%% @spec view(Db::db(), ViewName::string(), Options::list())
%%          -> {ok, View::view()}|{error, term()}
%% @deprecated use new api in {@link couch_view}.

view(#db{server=Server}=Db, ViewName, Options) ->
    ViewName1 = couchbeam_util:to_list(ViewName),
    Options1 = couchbeam_util:parse_options(Options),
    Url = case ViewName1 of
        {DName, VName} ->
            [db_url(Db), "/_design/", DName, "/_view/", VName];
        "_all_docs" ->
            [db_url(Db), "/_all_docs"];
        _Else ->
            case string:tokens(ViewName1, "/") of
            [DName, VName] ->
                [db_url(Db), "/_design/", DName, "/_view/", VName];
            _ ->
                undefined
            end
    end,
    case Url of
    undefined ->
        {error, invalid_view_name};
    _ ->
        {Method, Options2, Body} = case couchbeam_util:get_value("keys",
                Options1) of
            undefined ->
                {get, Options1, []};
            Keys ->
                Body1 = ejson:encode({[{<<"keys">>, Keys}]}),
                {post, proplists:delete("keys", Options1), Body1}
            end,
        Headers = case Method of
            post -> [{"Content-Type", "application/json"}];
            _ -> []
        end,
        NewView = #view{
            db = Db,
            name = ViewName,
            options = Options2,
            method = Method,
            body = Body,
            headers = Headers,
            url_parts = Url,
            url = make_url(Server, Url, Options2)
        },
        {ok, NewView}
    end.

%% @doc commit all docs in memory
%% @equiv ensure_full_commit(Db, [])
ensure_full_commit(Db) ->
    ensure_full_commit(Db, []).

%% @doc commit all docs in memory
%% @spec ensure_full_commit(Db::db(), Options::list())
%%                      -> {ok, term()}|{error, term()}
ensure_full_commit(#db{server=Server, options=IbrowseOpts}=Db, Options) ->
    Url = make_url(Server, [db_url(Db), "/_ensure_full_commit"], Options),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(post, Url, ["201"], IbrowseOpts, Headers) of
        {ok, _, _, Body} ->
            {[{<<"ok">>, true}|R]} = ejson:decode(Body),
            {ok, R};
        Error ->
            Error
    end.

%% @doc Compaction compresses the database file by removing unused
%% sections created during updates.
%% See [http://wiki.apache.org/couchdb/Compaction] for more informations
%% @spec compact(Db::db()) -> ok|{error, term()}
compact(#db{server=Server, options=IbrowseOpts}=Db) ->
    Url = make_url(Server, [db_url(Db), "/_compact"], []),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(post, Url, ["202"], IbrowseOpts, Headers) of
        {ok, _, _, _} ->
            ok;
        Error ->
            Error
    end.
%% @doc Like compact/1 but this compacts the view index from the
%% current version of the design document.
%% See [http://wiki.apache.org/couchdb/Compaction#View_compaction] for more informations
%% @spec compact(Db::db(), ViewName::string()) -> ok|{error, term()}
compact(#db{server=Server, options=IbrowseOpts}=Db, DesignName) ->
    Url = make_url(Server, [db_url(Db), "/_compact/", DesignName], []),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(post, Url, ["202"], IbrowseOpts, Headers) of
        {ok, _, _, _} ->
            ok;
        Error ->
            Error
    end.

-spec view_cleanup/1 :: (db()) -> 'ok' | {'error', term()}.
view_cleanup(#db{server=Server, options=IbrowseOpts}=Db) ->
    Url = make_url(Server, [db_url(Db), "/_view_cleanup/"], []),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(post, Url, ["202"], IbrowseOpts, Headers) of
        {ok, _, _, _} ->
            ok;
        Error ->
            Error
    end.

%% --------------------------------------------------------------------
%% Utilities functions.
%% --------------------------------------------------------------------

%% add missing docid to a list of documents if needed
maybe_docid(Server, {DocProps}) ->
    case couchbeam_util:get_value(<<"_id">>, DocProps) of
        undefined ->
            DocId = [get_uuid(Server)],
            {[{<<"_id">>, list_to_binary(DocId)}|DocProps]};
        _DocId ->
            {DocProps}
    end.

%% @doc Asemble the server URL for the given client
%% @spec server_url({Host, Port}) -> iolist()
server_url(#server{host=Host, port=Port, options=Options}) ->
    Ssl = couchbeam_util:get_value(is_ssl, Options, false),
    server_url({Host, Port}, Ssl).

%% @doc Assemble the server URL for the given client
%% @spec server_url({Host, Port}, Ssl) -> iolist()
server_url({Host, Port}, false) ->
    ["http://",Host,":",integer_to_list(Port)];
server_url({Host, Port}, true) ->
    ["https://",Host,":",integer_to_list(Port)].

uuids_url(Server) ->
    binary_to_list(iolist_to_binary([server_url(Server), "/", "_uuids"])).

dbname(DbName) when is_list(DbName) ->
    DbName;
dbname(DbName) when is_binary(DbName) ->
    binary_to_list(DbName);
dbname(DbName) ->
    erlang:error({illegal_database_name, DbName}).

db_url(#db{name=DbName}) ->
    [DbName].

doc_url(Db, DocId) ->
    [db_url(Db), "/", DocId].

make_url(Server=#server{prefix=Prefix}, Path, Query) ->
    Query1 = couchbeam_util:encode_query(Query),
    binary_to_list(
        iolist_to_binary(
            [server_url(Server),
             Prefix, "/",
             Path,
             [ ["?", mochiweb_util:urlencode(Query1)] || Query1 =/= [] ]
            ])).

db_request(Method, Url, Expect, Options) ->
    db_request(Method, Url, Expect, Options, [], []).
db_request(Method, Url, Expect, Options, Headers) ->
    db_request(Method, Url, Expect, Options, Headers, []).
db_request(Method, Url, Expect, Options, Headers, Body) ->
    case couchbeam_httpc:request(Method, Url, Expect, Options, Headers, Body) of
        Resp = {ok, _, _, _} ->
            Resp;
        {error, {ok, "404", _, _}} ->
            {error, not_found};
        {error, {ok, "409", _, _}} ->
            {error, conflict};
        {error, {ok, "412", _, _}} ->
            {error, precondition_failed};
        Error ->
            Error
    end.


