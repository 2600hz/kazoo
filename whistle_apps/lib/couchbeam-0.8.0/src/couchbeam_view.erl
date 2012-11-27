%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_view).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("couchbeam.hrl").

-export([stream/2, stream/3, stream/4,
         fetch/1, fetch/2, fetch/3,
         count/1, count/2, count/3,
         first/1, first/2, first/3,
         all/1, all/2,
         fold/4, fold/5,
         foreach/3, foreach/4,
         view_loop/2, parse_view_options/1]).

-spec all(Db::db()) -> {ok, Rows::list(ejson_object())} | {error, term()}.
%% @doc fetch all docs
%% @equiv fetch(Db, 'all_docs', [])
all(Db) ->
    fetch(Db, 'all_docs', []).

-spec all(Db::db(), Options::view_options())
        -> {ok, Rows::list(ejson_object())} | {error, term()}.
%% @doc fetch all docs
%% @equiv fetch(Db, 'all_docs', Options)
all(Db, Options) ->
    fetch(Db, 'all_docs', Options).

-spec fetch(Db::db()) -> {ok, Rows::list(ejson_object())} | {error, term()}.
%% @equiv fetch(Db, 'all_docs', [])
fetch(Db) ->
    fetch(Db, 'all_docs', []).

-spec fetch(Db::db(), ViewName::'all_docs' | {DesignName::string(),
        ViewName::string()}) ->
                   {ok, list(ejson_object())} |
                   {error, term()} |
                   {error, list(ejson_object()), term()}.
%% @equiv fetch(Db, ViewName, [])
fetch(Db, ViewName) ->
    fetch(Db, ViewName,[]).


-spec fetch(Db::db(), ViewName::'all_docs' | {DesignName::string(),
        ViewName::string()}, Options::view_options())
     -> {ok, Rows::list(ejson_object())} | {error, term()}.
%% @doc Collect view results
%%  <p>Db: a db record</p>
%%  <p>ViewName: <code>'all_docs'</code> to get all docs or <code>{DesignName,
%%  ViewName}</code></p>
%%  <pre>Options :: view_options() [{key, binary()} | {start_docid, binary()}
%%    | {end_docid, binary()} | {start_key, binary()}
%%    | {end_key, binary()} | {limit, integer()}
%%    | {stale, stale()}
%%    | descending
%%    | {skip, integer()}
%%    | group | {group_level, integer()}
%%    | {inclusive_end, boolean()} | {reduce, boolean()} | reduce | include_docs | conflicts
%%    | {keys, list(binary())}</pre>
%% <p>See {@link couchbeam_view:stream/4} for more information about
%% options.</p>
%% <p>Return: {ok, Rows} or {error, Rows, Error}</p>
fetch(Db, ViewName, Options) ->
    case stream(Db, ViewName, self(), Options) of
        {ok, StartRef, _} ->
            collect_view_results(StartRef, []);
        Error ->
            Error
    end.

-spec stream(Db::db(), Client::pid()) -> {ok, StartRef::term(),
        ViewPid::pid()} | {error, term()}.
%% @equiv stream(Db, 'all_docs', Client, [])
stream(Db, Client) ->
    stream(Db, 'all_docs', Client, []).


-spec stream(Db::db(), ViewName::'all_docs' | {DesignName::string(),
        ViewName::string()}, Client::pid()) -> {ok, StartRef::term(),
        ViewPid::pid()} | {error, term()}.
%% @equiv stream(Db, ViewName, Client, [])
stream(Db, ViewName, Client) ->
    stream(Db, ViewName, Client, []).

-spec stream(Db::db(), ViewName::'all_docs' | {DesignName::string(),
        ViewName::string()}, Client::pid(), Options::view_options())
    -> {ok, StartRef::term(), ViewPid::pid()} | {error, term()}.
%% @doc stream view results to a pid
%%  <p>Db: a db record</p>
%%  <p>ViewName: 'all_docs' to get all docs or {DesignName,
%%  ViewName}</p>
%%  <p>Client: pid where to send view events where events are:
%%  <dl>
%%      <dt>{row, StartRef, done}</dt>
%%          <dd>All view results have been fetched</dd>
%%      <dt>{row, StartRef, Row :: ejson_object()}</dt>
%%          <dd>A row in the view</dd>
%%      <dt>{error, StartRef, Error}</dt>
%%          <dd>Got an error, connection is closed when an error
%%          happend.</dd>
%%  </dl></p>
%%  <p><pre>Options :: view_options() [{key, binary()} | {start_docid, binary()}
%%    | {end_docid, binary()} | {start_key, binary()}
%%    | {end_key, binary()} | {limit, integer()}
%%    | {stale, stale()}
%%    | descending
%%    | {skip, integer()}
%%    | group | {group_level, integer()}
%%    | {inclusive_end, boolean()} | {reduce, boolean()} | reduce | include_docs | conflicts
%%    | {keys, list(binary())}</pre>
%%
%%  <ul>
%%      <li><code>{key, Key}</code>: key value</li>
%%      <li><code>{start_docid, DocId}</code>: document id to start with (to allow pagination
%%          for duplicate start keys</li>
%%      <li><code>{end_docid, DocId}</code>: last document id to include in the result (to
%%          allow pagination for duplicate endkeys)</li>
%%      <li><code>{start_key, Key}</code>: start result from key value</li>
%%      <li><code>{end_key, Key}</code>: end result from key value</li>
%%      <li><code>{limit, Limit}</code>: Limit the number of documents in the result</li>
%%      <li><code>{stale, Stale}</code>: If stale=ok is set, CouchDB will not refresh the view
%%      even if it is stale, the benefit is a an improved query latency. If
%%      stale=update_after is set, CouchDB will update the view after the stale
%%      result is returned.</li>
%%      <li><code>descending</code>: reverse the result</li>
%%      <li><code>{skip, N}</code>: skip n number of documents</li>
%%      <li><code>group</code>: the reduce function reduces to a single result
%%      row.</li>
%%      <li><code>{group_level, Level}</code>: the reduce function reduces to a set
%%      of distinct keys.</li>
%%      <li><code>{reduce, boolean()}</code>: whether to use the reduce function of the view. It defaults to
%%      true, if a reduce function is defined and to false otherwise.</li>
%%      <li><code>include_docs</code>: automatically fetch and include the document
%%      which emitted each view entry</li>
%%      <li><code>{inclusive_end, boolean()}</code>: Controls whether the endkey is included in
%%      the result. It defaults to true.</li>
%%      <li><code>conflicts</code>: include conflicts</li>
%%      <li><code>{keys, [Keys]}</code>: to pass multiple keys to the view query</li>
%%  </ul></p>
%%
%% <p> Return <code>{ok, StartRef, ViewPid}</code> or <code>{error,
%Error}</code>. Ref can be
%% used to disctint all changes from this pid. ViewPid is the pid of
%% the view loop process. Can be used to monitor it or kill it
%% when needed.</p>
stream(#db{options=IbrowseOpts}=Db, ViewName, ClientPid, Options) ->
    make_view(Db, ViewName, Options, fun(Args, Url) ->
                                             StartRef = make_ref(),
                                             UserFun = fun
                                                           (done) ->
                                                               ClientPid ! {row, StartRef, done};
                                                           ({error, Error}) ->
                                                               ClientPid ! {error, StartRef, Error};
                                                           (Row) ->
                                                               ClientPid ! {row, StartRef, Row}
                                                       end,
                                             Params = {Args, Url, IbrowseOpts},
                                             ViewPid = spawn_link(couchbeam_view, view_loop, [UserFun, Params]),
                                             
                                             %% if we send multiple keys, we do a Post
                                             Result = case Args#view_query_args.method of
                                                          get ->
                                                              couchbeam_httpc:request_stream({ViewPid, once}, get, Url, IbrowseOpts);
                                                          post ->
                                                              Body = ejson:encode({[{<<"keys">>, Args#view_query_args.keys}]}),
                                                              Headers = [{"Content-Type", "application/json"}],
                                                              couchbeam_httpc:request_stream({ViewPid, once}, post, Url,
                                                                                             IbrowseOpts, Headers, Body)
                                                      end,
                                             case Result of
                                                 {ok, ReqId} ->
                                                     ViewPid ! {ibrowse_req_id, ReqId},
                                                     {ok, StartRef, ViewPid};
                                                 Error ->
                                                     Error
                                             end
                                     end).

-spec count(Db::db()) -> integer() | {error, term()}.
%% @equiv count(Db, 'all_docs', [])
count(Db) ->
    count(Db, 'all_docs', []).

-spec count(Db::db(), ViewName::'all_docs' | {DesignName::string(),
        ViewName::string()}) -> integer() | {error, term()}.
%% @equiv count(Db, ViewName, [])
count(Db, ViewName) ->
    count(Db, ViewName, []).

-spec count(Db::db(), ViewName::'all_docs' | {DesignName::string(),
        ViewName::string()}, Options::view_options())
    -> integer() | {error, term()}.
%% @doc count number of doc in a view (or all docs)
count(#db{options=IbrowseOpts}=Db, ViewName, Options)->
    make_view(Db, ViewName, Options, fun(Args, Url) ->
        Result = case Args#view_query_args.method of
            get ->
                couchbeam_httpc:request(get, Url, ["200"], IbrowseOpts);
            post ->
                Body = ejson:encode({[{<<"keys">>, Args#view_query_args.keys}]}),
                Headers = [{"Content-Type", "application/json"}],
                couchbeam_httpc:request_stream(post, Url, ["200"],
                    IbrowseOpts, Headers, Body)
        end,
        case Result of
            {ok, _, _, RespBody} ->
                {Props} = ejson:decode(RespBody),
                case proplists:get_value("limit",
                        Args#view_query_args.options, 0) of
                0 ->
                    proplists:get_value(<<"total_rows">>, Props);
                _ ->
                    Rows = proplists:get_value(<<"rows">>, Props),
                    length(Rows)
                end;
            Error ->
                Error
        end
    end).

-spec first(Db::db()) -> {ok, Row::ejson_object()} | {error, term()}.
%% @equiv first(Db, 'all_docs', [])
first(Db) ->
    first(Db, 'all_docs', []).

-spec first(Db::db(), ViewName::'all_docs' | {DesignName::string(),
        ViewName::string()})
    -> {ok, Row::ejson_object()} | {error, term()}.
%% @equiv first(Db, ViewName, [])
first(Db, ViewName) ->
    first(Db, ViewName,[]).


-spec first(Db::db(), ViewName::'all_docs' | {DesignName::string(),
        ViewName::string()}, Options::view_options())
     -> {ok, Rows::ejson_object()} | {error, term()}.
%% @doc get first result of a view
%%  <p>Db: a db record</p>
%%  <p>ViewName: 'all_docs' to get all docs or {DesignName,
%%  ViewName}</p>
%%  <pre>Options :: view_options() [{key, binary()} | {start_docid, binary()}
%%    | {end_docid, binary()} | {start_key, binary()}
%%    | {end_key, binary()} | {limit, integer()}
%%    | {stale, stale()}
%%    | descending
%%    | {skip, integer()}
%%    | group | {group_level, integer()}
%%    | {inclusive_end, boolean()} | {reduce, boolean()} | reduce | include_docs | conflicts
%%    | {keys, list(binary())}</pre>
%% <p>See {@link couchbeam_view:stream/4} for more information about
%% options.</p>
%% <p>Return: {ok, Row} or {error, Error}</p>
first(Db, ViewName, Options) ->
    case stream(Db, ViewName, self(), Options) of
        {ok, StartRef, ViewPid} ->
            collect_view_first(StartRef, ViewPid);
        Error ->
            Error
    end.

-spec fold(Function::function(), Acc::list(), Db::db(),
           ViewName::'all_docs' | {DesignName::string(), ViewName::string()})
          -> list(term()) | {error, term()}.
%% @equiv fold(Function, Acc, Db, ViewName, [])
fold(Function, Acc, Db, ViewName) ->
    fold(Function, Acc, Db, ViewName, []).

-spec fold(Function::function(), Acc::list(), Db::db(),
           ViewName::'all_docs' | {DesignName::string(),
                                   ViewName::string()}, Options::view_options())
          -> list(term()) | {error, term()}.
%% @doc call Function(Row, AccIn) on succesive row, starting with
%% AccIn == Acc. Function/2 must return a new list accumultator or the
%% atom <em>done</em> to stop fetching results. Acc0 is returned if the
%% list is empty. For example:
%% ```
%% couchbeam_view:fold(fun(Row, Acc) -> [Row|Acc] end, [], Db, 'all_docs').
%% '''
fold(Function, Acc, Db, ViewName, Options) ->
    case stream(Db, ViewName, self(), Options) of
        {ok, StartRef, ViewPid} ->
            fold_view_results(StartRef, ViewPid, Function, Acc);
        Error ->
            Error
    end.

-spec foreach(Function::function(), Db::db(),
              ViewName::'all_docs' | {DesignName::string(), ViewName::string()})
             -> list(term()) | {error, term()}.
%% @equiv foreach(Function, Db, ViewName, [])
foreach(Function, Db, ViewName) ->
    foreach(Function, Db, ViewName, []).

-spec foreach(Function::function(),  Db::db(),
              ViewName::'all_docs' | {DesignName::string(),
                                      ViewName::string()}, Options::view_options())
             -> list(term()) | {error, term()}.
%% @doc call Function(Row) on succesive row. Example:
%% ```
%% couchbeam_view:foreach(fun(Row) -> io:format("got row ~p~n", [Row]) end, Db, 'all_docs').
%% '''
foreach(Function, Db, ViewName, Options) ->
    FunWrapper = fun(Row, Acc) ->
                         Function(Row),
                         Acc
                 end,
    fold(FunWrapper, [], Db, ViewName, Options).


%% ----------------------------------
%% utilities functions
%% ----------------------------------

-spec parse_view_options(Options::list()) -> view_query_args().
%% @doc parse view options
parse_view_options(Options) ->
    parse_view_options(Options, #view_query_args{}).

parse_view_options([], Args) ->
    Args;
parse_view_options([{key, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"key", ejson:encode(Value)}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{startkey_docid, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"startkey_docid", Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{endkey_docid, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"endkey_docid", Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{startkey, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"startkey", ejson:encode(Value)}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{endkey, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"endkey", ejson:encode(Value)}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{limit, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"limit", Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{stale, ok}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"stale", "ok"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{stale, update_after}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"stale", "update_after"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{stale, _}|_Rest], _Args) ->
    {error, "invalid stale value"};
parse_view_options([descending|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"descending", "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([group|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"group", "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{group_level, Level}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"group_level", Level}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([inclusive_end|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"inclusive_end", "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{inclusive_end, true}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"inclusive_end", "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{inclusive_end, false}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"inclusive_end", "false"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([reduce|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"reduce", "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{reduce, true}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"reduce", "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{reduce, false}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"reduce", "false"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([include_docs|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"include_docs", "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{include_docs, _}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"include_docs", "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([conflicts|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"conflicts", "true"}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{skip, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"skip", Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{list, Value}|Rest], #view_query_args{options=Opts}=Args) ->
    Opts1 = [{"list", Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{keys, Value}|Rest], Args) ->
    parse_view_options(Rest, Args#view_query_args{method=post,
            keys=Value});
parse_view_options([{Key, Value}|Rest], #view_query_args{options=Opts}=Args)
        when is_list(Key) ->
    Opts1 = [{Key, Value}|Opts],
    parse_view_options(Rest, Args#view_query_args{options=Opts1});
parse_view_options([{<<_/binary>> = Key, Value}|Rest], Args) ->
    parse_view_options([{list_to_existing_atom(binary_to_list(Key)), Value}|Rest], Args);
parse_view_options([_|Rest], Args) ->
    parse_view_options(Rest, Args).


view_loop(UserFun, Params) ->
    Callback = fun(200, _Headers, DataStreamFun) ->
                       EventFun = fun(Ev) ->
                                          view_ev1(Ev, UserFun)
                                  end,
                       couchbeam_json_stream:events(DataStreamFun, EventFun)
               end,
    
    receive
        {ibrowse_req_id, ReqId} ->
            process_view_results(ReqId, Params, UserFun, Callback)
    after ?DEFAULT_TIMEOUT ->
            UserFun({error, timeout})
    end.


%% @private

make_view(#db{server=Server}=Db, ViewName, Options, Fun) ->
    #view_query_args{options=Options1}=Args = parse_view_options(Options),
    case ViewName of
        'all_docs' ->
            Url = couchbeam:make_url(Server, [couchbeam:db_url(Db),
                                              "/_all_docs"],
                                     Options1),
            Fun(Args, Url);
        'design_docs' ->
            Url = couchbeam:make_url(Server, [couchbeam:db_url(Db)
                                              ,"/_all_docs"
                                             ],
                                     [{startkey, "\"_design\""}
                                      ,{endkey, "\"_design0\""}
                                      | Options1
                                     ]),
            Fun(Args, Url);
        {DName, VName} ->
            Url = couchbeam:make_url(Server, [couchbeam:db_url(Db),
                                              "/_design/", DName, "/_view/", VName],
                                     Options1),
            Fun(Args, Url);
        <<_/binary>> ->
            [DName, VName] = binary:split(ViewName, <<"/">>),

            Url = couchbeam:make_url(Server, [couchbeam:db_url(Db),
                                              "/_design/", DName, "/_view/", VName],
                                     Options1),
            Fun(Args, Url);
        _ ->
            {error, invalid_view_name}
    end.

collect_view_first(Ref, Pid) ->
    receive
        {row, Ref, done} ->
            {error, empty};
        {row, Ref, Row} ->
            couchbeam_util:shutdown_sync(Pid),
            {ok, Row};
        {error, Ref, Error} ->
            {error, Error}
    end.


-spec fold_view_results/4 :: (_, pid(), fun(), Acc) ->
                                     Acc |
                                     {error, Acc, _}.
fold_view_results(Ref, Pid, Fun, Acc) ->
    receive
        {row, Ref, done} ->
            Acc;
        {row, Ref, Row} ->
            case Fun(Row, Acc) of
                done ->
                    couchbeam_util:shutdown_sync(Pid),
                    Acc;
                Acc1 ->
                    fold_view_results(Ref, Pid, Fun, Acc1)
            end;
        {error, Ref, Error} ->
            {error, Acc, Error}
    end.



collect_view_results(Ref, Acc) ->
    receive
        {row, Ref, done} ->
            Rows = lists:reverse(Acc),
            {ok, Rows};
        {row, Ref, Row} ->
            collect_view_results(Ref, [Row|Acc]);
        {error, Ref, Error} ->
            %% in case we got some results
            Rows = lists:reverse(Acc),
            {error, Rows, Error}
    end.

process_view_results(ReqId, Params, UserFun, Callback) ->
    receive
        {ibrowse_async_headers, IbrowseRef, Code, Headers} ->
            case list_to_integer(Code) of
                Ok when Ok =:= 200 ; Ok =:= 201 ; (Ok >= 400 andalso Ok < 500) ->
                    StreamDataFun = fun() ->
                        process_view_results1(ReqId, UserFun, Callback)
                    end,
                    _ = ibrowse:stream_next(IbrowseRef),
                    try
                        _ = Callback(Ok, Headers, StreamDataFun),
                        couchbeam_httpc:clean_mailbox_req(ReqId)
                    catch
                        throw:http_response_end -> ok;
                        _:Error ->
                            UserFun({error, Error})
                    after
                        _ = ibrowse:stream_close(ReqId)
                    end,
                    ok;
                R when R =:= 301 ; R =:= 302 ; R =:= 303 ->
                    do_redirect(Headers, UserFun, Callback, Params),
                    ibrowse:stream_close(ReqId);
                Error ->
                    UserFun({error, {http_error, {status,
                                    Error}}})

            end;
        {ibrowse_async_response, ReqId, {error, _} = Error} ->
            UserFun({error, Error})
    end.

process_view_results1(ReqId, UserFun, Callback) ->
    receive
        {ibrowse_async_response, ReqId, {error, Error}} ->
            UserFun({error, Error});
        {ibrowse_async_response, ReqId, <<>>} ->
            _ = ibrowse:stream_next(ReqId),
            process_view_results1(ReqId, UserFun, Callback);
        {ibrowse_async_response, ReqId, Data} ->
            _ = ibrowse:stream_next(ReqId),
            {Data, fun() -> process_view_results1(ReqId, UserFun, Callback) end};
        {ibrowse_async_response_end, ReqId} ->
            UserFun(done),
            {<<"">>, fun() -> throw(http_response_end) end}
    end.

do_redirect(Headers, UserFun, Callback, {Args, Url, IbrowseOpts}) ->
    RedirectUrl = couchbeam_httpc:redirect_url(Headers, Url),
    Params = {Args, RedirectUrl, IbrowseOpts},

    %% if we send multiple keys, we do a Post
    Result = case Args#view_query_args.method of
        get ->
            couchbeam_httpc:request_stream({self(), once}, get, RedirectUrl, IbrowseOpts);
        post ->
            Body = ejson:encode({[{<<"keys">>, Args#view_query_args.keys}]}),
            Headers = [{"Content-Type", "application/json"}],
            couchbeam_httpc:request_stream({self(), once}, get, RedirectUrl,
                IbrowseOpts, Headers, Body)
    end,

    case Result of
        {ok, ReqId} ->
            process_view_results(ReqId, Params, UserFun, Callback);
        Error ->
            UserFun({error, {redirect, Error}})
    end.

%% view json stream events
view_ev1(object_start, UserFun) ->
    fun(Ev) -> view_ev2(Ev, UserFun) end.

view_ev2({key, <<"rows">>}, UserFun) ->
    fun(Ev) -> view_ev3(Ev, UserFun) end;
view_ev2(_, UserFun) ->
    fun(Ev) -> view_ev2(Ev, UserFun) end.

view_ev3(array_start, UserFun) ->
    fun(Ev) -> view_ev_loop(Ev, UserFun) end.

view_ev_loop(object_start, UserFun) ->
    fun(Ev) ->
        couchbeam_json_stream:collect_object(Ev,
            fun(Obj) ->
                UserFun(Obj),
                fun(Ev2) -> view_ev_loop(Ev2, UserFun) end
            end)
    end;
view_ev_loop(array_end, UserFun) ->
    UserFun(done),
    fun(_Ev) -> view_ev_done() end.

view_ev_done() ->
    fun(_Ev) -> view_ev_done() end.

