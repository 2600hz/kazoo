%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_changes).

-include_lib("couchbeam/include/couchbeam.hrl").
-include_lib("ibrowse/include/ibrowse.hrl").

-export([stream/2, stream/3,
         fetch/1, fetch/2,
         parse_changes_options/1,
         changes_loop/3]).

-spec stream(Db::db(), Client::pid() | function()) -> {ok, StartRef::term(),
        ChangesPid::pid()} | {error, term()}.
%% @equiv stream(Db, Client, [])
stream(Db, Client) ->
    stream(Db, Client, []).



-spec stream(Db::db(), Client::pid() | function(),
     Options::changes_options()) -> {ok, StartRef::term(), ChangesPid::pid()} |
        {ok, ChangesPid::pid()} | {error, term()}.
%% @doc Stream changes to a pid
%%  <p>Db : a db record</p>
%%  <p>Client : pid  or callback where to send changes events where events are
%%  The pid receive these events:
%%  <dl>
%%      <dt>{change, StartRef, {done, Lastseq::integer()}</dt>
%%          <dd>Connection terminated or you got all changes</dd>
%%      <dt>{change, StartRef, Row :: ejson_object()}</dt>
%%          <dd>Line of change</dd>
%%      <dt>{error, LastSeq::integer(), Msg::term()}</dt>
%%          <dd>Got an error, connection is closed when an error
%%          happend.</dd>
%% </dl>
%%    LastSeq is the last sequence of changes.</p>
%% While the callbac could be like:
%%<pre>
%%      fun({done, LastSeq}) ->
%%          ok;
%%      fun({done, LastSeq}) ->
%%          ok;
%%      fun({done, LastSeq}) ->
%%          ok.</pre>
%% <p><pre>>ChangesOptions :: changes_options() [continuous | longpoll | normal
%%    | include_docs | {since, integer()}
%%    | {timeout, integer()}
%%    | heartbeat | {heartbeat, integer()}
%%    | {filter, string()} | {filter, string(), list({string(), string() | integer()}</pre>
%%
%%   <ul>
%%      <li><code>continuous | longpoll | normal</code>: set the type of changes
%%          feed to get</li>
%%      <li><code>include_doc</code>: if you want to include the doc in the line of
%%          change</li>
%%      <li><code>{timeout, Timeout::integer()}</code>:: timeout</li>
%%      <li><code>heartbeat | {heartbeat, Heartbeat::integer()}</code>: set couchdb
%%          to send a heartbeat to maintain connection open</li>
%%      <li><code>{filter, FilterName} | {filter, FilterName, Args::list({key,
%%          value})</code>: set the filter to use with optional arguments</li>
%%   </ul></p>
%%
%% <p> Return {ok, StartRef, ChangesPid} or {error, Error}. Ref can be
%% used to disctint all changes from this pid. ChangesPid is the pid of
%% the changes loop process. Can be used to monitor it or kill it
%% when needed.</p>


stream(Db, ClientPid, Options) when is_pid(ClientPid) ->
    StartRef = make_ref(),
    UserFun = fun
        (done) ->
            LastSeq = get(last_seq),
            ClientPid ! {change, StartRef, {done, LastSeq}};
        ({error, Error}) ->
            LastSeq = get(last_seq),
            ClientPid ! {error, StartRef, LastSeq, Error};
        ({[{<<"last_seq">>, _}]}) ->
            ok;
        (Row) ->
            ClientPid ! {change, StartRef, Row},
            Seq = couchbeam_doc:get_value(<<"seq">>, Row),
            put(last_seq, Seq)
    end,
    do_stream(Db, UserFun, Options, StartRef);

stream(Db, Fun, Options) ->
    UserFun = fun
        (done) ->
            LastSeq = get(last_seq),
            Fun({done, LastSeq});
        ({error, Error}) ->
            LastSeq = get(last_seq),
            Fun({error, LastSeq, Error});
        ({[{<<"last_seq">>, _}]}) ->
            ok;
        (Row) ->
            Fun({change, Row}),
            Seq = couchbeam_doc:get_value(<<"seq">>, Row),
            put(last_seq, Seq)
    end,
    do_stream(Db, UserFun, Options).


-spec fetch(Db::db()) -> {ok, LastSeq::integer(), Rows::list()} | {error,
        LastSeq::integer(), Error::term()}.
%% @equiv fetch(Db, [])
fetch(Db) ->
    fetch(Db, []).

-spec fetch(Db::db(), Options::changes_options1()) -> {ok,
        LastSeq::integer(), Rows::list()} | {error,  LastSeq::integer(),
        Error::term()}.
%% @doc Collect Changes. Could be used to make a blocking call to a
%% longpoll change feed
%%  <p>Db : a db record</p>
%% <p><pre>ChangesOptions :: changes_options() [continuous | longpoll | normal
%%    | include_docs | {since, integer()}
%%    | {timeout, integer()}
%%    | heartbeat | {heartbeat, integer()}
%%    | {filter, string()} | {filter, string(), list({string(), string() | integer()}</pre>
%%
%%   <ul>
%%      <li><code>longpoll | normal</code>: set the type of changes
%%          feed to get</li>
%%      <li><code>include_docs</code>: if you want to include the doc in the line of
%%          change</li>
%%      <li><code>{timeout, Timeout::integer()}</code>: timeout</li>
%%      <li><code>heartbeat | {heartbeat, Heartbeat::integer()}</code>: set couchdb
%%          to send a heartbeat to maintain connection open</li>
%%      <li><code>{filter, FilterName} | {filter, FilterName, Args::list({key,
%%          value})</code>: set the filter to use with optional arguments</li>
%%   </ul></p>
%%
%% <p>Resut: <code>{ok, LastSeq::integer(), Rows::list()}</code> or
%% <code>{error, LastSeq, Error}</code>. LastSeq is the last sequence of changes.</p>
fetch(Db, Options) ->
    case stream(Db, self(), Options) of
        {ok, StartRef, _} ->
            collect_changes(StartRef, []);
        Error ->
            Error
    end.


%% @doc parse changes options and return a changes_args record
-spec parse_changes_options(Options::changes_options()) ->
    changes_args().
parse_changes_options(Options) ->
    parse_changes_options(Options, #changes_args{}).

parse_changes_options([], Args) ->
    Args;
parse_changes_options([continuous|Rest], #changes_args{http_options=Opts}) ->
    Opts1 = [{"feed", "continuous"}|Opts],
    parse_changes_options(Rest, #changes_args{type=continuous,
            http_options=Opts1});
parse_changes_options([longpoll|Rest], #changes_args{http_options=Opts}) ->
    Opts1 = [{"feed", "longpoll"}|Opts],
    parse_changes_options(Rest, #changes_args{type=longpoll,
            http_options=Opts1});
parse_changes_options([normal|Rest], Args) ->
    parse_changes_options(Rest, Args#changes_args{type=normal});
parse_changes_options([include_docs|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"include_docs", "true"}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([{since, Since}|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"since", Since}|Opts],
    parse_changes_options(Rest,
        Args#changes_args{since=Since, http_options=Opts1});
parse_changes_options([{timeout, Timeout}|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"timeout", Timeout}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([{heartbeat, Heartbeat}|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"heartbeat", Heartbeat}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([heartbeat|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"heartbeat", "true"}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([{filter, FilterName}|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"filter", FilterName}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([{filter, FilterName, FilterArgs}|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"filter", FilterName}|Opts] ++ FilterArgs,
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([{limit, Limit}|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"limit", Limit}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([conflicts|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"conflicts", "true"}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([{style, Style}|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"style", Style}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([descending|Rest], #changes_args{http_options=Opts} = Args) ->
    Opts1 = [{"descending", "true"}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([{Key, Val}|Rest], #changes_args{http_options=Opts} = Args)
  when is_list(Key) andalso is_list(Val) ->
    Opts1 = [{Key, Val}|Opts],
    parse_changes_options(Rest, Args#changes_args{http_options=Opts1});
parse_changes_options([_|Rest], Args) ->
    parse_changes_options(Rest, Args).

-spec changes_loop(Args::changes_args(), UserFun::function(),
    Params::{Url::string(), IbrowseOpts::list()}) -> ok.
changes_loop(#changes_args{since=Since}=Args, UserFun, Params) ->
    %% initialize last_seq /
    put(last_seq, Since),

    Callback = case Args#changes_args.type of
        continuous ->
            fun(200, _Headers, DataStreamFun) ->
                continuous_changes(DataStreamFun, UserFun)
            end;
        _ ->
            fun(200, _Headers, DataStreamFun) ->
                EventFun = fun(Ev) ->
                    changes_ev1(Ev, UserFun)
                end,
                couchbeam_json_stream:events(DataStreamFun, EventFun)
            end
    end,
    receive
        {ibrowse_req_id, ReqId} ->
            process_changes(ReqId, Params, UserFun, Callback)
    after ?DEFAULT_TIMEOUT ->
        UserFun({error, timeout})
    end.

%% @private
do_stream(Db, UserFun, Options) ->
    do_stream(Db, UserFun, Options, nil).

do_stream(#db{server=Server, options=IbrowseOpts}=Db, UserFun, Options,
        StartRef) ->

    Args = parse_changes_options(Options),
    Url = couchbeam:make_url(Server, [couchbeam:db_url(Db), "/_changes"],
        Args#changes_args.http_options),

    Params = {Url, IbrowseOpts},

    ChangesPid = spawn_link(couchbeam_changes, changes_loop,
        [Args, UserFun, Params]),

    case couchbeam_httpc:request_stream({ChangesPid, once}, get, Url, IbrowseOpts) of
        {ok, ReqId} ->
            ChangesPid ! {ibrowse_req_id, ReqId},
            case StartRef of
                nil ->
                    {ok, ChangesPid};
                _ ->
                    {ok, StartRef, ChangesPid}
            end;
        Error ->
            Error
    end.

collect_changes(Ref, Acc) ->
    receive
        {change, Ref, {done, LastSeq}} ->
            Rows = lists:reverse(Acc),
            {ok, LastSeq, Rows};
        {change, Ref, Row} ->
            collect_changes(Ref, [Row|Acc]);
        {error, Ref, LastSeq, Error} ->
            {error, LastSeq, Error}
    end.


process_changes(ReqId, Params, UserFun, Callback) ->
    receive
        {ibrowse_async_headers, IbrowseRef, Code, Headers} ->
            case list_to_integer(Code) of
                Ok when Ok =:= 200 ; Ok =:= 201 ; (Ok >= 400 andalso Ok < 500) ->
                    StreamDataFun = fun() ->
                        process_changes1(ReqId, UserFun, Callback)
                    end,
                    ibrowse:stream_next(IbrowseRef),
                    try
                        _ = Callback(Ok, Headers, StreamDataFun),
                        couchbeam_httpc:clean_mailbox_req(ReqId)
                    catch
                        throw:http_response_end -> ok;
                        _:Error ->
                            UserFun({error, Error})
                    end,
                    ok;
                R when R =:= 301 ; R =:= 302 ; R =:= 303 ->
                    do_redirect(Headers, UserFun, Callback, Params),
                    ibrowse:stream_close(reqId);
                Error ->
                    UserFun({error, {http_error, {status,
                                    Error}}})

            end;
        {ibrowse_async_response, ReqId, {error, _} = Error} ->
            UserFun({error, Error})
    end.

process_changes1(ReqId, UserFun, Callback) ->
    receive
        {ibrowse_async_response, ReqId, {error, Error}} ->
            UserFun({error, Error});
    {ibrowse_async_response, ReqId, <<>>} ->
            ibrowse:stream_next(ReqId),
            process_changes1(ReqId, UserFun, Callback);
        {ibrowse_async_response, ReqId, Data} ->
            ibrowse:stream_next(ReqId),
            {Data, fun() -> process_changes1(ReqId, UserFun, Callback) end};
    {ibrowse_async_response_end, ReqId} ->
            UserFun(done),
            {<<"">>, fun() -> throw(http_response_end) end}
    end.


do_redirect(Headers, UserFun, Callback, {Url, IbrowseOpts}) ->
    RedirectUrl = couchbeam_httpc:redirect_url(Headers, Url),
    Params = {RedirectUrl, IbrowseOpts},
    case couchbeam_httpc:request_stream({self(), once}, get, RedirectUrl,
            IbrowseOpts) of
        {ok, ReqId} ->
            process_changes(ReqId, Params, UserFun, Callback);
        Error ->
            UserFun({error, {redirect, Error}})
    end.


changes_ev1(object_start, UserFun) ->
    fun(Ev) -> changes_ev2(Ev, UserFun) end.

changes_ev2({key, <<"results">>}, UserFun) ->
    fun(Ev) -> changes_ev3(Ev, UserFun) end;
changes_ev2(_, UserFun) ->
    fun(Ev) -> changes_ev2(Ev, UserFun) end.

changes_ev3(array_start, UserFun) ->
    fun(Ev) -> changes_ev_loop(Ev, UserFun) end.

changes_ev_loop(object_start, UserFun) ->
    fun(Ev) ->
        couchbeam_json_stream:collect_object(Ev,
            fun(Obj) ->
                UserFun(Obj),
                fun(Ev2) -> changes_ev_loop(Ev2, UserFun) end
            end)
    end;
changes_ev_loop(array_end, UserFun) ->
    UserFun(done),
    fun(_Ev) -> changes_ev_done() end.

changes_ev_done() ->
    fun(_Ev) -> changes_ev_done() end.

continuous_changes(DataFun, UserFun) ->
    {DataFun2, _, Rest} = couchbeam_json_stream:events(
        DataFun,
        fun(Ev) -> parse_changes_line(Ev, UserFun) end),
    continuous_changes(fun() -> {Rest, DataFun2} end, UserFun).

parse_changes_line(object_start, UserFun) ->
    fun(Ev) ->
        couchbeam_json_stream:collect_object(Ev,
            fun(Obj) -> UserFun(Obj) end)
    end.
