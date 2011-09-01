%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Analyze CouchDB logs
%%% @end
%%% Created : 28 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(couch_log).

-export([analyze/1, has_more/1, reset_file/1, reset_stats/1]).

-export([req_by_date/1, req_by_http/1, req_by_uri/1, req_by_statuscode/1
	 ,req_lines_parsed/1, req_date_range/1, req_time_elapsed/1
	]).

-export([req_by_date/2, req_by_http/2, req_by_uri/2, req_by_statuscode/2]).

-export([req_by_uri/3, req_by_uri/4]).

-export([req_by_http/3]).

-export([discard_until/2, discard_until/3]).

-include_lib("wh_couch.hrl").

-define(MAX_PARSED_LOG_LINES, 100000).

-record(log_line, {
	  date = {{0,0,0},{0,0,0}} :: wh_datetime()
	 ,log_level = <<>> :: binary()
         ,couch_pid = <<>> :: binary()
         ,req_id = <<>> :: binary()
	 }).

-record(log_line_http, {
	  uri = <<>> :: binary()
	 ,qs = [] :: [binary()|integer(),...] | []
         ,status_code = 0 :: integer()
         ,time_elapsed = 0 :: integer() %% milliseconds
	 ,method = <<>> :: binary()
         ,req_id = <<>> :: binary()
         ,date = {{0,0,0},{0,0,0}} :: wh_datetime()
	 }).

-record(log_data, {
	  io_device = undefined :: undefined | file:io_device()
         ,filename = <<>> :: binary()
	 ,by_date = orddict:new() :: orddict:orddict()
	 ,by_http = dict:new() :: dict() %% URI, #log_line_http{}
         ,by_uri = dict:new() :: dict()
         ,by_statuscode = dict:new() :: dict()
         ,by_errors = dict:new() :: dict()
         ,lines_parsed = 0 :: integer()
	 }).

analyze(#log_data{io_device=undefined}) ->
    {error, no_file};
analyze(#log_data{io_device=IODev}=LogData) ->
    analyze(LogData, file:read_line(IODev), 0);
analyze(Logfile) when is_binary(Logfile) ->
    {ok, IODev} = file:open(Logfile, [read, binary]), %% possibly use [raw, read_ahead, binary] for speed
    analyze(#log_data{io_device=IODev, filename=Logfile}, file:read_line(IODev), 0).

%% Ignore log lines until date stamps are after Start
-spec discard_until/2 :: (LogData, Start) -> #log_data{} when
      LogData :: #log_data{} | binary(),
      Start :: wh_datetime().
discard_until(#log_data{io_device=IODev}=LogData, Start) ->
    analyze(LogData, file:read_line(IODev), 0, fun(DateTime) -> in_date_range(DateTime, Start) end, ?MAX_PARSED_LOG_LINES);
discard_until(Logfile, Start) ->
    {ok, IODev} = file:open(Logfile, [read, binary]),
    discard_until(#log_data{io_device=IODev, filename=Logfile}, Start).

%% Ignore log lines until date stamps are after Start and before Finish
-spec discard_until/3 :: (LogData, Start, Finish) -> #log_data{} when
      LogData :: #log_data{} | binary(),
      Start :: wh_datetime(),
      Finish :: wh_datetime().
discard_until(#log_data{io_device=IODev}=LogData, Start, Finish) ->
    analyze(LogData, file:read_line(IODev), 0, fun(DateTime) -> in_date_range(DateTime, Start, Finish) end, infinity);
discard_until(Logfile, Start, Finish) ->
    {ok, IODev} = file:open(Logfile, [read, binary]),
    LD = discard_until(#log_data{io_device=IODev, filename=Logfile}, Start, Finish),
    ok = file:close(IODev),
    LD#log_data{io_device=undefined}.

%% Reset datasets, and rewind file pointer to beginning of the file
reset_file(#log_data{io_device=IODev, filename=F}) ->
    ok = file:close(IODev),
    {ok, IODev1} = file:open(F, [read, binary]), %% possibly use [raw, read_ahead, binary] for speed
    #log_data{io_device=IODev1, filename=F}.

%% Resets collectors and counters, but leaves file pointer in place
reset_stats(#log_data{io_device=IODev, filename=F}) ->
    #log_data{io_device=IODev, filename=F}.

req_lines_parsed(#log_data{lines_parsed=LP}) ->
    LP.

req_date_range(#log_data{by_date=ByDate}) ->
    {element(1, hd(ByDate)), element(1,hd(lists:reverse(ByDate)))}.

req_time_elapsed(#log_data{by_date=ByDate}) ->
    Start = element(1, hd(ByDate)),
    Finish = element(1,hd(lists:reverse(ByDate))),
    calendar:datetime_to_gregorian_seconds(Finish) - calendar:datetime_to_gregorian_seconds(Start).

req_by_date(LD) ->
    req_by_date(LD, raw).

req_by_date(#log_data{by_date=ByDate}, Type) ->
    sort_list(orddict:fold(fun count_entries/3, [], ByDate), Type).

req_by_http(LD) ->
    req_by_http(LD, raw).

req_by_http(LogData, uri) ->
    req_by_http(LogData, raw, uri);
req_by_http(#log_data{by_http=ByHttp}, Type) ->
    sort_list(dict:fold(fun count_entries/3, [], ByHttp), Type).

req_by_http(#log_data{by_http=ByHttp}, Type, uri) ->
    sort_list(dict:fold(fun(Method, LogLineHttps, Acc) ->
				UriCounts = lists:foldl(fun(#log_line_http{uri=Uri}, Dict0) ->
								dict:update_counter(Uri, 1, Dict0)
							end, dict:new(), LogLineHttps),
				[ {Method, [ {total, length(LogLineHttps)} | sort_list(dict:to_list(UriCounts), Type)]} | Acc]
			end, [], ByHttp), Type).

req_by_uri(LD) ->
    req_by_uri(LD, raw).

req_by_uri(#log_data{by_uri=ByUri}, Type) ->
    sort_list(dict:fold(fun count_entries/3, [], ByUri), Type).

req_by_uri(LogData, {_,_}=Start, {_,_}=Finish) ->
    req_by_uri(LogData, raw, Start, Finish).

req_by_uri(#log_data{by_uri=ByUri}, Sort, Start, Finish) when Start =< Finish ->
    ByUri1 = dict:filter(fun(_, [#log_line_http{date=DateStamp}|_]) -> DateStamp >= Start andalso DateStamp =< Finish end, ByUri),
    req_by_uri(#log_data{by_uri=ByUri1}, Sort).

req_by_statuscode(LD) ->
    req_by_statuscode(LD, raw).

req_by_statuscode(#log_data{by_statuscode=ByStatus}, Type) ->
    sort_list(dict:fold(fun count_entries/3, [], ByStatus), Type).

count_entries(K, V, A) when is_list(V) ->
    [{K, length(V)} | A];
count_entries(K, V, A) when is_integer(V) ->
    [{K, V} | A].

sort_list(L, raw) ->
    L;
sort_list(L, asc) ->
    lists:keysort(2, L);
sort_list(L, desc) ->
    lists:reverse(sort_list(L, asc)).

has_more(#log_data{io_device=undefined}) ->
    false;
has_more(_) ->
    true.

analyze(LD, Result, Cnt) ->
    analyze(LD, Result, Cnt, fun reply_true/1, ?MAX_PARSED_LOG_LINES).

analyze(#log_data{io_device=IODev, lines_parsed=LP}=LogData, {ok, Data}, Cnt, Filter, infinity) ->
    case analyze_data(LogData, Data, Filter) of
	{true, LogData1} ->
	    analyze(LogData1, file:read_line(IODev), Cnt+1, Filter, infinity);
	{false, LogData2} ->
	    analyze(LogData2, file:read_line(IODev), Cnt, Filter, infinity);
	{stop, LogData3} ->
	    LogData3#log_data{lines_parsed=LP+Cnt}
    end;
analyze(#log_data{io_device=IODev, lines_parsed=LP}=LogData, {ok, Data}, Cnt, Filter, Max) when Cnt < Max ->
    case analyze_data(LogData, Data, Filter) of
	{true, LogData1} ->
	    analyze(LogData1, file:read_line(IODev), Cnt+1, Filter, Max);
	{false, LogData2} ->
	    analyze(LogData2, file:read_line(IODev), Cnt, Filter, Max);
	{stop, LogData3} ->
	    LogData3#log_data{lines_parsed=LP+Cnt}
    end;
analyze(#log_data{lines_parsed=LP}=LogData, {ok, Data}, Cnt, Filter, _Max) ->
    case analyze_data(LogData, Data, Filter) of
	{false, LogData1} ->
	    LogData1#log_data{lines_parsed=LP+Cnt};
	{true, LogData2} ->
	    LogData2#log_data{lines_parsed=LP+Cnt+1};
	{stop, LogData3} ->
	    LogData3#log_data{lines_parsed=LP+Cnt}
    end;
analyze(#log_data{io_device=IODev, lines_parsed=LP}=LD, eof, Cnt, _, _) ->
    ok = file:close(IODev),
    LD#log_data{io_device=undefined, lines_parsed=LP+Cnt};
analyze(_, {error, R}, _, _, _) ->
    throw(R).

-spec analyze_data/3 :: (LogData, LineData, Filter) -> {boolean() | stop, #log_data{}} when
      LogData :: #log_data{},
      LineData :: binary(),
      Filter :: fun((_) -> boolean() | stop).
analyze_data(LogData, <<"\n">>, _) ->
    {false, LogData};
analyze_data(LogData, <<"killed\n">>, _) -> %% skip killed
    {false, LogData};
analyze_data(LogData, <<" ", _/binary>>, _) -> %% skip continuations of stacktraces
    {false, LogData};
analyze_data(LogData, <<"**", _/binary>>, _) -> %% skip ** Last message...
    {false, LogData};
analyze_data(LogData, <<"{", _/binary>>, _) -> %% skip {{badmatch,... and the like
    {false, LogData};
analyze_data(LogData, <<"noproc", _/binary>>, _) -> %% skip noproc\n
    {false, LogData};

analyze_data(#log_data{by_date = ByDate}=LogData, LineData, Filter) ->
    [<<"[", DS/binary>>
	 ,<<"[", LogLevel/binary>>
	 ,<<"[", CouchPid/binary>>
	 ,<<"[", ReqID/binary>>
	 ,ReqData
    ] = binary:split(LineData, <<"] ">>, [global]),

    DateStamp = convert_datestamp(DS),

    case Filter(DateStamp) of
	false -> {false, LogData};
	stop -> {stop, LogData};
	true ->
	    LogLine = #log_line{date=DateStamp
				 ,log_level=LogLevel
				 ,couch_pid=CouchPid
				 ,req_id=ReqID
				},
	    {true, analyze_req_data(LogData#log_data{by_date=orddict:update(DateStamp, fun(Old) -> [LogLine | Old] end, [LogLine], ByDate)}, ReqData, ReqID, DateStamp)}
    end.

-spec analyze_req_data/4 :: (LogData, ReqData, ReqID, DateStamp) -> #log_data{} when
      LogData :: #log_data{},
      ReqData :: binary(),
      ReqID :: binary(),
      DateStamp :: wh_datetime().
analyze_req_data(#log_data{io_device=IODev}=LogData, ReqData, ReqID, DateStamp) ->
    ?LOG_SYS("Req Data: ~s", [ReqData]),
    case binary:split(ReqData, <<" ">>, [global]) of
	[_, <<"-">>, <<"-">>, Verb, Uri, SC] ->
	    #log_data{by_http=ByHttp, by_uri=ByUri, by_statuscode=ByStatus} = LogData,

	    HttpVerb = get_verb(Verb),
	    StatusCode = wh_util:to_integer(binary:replace(SC, <<"\n">>, <<>>)),

	    {U, QS} = case binary:split(Uri, <<"?">>) of
			  [U1, QS1] -> {U1, QS1};
			  [U1] -> {U1, <<>>}
		      end,

	    HTTP = #log_line_http{
	      uri = U
	      ,qs = mochiweb_util:parse_qs(QS)
	      ,status_code = StatusCode
	      ,method = HttpVerb
	      ,date = DateStamp
	      ,req_id = ReqID
	     },
	    AppendFun = fun(Old) -> [HTTP | Old] end,

	    LogData#log_data{by_http=dict:update(HttpVerb, AppendFun, [HTTP], ByHttp)
			     ,by_uri=dict:update(U, AppendFun, [HTTP], ByUri)
			     ,by_statuscode=dict:update(StatusCode, AppendFun, [HTTP], ByStatus)
			    };
	[<<"Uncaught">> | _] -> LogData; %% Skip errors in HTTP request
	[<<"could">>, <<"not">> | _] -> LogData; %% Skip errors
	[<<"Apache">>, <<"CouchDB">> | _] -> LogData; %% Skip restarts
	[<<"**">>, <<"Generic">> | _] -> LogData; %% Skip gen_server terminating
	[<<"config_event">> | _] -> LogData; %% Skip config event handler terminating
	[<<"Shutting">> | _] ->
	    %% Skip group servers being shut down, but forward the IODev
	    _ = file:read_file(IODev),
	    LogData;
	[<<"db">> | _] -> LogData; %% skip db shards/000000-1ffffff/... died with reason ...
	[_IP, _IP_Port, Verb, Uri, SC, Time] ->
	    #log_data{by_http=ByHttp, by_uri=ByUri, by_statuscode=ByStatus} = LogData,

	    HttpVerb = get_verb(Verb),
	    StatusCode = wh_util:to_integer(SC),

	    {U, QS} = case binary:split(Uri, <<"?">>) of
			  [U1, QS1] -> {U1, QS1};
			  [U1] -> {U1, <<>>}
		      end,

	    HTTP = #log_line_http{
	      uri = U
	      ,qs = mochiweb_util:parse_qs(QS)
	      ,status_code = StatusCode
	      ,method = HttpVerb
	      ,time_elapsed = wh_util:to_integer(binary:replace(Time, <<"\n">>, <<>>))
	     },
	    AppendFun = fun(Old) -> [HTTP | Old] end,

	    LogData#log_data{by_http=dict:update(HttpVerb, AppendFun, [HTTP], ByHttp)
			     ,by_uri=dict:update(U, AppendFun, [HTTP], ByUri)
			     ,by_statuscode=dict:update(StatusCode, AppendFun, [HTTP], ByStatus)
			    };
	_ -> LogData
    end.

reply_true(_) ->
    true.

in_date_range(DateTime, Start) ->
    DateTime >= Start.

in_date_range(DateTime, Start, Finish) ->
    case DateTime >= Start of
	true ->
	    case DateTime =< Finish of
		true -> true;
		false -> stop %% no more after we've passed Finish, so stop
	    end;
	false -> false
    end.

get_verb(V) ->
    binary:replace(V, <<"'">>, <<>>, [global]).

convert_datestamp(DS) ->
    httpd_util:convert_request_date(wh_util:to_list(DS)).

%% [Wed, 27 Jul 2011 16:55:21 GMT] [error] [<0.21421.543>] [--------] {error_report,<0.146.0>,
%%     {<0.21421.543>,crash_report,
%%      [[{initial_call,{mochiweb_socket_server,acceptor_loop,['Argument__1']}},
%%        {pid,<0.21421.543>},


%% [Wed, 27 Jul 2011 16:55:21 GMT] [error] [<0.1099.0>] [--------] {error_report,<0.146.0>,
%%     {<0.1099.0>,std_error,
%%      {mochiweb_socket_server,235,
%%          {child_error,{case_clause,{error,enotconn}}}}}}

%% [Wed, 27 Jul 2011 16:55:21 GMT] [info] [<0.21809.543>] [c7c8196e] Stacktrace: [{mochiweb_request,send,2},
%%              {couch_httpd,send_chunk,2},
%%              {couch_httpd,end_json_response,1}

%% [Wed, 27 Jul 2011 16:55:32 GMT] [info] [<0.29290.539>] [--------] Shutting down group server <<"_design/session">>, db <0.29296.539> closing w/ reason
%% killed
