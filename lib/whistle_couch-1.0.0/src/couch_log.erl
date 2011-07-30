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

-export([discard_until/2, discard_until/3]).

-include_lib("wh_couch.hrl").

-define(MAX_PARSED_LOG_LINES, 100000).

-record(log_line, {
	  date = <<>> :: binary()
	 ,log_level = <<>> :: binary()
         ,couch_pid = <<>> :: binary()
         ,req_id = <<>> :: binary()
	 }).

-record(log_line_http, {
	  uri = <<>> :: binary()
	 ,qs = [] :: [binary(),...] | []
         ,status_code = 0 :: integer()
         ,time_elapsed = 0 :: integer() %% milliseconds
	 ,method = <<>> :: binary()
         ,req_id = <<>> :: binary()
	 }).

-record(log_data, {
	  io_device = undefined :: undefined | file:io_device()
         ,filename = <<>> :: binary()
	 ,by_date = orddict:new() :: orddict:orddict()
	 ,by_http = dict:new() :: dict()
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

req_by_date(#log_data{by_date=ByDate}, raw) ->
    orddict:fold(fun count_entries/3, [], ByDate);
req_by_date(#log_data{by_date=ByDate}, asc) ->
    lists:keysort(2, orddict:fold(fun count_entries/3, [], ByDate));
req_by_date(#log_data{by_date=ByDate}, desc) ->
    lists:reverse(lists:keysort(2, orddict:fold(fun count_entries/3, [], ByDate))).

req_by_http(LD) ->
    req_by_http(LD, raw).

req_by_http(#log_data{by_http=ByHttp}, raw) ->
    dict:fold(fun count_entries/3, [], ByHttp);
req_by_http(#log_data{by_http=ByHttp}, asc) ->
    lists:keysort(2, dict:fold(fun count_entries/3, [], ByHttp));
req_by_http(#log_data{by_http=ByHttp}, desc) ->
    lists:reverse(lists:keysort(2, dict:fold(fun count_entries/3, [], ByHttp))).

req_by_uri(LD) ->
    req_by_uri(LD, raw).

req_by_uri(#log_data{by_uri=ByUri}, raw) ->
    dict:fold(fun count_entries/3, [], ByUri);
req_by_uri(#log_data{by_uri=ByUri}, asc) ->
    lists:keysort(2, dict:fold(fun count_entries/3, [], ByUri));
req_by_uri(#log_data{by_uri=ByUri}, desc) ->
    lists:reverse(lists:keysort(2, dict:fold(fun count_entries/3, [], ByUri))).

req_by_statuscode(LD) ->
    req_by_statuscode(LD, raw).

req_by_statuscode(#log_data{by_statuscode=ByStatus}, raw) ->
    dict:fold(fun count_entries/3, [], ByStatus);
req_by_statuscode(#log_data{by_statuscode=ByStatus}, asc) ->
    lists:keysort(2, dict:fold(fun count_entries/3, [], ByStatus));
req_by_statuscode(#log_data{by_statuscode=ByStatus}, desc) ->
    lists:reverse(lists:keysort(2, dict:fold(fun count_entries/3, [], ByStatus))).

count_entries(K, V, A) when is_list(V) ->
    [{K, length(V)} | A];
count_entries(K, V, A) when is_integer(V) ->
    [{K, V} | A].

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
	    LogLine = [#log_line{date=DateStamp
				 ,log_level=LogLevel
				 ,couch_pid=CouchPid
				 ,req_id=ReqID
				}],
	    {true, analyze_req_data(LogData#log_data{by_date=orddict:append_list(DateStamp, LogLine, ByDate)}, ReqData)}
    end.

-spec analyze_req_data/2 :: (LogData, ReqData) -> #log_data{} when
      LogData :: #log_data{},
      ReqData :: binary().
analyze_req_data(#log_data{io_device=IODev}=LogData, ReqData) ->
    case binary:split(ReqData, <<" ">>, [global]) of
	[_, <<"-">>, <<"-">>, Verb, Uri, SC] ->
	    #log_data{by_http=ByHttp, by_uri=ByUri, by_statuscode=ByStatus} = LogData,

	    HttpVerb = get_verb(Verb),
	    StatusCode = whistle_util:to_integer(binary:replace(SC, <<"\n">>, <<>>)),

	    {U, QS} = case binary:split(Uri, <<"?">>) of
			  [U1, QS1] -> {U1, QS1};
			  [U1] -> {U1, <<>>}
		      end,

	    HTTP = [#log_line_http{
	      uri = U
	      ,qs = mochiweb_util:parse_qs(QS)
	      ,status_code = StatusCode
	      ,method = HttpVerb
	     }],

	    LogData#log_data{by_http=dict:append_list(HttpVerb, HTTP, ByHttp)
			     ,by_uri=dict:append_list(U, HTTP, ByUri)
			     ,by_statuscode=dict:append_list(StatusCode, HTTP, ByStatus)
			    };
	[<<"Uncaught">> | _] -> LogData; %% Skip errors in HTTP request
	[<<"Shutting">> | _] ->
	    %% Skip group servers being shut down, but forward the IODev
	    _ = file:read_file(IODev),
	    LogData;
	[<<"db">> | _] -> LogData; %% skip db shards/000000-1ffffff/... died with reason ...
	[_IP, _IP_Port, Verb, Uri, SC, Time] ->
	    #log_data{by_http=ByHttp, by_uri=ByUri, by_statuscode=ByStatus} = LogData,

	    HttpVerb = get_verb(Verb),
	    StatusCode = whistle_util:to_integer(SC),

	    {U, QS} = case binary:split(Uri, <<"?">>) of
			  [U1, QS1] -> {U1, QS1};
			  [U1] -> {U1, <<>>}
		      end,

	    HTTP = [#log_line_http{
	      uri = U
	      ,qs = mochiweb_util:parse_qs(QS)
	      ,status_code = StatusCode
	      ,method = HttpVerb
	      ,time_elapsed = whistle_util:to_integer(binary:replace(Time, <<"\n">>, <<>>))
	     }],

	    LogData#log_data{by_http=dict:append_list(HttpVerb, HTTP, ByHttp)
			     ,by_uri=dict:append_list(U, HTTP, ByUri)
			     ,by_statuscode=dict:append_list(StatusCode, HTTP, ByStatus)
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
    httpd_util:convert_request_date(whistle_util:to_list(DS)).

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
