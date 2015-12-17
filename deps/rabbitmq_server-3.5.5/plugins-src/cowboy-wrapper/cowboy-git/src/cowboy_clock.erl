%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Date and time related functions.
%%
%% While a gen_server process runs in the background to update
%% the cache of formatted dates every second, all API calls are
%% local and directly read from the ETS cache table, providing
%% fast time and date computations.
-module(cowboy_clock).
-behaviour(gen_server).

-export([start_link/0, stop/0, rfc1123/0, rfc2109/1]). %% API.
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, terminate/2, code_change/3]). %% gen_server.

-record(state, {
	universaltime = undefined :: undefined | calendar:datetime(),
	rfc1123 = <<>> :: binary(),
	tref = undefined :: undefined | timer:tref()
}).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-include_lib("eunit/include/eunit.hrl").

%% API.

%% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
-spec stop() -> stopped.
stop() ->
	gen_server:call(?SERVER, stop).

%% @doc Return the current date and time formatted according to RFC-1123.
%%
%% This format is used in the <em>'Date'</em> header sent with HTTP responses.
-spec rfc1123() -> binary().
rfc1123() ->
	ets:lookup_element(?TABLE, rfc1123, 2).

%% @doc Return the current date and time formatted according to RFC-2109.
%%
%% This format is used in the <em>'Set-Cookie'</em> header sent with
%% HTTP responses.
-spec rfc2109(calendar:datetime()) -> binary().
rfc2109(LocalTime) ->
	{{YYYY,MM,DD},{Hour,Min,Sec}} =
	case calendar:local_time_to_universal_time_dst(LocalTime) of
	    [Gmt]   -> Gmt;
	    [_,Gmt] -> Gmt
	end,
	Wday = calendar:day_of_the_week({YYYY,MM,DD}),
	DayBin = pad_int(DD),
	YearBin = list_to_binary(integer_to_list(YYYY)),
	HourBin = pad_int(Hour),
	MinBin = pad_int(Min),
	SecBin = pad_int(Sec),
	WeekDay = weekday(Wday),
	Month = month(MM),
	<<WeekDay/binary, ", ",
	DayBin/binary, " ", Month/binary, " ",
	YearBin/binary, " ",
	HourBin/binary, ":",
	MinBin/binary, ":",
	SecBin/binary, " GMT">>.

%% gen_server.

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
	?TABLE = ets:new(?TABLE, [set, protected,
		named_table, {read_concurrency, true}]),
	T = erlang:universaltime(),
	B = update_rfc1123(<<>>, undefined, T),
	{ok, TRef} = timer:send_interval(1000, update),
	ets:insert(?TABLE, {rfc1123, B}),
	{ok, #state{universaltime=T, rfc1123=B, tref=TRef}}.

%% @private
-spec handle_call(_, _, State)
	-> {reply, ignored, State} | {stop, normal, stopped, State}.
handle_call(stop, _From, State=#state{tref=TRef}) ->
	{ok, cancel} = timer:cancel(TRef),
	{stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%% @private
-spec handle_cast(_, State) -> {noreply, State}.
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
-spec handle_info(_, State) -> {noreply, State}.
handle_info(update, #state{universaltime=Prev, rfc1123=B1, tref=TRef}) ->
	T = erlang:universaltime(),
	B2 = update_rfc1123(B1, Prev, T),
	ets:insert(?TABLE, {rfc1123, B2}),
	{noreply, #state{universaltime=T, rfc1123=B2, tref=TRef}};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
	ok.

%% @private
-spec code_change(_, State, _) -> {ok, State}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

-spec update_rfc1123(binary(), undefined | calendar:datetime(),
	calendar:datetime()) -> binary().
update_rfc1123(Bin, Now, Now) ->
	Bin;
update_rfc1123(<< Keep:23/binary, _/bits >>,
		{Date, {H, M, _}}, {Date, {H, M, S}}) ->
	<< Keep/binary, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< Keep:20/binary, _/bits >>,
		{Date, {H, _, _}}, {Date, {H, M, S}}) ->
	<< Keep/binary, (pad_int(M))/binary, $:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< Keep:17/binary, _/bits >>, {Date, _}, {Date, {H, M, S}}) ->
	<< Keep/binary, (pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< _:7/binary, Keep:10/binary, _/bits >>,
		{{Y, Mo, _}, _}, {Date = {Y, Mo, D}, {H, M, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<< (weekday(Wday))/binary, ", ", (pad_int(D))/binary, Keep/binary,
		(pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< _:11/binary, Keep:6/binary, _/bits >>,
		{{Y, _, _}, _}, {Date = {Y, Mo, D}, {H, M, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<< (weekday(Wday))/binary, ", ", (pad_int(D))/binary, " ",
		(month(Mo))/binary, Keep/binary,
		(pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(_, _, {Date = {Y, Mo, D}, {H, M, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<< (weekday(Wday))/binary, ", ", (pad_int(D))/binary, " ",
		(month(Mo))/binary, " ", (list_to_binary(integer_to_list(Y)))/binary,
		" ", (pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>.

%% Following suggestion by MononcQc on #erlounge.
-spec pad_int(0..59) -> binary().
pad_int(X) when X < 10 ->
	<< $0, ($0 + X) >>;
pad_int(X) ->
	list_to_binary(integer_to_list(X)).

-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(1..12) -> <<_:24>>.
month( 1) -> <<"Jan">>;
month( 2) -> <<"Feb">>;
month( 3) -> <<"Mar">>;
month( 4) -> <<"Apr">>;
month( 5) -> <<"May">>;
month( 6) -> <<"Jun">>;
month( 7) -> <<"Jul">>;
month( 8) -> <<"Aug">>;
month( 9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

%% Tests.

-ifdef(TEST).

update_rfc1123_test_() ->
	Tests = [
		{<<"Sat, 14 May 2011 14:25:33 GMT">>, undefined,
			{{2011, 5, 14}, {14, 25, 33}}, <<>>},
		{<<"Sat, 14 May 2011 14:25:33 GMT">>, {{2011, 5, 14}, {14, 25, 33}},
			{{2011, 5, 14}, {14, 25, 33}}, <<"Sat, 14 May 2011 14:25:33 GMT">>},
		{<<"Sat, 14 May 2011 14:25:34 GMT">>, {{2011, 5, 14}, {14, 25, 33}},
			{{2011, 5, 14}, {14, 25, 34}}, <<"Sat, 14 May 2011 14:25:33 GMT">>},
		{<<"Sat, 14 May 2011 14:26:00 GMT">>, {{2011, 5, 14}, {14, 25, 59}},
			{{2011, 5, 14}, {14, 26,  0}}, <<"Sat, 14 May 2011 14:25:59 GMT">>},
		{<<"Sat, 14 May 2011 15:00:00 GMT">>, {{2011, 5, 14}, {14, 59, 59}},
			{{2011, 5, 14}, {15,  0,  0}}, <<"Sat, 14 May 2011 14:59:59 GMT">>},
		{<<"Sun, 15 May 2011 00:00:00 GMT">>, {{2011, 5, 14}, {23, 59, 59}},
			{{2011, 5, 15}, { 0,  0,  0}}, <<"Sat, 14 May 2011 23:59:59 GMT">>},
		{<<"Wed, 01 Jun 2011 00:00:00 GMT">>, {{2011, 5, 31}, {23, 59, 59}},
			{{2011, 6,  1}, { 0,  0,  0}}, <<"Tue, 31 May 2011 23:59:59 GMT">>},
		{<<"Sun, 01 Jan 2012 00:00:00 GMT">>, {{2011, 5, 31}, {23, 59, 59}},
			{{2012, 1,  1}, { 0,  0,  0}}, <<"Sat, 31 Dec 2011 23:59:59 GMT">>}
	],
	[{R, fun() -> R = update_rfc1123(B, P, N) end} || {R, P, N, B} <- Tests].

pad_int_test_() ->
	Tests = [
		{ 0, <<"00">>}, { 1, <<"01">>}, { 2, <<"02">>}, { 3, <<"03">>},
		{ 4, <<"04">>}, { 5, <<"05">>}, { 6, <<"06">>}, { 7, <<"07">>},
		{ 8, <<"08">>}, { 9, <<"09">>}, {10, <<"10">>}, {11, <<"11">>},
		{12, <<"12">>}, {13, <<"13">>}, {14, <<"14">>}, {15, <<"15">>},
		{16, <<"16">>}, {17, <<"17">>}, {18, <<"18">>}, {19, <<"19">>},
		{20, <<"20">>}, {21, <<"21">>}, {22, <<"22">>}, {23, <<"23">>},
		{24, <<"24">>}, {25, <<"25">>}, {26, <<"26">>}, {27, <<"27">>},
		{28, <<"28">>}, {29, <<"29">>}, {30, <<"30">>}, {31, <<"31">>},
		{32, <<"32">>}, {33, <<"33">>}, {34, <<"34">>}, {35, <<"35">>},
		{36, <<"36">>}, {37, <<"37">>}, {38, <<"38">>}, {39, <<"39">>},
		{40, <<"40">>}, {41, <<"41">>}, {42, <<"42">>}, {43, <<"43">>},
		{44, <<"44">>}, {45, <<"45">>}, {46, <<"46">>}, {47, <<"47">>},
		{48, <<"48">>}, {49, <<"49">>}, {50, <<"50">>}, {51, <<"51">>},
		{52, <<"52">>}, {53, <<"53">>}, {54, <<"54">>}, {55, <<"55">>},
		{56, <<"56">>}, {57, <<"57">>}, {58, <<"58">>}, {59, <<"59">>}
	],
	[{I, fun() -> O = pad_int(I) end} || {I, O} <- Tests].

-endif.
