-module(whistle_util).

-export([call_response/3, call_response/4, call_response/5]).
-export([to_e164/1, to_npan/1, to_1npan/1]).
-export([to_integer/1, to_float/1, to_hex/1, to_list/1, to_binary/1, to_atom/1, to_atom/2]).
-export([to_boolean/1, is_true/1, is_false/1, binary_to_lower/1]).
-export([a1hash/3, floor/1, ceiling/1]).
-export([current_tstamp/0]).
-export([gregorian_seconds_to_unix_seconds/1, unix_seconds_to_gregorian_seconds/1]).
-export([microseconds_to_seconds/1]).
-export([whistle_version/0]).

-include_lib("proper/include/proper.hrl").

-spec(call_response/3 :: (CallId :: binary(), CtrlQ :: binary(), Code :: binary()) -> ok).
-spec(call_response/4 :: (CallId :: binary(), CtrlQ :: binary(), Code :: binary(), Cause :: undefined|binary()) -> ok).
-spec(call_response/5 :: (CallId :: binary(), CtrlQ :: binary(), Code :: binary(), Cause :: undefined|binary(), Media :: undefined|binary()) -> ok).

call_response(CallId, CtrlQ, Code) ->
    call_response(CallId, CtrlQ, Code, <<>>).
call_response(CallId, CtrlQ, Code, undefined) ->
    call_response(CallId, CtrlQ, Code, <<>>);
call_response(CallId, CtrlQ, Code, Cause) ->
    call_response(CallId, CtrlQ, Code, Cause, undefined).
call_response(CallId, CtrlQ, Code, Cause, Media) ->
    Respond = {struct, [{<<"Application-Name">>, <<"respond">>}
                        ,{<<"Response-Code">>, Code}
                        ,{<<"Response-Message">>, Cause}
                        ,{<<"Call-ID">>, CallId}]
              },
    call_response1(CallId, CtrlQ, Media, Respond).

call_response1(CallId, CtrlQ, undefined, Respond) ->
    call_response1(CallId, CtrlQ, [Respond]);
call_response1(CallId, CtrlQ, Media, Respond) ->
    call_response1(CallId, CtrlQ, [Respond
				   ,{struct, [{<<"Application-Name">>, <<"play">>}
					      ,{<<"Media-Name">>, Media}
					      ,{<<"Call-ID">>, CallId}]
				    }
				   ,{struct, [{<<"Application-Name">>, <<"progress">>}
					      ,{<<"Call-ID">>, CallId}]
				    }
				  ]).

call_response1(CallId, CtrlQ, Commands) ->
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Call-ID">>, CallId}
               ,{<<"Commands">>, Commands}
               | whistle_api:default_headers(<<>>, <<"call">>, <<"command">>, <<"call_response">>, <<"0.1.0">>)],
    {ok, Payload} = whistle_api:queue_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload).

%% must be a term that can be changed to a list
-spec(to_hex/1 :: (S :: term()) -> string()).
to_hex(S) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- to_list(S)])).

%% +18001234567 -> +18001234567
-spec(to_e164/1 :: (DID :: binary()) -> binary()).
to_e164(<<"011", N/binary>>) ->
    <<$+, N/binary>>;
to_e164(<<$+, $1, N/bitstring>>=E164) when erlang:bit_size(N) =:= 80 -> % 8bits/ch * 10ch
    E164;
%% 18001234567 -> +18001234567
to_e164(<<$1, N/binary>>=NPAN1) when erlang:bit_size(N) =:= 80 ->
    << $+, NPAN1/bitstring >>;
%% 8001234567 -> +18001234567
to_e164(NPAN) when erlang:bit_size(NPAN) =:= 80 ->
    <<$+, $1, NPAN/bitstring>>;
to_e164(Other) ->
    Other.

%% end up with 8001234567 from 1NPAN and E.164
-spec(to_npan/1 :: (NPAN :: binary()) -> binary()).
to_npan(<<"011", N/binary>>) ->
    to_npan(N);
to_npan(<<$+, $1, N/bitstring>>) when erlang:bit_size(N) =:= 80 ->
    N;
to_npan(<<$1, N/bitstring>>) when erlang:bit_size(N) =:= 80 ->
    N;
to_npan(NPAN) when erlang:bit_size(NPAN) =:= 80 ->
    NPAN;
to_npan(Other) ->
    Other.

-spec(to_1npan/1 :: (NPAN :: binary()) -> binary()).
to_1npan(<<"011", N/binary>>) ->
    to_1npan(N);
to_1npan(<<$+, $1, N/bitstring>>) when erlang:bit_size(N) =:= 80 ->
    <<$1, N/bitstring>>;
to_1npan(<<$1, N/bitstring>>=NPAN1) when erlang:bit_size(N) =:= 80 ->
    NPAN1;
to_1npan(NPAN) when erlang:bit_size(NPAN) =:= 80 ->
    <<$1, NPAN/bitstring>>;
to_1npan(Other) ->
    Other.

-spec(to_integer/1 :: (X :: string() | binary() | integer() | float() | atom()) -> integer()).
to_integer(X) when is_float(X) ->
    round(X);
to_integer(X) when is_binary(X) ->
    to_integer(binary_to_list(X));
to_integer(X) when is_list(X) ->
    try
	list_to_integer(X)
    catch
	error:badarg -> round(list_to_float(X))
    end;
to_integer(X) when is_integer(X) ->
    X.

-spec(to_float/1 :: (X :: string() | binary() | integer() | float()) -> float()).
to_float(X) when is_binary(X) ->
    to_float(binary_to_list(X));
to_float(X) when is_list(X) ->
    try
	list_to_float(X)
    catch
	error:badarg -> list_to_integer(X)*1.0 %% "500" -> 500.0
    end;
to_float(X) when is_integer(X) ->
    X * 1.0;
to_float(X) when is_float(X) ->
    X.

-spec(to_list/1 :: (X :: atom() | list() | binary() | integer() | float()) -> list()).
to_list(X) when is_float(X) ->
    mochinum:digits(X);
to_list(X) when is_integer(X) ->
    integer_to_list(X);
to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.

%% Known limitations:
%%   Converting [256 | _], lists with integers > 255
-spec(to_binary/1 :: (X :: atom() | list(0..255) | binary() | integer() | float()) -> binary()).
to_binary(X) when is_float(X) ->
    to_binary(mochinum:digits(X));
to_binary(X) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) ->
    iolist_to_binary(X);
to_binary(X) when is_binary(X) ->
    X.

%% the safer version, won't let you leak atoms
-spec(to_atom/1 :: (X :: term()) -> atom()).
to_atom(X) when is_list(X) ->
    list_to_existing_atom(X);
to_atom(X) ->
    to_atom(to_list(X)).

%% only if you're really sure you want this
-spec(to_atom/2 :: (X :: term(), true) -> atom()).
to_atom(X, true) when is_list(X) ->
    list_to_atom(X);
to_atom(X, true) ->
    to_atom(to_list(X), true).

-spec(to_boolean/1 :: (X :: term()) -> boolean()).
to_boolean(<<"true">>) -> true;
to_boolean("true") -> true;
to_boolean(true) -> true;
to_boolean(<<"false">>) -> false;
to_boolean("false") -> false;
to_boolean(false) -> false.

-spec(is_true/1 :: (X :: term()) -> boolean()).
is_true(<<"true">>) -> true;
is_true("true") -> true;
is_true(true) -> true;
is_true(_) -> false.

-spec(is_false/1 :: (X :: term()) -> boolean()).
is_false(<<"false">>) -> true;
is_false("false") -> true;
is_false(false) -> true;
is_false(_) -> false.

-spec(binary_to_lower/1 :: (B :: binary()) -> binary()).
binary_to_lower(B) when is_binary(B) ->
    to_binary(string:to_lower(to_list(B))).

-spec(a1hash/3 :: (User :: binary() | list(), Realm :: binary() | list(), Password :: binary() | list()) -> string()).
a1hash(User, Realm, Password) ->
    to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).

%% found via trapexit
-spec(floor/1 :: (X :: integer() | float()) -> integer()).
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T =:= 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).

%% found via trapexit
-spec(ceiling/1 :: (X :: integer() | float()) -> integer()).
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T =:= 0 of
        true -> T;
        false -> T + 1
    end.

%% returns current seconds
-spec(current_tstamp/0 :: () -> non_neg_integer()).
current_tstamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% fetch and cache the whistle version from the VERSION file in whistle's root folder
-spec(whistle_version/0 :: () -> binary()).
whistle_version() ->
    case wh_cache:fetch({whistle_util, whistle_version}) of
	{ok, V}    ->  V;
	{error, _} ->
	    wh_cache:store({whistle_util, whistle_version}, whistle_version(<<"/opt/whistle/whistle/VERSION">>)),
	    whistle_version()
    end.

-spec(whistle_version/1 :: (binary()) -> binary()).
whistle_version(FileName) ->
    case file:open(FileName, [read]) of
	{ok, Device} -> case io:get_line(Device, "") of
			    eof  -> file:close(Device), <<"not available">>;
			    Line -> file:close(Device), list_to_binary(string:strip(Line, right, $\n))
			end;
	_ ->  <<"not available">>
    end.

%% there are 86400 seconds in a day
%% there are 62167219200 seconds between Jan 1, 0 and Jan 1, 1970
-define(UNIX_EPOCH_AS_GREG_SECONDS, 62167219200).

-spec(gregorian_seconds_to_unix_seconds/1 :: (GregorianSeconds :: integer() | string() | binary()) -> non_neg_integer()).
gregorian_seconds_to_unix_seconds(GregorianSeconds) ->
    to_integer(GregorianSeconds) - ?UNIX_EPOCH_AS_GREG_SECONDS.

-spec(unix_seconds_to_gregorian_seconds/1 :: (UnixSeconds :: integer() | string() | binary()) -> non_neg_integer()).
unix_seconds_to_gregorian_seconds(UnixSeconds) ->
    to_integer(UnixSeconds) + ?UNIX_EPOCH_AS_GREG_SECONDS.

-spec(microseconds_to_seconds/1 :: (Microseconds :: integer() | string() | binary()) -> non_neg_integer()).
microseconds_to_seconds(Microseconds) ->
    erlang:trunc(to_integer(Microseconds) * math:pow(10, -6)).

%% PROPER TESTING
prop_to_integer() ->
    ?FORALL({F, I}, {float(), integer()},
	    begin
		Is = [ Fun(N) || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
		lists:all(fun(N) -> erlang:is_integer(to_integer(N)) end, Is)
	    end).

prop_to_float() ->
    ?FORALL({F, I}, {float(), integer()},
	    begin
		Fs = [ Fun(N) || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
		lists:all(fun(N) -> erlang:is_float(to_float(N)) end, Fs)
	    end).

prop_to_list() ->
    ?FORALL({A, L, B, I, F}, {atom(), list(), binary(), integer(), float()},
	    lists:all(fun(X) -> is_list(to_list(X)) end, [A, L, B, I, F])).

%-type iolist() :: maybe_improper_list(char() | binary() | iolist(), binary() | []).
prop_to_binary() ->
    ?FORALL({A, L, B, I, F, IO}, {atom(), list(range(0,255)), binary(), integer(), float(), iolist()},
	    lists:all(fun(X) -> is_binary(to_binary(X)) end, [A, L, B, I, F, IO])).

prop_iolist_t() ->
    ?FORALL(IO, iolist(), is_binary(iolist_to_binary(IO))).

%% (AAABBBCCCC, 1AAABBBCCCC) -> AAABBBCCCCCC.
prop_to_npan() ->
    ?FORALL(Number, range(1000000000,19999999999),
	    begin
		BinNum = to_binary(Number),
		NPAN = to_npan(BinNum),
		case byte_size(BinNum) of
		    11 -> BinNum =:= <<"1", NPAN/binary>>;
		    _ -> NPAN =:= BinNum
		end
	    end).

%% (AAABBBCCCC, 1AAABBBCCCC) -> 1AAABBBCCCCCC.
prop_to_1npan() ->
    ?FORALL(Number, range(1000000000,19999999999),
	    begin
		BinNum = to_binary(Number),
		OneNPAN = to_1npan(BinNum),
		case byte_size(BinNum) of
		    11 -> OneNPAN =:= BinNum;
		    _ -> OneNPAN =:= <<"1", BinNum/binary>>
		end
	    end).

%% (AAABBBCCCC, 1AAABBBCCCC) -> +1AAABBBCCCCCC.
prop_to_e164() ->
    ?FORALL(Number, range(1000000000,19999999999),
	    begin
		BinNum = to_binary(Number),
		E164 = to_e164(BinNum),
		case byte_size(BinNum) of
		    11 -> E164 =:= <<$+, BinNum/binary>>;
		    10 -> E164 =:= <<$+, $1, BinNum/binary>>;
		    _ -> E164 =:= BinNum
		end
	    end).

%% EUNIT TESTING

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

%-export([to_integer/1, to_float/1, to_hex/1, to_list/1, to_binary/1]).
%-export([a1hash/3, floor/1, ceiling/1]).

to_e164_test() ->
    Ns = [<<"+11234567890">>, <<"11234567890">>, <<"1234567890">>],
    Ans = <<"+11234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_e164(N), Ans) end, Ns).

to_npan_test() ->
    Ns = [<<"+11234567890">>, <<"11234567890">>, <<"1234567890">>],
    Ans = <<"1234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_npan(N), Ans) end, Ns).

to_1npan_test() ->
    Ns = [<<"+11234567890">>, <<"11234567890">>, <<"1234567890">>],
    Ans = <<"11234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_1npan(N), Ans) end, Ns).

greg_secs_to_unix_secs_test() ->
    GregSecs = current_tstamp(),
    ?assertEqual(GregSecs - ?UNIX_EPOCH_AS_GREG_SECONDS, gregorian_seconds_to_unix_seconds(GregSecs)).

unix_secs_to_greg_secs_test() ->
    UnixSecs = 1000000000,
    ?assertEqual(UnixSecs + ?UNIX_EPOCH_AS_GREG_SECONDS, unix_seconds_to_gregorian_seconds(UnixSecs)).

microsecs_to_secs_test() ->
    Microsecs = 1310157838405890,
    Secs = 1310157838,
    ?assertEqual(Secs, microseconds_to_seconds(Microsecs)).

no_whistle_version_test() ->
    ?assertEqual(<<"not available">>, whistle_version(<<"/path/to/nonexistent/file">>)).

-endif.
