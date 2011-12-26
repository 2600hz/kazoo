-module(wh_util).

-export([pad_binary/3, join_binary/1, join_binary/2]).
-export([call_response/3, call_response/4, call_response/5]).
-export([to_e164/1, to_npan/1, to_1npan/1]).
-export([is_e164/1, is_npan/1, is_1npan/1]).
-export([to_integer/1, to_integer/2, to_float/1, to_float/2, to_number/1
         ,to_hex/1, to_list/1, to_binary/1
         ,to_atom/1, to_atom/2]).
-export([to_boolean/1, is_true/1, is_false/1, is_empty/1, is_proplist/1]).
-export([binary_to_lower/1, binary_to_upper/1, binary_join/2]).
-export([a1hash/3, floor/1, ceiling/1]).
-export([current_tstamp/0, ensure_started/1]).
-export([gregorian_seconds_to_unix_seconds/1, unix_seconds_to_gregorian_seconds/1
         ,pretty_print_datetime/1
        ]).
-export([microseconds_to_seconds/1, put_callid/1, get_event_type/1]).
-export([whistle_version/0, write_pid/1]).
-export([is_ipv4/1, is_ipv6/1]).
-export([get_hostname/0]).
-export([get_transfer_state/1, get_transfer_state/2]).

-include_lib("kernel/include/inet.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(WHISTLE_VERSION_CACHE_KEY, {?MODULE, whistle_version}).

-spec get_hostname/0 :: () -> string().
get_hostname() ->
    {ok, Host} = inet:gethostname(),
    {ok, #hostent{h_name=Hostname}} = inet:gethostbyname(Host),
    Hostname.

-spec pad_binary/3 :: (binary(), non_neg_integer(), binary()) -> binary().
pad_binary(Bin, Size, Value) when size(Bin) < Size ->
    pad_binary(<<Bin/binary, Value/binary>>, Size, Value);
pad_binary(Bin, _, _) ->
    Bin.

-spec join_binary/1 :: ([binary(),...]) -> binary().
-spec join_binary/2 :: ([binary(),...], binary()) -> binary().

join_binary(Bins) ->
    join_binary(Bins, <<", ">>).

join_binary([Bin], _) ->
    Bin;
join_binary([Bin|Rest], Sep) -> 
    <<Bin/binary, Sep/binary, (join_binary(Rest, Sep))/binary>>.

-spec call_response/3 :: (ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
-spec call_response/4 :: (ne_binary(), ne_binary(), ne_binary(), 'undefined' | binary()) -> 'ok'.
-spec call_response/5 :: (ne_binary(), ne_binary(), ne_binary(), 'undefined' | binary(), 'undefined' | binary()) -> 'ok'.
call_response(CallId, CtrlQ, Code) ->
    call_response(CallId, CtrlQ, Code, <<>>).
call_response(CallId, CtrlQ, Code, undefined) ->
    call_response(CallId, CtrlQ, Code, <<>>);
call_response(CallId, CtrlQ, Code, Cause) ->
    call_response(CallId, CtrlQ, Code, Cause, undefined).
call_response(CallId, CtrlQ, Code, Cause, Media) ->
    Respond = wh_json:from_list([{<<"Application-Name">>, <<"respond">>}
                                 ,{<<"Response-Code">>, Code}
                                 ,{<<"Response-Message">>, Cause}
                                 ,{<<"Call-ID">>, CallId}
                                ]),
    call_response1(CallId, CtrlQ, Media, Respond).

call_response1(CallId, CtrlQ, undefined, Respond) ->
    call_response1(CallId, CtrlQ, [Respond]);
call_response1(CallId, CtrlQ, Media, Respond) ->
    call_response1(CallId, CtrlQ, [Respond
                                   ,wh_json:from_list([{<<"Application-Name">>, <<"play">>}
                                                       ,{<<"Media-Name">>, Media}
                                                       ,{<<"Call-ID">>, CallId}
                                                      ])
                                   ,wh_json:from_list([{<<"Application-Name">>, <<"progress">>}
                                                       ,{<<"Call-ID">>, CallId}
                                                      ])
                                  ]).

call_response1(CallId, CtrlQ, Commands) ->
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Call-ID">>, CallId}
               ,{<<"Commands">>, Commands}
               | wh_api:default_headers(<<>>, <<"call">>, <<"command">>, <<"call_response">>, <<"0.1.0">>)],
    {ok, Payload} = wapi_dialplan:queue(Command),
    wapi_dialplan:publish_action(CtrlQ, Payload).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an JSON Object extracts the Call-ID into the processes
%% dictionary, failing that the Msg-ID and finally a generic
%% @end
%%--------------------------------------------------------------------
-spec put_callid/1 :: (JObj) -> binary() | 'undefined' when
      JObj :: json_object().
put_callid(JObj) ->
    erlang:put(callid, wh_json:get_value(<<"Call-ID">>, JObj, wh_json:get_value(<<"Msg-ID">>, JObj, <<"0000000000">>))).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an API JSON object extract the category and name into a
%% tuple for easy processing
%% @end
%%--------------------------------------------------------------------
-spec get_event_type/1 :: (JObj) -> {binary(), binary()} when
      JObj :: json_object().
get_event_type(JObj) ->
    { wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj) }.

%% must be a term that can be changed to a list
-spec to_hex/1 :: (S) -> string() when
      S :: term().
to_hex(S) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- to_list(S)])).

-spec is_e164/1 :: (ne_binary()) -> boolean().
-spec is_npan/1 :: (ne_binary()) -> boolean().
-spec is_1npan/1 :: (ne_binary()) -> boolean().

is_e164(DID) ->
    re:run(DID, <<"^\\+1\\d{10}$">>) =/= nomatch.

is_npan(DID) ->
    re:run(DID, <<"^\\d{10}$">>) =/= nomatch.

is_1npan(DID) ->
    re:run(DID, <<"^1\\d{10}$">>) =/= nomatch.

%% +18001234567 -> +18001234567
-spec to_e164/1 :: (ne_binary()) -> ne_binary().
to_e164(<<"011", N/binary>>) ->
    to_e164(N);
to_e164(<<"+1", _/binary>> = E164) when erlang:byte_size(E164) =:= 12 ->
    E164;
%% 18001234567 -> +18001234567
to_e164(<<$1, _/binary>> = NPAN1) when erlang:byte_size(NPAN1) =:= 11 ->
    << $+, NPAN1/binary>>;
%% 8001234567 -> +18001234567
to_e164(NPAN) when erlang:byte_size(NPAN) =:= 10 ->
    <<$+, $1, NPAN/binary>>;
to_e164(Other) ->
    Other.

%% end up with 8001234567 from 1NPAN and E.164
-spec to_npan/1 :: (ne_binary()) -> ne_binary().
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

-spec to_1npan/1 :: (NPAN) -> binary() when
      NPAN :: binary().
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

-spec to_integer/1 :: (string() | binary() | integer() | float()) -> integer().
-spec to_integer/2 :: (string() | binary() | integer() | float(), 'strict' | 'notstrict') -> integer().
to_integer(X) ->
    to_integer(X, notstrict).

to_integer(X, strict) when is_float(X) ->
    error(badarg);
to_integer(X, notstrict) when is_float(X) ->
    round(X);
to_integer(X, S) when is_binary(X) ->
    to_integer(binary_to_list(X), S);
to_integer(X, S) when is_list(X) ->
    try
        list_to_integer(X)
    catch
        error:badarg when S =:= notstrict ->
            round(list_to_float(X))
    end;
to_integer(X, _) when is_integer(X) ->
    X.

-spec to_float/1 :: (string() | binary() | integer() | float()) -> float().
-spec to_float/2 :: (string() | binary() | integer() | float(), 'strict' | 'notstrict') -> float().
to_float(X) ->
    to_float(X, notstrict).

to_float(X, S) when is_binary(X) ->
    to_float(binary_to_list(X), S);
to_float(X, S) when is_list(X) ->
    try
        list_to_float(X)
    catch
        error:badarg when S =:= notstrict -> list_to_integer(X)*1.0 %% "500" -> 500.0
    end;
to_float(X, strict) when is_integer(X) ->
    error(badarg);
to_float(X, notstrict) when is_integer(X) ->
    X * 1.0;
to_float(X, _) when is_float(X) ->
    X.

-spec to_number/1 :: (binary() | string() | number()) -> number().
to_number(X) when is_number(X) ->
    X;
to_number(X) when is_binary(X) ->
    to_number(to_list(X));
to_number(X) when is_list(X) ->
    try list_to_integer(X) of
        Int -> Int
    catch
        error:badarg ->
            list_to_float(X)
    end.

-spec to_list/1 :: (X) -> list() when
      X :: atom() | list() | binary() | integer() | float().
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
-spec to_binary/1 :: (X) -> binary() when
      X :: atom() | list(0..255) | binary() | integer() | float().
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
-spec to_atom/1 :: (X) -> atom() when
      X :: atom() | list() | binary() | integer() | float().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_existing_atom(X);
to_atom(X) -> to_atom(to_list(X)).

%% only if you're really sure you want this
-spec to_atom/2 :: (X, true) -> atom() when
      X :: atom() | list() | binary() | integer() | float().
to_atom(X, _) when is_atom(X) -> X;
to_atom(X, true) when is_list(X) -> list_to_atom(X);
to_atom(X, true) -> to_atom(to_list(X), true).

-spec to_boolean/1 :: (X) -> boolean() when
      X :: binary() | string() | atom().
to_boolean(<<"true">>) -> true;
to_boolean("true") -> true;
to_boolean(true) -> true;
to_boolean(<<"false">>) -> false;
to_boolean("false") -> false;
to_boolean(false) -> false.

-spec is_true/1 :: (X) -> boolean() when
      X :: binary() | string() | atom().
is_true(<<"true">>) -> true;
is_true("true") -> true;
is_true(true) -> true;
is_true(_) -> false.

-spec is_false/1 :: (X) -> boolean() when
      X :: binary() | string() | atom().
is_false(<<"false">>) -> true;
is_false("false") -> true;
is_false(false) -> true;
is_false(_) -> false.

-spec is_empty/1 :: (term()) -> boolean().
is_empty(0) -> true;
is_empty([]) -> true;
is_empty("0") -> true;
is_empty("false") -> true;
is_empty("NULL") -> true;
is_empty(<<>>) -> true;
is_empty(<<"0">>) -> true;
is_empty(<<"false">>) -> true;
is_empty(<<"NULL">>) -> true;
is_empty(false) -> true;
is_empty(undefined) -> true;
is_empty(?EMPTY_JSON_OBJECT) -> true;
is_empty(_) -> false.

-spec is_proplist/1 :: (Term) -> boolean() when
      Term :: term().
is_proplist(Term) when is_list(Term) ->
    lists:all(fun({_,_}) -> true; (_) -> false end, Term);
is_proplist(_) ->
    false.

-spec binary_to_lower/1 :: (B) -> undefined | binary() when
      B :: undefined | binary().
binary_to_lower(undefined) ->
    undefined;
binary_to_lower(Bin) when is_binary(Bin) ->
    << <<(binary_to_lower_char(B))>> || <<B>> <= Bin>>.

-spec binary_to_lower_char/1 :: (C) -> char() when
      C :: char().
binary_to_lower_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
%% Converts latin capital letters to lowercase, skipping 16#D7 (extended ascii 215) "multiplication sign: x"
binary_to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
binary_to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
binary_to_lower_char(C) -> C.

-spec binary_to_upper/1 :: (B) -> binary() when
      B :: binary().
binary_to_upper(Bin) when is_binary(Bin) ->
    << <<(binary_to_upper_char(B))>> || <<B>> <= Bin>>.

-spec binary_to_upper_char/1 :: (C) -> char() when
      C :: char().
binary_to_upper_char(C) when is_integer(C), $a =< C, C =< $z -> C - 32;
binary_to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 -> C - 32;
binary_to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE -> C - 32;
binary_to_upper_char(C) -> C.

-spec binary_join/2 :: ([ne_binary(),...], binary()) -> ne_binary().
binary_join([H|T], Glue) when is_binary(Glue) ->
    list_to_binary([H, [ [Glue, I] || I <- T]]).

-spec a1hash/3 :: (User, Realm, Password) -> string() when
      User :: binary() | list(),
      Realm :: binary() | list(),
      Password :: binary() | list().
a1hash(User, Realm, Password) ->
    to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).

%% found via trapexit
-spec floor/1 :: (X) -> integer() when
      X :: integer() | float().
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T =:= 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).

%% found via trapexit
-spec ceiling/1 :: (X) -> integer() when
      X :: integer() | float().
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T =:= 0 of
        true -> T;
        false -> T + 1
    end.

%% returns current seconds
-spec current_tstamp/0 :: () -> non_neg_integer().
current_tstamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% fetch and cache the whistle version from the VERSION file in whistle's root folder
-spec whistle_version/0 :: () -> binary().
whistle_version() ->
    case wh_cache:fetch(?WHISTLE_VERSION_CACHE_KEY) of
        {ok, Version} ->  Version;
        {error, _} ->
            VersionFile = filename:join([code:lib_dir(whistle), "..", "..", "VERSION"]),
            whistle_version(VersionFile)
    end.

-spec whistle_version/1 :: (binary() | string()) -> binary().
whistle_version(FileName) ->
    case file:consult(FileName) of
        {ok, [Version]} ->
            wh_cache:store(?WHISTLE_VERSION_CACHE_KEY, Version),
            Version;
        _ ->
            Version = <<"not available">>,
            wh_cache:store(?WHISTLE_VERSION_CACHE_KEY, Version),
            Version
    end.

-spec write_pid/1 :: (FileName) -> ok | {error, atom()} when
      FileName :: binary() | string().
write_pid(FileName) ->
    file:write_file(FileName, io_lib:format("~s", [os:getpid()]), [write, binary]).

-spec ensure_started/1 :: (App) -> 'ok' when
      App :: atom().
ensure_started(App) when is_atom(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% there are 86400 seconds in a day
%% there are 62167219200 seconds between Jan 1, 0000 and Jan 1, 1970
-define(UNIX_EPOCH_AS_GREG_SECONDS, 62167219200).

-spec gregorian_seconds_to_unix_seconds/1 :: (integer() | string() | binary()) -> non_neg_integer().
gregorian_seconds_to_unix_seconds(GregorianSeconds) ->
    to_integer(GregorianSeconds) - ?UNIX_EPOCH_AS_GREG_SECONDS.

-spec unix_seconds_to_gregorian_seconds/1 :: (integer() | string() | binary()) -> non_neg_integer().
unix_seconds_to_gregorian_seconds(UnixSeconds) ->
    to_integer(UnixSeconds) + ?UNIX_EPOCH_AS_GREG_SECONDS.

-spec pretty_print_datetime/1 :: (wh_datetime()) -> ne_binary().
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w_~2..0w-~2..0w-~2..0w",
                                   [Y, Mo, D, H, Mi, S])).

-spec microseconds_to_seconds/1 :: (integer() | string() | binary()) -> non_neg_integer().
microseconds_to_seconds(Microseconds) ->
    erlang:trunc(to_integer(Microseconds) * math:pow(10, -6)).

-spec is_ipv4/1 :: (nonempty_string() | ne_binary()) -> boolean().
is_ipv4(Address) when is_binary(Address) ->
    is_ipv4(to_list(Address));
is_ipv4(Address) when is_list(Address) ->
    case inet_parse:ipv4_address(Address) of
        {ok, _} ->
            true;
        {error, _} -> false
    end.

-spec is_ipv6/1 :: (nonempty_string() | ne_binary()) -> boolean().
is_ipv6(Address) when is_binary(Address) ->
    is_ipv6(to_list(Address));
is_ipv6(Address) when is_list(Address) ->
    case inet_parse:ipv6_address(Address) of
        {ok, _} -> true;
        {error, _} -> false
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_transfer_state/1 :: (json_object()) -> undefined | transferer | transferee.
-spec get_transfer_state/2 :: ({ne_binary(), ne_binary()}, json_object()) -> undefined | transferer | transferee.
-spec do_get_transfer_state/2 :: (ne_binary() | undefined,  json_object()) ->  undefined | transferer | transferee.

get_transfer_state(JObj) ->
    get_transfer_state(get_event_type(JObj), JObj).

get_transfer_state({<<"call_event">>, <<"CHANNEL_DESTROY">>}, JObj) ->
    do_get_transfer_state(<<"CHANNEL_DESTROY">>, JObj);
get_transfer_state({<<"call_event">>, <<"CHANNEL_HANGUP">>}, JObj) ->
    do_get_transfer_state(<<"CHANNEL_HANGUP">>, JObj);
get_transfer_state({<<"call_event">>, <<"CHANNEL_UNBRIDGE">>}, JObj) ->
    do_get_transfer_state(<<"CHANNEL_UNBRIDGE">>, JObj);
get_transfer_state(_, _) ->
    undefined.

do_get_transfer_state(<<"CHANNEL_UNBRIDGE">>, JObj) ->
    Timestamp = wh_json:get_value(<<"Timestamp">>, JObj, <<>>),
    Epoch = binary:part(wh_util:pad_binary(Timestamp, 10, <<"0">>), 0, 10),
    Transfer = wh_json:get_value([<<"Transfer-History">>, Epoch], JObj),
    Disposition = wh_json:get_value(<<"Disposition">>, JObj),
    case {Disposition, Transfer} of
        %% caller preforms a blind transfer
        {<<"BLIND_TRANSFER">>, undefined} ->
            ?LOG("channel was unbridged as a result of a blind transfer"),
            transferer;
        %% callee preforms a attended transfer (on C-leg)
        {<<"ATTENDED_TRANSFER">>, undefined} ->
            ?LOG("channel was unbridged as a result of an attended transfer, acquire control"),
            transferee;
        %% caller preforms a attended transfer
        %% caller preforms a partial attended
        {<<"ANSWER">>, undefined} ->
            %% to be sure check if it was during a transfer, may not be necessary...
            case wh_json:get_value(<<"Hangup-Cause">>, JObj) of
                undefined ->
                    ?LOG("channel was unbridged as a result of a transfer"),
                    transferer;
                _Else ->
                    undefined
            end;
        %% just a catch for undefined Transfer History Item
        %% IE: This unbridge was NOT part of the transfer history,
        %%     otherwise it WAS and the next clause will handle it.
        {_, undefined} ->
            undefined;
        %% callee preforms a blind transfer
        %% callee preforms a partial attended
        %% callee preforms a attended transfer
        {_, _} ->
            ?LOG("channel was unbridged as a result of a transfer"),
            transferer
    end;    
do_get_transfer_state(_, JObj) ->
    case wh_json:get_value(<<"Disposition">>, JObj) of
        %% caller preforms a blind transfer
        <<"BLIND_TRANSFER">> ->
            ?LOG("channel was hungup as a result of a blind transfer"),                            
            transferer;
        %% callee preforms partial attended
        %% callee preforms attended transfer
        <<"ATTENDED_TRANSFER">> ->
            ?LOG("channel was hungup as a result of an attended transfer, acquire control"),
            transferee;
        %% caller preforms a attended transfer
        %% caller preforms a partial attended
        <<"ANSWER">> ->
            %% to be sure check if it was during a transfer, may not be necessary...
            case wh_json:get_value(<<"Hangup-Cause">>, JObj) of
                undefined ->
                    ?LOG("channel was hungup as a result of a transfer"),
                    trasferer;
                _Else ->
                    undefined
            end;
        %% missing events:
        %% callee preforms blind transfer
        _Else ->
            undefined
    end.

%% PROPER TESTING
prop_to_integer() ->
    ?FORALL({F, I}, {float(), integer()},
            begin
                Is = [ [Fun(N), N] || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
                lists:all(fun([FN, N]) -> erlang:is_integer(to_integer(N)) andalso erlang:is_integer(to_integer(FN)) end, Is)
            end).

prop_to_number() ->
    ?FORALL({F, I}, {float(), integer()},
            begin
                Is = [ [Fun(N), N] || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
                lists:all(fun([FN, N]) -> erlang:is_number(to_number(N)) andalso erlang:is_number(to_number(FN)) end, Is)
            end).

prop_to_float() ->
    ?FORALL({F, I}, {float(), integer()},
            begin
                Fs = [ [Fun(N), N] || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
                lists:all(fun([FN, N]) -> erlang:is_float(to_float(N)) andalso erlang:is_float(to_float(FN)) end, Fs)
            end).

prop_to_list() ->
    ?FORALL({A, L, B, I, F}, {atom(), list(), binary(), integer(), float()},
            lists:all(fun(X) -> is_list(to_list(X)) end, [A, L, B, I, F])).

%-type iolist() :: maybe_improper_list(char() | binary() | iolist(), binary() | []).
prop_to_binary() ->
    ?FORALL({A, L, B, I, F, IO}, {atom(), list(range(0,255)), binary(), integer(), float(), iolist()},
            lists:all(fun(X) -> is_binary(to_binary(X)) end, [A, L, B, I, F, IO])).

prop_iolist_t() ->
    ?FORALL(IO, iolist(), is_binary(to_binary(IO))).

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

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {timeout, 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{max_shrinks, 0}]))
      ]}}.

pad_binary_test() ->
    ?assertEqual(<<"1234500000">>, pad_binary(<<"12345">>, 10, <<"0">>)).

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

binary_join_test() ->
    ?assertEqual(<<"foo">>, binary_join([<<"foo">>], <<", ">>)),
    ?assertEqual(<<"foo, bar">>, binary_join([<<"foo">>, <<"bar">>], <<", ">>)),
    ?assertEqual(<<"foo, bar, baz">>, binary_join([<<"foo">>, <<"bar">>, <<"baz">>], <<", ">>)).

-endif.
