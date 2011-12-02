-module(cf_util).

-export([alpha_to_dialpad/1, ignore_early_media/1]).
-export([call_info_to_string/1]).
-export([lookup_callflow/1, lookup_callflow/2]).

-include("callflow.hrl").

-spec alpha_to_dialpad/1 :: (Value) -> binary() when
      Value :: binary().
alpha_to_dialpad(Value) ->
    << <<(dialpad_digit(C))>> || <<C>> <= strip_nonalpha(wh_util:binary_to_lower(Value))>>.

-spec strip_nonalpha/1 :: (Value) -> binary() when
      Value :: binary().
strip_nonalpha(Value) ->
    re:replace(Value, <<"[^[:alpha:]]">>, <<>>, [{return,binary}, global]).

-spec dialpad_digit/1 :: (Char) -> 50..57 when
      Char :: 97..122.
dialpad_digit(ABC) when ABC =:= $a orelse ABC =:= $b orelse ABC =:= $c -> $2;
dialpad_digit(DEF) when DEF =:= $d orelse DEF =:= $e orelse DEF =:= $f -> $3;
dialpad_digit(GHI) when GHI =:= $g orelse GHI =:= $h orelse GHI =:= $i -> $4;
dialpad_digit(JKL) when JKL =:= $j orelse JKL =:= $k orelse JKL =:= $l -> $5;
dialpad_digit(MNO) when MNO =:= $m orelse MNO =:= $n orelse MNO =:= $o -> $6;
dialpad_digit(PQRS) when PQRS =:= $p orelse PQRS =:= $q orelse PQRS =:= $r orelse PQRS =:= $s -> $7;
dialpad_digit(TUV) when TUV =:= $t orelse TUV =:= $u orelse TUV =:= $v -> $8;
dialpad_digit(WXYZ) when WXYZ =:= $w orelse WXYZ =:= $x orelse WXYZ =:= $y orelse WXYZ =:= $z -> $9.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if we should ignore early media
%% @end
%%--------------------------------------------------------------------
-spec ignore_early_media/1 :: (Endpoints) -> binary() when
      Endpoints :: json_objects().
ignore_early_media(Endpoints) ->
    Ignore = lists:foldr(fun(Endpoint, Acc) ->
                                 wh_json:is_true(<<"Ignore-Early-Media">>, Endpoint)
                                     or Acc
                         end, false, Endpoints),
    wh_util:to_binary(Ignore).

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% Convert the call record to a string
%% @end
%%-----------------------------------------------------------------------------
-spec call_info_to_string/1 :: (#cf_call{}) -> io_lib:chars().
call_info_to_string(#cf_call{account_id=AccountId, flow_id=FlowId, call_id=CallId, cid_name=CIDName, cid_number=CIDNumber
                             ,request=Request, from=From, to=To, inception=Inception, authorizing_id=AuthorizingId }) ->
    Format = ["Call-ID: ~s~n"
              ,"Callflow: ~s~n"
              ,"Account ID: ~s~n"
              ,"Request: ~s~n"
              ,"To: ~s~n"
              ,"From: ~s~n"
              ,"CID: ~s ~s~n"
              ,"Innception: ~s~n"
              ,"Authorizing ID: ~s~n"],
    io_lib:format(lists:flatten(Format)
                  ,[CallId, FlowId, AccountId, Request, To, From
                    ,CIDNumber, CIDName, Inception, AuthorizingId]).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% lookup the callflow based on the requested number in the account
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_callflow/1 :: (#cf_call{}) -> {ok, binary(), boolean()} | {error, term()}.
-spec lookup_callflow/2 :: (ne_binary(), ne_binary()) -> {ok, binary(), boolean()} | {error, term()}.

lookup_callflow(#cf_call{request_user=Number, account_id=AccountId}) ->
    lookup_callflow(Number, AccountId).

lookup_callflow(Number, AccountId) ->
    case wh_util:is_empty(Number) of
        true -> {error, invalid_number};
        false ->
            Db = whapps_util:get_db_name(AccountId, encoded),
            do_lookup_callflow(wh_util:to_binary(Number), Db)
    end.

do_lookup_callflow(Number, Db) ->
    ?LOG("searching for callflow in ~s to satisfy '~s'", [Db, Number]),
%%    case wh_cache:fetch({cf_flow, Number, Db}) of
%%	{ok, Flow} ->
%%	    {ok, Flow, Number =:= ?NO_MATCH_CF};
%%	{error, not_found} ->
            Options = [{<<"key">>, Number}, {<<"include_docs">>, true}],
	    case couch_mgr:get_results(Db, ?LIST_BY_NUMBER, Options) of
		{ok, []} when Number =/= ?NO_MATCH_CF ->
                    case lookup_callflow_patterns(Number, Db) of
                        {error, _} ->
                            do_lookup_callflow(?NO_MATCH_CF, Db);
                        {ok, {Flow, Capture}} ->
                            F = wh_json:set_value(<<"capture_group">>, Capture, Flow),
                            wh_cache:store({cf_flow, Number, Db}, F),
                            {ok, F, false}
                    end;
		{ok, []} ->
                    {error, not_found};
		{ok, [{struct, _}=JObj]} ->
                    Flow = wh_json:get_value(<<"doc">>, JObj),
                    wh_cache:store({cf_flow, Number, Db}, Flow),
		    {ok, Flow, Number =:= ?NO_MATCH_CF};
		{ok, [{struct, _}=JObj | _Rest]} ->
		    ?LOG("lookup resulted in more than one result, using the first"),
                    Flow = wh_json:get_value(<<"doc">>, JObj),
                    wh_cache:store({cf_flow, Number, Db}, Flow),
		    {ok, Flow, Number =:= ?NO_MATCH_CF};
                {error, _}=E ->
		    E
	    end.
%%    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_callflow_patterns/2 :: (ne_binary(), ne_binary())
                                    -> {ok, {json_object(), binary()}} | {error, term()}.
lookup_callflow_patterns(Number, Db) ->
    ?LOG("lookup callflow patterns for ~s in ~s", [Number, Db]),
    case couch_mgr:get_results(Db, ?LIST_BY_PATTERN, [{<<"include_docs">>, true}]) of
        {ok, Patterns} ->
            case test_callflow_patterns(Patterns, Number, {undefined, <<>>}) of
                {undefined, <<>>} -> {error, not_found};
                Match -> {ok, Match}
            end;
        {error, _}=E ->
            E
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec test_callflow_patterns/3 :: (json_objects(), ne_binary()
                                  ,{undefined, <<>>} | {json_object(), binary()})
                                  -> {undefined, <<>>} | {json_object(), binary()}.
test_callflow_patterns([], _, Result) ->
    Result;
test_callflow_patterns([Pattern|T], Number, {_, Capture}=Result) ->
    Regex = wh_json:get_value(<<"key">>, Pattern),
    case re:run(Number, Regex) of
        {match, [{Start,End}]} ->
            Match = binary:part(Number, Start, End),
            case binary:part(Number, Start, End) of
                Match when size(Match) > size(Capture) ->
                    F = wh_json:get_value(<<"doc">>, Pattern),
                    test_callflow_patterns(T, Number, {F, Match});
                _ ->
                    test_callflow_patterns(T, Number, Result)
            end;
        {match, CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            case binary:part(Number, Start, End) of
                Match when size(Match) > size(Result) ->
                    F = wh_json:get_value(<<"doc">>, Pattern),
                    test_callflow_patterns(T, Number, {F, Match});
                _ ->
                    test_callflow_patterns(T, Number, Result)
            end;
        _ ->
            test_callflow_patterns(T, Number, Result)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

alpha_to_dialpad_test() ->
    ?assertEqual(<<"222">>, alpha_to_dialpad(<<"abc">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"behknqux">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"BeHkNqUx">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"1BeH@k(N$q-u+x=">>)).

-endif.
