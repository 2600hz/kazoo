-module(cf_util).

-export([alpha_to_dialpad/1, ignore_early_media/1]).
-export([call_info_to_string/1]).

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

alpha_to_dialpad_test() ->
    ?assertEqual(<<"222">>, alpha_to_dialpad(<<"abc">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"behknqux">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"BeHkNqUx">>)),
    ?assertEqual(<<"23456789">>, alpha_to_dialpad(<<"1BeH@k(N$q-u+x=">>)).

-endif.
