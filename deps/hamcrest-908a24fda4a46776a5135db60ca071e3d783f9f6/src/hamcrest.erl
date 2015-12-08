%% -----------------------------------------------------------------------------
%%
%% Hamcrest Erlang.
%%
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2010 Tim Watson.
%% @doc Hamcrest API
%% @reference See <a href="http://code.google.com/p/hamcrest/">Hamcrest</a>
%% for more information.
%% -----------------------------------------------------------------------------

-module(hamcrest).

-include("hamcrest_internal.hrl").

-export_type([matchspec/0]).

-export([is_matcher/1,
         match/2,
         match/3,
         check/2,
         assert_that/2,
         assert_that/3,
         describe/2,
         heckle/2]).

%%%============================================================================
%%% Types
%%%============================================================================

-opaque matchspec() :: #'hamcrest.matchspec'{}.

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Returns `true' if the specified term is a valid hamcrest matcher,
%% otherwise `false'.
-spec(is_matcher/1 :: (any()) -> boolean()).
is_matcher(Something) ->
  erlang:is_record(Something, 'hamcrest.matchspec').

-spec(match/2 :: (term(), matchspec()) -> boolean()).
match(Value, MatchSpec) ->
  match(Value, MatchSpec, fun() -> ok end).

-spec(match/3 :: (term(), matchspec(),
                  fun(() -> any())) -> boolean()).
match(Value, MatchSpec, RunAfter) ->
  (catch assert_that(Value, MatchSpec, RunAfter)) == true.

-spec(assert_that/3 :: (term(), matchspec(),
                        fun(() -> any())) -> 'true' | no_return()).
assert_that(Value, MatchSpec, RunAfter) when is_function(RunAfter, 0) ->
  try assert_that(Value, MatchSpec)
  after RunAfter()
  end.

-spec(assert_that/2 :: (term(), matchspec()) -> 'true' | no_return()).
assert_that(Value, MatchSpec) ->
  case check(Value, MatchSpec) of
    {assertion_failed, _}=Failure ->
      erlang:error(Failure);
    true ->
      true;
    Other ->
      exit({what_the, Other})
  end.

-spec(check/2 :: (term(), matchspec()) -> 'true' | {assertion_failed, term()}).
check(Value, #'hamcrest.matchspec'{ matcher=MatchFunc }=MatchSpec) ->
  heckle(MatchSpec, Value),
  try MatchFunc(Value) of
    true -> true;
    {assertion_failed, _} ->
      {assertion_failed, describe(MatchSpec, Value)};
    {assertion_override, _}=Err ->
      {assertion_failed, describe(MatchSpec, Err)};
    false ->
      {assertion_failed, describe(MatchSpec, Value)};
    What ->
      {assertion_failed, What}
  catch
    Class:Reason ->
      {assertion_failed, describe(MatchSpec, {Class, Reason})}
  end.

-spec(heckle/2 :: (matchspec(), any()) -> any()).
heckle(MatchSpec, Actual) ->
  case application:get_env(hamcrest, heckle) of
    {ok, [M,F,A]} ->
      Argv = [MatchSpec, Actual|A],
      apply(M, F, Argv);
    _ ->
      ok
  end.

describe(Ms, Actual) when is_function(Actual, 0) ->
    describe(Ms, erlang:fun_info(Actual));
describe(#'hamcrest.matchspec'{ desc=Desc, expected=Expected }, Actual) ->
  [{expected, Expected},
   {actual, Actual},
   {matcher, Desc}].
