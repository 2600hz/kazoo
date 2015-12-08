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
%% @doc Hamcrest Matchers
%% @reference See <a href="http://code.google.com/p/hamcrest/">Hamcrest</a>
%% for more information.
%% -----------------------------------------------------------------------------

-module(hamcrest_matchers).
-author('Tim Watson <watson.timothy@gmail.com>').

-include("hamcrest_internal.hrl").

-import(hamcrest, [message/4]).

-export([
    all_of/1,
    anything/0,
    any_of/1,
    foreach/1,
    is/1,
    is_true/0,
    is_false/0,
    is_not/1,
    equal_to/1,
    exactly_equal_to/1,
    greater_than/1,
    greater_than_or_equal_to/1,
    less_than/1,
    less_than_or_equal_to/1,
    contains_string/1,
    starts_with/1,
    ends_with/1,
    will_fail/0,
    will_fail/2,
    has_length/1,
    has_same_contents_as/1,
    contains_member/1,
    matches_regex/1,
    match_mfa/3,
    match_mfa/4,
    reverse_match_mfa/3,
    reverse_match_mfa/4,
    isalive/0,
    isdead/0,
    isempty/0,
    check_isempty/1,
    check_member/2]).

%%%============================================================================
%%% Types
%%%============================================================================

-type matchfun(A) :: fun((A) -> boolean()).
-type container_t() :: list() | hc_set() | hc_gb_set().

%%%============================================================================
%%% API
%%%============================================================================

-spec(will_fail/0 :: () -> hamcrest:matchspec()).
will_fail() ->
    %% Matcher :: fun((fun(() -> any())) -> boolean())
    Matcher = fun(F) ->
        try F() of
            _ -> false
        catch _:_ -> true end
    end,
    #'hamcrest.matchspec'{
        matcher     = Matcher,
        desc        = will_fail,
        expected    = {oneof,{exit,error,exception}}
    }.

-spec(will_fail/2 :: (atom(), term()) -> hamcrest:matchspec()).
will_fail(Type, Reason) ->
    %% Matcher :: fun((fun(() -> any())) -> boolean())
    Matcher = fun(F) ->
        try F() of
            _ -> false
        catch Type:Reason -> true end
    end,
    #'hamcrest.matchspec'{
        matcher     = Matcher,
        desc        = will_fail,
        expected    = {Type, Reason}
    }.

-spec(anything/0 :: () -> hamcrest:matchspec()).
anything() ->
    ?MATCHER(fun(_) -> true end, any, anything).

-spec(foreach/1 :: (hamcrest:matchspec()) -> hamcrest:matchspec()).
foreach(M) when is_record(M, 'hamcrest.matchspec') ->
    ?MATCHER(fun(L) -> drop_matches(M, L) == [] end, M,
             {foreach, M#'hamcrest.matchspec'.desc}).

drop_matches(Match, []) ->
    case hamcrest:match([], Match) of
        true -> [];
        _    -> false
    end;
drop_matches(Match, L) ->
    lists:dropwhile(fun(E) -> hamcrest:match(E, Match) end, L).

-spec(any_of/1 :: (list(matchfun(term())))     -> hamcrest:matchspec();
                  (list(hamcrest:matchspec())) -> hamcrest:matchspec()).
any_of(Matchers) when is_list(Matchers) ->
    MatchFun =
    fun(M) when is_function(M) -> M;
     (#'hamcrest.matchspec'{matcher=F}) -> F
    end,
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> lists:member(true,
                                    [ (MatchFun(M))(X) || M <- Matchers ]) end,
        desc        = any_of,
        expected    = {any, Matchers}
    }.

%% TODO: older syntax for type specifications - we need to support
%% >= R13B for the most part...
-spec all_of(list(matchfun(term())))     -> hamcrest:matchspec();
            (list(hamcrest:matchspec())) -> hamcrest:matchspec().
all_of(Matchers) when is_list(Matchers) ->
    MatchFun = fun(M) when is_function(M) -> M;
       (#'hamcrest.matchspec'{matcher=F}) -> F
    end,
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> not(lists:member(false,
                                    [ (MatchFun(M))(X) || M <- Matchers ])) end,
        desc        = all_of,
        expected    = {all, Matchers}
    }.

-spec(equal_to/1 :: (term()) -> hamcrest:matchspec()).
equal_to(Y) ->
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> X == Y end,
        desc        = equal_to,
        expected    = Y
    }.

-spec(exactly_equal_to/1 :: (term()) -> hamcrest:matchspec()).
exactly_equal_to(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> X =:= Y end,
        desc        = exactly_equal_to,
        expected    = X
    }.

-spec(is/1 :: (matchfun(term()))     -> hamcrest:matchspec();
              (hamcrest:matchspec()) -> hamcrest:matchspec();
              (any())                -> hamcrest:matchspec()).
is(Matcher) when is_record(Matcher, 'hamcrest.matchspec') ->
    Matcher;
is(Term) ->
    equal_to(Term).

-spec(is_true/0 :: () -> hamcrest:matchspec()).
is_true() ->
    equal_to(true).

-spec(is_false/0 :: () -> hamcrest:matchspec()).
is_false() ->
    is_not(equal_to(true)).

-spec(is_not/1 :: (matchfun(term()))     -> hamcrest:matchspec();
                  (hamcrest:matchspec()) -> hamcrest:matchspec();
                  (term())               -> hamcrest:matchspec()).
is_not(#'hamcrest.matchspec'{ matcher=MatchFun }=MatchSpec)
    when is_record(MatchSpec, 'hamcrest.matchspec') ->
  MatchSpec#'hamcrest.matchspec'{ matcher = (fun(X) -> not(MatchFun(X)) end) };
is_not(Term) ->
  is_not(equal_to(Term)).

-spec(greater_than/1 :: (number()) -> hamcrest:matchspec()).
greater_than(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> Y > X end,
        desc        = greater_than,
        expected    = X
    }.

-spec(greater_than_or_equal_to/1 :: (number()) -> hamcrest:matchspec()).
greater_than_or_equal_to(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> Y >= X end,
        desc        = greater_than_or_equal_to,
        expected    = X
    }.

-spec(less_than/1 :: (number()) -> hamcrest:matchspec()).
less_than(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> Y < X end,
        desc        = less_than,
        expected    = X
    }.

-spec(less_than_or_equal_to/1 :: (number()) -> hamcrest:matchspec()).
less_than_or_equal_to(X) ->
    #'hamcrest.matchspec'{
        matcher     = fun(Y) -> Y =< X end,
        desc        = {less_than_or_equal_to, X},
        expected    = X
    }.

%% fun((string()) -> boolean()) matchers...

-spec(contains_string/1 :: (string()) -> hamcrest:matchspec()).
contains_string([_|_]=X) ->
    ?MATCHER(fun(Y) -> string:str(Y, X) > 0 end, X, {contains_string, X}).

-spec(starts_with/1 :: (string()) -> hamcrest:matchspec()).
starts_with(X) ->
    ?MATCHER(fun(Y) -> string:str(Y, X) == 1 end, X, {starts_with, X}).

-spec(ends_with/1 :: (string()) -> hamcrest:matchspec()).
ends_with(X) ->
    ?MATCHER(fun(Y) -> string:equal(string:right(Y, length(X)), X) end,
             X, {ends_with, X}).

-spec(matches_regex/1 :: (string()) -> hamcrest:matchspec()).
matches_regex(Rx) ->
    #'hamcrest.matchspec'{
               matcher     =
                   fun(X) ->
                           case re:run(X, Rx) of
                               {match,_} -> true;
                               _         -> false
                           end
                   end,
               desc        = {regex_match, Rx},
               expected    = Rx
              }.

-spec(match_mfa/3 :: (module(), atom(), list(term())) -> hamcrest:matchspec()).
match_mfa(Mod, Func, Args) ->
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> catch(apply(Mod, Func, Args ++ [X])) == true end,
        desc        = {eval, [Mod, Func, Args]},
        expected    = true
    }.

-spec(match_mfa/4 :: (module(), atom(),
                      list(term()), term()) -> hamcrest:matchspec()).
match_mfa(Mod, Func, Args, Desc) ->
    MS = match_mfa(Mod, Func, Args),
    MS#'hamcrest.matchspec'{desc=Desc}.

-spec(reverse_match_mfa/3 :: (module(), atom(),
                              list(term())) -> hamcrest:matchspec()).
reverse_match_mfa(Mod, Func, Args) when is_list(Args) ->
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> catch(apply(Mod, Func, [X|Args])) == true end,
        desc        = {eval, [Mod, Func, lists:reverse(Args)]},
        expected    = true
    }.

-spec(reverse_match_mfa/4 :: (module(), atom(),
                              list(term()), term()) -> hamcrest:matchspec()).
reverse_match_mfa(Mod, Func, Args, Desc) ->
    MS = reverse_match_mfa(Mod, Func, Args),
    MS#'hamcrest.matchspec'{desc=Desc}.

-spec(isalive/0 :: () -> hamcrest:matchspec()).
isalive() ->
    MS = match_mfa(erlang, is_process_alive, []),
    MS#'hamcrest.matchspec'{desc=is_process_alive, expected=true}.

-spec(isdead/0 :: () -> hamcrest:matchspec()).
isdead() ->
    #'hamcrest.matchspec'{
        matcher     = fun(X) -> not erlang:is_process_alive(X) end,
        desc        = is_process_alive,
        expected    = false
    }.

-spec(has_length/1 :: (number()) -> hamcrest:matchspec()).
has_length(Size) when is_number(Size) ->
  ?MATCHER(fun(XS) -> length(XS) == Size end,
           Size, {length, Size}).

-spec(has_same_contents_as/1 :: (container_t()) -> hamcrest:matchspec()).
has_same_contents_as(Container) when is_list(Container) ->
    MS = foreach(match_mfa(?MODULE, check_member, [Container])),
    MS#'hamcrest.matchspec'{desc=has_same_contents_as, expected=Container}.

-spec(contains_member/1 :: (term()) -> hamcrest:matchspec()).
contains_member(E) ->
    reverse_match_mfa(?MODULE, check_member, [E]).

check_member([], []) ->
    true;
check_member(Container, E) when is_list(Container) ->
    lists:member(E, Container);
check_member(Container, E) ->
    case sets:is_set(Container) of
        true ->
            sets:is_element(E, Container);
        false ->
            case gb_sets:is_set(Container) of
                true -> gb_sets:is_element(E, Container);
                false ->
                    case ordsets:is_set(Container) of
                        true ->
                            ordsets:is_element(E, Container);
                        false ->
                            case is_tuple(Container) of
                                true -> check_member(tuple_to_list(Container), E);
                                false -> false
                            end
                    end
            end
    end.

-spec(isempty/0 :: () -> hamcrest:matchspec()).
isempty() ->
    match_mfa(?MODULE, check_isempty, []).

check_isempty([]) ->
    true;
check_isempty({}) ->
    true;
check_isempty(X) ->
    case sets:is_set(X) of
        true ->
            sets:size(X) == 0;
        _ ->
            case gb_sets:is_set(X) of
                true -> gb_sets:is_empty(X);
                _ ->
                    case ordsets:is_set(X) of
                        true -> ordsets:size(X) == 0;
                        false -> false
                    end
            end
    end.
