%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(escalus_new_assert).

%% This module is meant to replace legacy escalus_assert in future versions
%% of Escalus

-export([assert/2, assert/3, assert_many/2, mix_match/2]).

%%==============================================================================
%% API functions
%%==============================================================================

assert(PredSpec, Arg) ->
    Fun = predspec_to_fun(PredSpec),
    StanzaStr = arg_to_list(Arg),
    assert_true(Fun(Arg),
        {assertion_failed, assert, PredSpec, Arg, StanzaStr}).

assert(PredSpec, Params, Arg) ->
    Fun = predspec_to_fun(PredSpec, length(Params) + 1),
    StanzaStr = arg_to_list(Arg),
    assert_true(apply(Fun, Params ++ [Arg]),
        {assertion_failed, assert, PredSpec, Params, Arg, StanzaStr}).

assert_many(Predicates, Stanzas) ->
    AllStanzas = length(Predicates) == length(Stanzas),
    Ok = escalus_utils:mix_match(fun predspec_to_fun/1, Predicates, Stanzas),
    StanzasStr = escalus_utils:pretty_stanza_list(Stanzas),
    case Ok of
        true -> ok;
        false ->
            escalus_utils:log_stanzas("multi-assertion failed on", Stanzas)
    end,
    assert_true(Ok and AllStanzas,
        {assertion_failed, assert_many, AllStanzas, Predicates, Stanzas, StanzasStr}).

mix_match(Predicates, Stanzas) ->
    assert_many(Predicates, Stanzas).

%%==============================================================================
%% Helpers
%%==============================================================================

arg_to_list(A) when is_atom(A) ->
    atom_to_list(A);
arg_to_list({'EXIT', {Err, _}}) ->
    "Exit:" ++ atom_to_list(Err);
arg_to_list(Arg) ->
    exml:to_list(Arg).

predspec_to_fun(F) ->
    predspec_to_fun(F, 1).

predspec_to_fun(F, N) when is_atom(F), is_integer(N) ->
    %% Fugly, avert your eyes :-/
    %% R15B complains about {escalus_pred, F} syntax, where
    %% R14B04 doesn't allow fun escalus_pred:F/A yet.
    case N of
        1 -> fun (A) -> escalus_pred:F(A) end;
        2 -> fun (A, B) -> escalus_pred:F(A, B) end;
        3 -> fun (A, B, C) -> escalus_pred:F(A, B, C) end;
        4 -> fun (A, B, C, D) -> escalus_pred:F(A, B, C, D) end
    end;
predspec_to_fun(Other, _) ->
    Other.

assert_true(true, _) -> ok;
assert_true(false, Fail) ->
    error(Fail);
assert_true(WTF, Pred) ->
    error(bad_predicate_return_value, [WTF, Pred]).
