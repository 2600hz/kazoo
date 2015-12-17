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

-module(escalus_utils).

-export([log_stanzas/2,
         pretty_stanza_list/1,
         distinct_pairs/2,
         distinct_ordered_pairs/2,
         each_with_index/3,
         all_true/1,
         any_true/1,
         identity/1,
         mix_match/3,
         get_jid/1,
         get_short_jid/1,
         jid_to_lower/1,
         get_username/1,
         get_server/1,
         drop_first_such/2,
         show_backtrace/0,
         is_prefix/2,
         start_clients/1,
         start_clients/2,
         regexp_get/2]).

-ifdef(no_binary_to_integer).
-export([binary_to_integer/1,
         integer_to_binary/1]).
-endif.

-import(escalus_compat, [unimplemented/0, bin/1]).

-include("escalus.hrl").
-include_lib("exml/include/exml.hrl").

-type jid_spec() :: #client{} | atom() | binary() | string().
-export_type([jid_spec/0]).

-spec log_stanzas(iolist(), [#xmlel{}]) -> any().
log_stanzas(Comment, Stanzas) ->
    error_logger:info_msg("~s:~s~n", [Comment, stanza_lines("\n  * ", Stanzas)]).

-spec pretty_stanza_list([#xmlel{}]) -> string().
pretty_stanza_list(Stanzas) ->
    binary_to_list(list_to_binary(stanza_lines("     ", Stanzas))).

%% calls Fun(A, B) on each distinct (A =/= B) pair of elements in List
%% if Fun(A, B) was called, then Fun(B, A) won't
distinct_pairs(Fun, List) ->
    K = each_with_index(fun(A, N) ->
        each_with_index(fun(B, M) ->
            if N < M ->
                    Fun(A, B);
               true ->
                   skip
           end
        end, 0, List)
    end, 0, List),
    K * (K - 1) div 2.

%% calls Fun(A, B) on each distinct (A =/= B) ordered pair of elements in List
distinct_ordered_pairs(Fun, List) ->
    K = each_with_index(fun(A, N) ->
        each_with_index(fun(B, M) ->
            if N =/= M ->
                    Fun(A, B);
               true ->
                   skip
           end
        end, 0, List)
    end, 0, List),
    K * (K - 1).

%% Calls Fun(Element, Index) for indices (starting from Start) and elements of List
each_with_index(Fun, Start, List) ->
    lists:foldl(fun(Element, N) ->
        Fun(Element, N),
        N + 1
    end, Start, List).

all_true(List) ->
    lists:foldl(fun erlang:'and'/2, true, List).

any_true(List) ->
    lists:foldl(fun erlang:'or'/2, false, List).

identity(X) ->
    X.

%% Does for each Case in Cases exist a Cond in Conds such that
%% (Predgen(Cond))(Case) == true?
mix_match(Predgen, Conds, Cases) ->
    [] == lists:foldl(fun(Cond, CasesLeft) ->
              Pred = Predgen(Cond),
              drop_first_such(Pred, CasesLeft)
          end, Cases, Conds).

drop_first_such(Pred, List) ->
    drop_first_such(Pred, List, []).

drop_first_such(_, [], Acc) ->
    lists:reverse(Acc);
drop_first_such(Pred, [H|T], Acc) ->
    case Pred(H) of
        true ->
            lists:reverse(Acc) ++ T;
        false ->
            drop_first_such(Pred, T, [H|Acc])
    end.

stanza_lines(Prefix, Stanzas) ->
    [[Prefix, exml:to_iolist(S)] || S <- Stanzas].

show_backtrace() ->
    try throw(catch_me)
    catch _:_ ->
        error_logger:info_msg("Backtrace:~n~p~n", [tl(erlang:get_stacktrace())])
    end.

-spec get_jid(jid_spec()) -> binary().
get_jid(#client{jid=Jid}) ->
    Jid;
get_jid(Username) when is_atom(Username) ->
    bin(escalus_users:get_jid([], Username));
get_jid(Jid) when is_list(Jid) ->
    bin(Jid);
get_jid(Jid) when is_binary(Jid) ->
    Jid.

-spec get_short_jid(#client{} | atom() | binary() | string()) -> binary().
get_short_jid(#client{}=Recipient) ->
    escalus_client:short_jid(Recipient);
get_short_jid(Username) when is_atom(Username) ->
    escalus_users:get_jid([], Username);
get_short_jid(Jid) when is_list(Jid) ->
    list_to_binary(Jid);
get_short_jid(Jid) when is_binary(Jid) ->
    Jid.

-spec jid_to_lower(binary()) -> binary().
jid_to_lower(Jid) ->
    %% simplified lowercaseing
    list_to_binary(string:to_lower(binary_to_list(Jid))).

get_username(UserOrClient) ->
    regexp_get(get_short_jid(UserOrClient), <<"^([^@]*)">>).

get_server(UserOrClient) ->
    regexp_get(get_short_jid(UserOrClient), <<"^[^@]*[@]([^/]*)">>).

is_prefix(Prefix, Full) when is_binary(Prefix), is_binary(Full) ->
    LCP = binary:longest_common_prefix([Prefix, Full]),
    size(Prefix) =< size(Full) andalso LCP == size(Prefix).

start_clients(Clients) ->
    start_clients([], Clients).

start_clients(Config0, ClientRecipes) ->
    AllCDs = escalus_config:get_config(escalus_users, Config0),
    FlatCDs = [{CD, Res} || {Username, Resources} <- ClientRecipes,
                            {_, CD} <- [lists:keyfind(Username, 1, AllCDs)],
                            Res <- Resources],
    Config1 = [{_, Cleaner} | _] = escalus_cleaner:start(Config0),
    Clients = escalus_overridables:do(Config0,
                                      start_ready_clients, [Config1, FlatCDs],
                                      {escalus_story, start_ready_clients}),
    {Cleaner, Clients}.

regexp_get(Jid, Regex) ->
    {match, [ShortJid]} =
        re:run(Jid, Regex, [{capture, all_but_first, binary}]),
    ShortJid.

-ifdef(no_binary_to_integer).

binary_to_integer(B) when is_binary(B) ->
    list_to_integer(binary_to_list(B));
binary_to_integer(B) ->
    error(badarg, [B]).

integer_to_binary(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I));
integer_to_binary(I) ->
    error(badarg, [I]).

-endif.
