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

-module(escalus_story).

% Public API
-export([story/3,
         make_everyone_friends/1,
         make_everyone_friends/2,
         start_ready_clients/2,
         send_initial_presence/1]).

-include("escalus.hrl").
-include_lib("test_server/include/test_server.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

story(ConfigIn, ResourceCounts, Story) ->
    ClientDescs = clients_from_resource_counts(ConfigIn, ResourceCounts),
    try
        Config = escalus_server:pre_story(ConfigIn),
        Clients = start_clients(Config, ClientDescs),
        ensure_all_clean(Clients),
        escalus_event:story_start(Config),
        apply_w_arity_check(Story, Clients),
        escalus_event:story_end(Config),
        post_story_checks(Config, Clients),
        escalus_server:post_story(Config)
    catch Class:Reason ->
        Stacktrace = erlang:get_stacktrace(),
        escalus_event:print_history(ConfigIn),
        erlang:raise(Class, Reason, Stacktrace)
    after
        escalus_cleaner:clean(ConfigIn)
    end.

make_everyone_friends(Config) ->
    Users = escalus_config:get_config(escalus_users, Config),
    make_everyone_friends(Config, Users).

make_everyone_friends(Config0, Users) ->
    % start the clients
    Config1 = escalus_cleaner:start(Config0),
    Clients = start_clients(Config1, [[{US, <<"friendly">>}] || {_Name, US} <- Users]),

    % exchange subscribe and subscribed stanzas
    escalus_utils:distinct_pairs(fun(C1, C2) ->
        send_presence(C1, <<"subscribe">>, C2),
        swallow_stanzas(C1, 1, 0),
        swallow_stanzas(C2, 0, 1),
        send_presence(C2, <<"subscribe">>, C1),
        swallow_stanzas(C1, 1, 1),
        swallow_stanzas(C2, 1, 0),
        send_presence(C2, <<"subscribed">>, C1),
        swallow_stanzas(C1, 1, 2),
        swallow_stanzas(C2, 1, 0),
        send_presence(C1, <<"subscribed">>, C2),
        swallow_stanzas(C1, 1, 0),
        swallow_stanzas(C2, 1, 2)
    end, Clients),

    ensure_all_clean(Clients),

    % stop the clients
    escalus_cleaner:clean(Config1),
    escalus_cleaner:stop(Config1),

    % return Config0
    [{everyone_is_friends, true} | Config0].

call_start_ready_clients(Config, UserCDs) ->
    escalus_overridables:do(Config, start_ready_clients, [Config, UserCDs],
                            {?MODULE, start_ready_clients}).

start_ready_clients(Config, FlatCDs) ->
    {_, RClients} = lists:foldl(fun({UserSpec, BaseResource}, {N, Acc}) ->
        Resource = escalus_overridables:do(Config, modify_resource, [BaseResource],
                                           {escalus_utils, identity}),
        {ok, Client} = escalus_client:start(Config, UserSpec, Resource),
        escalus_overridables:do(Config, initial_activity, [Client],
                                {?MODULE, send_initial_presence}),
        %% drop 1 own presence + N-1 probe replies = N presence stanzas
        drop_presences(Client, N),
        {N+1, [Client|Acc]}
    end, {1, []}, FlatCDs),
    Clients = lists:reverse(RClients),
    ClientsCount = length(Clients),
    escalus_utils:each_with_index(fun(Client, N) ->
        %% drop presence updates of guys who have logged in after you did
        drop_presences(Client, ClientsCount - N)
    end, 1, Clients),
    ensure_all_clean(Clients),
    Clients.

send_initial_presence(Client) ->
    escalus_client:send(Client, escalus_stanza:presence(<<"available">>)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

send_presence(From, Type, To) ->
    ToJid = escalus_client:short_jid(To),
    Stanza = escalus_stanza:presence_direct(ToJid, Type),
    escalus_client:send(From, Stanza).

swallow_stanzas(User, NoRosters, NoPresences) ->
    Rosters = lists:duplicate(NoRosters, is_roster_set),
    Presences = lists:duplicate(NoPresences, is_presence),
    Stanzas = escalus:wait_for_stanzas(User, NoRosters + NoPresences),
    escalus:assert_many(Rosters ++ Presences, Stanzas).

ensure_all_clean(Clients) ->
    lists:foreach(fun(Client) ->
        escalus_assert:has_no_stanzas(Client)
    end, Clients).

start_clients(Config, ClientDescs) ->
    case proplists:get_bool(everyone_is_friends, Config) of
        true ->
            call_start_ready_clients(Config, lists:append(ClientDescs));
        false ->
            lists:flatmap(fun(UserCDs) ->
                call_start_ready_clients(Config, UserCDs)
            end, ClientDescs)
    end.

drop_presences(Client, N) ->
    Dropped = escalus_client:wait_for_stanzas(Client, N),
    [escalus:assert(is_presence, Stanza) || Stanza <- Dropped],
    N = length(Dropped).

post_story_checks(Config, Clients) ->
    case proplists:get_bool(escalus_no_stanzas_after_story, Config) of
        true ->
            lists:foreach(
                fun escalus_assert:has_no_stanzas/1,
                Clients
            );
        _ ->
            ok
    end.

zip_shortest([H1|T1], [H2|T2]) ->
    [{H1,H2}|zip_shortest(T1, T2)];
zip_shortest(_, _) ->
    [].

%% ResourceCounts is a list of tuples: [{alice,2}, {bob,1}]
clients_from_resource_counts(Config, ResourceCounts = [{_, _} | _]) ->
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    [resources_per_spec(UserSpec, ResCount)
     || { User, ResCount} <- ResourceCounts,
        {_User, UserSpec} <- [lists:keyfind(User, 1, NamedSpecs)]];
%% Old-style ResourceCounts: [2, 1]
clients_from_resource_counts(Config, ResourceCounts) ->
    Deprecated = io_lib:format("DEPRECATED resource counts ~p (use [{alice, 1}, ...] or similar)",
                              [ResourceCounts]),
    escalus_compat:complain(Deprecated),
    NamedSpecs = escalus_config:get_config(escalus_users, Config),
    [resources_per_spec(UserSpec, ResCount)
     || {{_, UserSpec}, ResCount} <- zip_shortest(NamedSpecs,
                                                  ResourceCounts)].

resources_per_spec(UserSpec, ResCount) ->
    [{UserSpec, list_to_binary("res" ++ integer_to_list(N))}
     || N <- lists:seq(1, ResCount)].

apply_w_arity_check(Fun, Args) when is_function(Fun, 1) ->
    case length(Args) of
        1 -> apply(Fun, Args);  %% Fun expects one logged-in user
        _ -> apply(Fun, [Args]) %% Fun expects list of users
    end;
apply_w_arity_check(Fun, Args) when is_function(Fun) ->
    apply(Fun, Args).
