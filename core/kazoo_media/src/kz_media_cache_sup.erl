%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([find_file_server/3]).
-export([start_file_server/3]).
-export([find_tts_server/1]).
-export([find_tts_server/2]).

%% Supervisor callbacks
-export([init/1]).

-include("kazoo_media.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, []).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec find_file_server(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', pid()} |
                              {'error', 'no_file_server'}.
find_file_server(Id, Doc, Attachment) ->
    Name = [Id, Doc, Attachment],
    case [P||{N,P,_,_} <- supervisor:which_children(?SERVER), N =:= Name, is_pid(P)] of
        [] -> {'error', 'no_file_server'};
        [P] -> {'ok', P}
    end.

-spec start_file_server(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                               {'ok', pid()} |
                               {'error', any()}.
start_file_server(Id, Doc, Attachment) ->
    Name = [Id, Doc, Attachment],
    start_file_server(Id, Doc, Attachment, Name).
start_file_server(Id, Doc, Attachment, Name) ->
    ChildSpec = ?WORKER_NAME_ARGS_TYPE(Name, 'kz_media_file_cache', [Id, Doc, Attachment], 'temporary'),
    case supervisor:start_child(?SERVER, ChildSpec) of
        {'ok', _Pid}=OK -> OK;
        {'error', {'already_started', Pid}} -> {'ok', Pid};
        {'error', 'already_present'} ->
            _ = supervisor:delete_child(?SERVER, Name), %% FIXME: this didn't kill the child
            start_file_server(Id, Doc, Attachment, Name);
        {'error', _}=E -> E
    end.

-spec find_tts_server(any()) -> {'ok', pid()} |
                                {'error', 'no_file_server'}.
find_tts_server(Id) ->
    case [P||{N,P,_,_} <- supervisor:which_children(?SERVER), N =:= Id, is_pid(P)] of
        [] -> {'error', 'no_file_server'};
        [P] -> {'ok', P}
    end.

-spec find_tts_server(kz_term:ne_binary(), kz_json:object()) ->
                             {'ok', pid()} |
                             {'error', any()}.
find_tts_server(Id, JObj) ->
    ChildSpec = ?WORKER_NAME_ARGS_TYPE(Id, 'kz_media_tts_cache', [Id, JObj], 'temporary'),
    case supervisor:start_child(?SERVER, ChildSpec) of
        {'ok', _Pid}=OK -> OK;
        {'error', {'already_started', Pid}} -> {'ok', Pid};
        {'error', 'already_present'} ->
            _ = supervisor:delete_child(?SERVER, Id),
            find_tts_server(Id, JObj);
        {'error', _}=E -> E
    end.

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 36,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
