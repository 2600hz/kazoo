%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([find_file_server/3]).
-export([start_file_server/3]).
-export([find_tts_server/1]).
-export([find_tts_server/2]).

%% Supervisor callbacks
-export([init/1]).

-include("whistle_media.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, []).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec find_file_server(ne_binary(), ne_binary(), ne_binary()) ->
                              {'ok', pid()} |
                              {'error', 'no_file_server'}.
find_file_server(Id, Doc, Attachment) ->
    Name = [Id, Doc, Attachment],
    case [P||{N,P,_,_} <- supervisor:which_children(?SERVER), N =:= Name, is_pid(P)] of
        [] -> {'error', 'no_file_server'};
        [P] -> {'ok', P}
    end.

-spec start_file_server(ne_binary(), ne_binary(), ne_binary()) ->
                               {'ok', pid()} |
                               {'error', any()}.
start_file_server(Id, Doc, Attachment) ->
    Name = [Id, Doc, Attachment],
    start_file_server(Id, Doc, Attachment, Name).
start_file_server(Id, Doc, Attachment, Name) ->
    ChildSpec = ?WORKER_NAME_ARGS_TYPE(Name, 'wh_media_file_cache', [Id, Doc, Attachment], 'temporary'),
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

-spec find_tts_server(ne_binary(), wh_json:object()) ->
                             {'ok', pid()} |
                             {'error', any()}.
-spec find_tts_server(ne_binary(), wh_json:object(), ne_binary()) ->
                             {'ok', pid()} |
                             {'error', any()}.
find_tts_server(Text, JObj) ->
    Id = wh_util:binary_md5(Text),
    find_tts_server(Text, JObj, Id).

find_tts_server(Text, JObj, Id) ->
    ChildSpec = ?WORKER_NAME_ARGS_TYPE(Id, 'wh_media_tts_cache', [Text, JObj], 'temporary'),
    case supervisor:start_child(?SERVER, ChildSpec) of
        {'ok', _Pid}=OK -> OK;
        {'error', {'already_started', Pid}} -> {'ok', Pid};
        {'error', 'already_present'} ->
            _ = supervisor:delete_child(?SERVER, Id),
            find_tts_server(Text, JObj, Id);
        {'error', _}=E -> E
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 36,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
