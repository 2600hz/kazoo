%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
         ,find_file_server/3, find_file_server/4
         ,find_tts_server/1, find_tts_server/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(Name, Id, Doc, Attachment, Meta), {Name, {wh_media_file_cache, start_link, [Id, Doc, Attachment, Meta]}, temporary, 5000, worker, [wh_media_file_cache]}).
-define(CHILD(Name, Text, JObj), {Name, {wh_media_tts_cache, start_link, [Text, JObj]}, temporary, 5000, worker, [wh_media_tts_cache]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

find_file_server(Id, Doc, Attachment) ->
    Name = [Id,Doc,Attachment],
    case [P||{N,P,_,_} <- supervisor:which_children(?MODULE), N =:= Name, is_pid(P)] of
        [] -> {error, no_file_server};
        [P] -> {ok, P}
    end.

find_file_server(Id, Doc, Attachment, Meta) ->
    Name = [Id,Doc,Attachment],
    find_file_server(Id, Doc, Attachment, Meta, Name).
find_file_server(Id, Doc, Attachment, Meta, Name) ->
    case supervisor:start_child(?MODULE, ?CHILD(Name, Id, Doc, Attachment, Meta)) of
        {ok, _Pid}=OK -> OK;
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, already_present} ->
            _ = supervisor:delete_child(?MODULE, Name),
            find_file_server(Id, Doc, Attachment, Meta, Name);
        {error, _}=E -> E
    end.

find_tts_server(Id) ->
    case [P||{N,P,_,_} <- supervisor:which_children(?MODULE), N =:= Id, is_pid(P)] of
        [] -> {error, no_file_server};
        [P] -> {ok, P}
    end.

find_tts_server(Text, JObj) ->
    Id =  wh_util:binary_md5(Text),    
    find_tts_server(Text, JObj, Id).
find_tts_server(Text, JObj, Id) ->
    case supervisor:start_child(?MODULE, ?CHILD(Id, Text, JObj)) of
        {ok, _Pid}=OK -> OK;
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, already_present} ->
            _ = supervisor:delete_child(?MODULE, Id),
            find_tts_server(Text, JObj, Id);
        {error, _}=E -> E
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
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 36,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
