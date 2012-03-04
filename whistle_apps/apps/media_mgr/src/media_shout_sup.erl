%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_shout_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, new_stream/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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

new_stream(JObj, Db, Doc, Attachment) ->
    supervisor:start_child(?SERVER, [JObj, Db, Doc, Attachment]).

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
    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    AChild = {media_stream, {media_stream, start_link, []},
              Restart, Shutdown, Type, [media_stream]},

    {ok, {{simple_one_for_one, 1, 2}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
