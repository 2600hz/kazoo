%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_continuous_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_shout/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Name, Mod, Args), {Name, {Mod, start_link, Args}, transient, 5000, worker, [Mod]}).

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

join_stream(JObj, Db, Doc, Attachment) ->
    ID = wh_util:to_hex_binary(wh_json:get_value(<<"Media-Name">>, JObj)),

    case find_stream(ID) of
        [] ->
            supervisor:start_child(?SERVER, ?CHILD(ID, media_stream, [JObj, Db, Doc, Attachment, continuous]));
        [StreamPid] ->
            media_stream:join_stream(StreamPid, JObj)
    end.

-spec find_stream/1 :: (ne_binary()) -> [pid()] | [].
find_stream(ID) ->
    [P || {ChildID, P, _, _} <- supervisor:which_children(?SERVER),
          ChildID =:= ID].

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
    {ok, {{one_for_one, 1, 5}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
