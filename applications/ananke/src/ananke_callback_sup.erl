%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(ananke_callback_sup).

-export([start_link/0
        ,start_child/4
        ,delete_child/1
        ,delete_child/2
        ]).

-export([init/1]).

-include("ananke.hrl").

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

-spec start_child(any(), wh_proplist(), integer(), integer()) ->
    sup_startchild_ret().
start_child(Id, OriginateReq, Attempts, Interval) ->
    case supervisor:start_child(?MODULE, {Id
                                          ,{'ananke_callback_wrkr', 'start_link'
                                            ,[OriginateReq, Attempts, Interval]}
                                          ,'transient'
                                          ,'brutal_kill'
                                          ,'worker'
                                          ,['ananke_voicemail_call_wrkr']
                                         })
    of
        {'error', 'already_present'} ->
            _ = supervisor:delete_child(?MODULE, Id),
            start_child(Id, OriginateReq, Attempts, Interval);
        Reply -> Reply
    end.

-spec delete_child(any()) -> 'ok' | {'error', any()}.
delete_child(Pid) when is_pid(Pid) ->
    case [Id || {Id, Child, _Type, _Modules} <- supervisor:which_children(?MODULE), Child =:= Pid] of
        [Id] ->
            delete_child(Id);
        [] ->
            'ok'
    end;
delete_child(Id) ->
    supervisor:delete_child(?MODULE, Id).

-spec delete_child(any(), non_neg_integer()) -> 'ok'.
delete_child(Id, Timeout) ->
    _ = wh_util:spawn(delete_child_after_timeout(Id, Timeout)),
    'ok'.

-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, []}}.

-spec delete_child_after_timeout(any(), non_neg_integer()) -> 'ok' | {'error', any()}.
delete_child_after_timeout(Id, Timeout) ->
    fun() ->
        receive
        after Timeout ->
                  delete_child(Id)
        end
    end.
