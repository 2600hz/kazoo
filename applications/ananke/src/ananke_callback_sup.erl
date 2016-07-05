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
        ,start_child/3
        ,start_child/4
        ,delete_child/1
        ,delete_child/2
        ]).

-export([init/1]).

-include("ananke.hrl").

-define(SERVER, ?MODULE).

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_child(any(), kz_proplist(), pos_integers()) ->
			 sup_startchild_ret().
-spec start_child(any(), kz_proplist(), pos_integers(), check_fun()) ->
			 sup_startchild_ret().
start_child(Id, OriginateReq, Schedule) ->
    start_child(Id, [OriginateReq, Schedule]).

start_child(Id, OriginateReq, Schedule, CheckFun) ->
    start_child(Id, [OriginateReq, Schedule, CheckFun]).

-spec start_child(any(), list()) -> sup_startchild_ret().
start_child(Id, Args) ->
    case supervisor:start_child(?SERVER, {Id
					 ,{'ananke_callback_wrkr', 'start_link', Args}
					 ,'transient'
					 ,'brutal_kill'
					 ,'worker'
					 ,['ananke_voicemail_call_wrkr']
                                         })
    of
        {'error', 'already_present'} ->
            _ = supervisor:delete_child(?SERVER, Id),
            start_child(Id, Args);
        Reply -> Reply
    end.


-spec delete_child(any()) -> 'ok' | {'error', any()}.
delete_child(Pid) when is_pid(Pid) ->
    case [Id || {Id, Child, _Type, _Modules} <- supervisor:which_children(?SERVER), Child =:= Pid] of
        [Id] ->
            delete_child(Id);
        [] ->
            'ok'
    end;
delete_child(Id) ->
    supervisor:delete_child(?SERVER, Id).

-spec delete_child(any(), non_neg_integer()) -> 'ok'.
delete_child(Id, Timeout) ->
    _ = kz_util:spawn(delete_child_after_timeout(Id, Timeout)),
    'ok'.

-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, []}}.

-spec delete_child_after_timeout(any(), non_neg_integer()) -> fun(() -> 'ok' | {'error', any()}).
delete_child_after_timeout(Id, Timeout) ->
    fun() ->
	    receive
	    after Timeout ->
		    delete_child(Id)
	    end
    end.
