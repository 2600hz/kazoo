%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Manage the ETS table separate from the main process to use the ETS table
%%% Protects against the main writer dying.
%%%
%%% Inspired by:
%%% <a href="http://steve.vinoski.net/blog/2011/03/23/dont-lose-your-ets-tables/">
%%% Don't Lose Your ETS Tables</a>.
%%%
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_etsmgr_srv).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2
        ,default_table_options/0
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

%% Internal
-export([find_me/2]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(SERVER, ?MODULE).

-define(TABLE_DATA, 0).

-type start_arg() :: {'table_id', atom()} |
                     {'table_options', list()} |
                     {'gift_data', any()} |
                     {'find_me_function', find_me_fun()}.
-type start_args() :: [start_arg()].

-type find_me_fun() :: fun(() -> pid()).
-export_type([start_arg/0, start_args/0
             ,find_me_fun/0
             ]).

-record(state, {table_id :: atom()
               ,give_away_pid :: kz_term:api_pid()
               ,find_me_fun :: find_me_fun() | undefined
               ,find_me_pid_ref :: kz_term:api_pid_ref()
               ,gift_data :: any()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(start_args()) -> kz_types:startlink_ret().
start_link(Opts) ->
    'true' = valid_options(Opts),
    gen_server:start_link(?SERVER, [Opts], []).

-spec start_link(atom(), start_args()) -> kz_types:startlink_ret().
start_link(Name, Opts) ->
    'true' = valid_options(Opts),
    gen_server:start_link({'local', Name}, ?MODULE, [Opts], []).

-spec valid_options(start_args()) -> boolean().
valid_options(Opts) ->
    (TID = props:get_value('table_id', Opts)) =/= 'undefined'
        andalso is_atom(TID)
        andalso is_function(props:get_value('find_me_function', Opts), 0)
        orelse props:is_defined('local', Opts).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init([Opts]) ->
    process_flag('trap_exit', 'true'),

    TableId = opt_table_id(Opts),
    TableOptions = opt_table_options(Opts),

    kz_log:put_callid(<<"etssrv_", (kz_term:to_binary(TableId))/binary>>),
    gen_server:cast(self(), {'begin', TableId, TableOptions}),

    lager:debug("started etsmgr for table ~p", [TableId]),

    {'ok', #state{table_id=TableId
                 ,find_me_fun=opt_find_me_fun(Opts)
                 ,gift_data=opt_gift_data(Opts)
                 }}.

-define(DEFAULT_TABLE_OPTIONS, ['set', 'protected', {'keypos', 2}]).
-spec default_table_options() -> list().
default_table_options() ->
    ?DEFAULT_TABLE_OPTIONS.

opt_table_id(Opts) -> props:get_value('table_id', Opts).
opt_table_options(Opts) ->
    case props:get_value('table_options', Opts) of
        'undefined' -> ?DEFAULT_TABLE_OPTIONS;
        TblOpts when is_list(TblOpts) -> TblOpts
    end.

opt_find_me_fun(Opts) -> props:get_value('find_me_function', Opts).
opt_gift_data(Opts) -> props:get_value('gift_data', Opts, 'ok').

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'begin', TableId, TableOptions}, #state{gift_data=GiftData}=State) ->
    TID = ets:new(TableId, TableOptions),

    lager:debug("created new table ~p(~p): ~p", [TableId, TID, TableOptions]),
    ets:setopts(TID, {'heir', self(), GiftData}),
    send_give_away_retry(TID),
    {'noreply', State#state{table_id=TID}};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'EXIT', Pid, 'killed'}, #state{give_away_pid=Pid}=State) ->
    lager:debug("ets mgr ~p killed", [Pid]),
    {'noreply', State#state{give_away_pid='undefined'}};
handle_info({'EXIT', Pid, 'shutdown'}, #state{give_away_pid=Pid}=State) ->
    lager:debug("ets mgr ~p shutdown", [Pid]),
    {'noreply', State#state{give_away_pid='undefined'}};
handle_info({'EXIT', _Pid, _Reason}, #state{give_away_pid='undefined'}=State) ->
    {'noreply', State};
handle_info({'ETS-TRANSFER', Tbl, Pid, _Data}, #state{table_id=Tbl
                                                     ,give_away_pid=Pid
                                                     }=State) ->
    lager:debug("ets table '~p' transferred back to ourselves", [Tbl]),
    send_give_away_retry(Tbl),
    {'noreply', State#state{give_away_pid='undefined'}};
handle_info({'give_away', Tbl}, #state{table_id=Tbl
                                      ,give_away_pid='undefined'
                                      ,find_me_fun='undefined'
                                      }=State) ->
    lager:debug("no find_me_fun, ets table ~p will live here", [Tbl]),
    {'noreply', State};
handle_info({'give_away', Tbl}, #state{table_id=Tbl
                                      ,give_away_pid='undefined'
                                      ,find_me_fun=F
                                      }=State) ->
    lager:debug("give away ~p", [Tbl]),
    FindMe = kz_process:spawn_monitor(fun find_me/2, [F, self()]),
    lager:debug("finding the successor in ~p", [FindMe]),
    {'noreply', State#state{find_me_pid_ref=FindMe}};
handle_info({'found_me', Pid}, #state{table_id=Tbl
                                     ,give_away_pid='undefined'
                                     ,find_me_pid_ref={_FindMePid, FindMeRef}
                                     ,gift_data=GiftData
                                     }=State) ->
    lager:debug("found our new writer pid: ~p", [Pid]),
    erlang:demonitor(FindMeRef, ['flush']),
    link(Pid),
    ets:give_away(Tbl, Pid, GiftData),
    {'noreply', State#state{give_away_pid=Pid
                           ,find_me_pid_ref='undefined'
                           }, 'hibernate'};
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, #state{table_id=Tbl
                                                          ,find_me_pid_ref={Pid, Ref}
                                                          }=State) ->
    lager:debug("our find_me pid ~p went down: ~p", [Pid, _Reason]),
    send_give_away_retry(Tbl),
    {'noreply', State#state{find_me_pid_ref='undefined'
                           ,give_away_pid='undefined'
                           }};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

send_give_away_retry(Tbl) ->
    self() ! {'give_away', Tbl},
    'ok'.

-spec find_me(find_me_fun(), pid()) -> 'ok'.
find_me(Fun, Srv) ->
    lager:debug("trying to find successor for ~p", [Srv]),
    try Fun() of
        P when is_pid(P) ->
            Srv ! {'found_me', P},
            lager:debug("successor ~p found", [P]);
        'undefined' ->
            timer:sleep(250),
            lager:debug("haven't found a successor yet"),
            find_me(Fun, Srv)
    catch
        _E:_R ->
            lager:debug("failed to find successor: ~s: ~p", [_E, _R])
    end.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("ETS mgr going down: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
