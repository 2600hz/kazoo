%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Manage the ETS table lookup for token server to account/client IP
%%% @author James Aimonetti
%%%
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_stats_etsmgr).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

-record(state, {table_id :: ets:tid() | 'undefined'
               ,etssrv :: kz_term:api_pid()
               ,give_away_ref :: kz_term:api_reference()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link(ets:tab(), any()) -> kz_term:startlink_ret().
start_link(TableId, TableOptions) ->
    gen_server:start_link(?SERVER, [TableId, TableOptions], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', #state{}}.
init([TableId, TableOptions]) ->
    kz_log:put_callid(?MODULE),
    gen_server:cast(self(), {'begin', TableId, TableOptions}),
    lager:debug("started etsmgr for stats for ~s", [TableId]),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'begin', TableId, TableOptions}, State) ->
    Tbl = ets:new(TableId, TableOptions),

    ets:setopts(Tbl, {'heir', self(), 'ok'}),
    {'noreply', State#state{table_id=Tbl
                           ,give_away_ref=send_give_away_retry(Tbl, 'ok', 0)
                           }};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'ETS-TRANSFER', Tbl, Etssrv, Data}, #state{table_id=Tbl
                                                       ,etssrv=Etssrv
                                                       ,give_away_ref='undefined'
                                                       }=State) ->
    lager:debug("ets table ~p transferred back to ourselves", [Tbl]),
    {'noreply', State#state{etssrv='undefined'
                           ,give_away_ref=send_give_away_retry(Tbl, Data, 0)
                           }};
handle_info({'give_away', Tbl, Data}, #state{table_id=Tbl
                                            ,etssrv='undefined'
                                            ,give_away_ref=Ref
                                            }=State) when is_reference(Ref) ->
    lager:debug("give away ~p: ~p", [Tbl, Data]),
    case find_ets_mgr(Tbl, Data) of
        P when is_pid(P) ->
            lager:debug("handing tbl ~p back to ~p and then to ~p", [Tbl, self(), P]),
            {'noreply', State#state{etssrv=P
                                   ,give_away_ref='undefined'
                                   }};
        Ref when is_reference(Ref) ->
            lager:debug("ets mgr died already, hasn't resumed life yet; waiting"),
            {'noreply', State#state{etssrv='undefined'
                                   ,give_away_ref=Ref
                                   }}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

find_ets_mgr(Tbl, Data) ->
    case acdc_stats_sup:stats_srv() of
        {'error', 'not_found'} -> send_give_away_retry(Tbl, Data);
        {'ok', P} when is_pid(P) ->
            link(P),
            ets:give_away(Tbl, P, Data),
            P
    end.

send_give_away_retry(Tbl, Data) ->
    send_give_away_retry(Tbl, Data, 10).
send_give_away_retry(Tbl, Data, Timeout) ->
    erlang:send_after(Timeout, self(), {'give_away', Tbl, Data}).

%%------------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("ETS mgr going down: ~p", [_Reason]),
    ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
