%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_monitor).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("fax.hrl").

-record(state, {}).
-type state() :: #state{}.

-define(NAME, ?MODULE).
-define(SERVER, {'via', 'kz_globals', ?NAME}).

-define(POLLING_INTERVAL, 5000).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    case gen_server:start_link(?SERVER, ?MODULE, [], []) of
        {'error', {'already_started', Pid}}
          when is_pid(Pid)->
            erlang:link(Pid),
            {'ok', Pid};
        Other -> Other
    end.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state(), timeout()}.
init([]) ->
    {'ok', #state{}, ?POLLING_INTERVAL}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State, ?POLLING_INTERVAL}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, ?POLLING_INTERVAL}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('timeout', State) ->
    ViewOptions = ['reduce'
                  ,'group'
                  ,{'group_level', 1}
                  ],
    case kz_datamgr:get_result_keys(?KZ_FAXES_DB, <<"faxes/schedule_accounts">>, ViewOptions) of
        {'ok', []} -> {'noreply', State, ?POLLING_INTERVAL};
        {'ok', AccountIds} ->
            _ = distribute_accounts(AccountIds),
            _ = garbage_collect(),
            {'noreply', State, ?POLLING_INTERVAL};
        {'error', 'not_found'} ->
            fax_maintenance:refresh_views(),
            {'noreply', State, ?POLLING_INTERVAL};
        {'error', _Reason} ->
            lager:debug("failed to fetch fax account jobs: ~p", [_Reason]),
            {'noreply', State, ?POLLING_INTERVAL}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, ?POLLING_INTERVAL}.

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
    lager:debug("fax jobs terminating: ~p", [_Reason]).

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec distribute_accounts(kz_term:ne_binaries()) -> kz_term:ne_binaries().
distribute_accounts([]) -> [];
distribute_accounts([AccountId|AccountIds]) ->
    maybe_start_account(fax_jobs:is_running(AccountId), AccountId),
    distribute_accounts(AccountIds).

-spec maybe_start_account(boolean(), kz_term:ne_binary()) -> 'ok'.
maybe_start_account('true', _AccountId) -> 'ok';
maybe_start_account('false', AccountId) ->
    lager:debug("sending start fax account jobs for ~s", [AccountId]),
    Payload = [{<<"Account-ID">>, AccountId}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Payload, fun kapi_fax:publish_start_account/1).
