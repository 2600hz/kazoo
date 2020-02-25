%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc Periodically walk the cache's ETS table and remove expired entries
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_lru).
-behaviour(gen_server).

-export([start_link/2
        ,update_expire_period/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_caches.hrl").

-record(state, {name :: atom()
               ,expire_period = ?EXPIRE_PERIOD_MS :: timeout()
               ,expire_period_ref :: reference()
               }).
-type state() :: #state{}.

-spec start_link(atom(), timeout()) -> kz_types:startlink_ret().
start_link(Name, ExpirePeriod) ->
    gen_server:start_link({'local', lru_name(Name)}, ?MODULE, [Name, ExpirePeriod], []).

-spec update_expire_period(atom(), kz_time:gregorian_seconds()) -> 'ok'.
update_expire_period(Name, ExpirePeriodS) ->
    gen_server:cast(lru_name(Name), {'update_expire_period', ExpirePeriodS}).

-spec init(list()) -> {'ok', state()}.
init([Name, ExpirePeriod]) ->
    kz_log:put_callid(lru_name(Name)),
    lager:debug("LRU expiration checks every ~pms", [ExpirePeriod]),

    {'ok', #state{name=Name
                 ,expire_period=ExpirePeriod
                 ,expire_period_ref=start_expire_period_timer(ExpirePeriod)
                 }}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> {'noreply', state()}.
handle_call(_Req, _From, State) ->
    {'noreply', State}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'update_expire_period', ExpirePeriod}, State) ->
    {'noreply', maybe_update_expire_period(State, ExpirePeriod)};
handle_cast(_Req, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info({'timeout', Ref, ?EXPIRE_PERIOD_MSG}
           ,#state{name=Name
                  ,expire_period_ref=Ref
                  ,expire_period=Period
                  }=State
           ) ->
    _Expired = kz_cache_ets:expire_objects(Name),
    _Expired > 0
        andalso lager:debug("expired objects: ~p", [_Expired]),
    {'noreply', State#state{expire_period_ref=start_expire_period_timer(Period)}};
handle_info(_Msg, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> any().
terminate(_Reason, Name) ->
    ?LOG_INFO("terminating ~p: ~p", [Name, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec lru_name(atom()) -> atom().
lru_name(Name) ->
    kz_term:to_atom(atom_to_list(Name) ++ "_lru", 'true').

-spec start_expire_period_timer(pos_integer()) -> reference().
start_expire_period_timer(ExpirePeriod) ->
    erlang:start_timer(ExpirePeriod, self(), ?EXPIRE_PERIOD_MSG).

-spec maybe_update_expire_period(state(), timeout()) -> state().
maybe_update_expire_period(#state{expire_period=ExpirePeriodMs
                                 ,expire_period_ref=Ref
                                 }=State
                          ,ExpiresS
                          )
  when is_integer(ExpiresS)
       andalso (ExpiresS * ?MILLISECONDS_IN_SECOND) < ExpirePeriodMs
       ->
    ExpiresMs = ExpiresS * ?MILLISECONDS_IN_SECOND,
    ?LOG_INFO("updating expires period to smaller ~p (from ~p)", [ExpiresMs, ExpirePeriodMs]),
    NewRef = case erlang:read_timer(Ref) of
                 Left when Left =< ExpiresMs -> Ref;
                 _Left ->
                     _ = erlang:cancel_timer(Ref),
                     start_expire_period_timer(ExpiresMs)
             end,
    State#state{expire_period=ExpiresMs
               ,expire_period_ref=NewRef
               };
maybe_update_expire_period(State, _Expires) -> State.
