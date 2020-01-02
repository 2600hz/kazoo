%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Listen for CDR events and record them to the database
%%% @author James Aimonetti
%%% @author Edouard Swiac
%%% @author Ben Wann
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cdr_listener).
-behaviour(gen_listener).

%% API
-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("cdr.hrl").
-define(VIEW_TO_UPDATE, <<"interactions/interaction_listing">>).
-define(REFRESH_THRESHOLD, kapps_config:get_integer(?CONFIG_CAT, <<"refresh_view_threshold">>, 10)).
-define(REFRESH_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, <<"refresh_view_timeout">>, 900)).
-define(REFRESH_ENABLED, kapps_config:get_is_true(?CONFIG_CAT, <<"refresh_view_enabled">>, 'false')).

-record(state, {counter = #{} :: map()}).
-type state() :: #state{}.
-type counter_element() :: {kz_time:gregorian_seconds(), non_neg_integer()}.

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{'cdr_channel_destroy'
                     ,[{<<"call_event">>, <<"CHANNEL_DESTROY">>}]
                     }
                    ]).
-define(BINDINGS, [{'call'
                   ,[{'restrict_to', ['CHANNEL_DESTROY']}]
                   }
                  ]).
-define(QUEUE_NAME, <<"cdr_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    lager:info("cdr refresher threshold: ~p, timeout: ~p", [?REFRESH_THRESHOLD, ?REFRESH_TIMEOUT]),
    erlang:send_after(?REFRESH_TIMEOUT * ?MILLISECONDS_IN_SECOND, self(), 'timeout'),
    {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('timeout', #state{counter = Counter} = State) ->
    _ = erlang:send_after(?REFRESH_TIMEOUT * ?MILLISECONDS_IN_SECOND, self(), 'timeout'),
    case ?REFRESH_ENABLED of
        'false' -> {'noreply', State};
        'true' ->
            {'noreply', State#state{counter = refresh_views(Counter)}}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, #state{counter = Counter} = State) ->
    case ?REFRESH_ENABLED of
        'false' -> {'reply', []};
        'true' ->
            AccountId = kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
            {'reply', [], State#state{counter = handle_account(Counter, AccountId)}}
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
    lager:debug("cdr listener terminating: ~p", [_Reason]).

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
-spec refresh_views(map()) -> map().
refresh_views(Counter) ->
    Now = kz_time:now_s(),
    RefreshTimeout = ?REFRESH_TIMEOUT,
    HandleTimeout =
        fun(AccountId, {Map, List}) ->
                {From, Count} = maps:get(AccountId, Map),
                case Now - From > RefreshTimeout
                    andalso Count > 0 of
                    true -> { Map#{ AccountId => {Now, 0} }, [{AccountId, From, Now} | List] };
                    false -> {Map, List}
                end
        end,
    {NewCounter, RefreshList} = lists:foldl(HandleTimeout, {Counter, []}, maps:keys(Counter)),
    _ = maybe_spawn_refresh_process(RefreshList),
    NewCounter.

-spec handle_account(map(), kz_term:api_binary()) -> map().
handle_account(Counter, 'undefined') -> Counter;
handle_account(Counter, AccountId) ->
    RefreshThreshold = ?REFRESH_THRESHOLD,
    Count = case value(maps:find(AccountId, Counter)) of
                {From, Value} when Value >= RefreshThreshold ->
                    _ = kz_process:spawn(fun update_account_view/3, [AccountId, From, kz_time:now_s()]),
                    {kz_time:now_s(), 0};
                {From, Value} -> {From, Value + 1}
            end,
    Counter#{ AccountId => Count }.

-spec value({'ok', counter_element()} | 'error') -> counter_element().
value({ok, Value}) -> Value;
value(_) -> {kz_time:now_s(), 0}.

-spec update_account_view(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) -> any().
update_account_view(AccountId, From, To) ->
    Dbs = kazoo_modb:get_range(AccountId, From, To),
    lager:info("refresh cdr view account:~p dbs:~p", [AccountId, Dbs]),
    [ kz_datamgr:get_results(Db, ?VIEW_TO_UPDATE, [{'limit', 1}]) || Db <- Dbs ].

-spec update_accounts_view([{kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()}]) -> any().
update_accounts_view(UpdateList) ->
    [ update_account_view(AccountId, From, To) || {AccountId, From, To} <- UpdateList ].

-spec maybe_spawn_refresh_process([{kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()}]) -> any().
maybe_spawn_refresh_process([]) -> ok;
maybe_spawn_refresh_process(RefreshList) ->
    _ = kz_process:spawn(fun update_accounts_view/1, [RefreshList]).
