%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc NkSIP In-Memory Database.
%%
%% This module implements an ETS-based database used for registrations
%% and caches.
%% It is capable of timed auto expiring of records, server-side update funs 
%% and calling an user fun on record expire.

-module(nksip_store).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(gen_server).

-export([put/2, put/3, put_dirty/2, update/2, update/3]).
-export([get/1, get/2, del/1]).
-export([pending/0, fold/2]).
-export([start_link/0, stop/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

-include("nksip.hrl").

-define(STORE_TIMER, 5000).

%% ===================================================================
%% Public
%% ===================================================================


%% @doc Equivalent to `get(Key, not_found)'.
-spec get(term()) -> term() | not_found.
get(Key) -> get(Key, not_found).


%% @doc Gets a record from database using `Default' if not found
-spec get(term(), term()) -> term().
get(Key, Default) ->
    case ets:lookup(nksip_store, Key) of
        [] -> Default;
        [{_, Value, _Expire, _Fun}] -> Value
    end.


%% @doc Equivalent to `put(Key, Value, [])'.
-spec put(term(), term()) -> ok.
put(Key, Value) -> put(Key, Value, []).


%% @doc Inserts a `Value' in database under `Key'.
%% If `ttl' option is used (seconds), record will be deleted after this time.
%% If `notify' options is used, function will be called on record's delete.
-spec put(term(), term(), [Opt]) -> ok
    when Opt :: {ttl, integer()} | {notify, function()}.
put(Key, Value, Opts) when is_list(Opts) ->
    gen_server:call(?MODULE, {put, Key, Value, Opts}).


%% @private
-spec put_dirty(term(), term()) -> true.
put_dirty(Key, Value) ->
    ets:insert(nksip_store, {Key, Value, 0, none}).


%% @doc Equivalent to `update(Key, Fun, [])'.
-spec update(term(), function()) -> 
    {ok, FunResult::term()} | {error, Error::term()}.
update(Key, Fun) ->
    update(Key, Fun, []).


%% @doc Updates a record in database, applying `Fun' to the old record 
%% to get the new value. 
%% If no record is found, old value would be `[]'. 
%% If the new generated value is `[]' record will be deleted.
%% See {@link put/3} for options.
-spec update(term(), function(), nksip:optslist()) -> 
    {ok, FunResult::term()} | {error, Error::term()}.
update(Key, Fun, Opts) when is_function(Fun, 1), is_list(Opts) ->
    gen_server:call(?MODULE, {update, Key, Fun, Opts}).


%% @doc Deletes a record from database.
-spec del(term()) -> ok | not_found.
del(Key) -> gen_server:call(?MODULE, {del, Key}).


%% @doc Folds over all records in database like `lists:foldl/3'.
%% UserFun will be called as `UserFun(Key, Value, Acc)' for each record.
-spec fold(function(), term()) -> term().
fold(UserFun, Acc0) when is_function(UserFun, 3) ->
    Fun = fun({Key, Value, _Exp, _ExpFun}, Acc) -> UserFun(Key, Value, Acc) end,
    ets:foldl(Fun, Acc0, nksip_store).


%% @private Gets all records with pending expiration
pending() ->
    gen_server:call(?MODULE, get_pending).



%% ===================================================================
%% gen_server
%% ===================================================================


-record(state, {
}).


%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @private
stop() ->
    gen_server:call(?MODULE, stop).


%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([]) ->
    ets:new(nksip_store, [public, named_table]),
    ets:new(nksip_store_ord, [protected, ordered_set, named_table]),
    erlang:start_timer(timeout(), self(), timer),
    {ok, #state{}}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call({put, Key, Value, Opts}, _From, State) ->
    case ets:lookup(nksip_store, Key) of
        [{_, _OldValue, OldExpire, _Fun}] when OldExpire > 0 ->
            ets:delete(nksip_store_ord, {ttl, OldExpire, Key});
        _ -> 
            ok
    end,
    case lists:keyfind(notify, 1, Opts) of
        {notify, ExpFun} when is_function(ExpFun, 1) -> ok;
        _ -> ExpFun = none
    end,
    case lists:keyfind(ttl, 1, Opts) of
        {ttl, TTL} when is_integer(TTL), TTL > 0 ->
            Expire = nksip_lib:timestamp() + TTL,
            ets:insert(nksip_store, {Key, Value, Expire, ExpFun}),
            ets:insert(nksip_store_ord, {{ttl, Expire, Key}, none});
        _ ->
            ets:insert(nksip_store, {Key, Value, 0, ExpFun})
    end,
    {reply, ok, State};

handle_call({update, Key, UpdateFun, Opts}, From, State) ->
    case ets:lookup(nksip_store, Key) of
        [] -> OldValue = [], OldExpire = 0;
        [{_, OldValue, OldExpire, _Fun}] -> ok
    end,
    case catch UpdateFun(OldValue) of
        {'EXIT', Error} -> 
            {reply, {error, Error}, State};
        Value ->
            case OldExpire of
                0 -> ok;
                _ -> ets:delete(nksip_store_ord, {ttl, OldExpire, Key})
            end,
            case Value of
                [] ->
                    {reply, _, _ } = handle_call({del, Key}, From, State);
                _ ->
                    case lists:keyfind(notify, 1, Opts) of
                        {notify, ExpFun} when is_function(ExpFun, 1) -> ok;
                        _ -> ExpFun = none
                    end,
                    case lists:keyfind(ttl, 1, Opts) of
                        {ttl, TTL} when is_integer(TTL), TTL > 0 ->
                            Expire = nksip_lib:timestamp() + TTL,
                            ets:insert(nksip_store, {Key, Value, Expire, ExpFun}),
                            ets:insert(nksip_store_ord, {{ttl, Expire, Key}, none});
                        _ ->
                            ets:insert(nksip_store, {Key, Value, 0, ExpFun})
                    end
            end,
            {reply, {ok, Value}, State}
    end;

handle_call({del, Key}, _From, State) ->
    case ets:lookup(nksip_store, Key) of
        [] ->
            {reply, not_found, State};
        [{_, _, Expire, ExpFun}] ->
            case is_function(ExpFun, 1) of
                true -> proc_lib:spawn(fun() -> ExpFun(Key) end);
                _ -> ok
            end,
            ets:delete(nksip_store, Key),
            case Expire of
                0 -> ok;
                _ -> ets:delete(nksip_store_ord, {ttl, Expire, Key})
            end,
            {reply, ok, State}
    end;

handle_call(get_pending, _From, State) ->
    Now = nksip_lib:timestamp(),
    Pending = pending_iter(ets:first(nksip_store_ord), Now, []),
    {reply, Pending, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Msg, _From, State) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast(Msg, State) -> 
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info({timeout, _, timer}, State) -> 
    Self = self(),
    proc_lib:spawn(
        fun() -> 
            Now = nksip_lib:timestamp(),
            Last = ets:prev(nksip_store_ord, {ttl, Now, 0}),
            delete_expired_iter(Last),
            erlang:start_timer(timeout(), Self, timer)
        end),
    {noreply, State};

handle_info(Info, State) -> 
    lager:warning("Module ~p received unexpected info: ~p", [?MODULE, Info]),
    {noreply, State}.


%% @private
-spec code_change(term(), #state{}, term()) ->
    gen_server_code_change(#state{}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    gen_server_terminate().

terminate(_Reason, _State) ->  
    ok.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
-spec timeout() -> integer().
timeout() ->
    nksip_config:get(nksip_store_timer, ?STORE_TIMER).

%% @private
delete_expired_iter('$end_of_table') ->
    ok;
delete_expired_iter({ttl, _Time, Key}=Last) ->
    del(Key),
    delete_expired_iter(ets:prev(nksip_store_ord, Last)).

%% @private
pending_iter('$end_of_table', _Now, Acc) ->
    Acc;
pending_iter({ttl, Time, Key}=Current, Now, Acc) ->
    Next = ets:next(nksip_store_ord, Current),
    pending_iter(Next, Now, [{Key, Time-Now}|Acc]).



%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile({no_auto_import, [get/1, put/2]}).

basic_test_() ->
    {setup, 
        fun() -> 
            ?debugFmt("Starting ~p", [?MODULE]),
            case start_link() of
                {error, {already_started, _}} ->
                    ok;
                {ok, _} ->
                    do_stop
            end
        end,
        fun(Stop) -> 
            case Stop of 
                do_stop -> stop();
                ok -> ok 
            end
        end,
        [
            {timeout, 60, fun normal_insert/0},
            {timeout, 60, fun ttl_insert/0}
        ]
    }.

normal_insert() ->
    ok = put(k1, v11),
    ok = put("k2", "v12"),
    ok = put(<<"k3">>, <<"v13">>),
    ?assertMatch(v11, get(k1)),
    ?assertMatch("v12", get("k2")),
    ?assertMatch(<<"v13">>, get(<<"k3">>)),
    ?assertMatch(not_found, get(k2)),
    ?assertMatch(not_found, get(k3)),
    ok = put(k1, "v11b"),
    ?assertMatch("v11b", get(k1)),
    ok = del("k2"),
    ?assertMatch(not_found, get("k2")),
    Self = self(),
    Fun = fun(k4) -> Self ! k4 end,
    ok = put(k4, v4, [{notify, Fun}]),
    ?assertMatch(v4, get(k4)),
    ok = del(k4),
    ?assertMatch(not_found, get(k4)),
    ok = receive k4 -> ok after 500 -> error end,
    not_found = del(non_existent),
    ok.

ttl_insert() ->
    ok = put(k1, v21a),
    ?assertMatch(v21a, get(k1)),
    ?assertMatch(false, is_pending(k1)),
    ok = put(k1, v21b, [{ttl, 2}]),
    ?assertMatch(v21b, get(k1)),
    ?assertMatch(true, is_pending(k1)),
    ok = put(k1, v21c, [{ttl, 3}]),
    ?assertMatch(v21c, get(k1)),
    ?assertMatch(true, is_pending(k1)),
    ok = put(k1, v21d, [{ttl, 3}]),
    ?assertMatch(v21d, get(k1)),
    ?assertMatch(true, is_pending(k1)),
    ok = put(k1, v21e),
    ?assertMatch(v21e, get(k1)),
    ?assertMatch(false, is_pending(k1)),
    ok = put(k1, v21f, [{ttl, 10}]),
    ?assertMatch(v21f, get(k1)),
    ?assertMatch(true, is_pending(k1)),
    ok = del(k1),
    ?assertMatch(not_found, get(k1)),
    ?assertMatch(false, is_pending(k1)),
    Self = self(),
    Fun = fun(k1) -> Self ! k1 end,
    ok = put(k1, v21g, [{ttl, 1}, {notify, Fun}]),
    ?assertMatch(v21g, get(k1)),
    ?assertMatch(true, is_pending(k1)),
    ?debugMsg("Waiting store timeout"),
    ok = receive k1 -> ok after 10000 -> error end,
    ?assertMatch(not_found, get(k1)),
    ?assertMatch(false, is_pending(k1)),
    ok.

is_pending(Key) ->
    lists:keymember(Key, 1, pending()).




-endif.









