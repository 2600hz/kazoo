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

%% @doc NkSIP Config Server.
-module(nksip_config).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-include("nksip.hrl").

-export([get/1, get/2, put/2, del/1, cseq/0, increment/2]).
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, 
         handle_info/2]).
-export([make_cache/0, put_log_cache/2]).

-compile({no_auto_import,[put/2]}).

-define(MINUS_CSEQ, 46111468).  % Generate lower values to debug
-define(RE_CALL_ID, "\r\n\s*(i|call\-id)\s*:\s*(.*?)\s*\r\n").
-define(RE_CONTENT_LENGTH, "\r\n\s*(l|content-length)\s*:\s*(.*?)\s*\r\n").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Equivalent to `get(Key, undefined)'.
-spec get(term()) -> 
    Value :: term().

get(Key) ->
    get(Key, undefined).


%% @doc Gets an config value.
-spec get(term(), term()) -> 
    Value :: term().

get(Key, Default) -> 
    case ets:lookup(?MODULE, Key) of
        [] -> Default;
        [{_, Value}] -> Value
    end.


%% @doc Sets a config value.
-spec put(term(), term()) -> 
    ok.

put(Key, Val) -> 
    true = ets:insert(?MODULE, {Key, Val}),
    ok.


%% @doc Deletes a config value.
-spec del(term()) -> 
    ok.

del(Key) -> 
    true = ets:delete(?MODULE, Key),
    ok.


%% @doc Gets a new `CSeq'.
%% After booting, CSeq's counter is set using {@link nksip_lib:cseq/0}. Then each call 
%% to this function increments the counter by one.
-spec cseq() -> 
    nksip:cseq().

cseq() ->
    ets:update_counter(?MODULE, current_cseq, 1).


%% @doc Atomically increments or decrements a counter
-spec increment(term(), integer()) ->
    integer().

increment(Key, Count) ->
    ets:update_counter(?MODULE, Key, Count).


%% @private Default config values
-spec default_config() ->
    nksip:optslist().

default_config() ->
    [
        {global_max_calls, 100000},         % Each Call-ID counts as a call
        {global_max_connections, 1024},     % 
        {dns_cache_ttl, 3600},              % (secs) 1 hour
        {local_data_path, "log"},           % To store UUID
        {sync_call_time, 30},               % Synchronous call timeout
        {msg_routers, 16}                   % Number of parallel msg routers 
    ].


%% @private 
-spec make_cache() ->
    ok.

make_cache() ->
    case parse_config(application:get_all_env(nksip), []) of
        {ok, EnvConfig1} ->
            EnvConfig2 = nksip_lib:defaults(EnvConfig1, default_config()),
            GlobalOpts = [Key || {Key, _} <- default_config()],
            AppConfig = nksip_lib:delete(EnvConfig2, 
                                         [included_applications|GlobalOpts]),
            CacheConfig = [
                {global_id, nksip_lib:luid()},
                {local_ips, nksip_lib:get_local_ips()},
                {main_ip, nksip_lib:find_main_ip()},
                {main_ip6, nksip_lib:find_main_ip(auto, ipv6)},
                {sync_call_time, 
                    1000*nksip_lib:get_value(sync_call_time, EnvConfig2)},
                {dns_cache_ttl, 
                    nksip_lib:get_value(dns_cache_ttl, EnvConfig2)},
                {local_data_path, 
                    nksip_lib:get_value(local_data_path, EnvConfig2)},
                {global_max_connections, 
                    nksip_lib:get_value(global_max_connections, EnvConfig2)},
                {global_max_calls, 
                    nksip_lib:get_value(global_max_calls, EnvConfig2)},
                {msg_routers, 
                    nksip_lib:get_value(msg_routers, EnvConfig2)},
                {re_call_id, 
                    element(2, re:compile(?RE_CALL_ID, [caseless]))},
                {re_content_length, 
                    element(2, re:compile(?RE_CONTENT_LENGTH, [caseless]))},
                {app_config, AppConfig}
            ],
            make_cache(CacheConfig),
            ok;
        {error, Error} ->
            lager:error("Config error: ~p", [Error]),
            error(config_error)
    end.



%% ===================================================================
%% gen_server
%% ===================================================================

-record(state, {
}).


%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
        

%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([]) ->
    ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
    ?MODULE:put(current_cseq, nksip_lib:cseq()-?MINUS_CSEQ),
    CacheConfig = [
        global_id, local_ips, main_ip, main_ip6, sync_call_time,
        dns_cache_ttl, local_data_path, global_max_connections, 
        global_max_calls, msg_routers, app_config
    ],
    lists:foreach(
        fun(Key) -> nksip_config:put(Key, nksip_config_cache:Key()) end,
        CacheConfig),
    {ok, #state{}}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

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
%% Private
%% ===================================================================


%% @private Save cache for speed log access
put_log_cache(AppId, CallId) ->
    erlang:put(nksip_app_id, AppId),
    erlang:put(nksip_call_id, CallId),
    erlang:put(nksip_app_name, AppId:name()),
    erlang:put(nksip_log_level, AppId:config_log_level()).


%% @private
parse_config([], Opts) ->
    {ok, Opts};

parse_config([Term|Rest], Opts) ->
    Op = case Term of
        {sync_call_time, Secs} ->
            case is_integer(Secs) andalso Secs>=1 of
                true -> update;
                false -> error
            end;
        {dns_cache_ttl, Secs} ->
            case is_integer(Secs) andalso Secs>=5 of
                true -> update;
                false -> error
            end;
        {local_data_path, Dir} ->
            case is_list(Dir) andalso filename:join(Dir, "write_test") of
                false ->
                    error;
                Path ->
                    case file:write_file(Path, <<"test">>) of
                       ok ->
                            case file:delete(Path) of
                                ok -> update;
                                _ -> error
                            end;
                        _ ->
                            error
                    end
            end;
        {global_max_calls, Max} ->
            case is_integer(Max) andalso Max>=1 andalso Max=<1000000 of
                true -> update;
                false -> error
            end;
        {global_max_connections, Max} ->
            case is_integer(Max) andalso Max>=1 andalso Max=<1000000 of
                true -> update;
                false -> error
            end;
        {msg_routers, Routers} ->
            case is_integer(Routers) andalso Routers>=1 andalso Routers=<127 of
                true -> update;
                false -> error
            end;
        _ ->
            update
    end,
    case Op of
        update -> 
            Opts1 = nksip_lib:store_value(Term, Opts),
            parse_config(Rest, Opts1);
        error when is_tuple(Term) ->
            {error, {invalid, element(1, Term)}};
        error ->
            {error, {invalid, Term}}
    end.


%% @private
make_cache(Config) ->
    Syntax = lists:foldl(
        fun({Key, Value}, Acc) -> [nksip_code_util:getter(Key, Value)|Acc] end,
        [],
        Config),
    {ok, Tree} = nksip_code_util:compile(nksip_config_cache, Syntax),
    ok = nksip_code_util:write(nksip_config_cache, Tree).

