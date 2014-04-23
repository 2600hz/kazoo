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
-export([get_cached/2, get_cached/3, parse_config/1, parse_config/2]).
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, 
         handle_info/2]).
-export([put_log_cache/2]).

-compile({no_auto_import,[put/2]}).

-define(MINUS_CSEQ, 46111468).  % Lower values to debug


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


%% @doc Equivalent to `get_cached(Key, undefined)'.
-spec get_cached(term(), nksip_lib:optslist()) -> 
    Value :: term().

get_cached(Key, CachedList) ->
    case nksip_lib:get_value(Key, CachedList, '$nksip_config_undefined') of
        '$nksip_config_undefined' -> ?MODULE:get(Key);
        Cached -> Cached
    end.


%% @doc Tries to get a value from `CacheList' if it is not present,
%% gets it from config.
-spec get_cached(term(), nksip_lib:optslist(), term()) -> 
    Value :: term().

get_cached(Key, CachedList, Default) ->
    case nksip_lib:get_value(Key, CachedList, '$nksip_config_undefined') of
        '$nksip_config_undefined' -> get(Key, Default);
        Cached -> Cached
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
    nksip_lib:optslist().

default_config() ->
    [
        {timer_t1, 500},                    % (msecs) 0.5 secs
        {timer_t2, 4000},                   % (msecs) 4 secs
        {timer_t4, 5000},                   % (msecs) 5 secs
        {timer_c,  180},                    % (secs) 3min
        {sipapp_timer, 5},                  % Time to refresh SipApp (registrations...)
        {session_expires, 1800},            % (secs) 30 min
        {min_session_expires, 90},          % (secs) 90 secs (min 90, recomended 1800)
        {udp_timeout, 180},                 % (secs) 3 min
        {tcp_timeout, 180},                 % (secs) 3 min
        {sctp_timeout, 180},                % (secs) 3 min
        {ws_timeout, 180},                  % (secs) 3 min
        {nonce_timeout, 30},                % (secs) 30 secs
        {sipapp_timeout, 32},               % (secs) 32 secs  
        {max_calls, 100000},                % Each Call-ID counts as a call
        {max_connections, 1024},            % Per transport and SipApp
        {registrar_default_time, 3600},     % (secs) 1 hour
        {registrar_min_time, 60},           % (secs) 1 miluen
        {registrar_max_time, 86400},        % (secs) 24 hour
        {outbound_time_all_fail, 30},       % (secs)
        {outbound_time_any_ok, 90},         % (secs)
        {outbound_max_time, 1800},          % (secs)
        {dns_cache_ttl, 3600},              % (secs) 1 hour
        {local_data_path, "log"}            % To store UUID
    ].


%% @doc Parses a list of options
-spec parse_config(nksip_lib:optslist()) ->
    {ok, nksip_lib:optslist()} | {error, term()}.

parse_config(Opts) ->
    parse_config_opts(Opts, []).


%% @doc Parses a single config option
-spec parse_config(atom(), term()) ->
    {ok, term()} | {error, term()}.

parse_config(Name, Value) ->
    case parse_config_opts([{Name, Value}], []) of
        {ok, [{_, Value1}]} -> {ok, Value1};
        {error, Error} -> {error, Error}
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
    AppConfig = lists:map(
        fun({Key, Default}) ->
            case application:get_env(nksip, Key) of
                {ok, Value} -> {Key, Value};
                _ -> {Key, Default}
            end
        end,
        default_config()),
    case parse_config(AppConfig) of
        {ok, AppConfig1} ->
            AppConfig2 = nksip_lib:delete(AppConfig1, [local_data_path, dns_cache_ttl]),
            GlobalConfig = [
                {global_id, nksip_lib:luid()},
                {local_ips, nksip_lib:get_local_ips()},
                {main_ip, nksip_lib:find_main_ip()},
                {main_ip6, nksip_lib:find_main_ip(auto, ipv6)},
                {app_config, AppConfig2},
                {max_connections, nksip_lib:get_value(max_connections, AppConfig1)}
            ],
            lists:foreach(
                fun({Key, Value}) -> nksip_config:put(Key, Value) end,
                AppConfig1++GlobalConfig),
            make_cache(GlobalConfig),
            {ok, #state{}};
        {error, Error} ->
            lager:error("Config error: ~p", [Error]),
            {error, config_error}
    end.


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
    erlang:put(nksip_log_level, AppId:config_log_level()),
    erlang:put(nksip_app_name, AppId:name()),
    erlang:put(nksip_call_id, CallId).

%% @private
parse_config_opts([], Opts) ->
    {ok, Opts};

parse_config_opts([Term|Rest], Opts) ->
    try 
        Op = case Term of
            {timer_t1, MSecs} when is_integer(MSecs), MSecs>=10, MSecs=<2500 ->
                update;
            {timer_t2, MSecs} when is_integer(MSecs), MSecs>=100, MSecs=<16000 ->
                update;
            {timer_t4, MSecs} when is_integer(MSecs), MSecs>=100, MSecs=<25000 ->
                update;
            {timer_c, Secs}  when is_integer(Secs), Secs>=1 ->
                update;
            {session_expires, Secs} when is_integer(Secs), Secs>=5 ->
                update;
            {min_session_expires, Secs} when is_integer(Secs), Secs>=1 ->
                update;
            {udp_timeout, Secs} when is_integer(Secs), Secs>=5 ->
                update;
            {tcp_timeout, Secs} when is_integer(Secs), Secs>=5 ->
                update;
            {sctp_timeout, Secs} when is_integer(Secs), Secs>=5 ->
                update;
            {ws_timeout, Secs} when is_integer(Secs), Secs>=5 -> 
                update;
            {nonce_timeout, Secs} when is_integer(Secs), Secs>=5 ->
                update;
            {sipapp_timeout, MSecs} when is_float(MSecs), MSecs>=0.01 ->
                update;
            {sipapp_timeout, Secs} when is_integer(Secs), Secs>=5, Secs=<180 ->
                update;
            {max_calls, Max} when is_integer(Max), Max>=1, Max=<1000000 ->
                update;
            {max_connections, Max} when is_integer(Max), Max>=1, Max=<1000000 ->
                update;
            {registrar_default_time, Secs} when is_integer(Secs), Secs>=5 ->
                update;
            {registrar_min_time, Secs} when is_integer(Secs), Secs>=1 ->
                update;
            {registrar_max_time, Secs} when is_integer(Secs), Secs>=60 ->
                update;
            {dns_cache_ttl, Secs} when is_integer(Secs), Secs>=5 ->
                update;
            {local_data_path, Dir} when is_list(Dir) ->
                Path = filename:join(Dir, "write_test"),
                case file:write_file(Path, <<"test">>) of
                   ok ->
                        case file:delete(Path) of
                            ok -> update;
                            _ -> throw(invalid)
                        end;
                    _ ->
                        throw(invalid)
                end;
            {outbound_time_all_fail, Secs} when is_integer(Secs), Secs>=1 ->
                update;
            {outbound_time_any_ok, Secs} when is_integer(Secs), Secs>=1 ->
                update;
            {outbound_max_time, Secs} when is_integer(Secs), Secs>=1 ->
                update;
            {sipapp_timer, Secs} when is_integer(Secs), Secs>=1 ->
                update;
            _ ->
                throw(invalid)
        end,
        case Op of
            update -> 
                Opts1 = nksip_lib:store_value(Term, Opts),
                parse_config_opts(Rest, Opts1)
        end
    catch
        throw:invalid when is_tuple(Term) -> 
            {error, {invalid, element(1, Term)}};
        throw:invalid ->
            {error, {invalid, Term}}
    end.


%% @private
make_cache(Config) ->
    Syntax = lists:foldl(
        fun({Key, Value}, Acc) -> [nksip_code_util:getter(Key, Value)|Acc] end,
        [],
        Config),
    ok = nksip_code_util:compile(nksip_config_cache, Syntax).

