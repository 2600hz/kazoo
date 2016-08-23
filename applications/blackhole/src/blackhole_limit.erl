%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(blackhole_limit).
-include("blackhole.hrl").
-behaviour(gen_server).

-export([start_link/0, stop/0, reload/0, ip/1, account/1, conn_rate/1, acc_rate/1, release/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {buckets :: #{}
               ,conn_bs, conn_br
               ,acc_bs, acc_br
               ,connections_per_source_ip
               ,connections_per_account
               }).

%% API
start_link() -> gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:cast(?MODULE, {'stop'}).
reload() -> gen_server:cast(?MODULE, {'reload'}).
release(Context) -> gen_server:call(?MODULE, {'release', Context}).
ip(Ip) -> die(gen_server:call(?MODULE, {ip, Ip}), 'too_many_connections_per_ip').
account(Account) -> die(gen_server:call(?MODULE, {'account', Account}), 'too_many_connections_per_account').
conn_rate(Pid) -> die(gen_server:call(?MODULE, {'conn_rate', Pid}), 'connection_too_fast').
acc_rate(Pid) -> die(gen_server:call(?MODULE, {'acc_rate', Pid}), 'account_too_fast').

fetch_config(#state{}=S) ->
    ConnBs = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"connection_bucket_size">>, 100),
    ConnBr = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"connecttion_bucket_rate">>, 10),
    AccBs = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"account_bucket_size">>, 100),
    AccBr = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"account_bucket_rate">>, 10),
    SourceIpLimit = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"connections_per_source_ip">>, 100),
    AccountLimit = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"connections_per_account">>, 100),
    S#state{conn_bs=ConnBs
           ,conn_br=ConnBr
           ,acc_bs=AccBs
           ,acc_br=AccBr
           ,connections_per_source_ip=SourceIpLimit
           ,connections_per_account=AccountLimit
           }.

init([]) ->
    {'ok', fetch_config(#state{ buckets = #{} })}.

handle_call({'conn_rate', Key}, _From, S=#state{buckets=Buckets, conn_bs=Bs, conn_br=Br}) ->
    Bucket = get_bucket(Key, Buckets, Bs, Br),
    {reply, kz_token_bucket:consume(Bucket, 1), S#state{buckets=Buckets#{ Key => Bucket }}};
handle_call({'acc_rate', Key}, _From, S=#state{buckets=Buckets, acc_bs=Bs, acc_br=Br}) ->
    Bucket = get_bucket(Key, Buckets, Bs, Br),
    {reply, kz_token_bucket:consume(Bucket, 1), S#state{buckets=Buckets#{ Key => Bucket }}};
handle_call({'release', #bh_context{auth_account_id=AuthAccountId, websocket_pid=Pid}}, _From, S=#state{buckets=Buckets}) ->
    %%% XXX: blackhole_counters:dec({?APP_NAME, 'source_ip', SrcIp}),
    blackhole_counters:dec({?APP_NAME, 'account', AuthAccountId}),
    {'reply', 'ok', S#state{buckets=maps:remove(Pid, maps:remove(AuthAccountId, Buckets))}};
handle_call({'ip', Ip}, _From, #state{connections_per_source_ip=Limit} = S) ->
    {'reply', blackhole_counters:inc({?APP_NAME, 'source_ip', Ip}) < Limit, S};
handle_call({'account', AccountId}, _From, #state{connections_per_source_ip=Limit} = S) ->
    {'reply', blackhole_counters:inc({?APP_NAME, 'account', AccountId}) < Limit, S};
handle_call(_Request, _From, S=#state{}) -> {'reply', 'ok', S}.

handle_cast({'reload'}, S=#state{}) ->
    {'noreply', fetch_config(S)};
handle_cast({'stop'}, S=#state{}) ->
    {'stop', 'normal', S};
handle_cast(_Msg, S=#state{}) -> {'noreply', S}.

handle_info(_Info, S=#state{}) -> {'noreply', S}.
terminate(_Reason, _S) -> 'ok'.
code_change(_OldVsn, S=#state{}, _Extra) -> {'ok', S}.

die('true', _Reason) -> 'true';
die('false', Reason) -> erlang:error(Reason).

get_bucket(Key, Buckets, Bs, Br) ->
    case maps:is_key(Key, Buckets) of
        'true' ->
            maps:get(Key, Buckets);
        'false' ->
            {'ok', NewBucket} = kz_token_bucket:start_link(Bs, Br),
            NewBucket
    end.
