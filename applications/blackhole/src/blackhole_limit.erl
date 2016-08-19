-module(blackhole_limit).
-include("blackhole.hrl").
-behaviour(gen_server).

-export([start_link/0, stop/0, reload/0, ip/1, account/1, message/1, release/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {buckets :: #{}
               ,message_bucket_size
               ,message_bucket_rate
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
message(Pid) -> die(gen_server:call(?MODULE, {'message', Pid}), 'too_fast').

fetch_config(#state{}=S) ->
    BucketSize = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"message_bucket_size">>, 100),
    BucketRate = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"message_bucket_rate">>, 10),
    SourceIpLimit = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"connections_per_source_ip">>, 100),
    AccountLimit = kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"connections_per_account">>, 100),
    S#state{message_bucket_size=BucketSize
           ,message_bucket_rate=BucketRate
           ,connections_per_source_ip=SourceIpLimit
           ,connections_per_account=AccountLimit
           }.

init([]) -> 
    {'ok', fetch_config(#state{ buckets = #{} })}.

handle_call({'message', Pid}, _From, S=#state{buckets=Buckets}) ->
    Bucket = 
        case maps:is_key(Pid, Buckets) of
            'true' ->
                maps:get(Pid, Buckets);
            'false' ->
                {'ok', NewBucket} = kz_token_bucket:start_link(100, 10),
                NewBucket
        end,
    {reply, kz_token_bucket:consume(Bucket, 1), S#state{buckets=Buckets#{ Pid => Bucket }}};
handle_call({'release', #bh_context{source=SrcIp, auth_account_id=AuthAccountId, websocket_pid=Pid}}, _From, S=#state{buckets=Buckets}) ->
    blackhole_counters:dec({?APP_NAME, 'source_ip', SrcIp}),
    blackhole_counters:dec({?APP_NAME, 'account', AuthAccountId}),
    {'reply', 'ok', S#state{buckets=maps:remove(Pid, Buckets)}};
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
