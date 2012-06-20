%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%% When connecting to a FreeSWITCH node, we create three processes: one to
%%% handle authentication (directory) requests; one to handle route (dialplan)
%%% requests, and one to monitor the node and various stats about the node.
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_nodes).

-behaviour(gen_server).

-export([start_link/0]).
-export([connected/0]).
-export([all_nodes_connected/0]).
-export([add/1, add/2, add/3]).
-export([remove/1]).
-export([is_node_up/1]).

-export([account_summary/1]).

-export([show_channels/0]).
-export([new_channel/2]).
-export([channel_node/1]).
-export([channel_account_summary/1]).
-export([channel_match_presence/1]).
-export([channel_exists/1]).
-export([channel_import_moh/1]).
-export([channel_set_account_id/2]).
-export([channel_set_account_billing/2]).
-export([channel_set_reseller_id/2]).
-export([channel_set_reseller_billing/2]).
-export([channel_set_authorizing_id/2]).
-export([channel_set_resource_id/2]).
-export([channel_set_authorizing_type/2]).
-export([channel_set_owner_id/2]).
-export([channel_set_presence_id/2]).
-export([channel_set_import_moh/2]).
-export([fetch_channel/1]).
-export([destroy_channel/2]).
-export([props_to_channel_record/2]).
-export([channel_record_to_json/1]).
-export([sync_channels/0, sync_channels/1]).
-export([flush_node_channels/1]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).
-define(EXPIRE_CHECK, 60000).

-record(node, {node = 'undefined' :: atom()
               ,cookie = 'undefined' :: atom()
               ,options = [] :: proplist()
              }).

-record(astats, {billing_ids=[]
                 ,using_res=[]
                 ,outbound_bridges=[]
                 ,outbound_fr=0
                 ,inbound_fr=0
                 ,outbound_pm=0
                 ,inbound_pm=0
                 ,calls_using_res=0
                }).

-record(state, {nodes = [] :: [#node{},...] | []
                ,preconfigured_lookup :: pid()
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% returns ok or {error, some_error_atom_explaining_more}
-spec add/1 :: (atom()) -> 'ok' | {'error', 'no_connection'}.
-spec add/2 :: (atom(), proplist() | atom()) -> 'ok' | {'error', 'no_connection'}.
-spec add/3 :: (atom(), atom(), proplist() | atom()) -> 'ok' | {'error', 'no_connection'}.

add(Node) -> 
    add(Node, []).

add(Node, Opts) when is_list(Opts) ->
    add(Node, erlang:get_cookie(), Opts);
add(Node, Cookie) when is_atom(Cookie) ->
    add(Node, Cookie, []).

add(Node, Cookie, Opts) ->
    gen_server:call(?MODULE, {add_fs_node, Node, Cookie, Opts}, 30000).

%% returns ok or {error, some_error_atom_explaining_more}
-spec remove/1 :: (atom()) -> 'ok'.
remove(Node) ->
    gen_server:cast(?MODULE, {rm_fs_node, Node}).

-spec connected/0 :: () -> [atom(),...] | [].
connected() ->
    gen_server:call(?MODULE, connected_nodes).

-spec is_node_up/1 :: (atom()) -> boolean().
is_node_up(Node) ->
    gen_server:call(?MODULE, {is_node_up, Node}).

-spec all_nodes_connected/0 :: () -> boolean().
all_nodes_connected() ->
    length(ecallmgr_config:get(<<"fs_nodes">>, [])) =:= length(connected()).

-spec account_summary/1 :: (ne_binary()) -> wh_json:json_object().
account_summary(AccountId) ->
    Channels = channel_account_summary(AccountId),
    summarize_account_usage(Channels).

-spec show_channels/0 :: () -> wh_json:json_objects().
show_channels() ->
    ets:foldl(fun(Channel, Acc) ->
                      [channel_record_to_json(Channel) | Acc]
              end, [], ecallmgr_channels).

-spec new_channel/2 :: (proplist(), atom()) -> 'ok'.
new_channel(Props, Node) ->
    CallId = props:get_value(<<"Unique-ID">>, Props),
    put(callid, CallId),
    gen_server:cast(?MODULE, {new_channel, props_to_channel_record(Props, Node)}),
    ecallmgr_call_control:add_leg(Props),
    Authorized = ecallmgr_authz:maybe_authorize_channel(Props, Node),
    wh_cache:store_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId), Authorized).

-spec fetch_channel/1 :: (ne_binary()) -> {'ok', wh_json:json_object()} |
                                          {'error', 'not_found'}.
fetch_channel(UUID) ->
    case ets:lookup(ecallmgr_channels, UUID) of
        [Channel] -> {ok, channel_record_to_json(Channel)};
        _Else -> {error, not_found}
    end.

-spec channel_node/1 :: (ne_binary()) -> {'ok', atom()} | {'error', _}.
channel_node(UUID) ->
    MatchSpec = [{#channel{uuid = '$1', node = '$2'}
                  ,[{'=:=', '$1', {const, UUID}}]
                  ,['$2']}
                ],  
    case ets:select(ecallmgr_channels, MatchSpec) of
        [Node] -> {ok, Node};
        _ -> {error, not_found}
    end.
        

-spec channel_exists/1 :: (ne_binary()) -> boolean().
channel_exists(UUID) ->
    ets:member(ecallmgr_channels, UUID).

-spec channel_import_moh/1 :: (ne_binary()) -> boolean().
channel_import_moh(UUID) ->
    try ets:lookup_element(ecallmgr_channels, UUID, #channel.import_moh) of
        Import -> Import
    catch
        error:badarg -> false
    end.

-type channel_summary() :: {ne_binary(), ne_binary(), 'undefined' | ne_binary(), 'undefined' | ne_binary(), 'undefined' | ne_binary(), ne_binary()}.
-spec channel_account_summary/1 :: (ne_binary()) -> [channel_summary(),...].
channel_account_summary(AccountId) ->
    MatchSpec = [{#channel{direction = '$1', account_id = '$2', account_billing = '$7'
                           ,authorizing_id = '$3', resource_id = '$4', billing_id = '$5'
                           ,bridge_id = '$6'
                          }
                  ,[{'=:=', '$2', {const, AccountId}}]
                  ,[{{'$1', '$5', '$3', '$6', '$4', '$7'}}]}
                ],  
    ets:select(ecallmgr_channels, MatchSpec).    
 
-spec channel_match_presence/1 :: (ne_binary()) -> [{ne_binary(), atom()},...] | [].
channel_match_presence(PresenceId) ->
    MatchSpec = [{#channel{uuid = '$1', presence_id = '$2', node = '$3'}
                  ,[{'=:=', '$2', {const, PresenceId}}]
                  ,[{{'$1', '$3'}}]}
                ],  
    ets:select(ecallmgr_channels, MatchSpec).

-spec channel_set_account_id/2 :: (ne_binary(), string() | ne_binary()) -> 'ok'.    
channel_set_account_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.account_id, Value}});
channel_set_account_id(UUID, Value) ->
    channel_set_account_id(UUID, wh_util:to_binary(Value)). 

-spec channel_set_account_billing/2 :: (ne_binary(), string() | ne_binary()) -> 'ok'.    
channel_set_account_billing(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.account_billing, Value}});
channel_set_account_billing(UUID, Value) ->
    channel_set_account_billing(UUID, wh_util:to_binary(Value)). 

-spec channel_set_reseller_id/2 :: (ne_binary(), string() | ne_binary()) -> 'ok'.    
channel_set_reseller_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.reseller_id, Value}});
channel_set_reseller_id(UUID, Value) ->
    channel_set_reseller_id(UUID, wh_util:to_binary(Value)). 

-spec channel_set_reseller_billing/2 :: (ne_binary(), string() | ne_binary()) -> 'ok'.    
channel_set_reseller_billing(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.reseller_billing, Value}});
channel_set_reseller_billing(UUID, Value) ->
    channel_set_reseller_billing(UUID, wh_util:to_binary(Value)). 

-spec channel_set_resource_id/2 :: (ne_binary(), string() | ne_binary()) -> 'ok'.
channel_set_resource_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.resource_id, Value}});
channel_set_resource_id(UUID, Value) ->
    channel_set_resource_id(UUID, wh_util:to_binary(Value)).

-spec channel_set_authorizing_id/2 :: (ne_binary(), string() | ne_binary()) -> 'ok'.    
channel_set_authorizing_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.authorizing_id, Value}});
channel_set_authorizing_id(UUID, Value) ->
    channel_set_authorizing_id(UUID, wh_util:to_binary(Value)). 

-spec channel_set_authorizing_type/2 :: (ne_binary(), string() | ne_binary()) -> 'ok'.    
channel_set_authorizing_type(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.authorizing_type, Value}});
channel_set_authorizing_type(UUID, Value) ->
    channel_set_authorizing_type(UUID, wh_util:to_binary(Value)). 

-spec channel_set_owner_id/2 :: (ne_binary(), string() | ne_binary()) -> 'ok'.    
channel_set_owner_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.owner_id, Value}});
channel_set_owner_id(UUID, Value) ->
    channel_set_owner_id(UUID, wh_util:to_binary(Value)).

-spec channel_set_presence_id/2 :: (ne_binary(), string() | ne_binary()) -> 'ok'.    
channel_set_presence_id(UUID, Value) when is_binary(Value) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.presence_id, Value}});
channel_set_presence_id(UUID, Value) ->
    channel_set_presence_id(UUID, wh_util:to_binary(Value)). 

-spec channel_set_import_moh/2 :: (ne_binary(), boolean()) -> 'ok'.                                                          
channel_set_import_moh(UUID, Import) ->
    gen_server:cast(?MODULE, {channel_update, UUID, {#channel.import_moh, Import}}).    

-spec destroy_channel/2 :: (proplist(), atom()) -> 'ok'.
destroy_channel(Props, _) ->
    UUID = props:get_value(<<"Unique-ID">>, Props),
    gen_server:cast(?MODULE, {destroy_channel, UUID}),
    ecallmgr_call_control:rm_leg(Props),
    ecallmgr_call_events:publish_channel_destroy(Props).

-spec props_to_channel_record/2 :: (proplist(), atom()) -> #channel{}.
props_to_channel_record(Props, Node) ->
    #channel{uuid=props:get_value(<<"Unique-ID">>, Props)
             ,destination=props:get_value(<<"Caller-Destination-Number">>, Props)
             ,direction=props:get_value(<<"Call-Direction">>, Props)
             ,account_id=props:get_value(?GET_CCV(<<"Account-ID">>), Props)
             ,account_billing=props:get_value(?GET_CCV(<<"Account-Billing">>), Props)
             ,authorizing_id=props:get_value(?GET_CCV(<<"Authorizing-ID">>), Props)
             ,authorizing_type=props:get_value(?GET_CCV(<<"Authorizing-Type">>), Props)
             ,owner_id=props:get_value(?GET_CCV(<<"Owner-ID">>), Props)
             ,resource_id=props:get_value(?GET_CCV(<<"Resource-ID">>), Props)
             ,presence_id=props:get_value(?GET_CCV(<<"Channel-Presence-ID">>), Props
                                          ,props:get_value(<<"variable_presence_id">>, Props))
             ,billing_id=props:get_value(?GET_CCV(<<"Billing-ID">>), Props)
             ,bridge_id=props:get_value(?GET_CCV(<<"Bridge-ID">>), Props)
             ,reseller_id=props:get_value(?GET_CCV(<<"Reseller-ID">>), Props)
             ,reseller_billing=props:get_value(?GET_CCV(<<"Reseller-Billing">>), Props)
             ,realm=props:get_value(?GET_CCV(<<"Realm">>), Props
                                    ,props:get_value(<<"variable_domain_name">>, Props))
             ,username=props:get_value(?GET_CCV(<<"Username">>), Props
                                       ,props:get_value(<<"variable_user_name">>, Props))
             ,import_moh=props:get_value(<<"variable_hold_music">>, Props) =:= undefined 
             ,node=Node
             ,timestamp=wh_util:current_tstamp()
            }.
    
-spec channel_record_to_json/1 :: (#channel{}) -> wh_json:json_object().
channel_record_to_json(Channel) -> 
    wh_json:from_list([{<<"uuid">>, Channel#channel.uuid}
                       ,{<<"destination">>, Channel#channel.destination}
                       ,{<<"direction">>, Channel#channel.direction}
                       ,{<<"account_id">>, Channel#channel.account_id}
                       ,{<<"account_billing">>, Channel#channel.account_billing}
                       ,{<<"authorizing_id">>, Channel#channel.authorizing_id}
                       ,{<<"authorizing_type">>, Channel#channel.authorizing_type}
                       ,{<<"owner_id">>, Channel#channel.owner_id}
                       ,{<<"resource_id">>, Channel#channel.resource_id}
                       ,{<<"presence_id">>, Channel#channel.presence_id}
                       ,{<<"billing_id">>, Channel#channel.billing_id}
                       ,{<<"bridge_id">>, Channel#channel.bridge_id}
                       ,{<<"reseller_id">>, Channel#channel.reseller_id}
                       ,{<<"reseller_billing">>, Channel#channel.reseller_billing}
                       ,{<<"realm">>, Channel#channel.realm}                                          
                       ,{<<"username">>, Channel#channel.username}
                       ,{<<"billing_id">>, Channel#channel.billing_id}
                       ,{<<"bridge_id">>, Channel#channel.bridge_id}
                       ,{<<"reseller_id">>, Channel#channel.reseller_id}
                       ,{<<"reseller_billing">>, Channel#channel.reseller_billing}
                       ,{<<"realm">>, Channel#channel.realm}                                          
                       ,{<<"username">>, Channel#channel.username}
                       ,{<<"node">>, Channel#channel.node}
                       ,{<<"timestamp">>, Channel#channel.timestamp}
                      ]).

-spec sync_channels/0 :: () -> 'ok'.
-spec sync_channels/1 :: (string() | binary() | atom()) -> 'ok'.

sync_channels() ->
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({p, l, fs_node})
        ],
    ok.

sync_channels(Node) ->
    N = wh_util:to_atom(Node, true),
    _ = [ecallmgr_fs_node:sync_channels(Srv)
         || Srv <- gproc:lookup_pids({p, l, fs_node})
                ,ecallmgr_fs_node:fs_node(Srv) =:= N
        ],
    ok.

-spec flush_node_channels/1 :: (string() | binary() | atom()) -> 'ok'.
flush_node_channels(Node) ->
    gen_server:cast(?MODULE, {flush_node_channels, wh_util:to_atom(Node, true)}).
        
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("starting new fs handler"),
    Pid = spawn(fun() -> start_preconfigured_servers() end),
    _ = ets:new(sip_subscriptions, [set, public, named_table, {keypos, #sip_subscription.key}]),
    _ = ets:new(ecallmgr_channels, [set, protected, named_table, {keypos, #channel.uuid}]),
    _ = erlang:send_after(?EXPIRE_CHECK, self(), expire_sip_subscriptions),
    {ok, #state{preconfigured_lookup=Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%% #state{nodes=[{FSNode, HandlerPid}]}
%%--------------------------------------------------------------------
handle_call({is_node_up, Node}, _From, #state{nodes=Nodes}=State) ->
    {reply, [ Node1 || #node{node=Node1} <- Nodes, Node1 =:= Node ] =/= [], State};
handle_call(connected_nodes, _From, #state{nodes=Nodes}=State) ->
    {reply, [ Node || #node{node=Node} <- Nodes ], State};
handle_call({add_fs_node, Node, Cookie, Options}, {Pid, _}, #state{preconfigured_lookup=Pid}=State) ->
    lager:debug("trying to add ~s(~s)", [Node, Cookie]),
    {Resp, State1} = add_fs_node(Node, Cookie, Options, State),
    {reply, Resp, State1, hibernate};
handle_call({add_fs_node, Node, Cookie, Options}, _From, #state{preconfigured_lookup=Pid}=State) ->
    lager:debug("trying to add ~s(~s)", [Node, Cookie]),
    {Resp, State1} = add_fs_node(Node, Cookie, Options, State),
    Pid1 = maybe_stop_preconfigured_lookup(Resp, Pid),
    {reply, Resp, State1#state{preconfigured_lookup=Pid1}, hibernate};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({new_channel, Channel}, State) ->
    ets:insert(ecallmgr_channels, Channel),
    {noreply, State, hibernate};
handle_cast({channel_update, UUID, Update}, State) ->
    ets:update_element(ecallmgr_channels, UUID, Update),
    {noreply, State, hibernate};
handle_cast({destroy_channel, UUID}, State) ->
    ets:delete(ecallmgr_channels, UUID),
    {noreply, State, hibernate};
handle_cast({rm_fs_node, Node}, State) ->
    {noreply, rm_fs_node(Node, State), hibernate};
handle_cast({sync_channels, Node, Channels}, State) ->
    lager:debug("ensuring channel cache is in sync with ~s", [Node]),

    MatchSpec = [{#channel{uuid = '$1', node = '$2'}
                  ,[{'=:=', '$2', {const, Node}}]
                  ,['$1']}
                ],
    CachedChannels = sets:from_list(ets:select(ecallmgr_channels, MatchSpec)),
    SyncChannels = sets:from_list(Channels),
    Remove = sets:subtract(CachedChannels, SyncChannels),
    Add = sets:subtract(SyncChannels, CachedChannels),
    _ = [begin
             lager:debug("removed channel ~s from cache during sync with ~s", [UUID, Node]),
             ets:delete(ecallmgr_channels, UUID)
         end
         || UUID <- sets:to_list(Remove)
        ],
    _ = [begin
             lager:debug("added channel ~s to cache during sync with ~s", [UUID, Node]),
             case build_channel_record(Node, UUID) of
                 {ok, C} -> ets:insert(ecallmgr_channels, C);
                 {error, _R} -> lager:warning("failed to sync channel ~s: ~p", [UUID, _R])
             end
         end
         || UUID <- sets:to_list(Add)
        ],
    {noreply, State, hibernate};
handle_cast({flush_node_channels, Node}, State) ->
    lager:debug("flushing all channels in cache associated to node ~s", [Node]),
    MatchSpec = [{#channel{node = '$1'}
                  ,[{'=:=', '$1', {const, Node}}]
                  ,['true']}
                ],
    ets:select_delete(ecallmgr_channels, MatchSpec),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State, hibernate}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(expire_sip_subscriptions, Cache) ->
    Now = wh_util:current_tstamp(),
    DeleteSpec = [{#sip_subscription{key = '_', to = '_', expires = '$3',
                                     timestamp = '$4', from = '_', node = '_'},
                   [{'>', {const, Now}, {'+', '$4', '$3'}}],
                   [true]}
                 ],
    ets:select_delete(sip_subscriptions, DeleteSpec),
    _ = erlang:send_after(?EXPIRE_CHECK, self(), expire_sip_subscriptions),
    {noreply, Cache, hibernate};
handle_info({nodedown, Node}, #state{nodes=Nodes}=State) ->
    _ = ecallmgr_fs_sup:remove_node(Node),
    Opts = case lists:keyfind(Node, #node.node, Nodes) of 
               #node{options=O} -> O;
               false -> []
           end,
    case ecallmgr_fs_pinger_sup:add_node(Node, Opts) of
        {ok, _} -> 
            lager:debug("started fs pinger for node '~s'", [Node]),
            NodeBin = amqp_util:encode(wh_util:to_binary(Node)),
            wh_gauge:set(<<"freeswitch.nodes.", NodeBin/binary, ".up">>, 0),
            wh_timer:delete(<<"freeswitch.nodes.", NodeBin/binary, ".uptime">>),
            {noreply, State#state{nodes=lists:keydelete(Node, #node.node, Nodes)}};
        {error, {already_started, _}} ->
            lager:debug("fs pinger for node '~s' already exists", [Node]),
            NodeBin = amqp_util:encode(wh_util:to_binary(Node)),
            wh_gauge:set(<<"freeswitch.nodes.", NodeBin/binary, ".up">>, 0),
            wh_timer:delete(<<"freeswitch.nodes.", NodeBin/binary, ".uptime">>),
            {noreply, State#state{nodes=lists:keydelete(Node, #node.node, Nodes)}};
        _Else ->
            _ = ecallmgr_fs_pinger_sup:remove_node(Node),
            self() ! {nodedown, Node},
            lager:debug("failed to start fs pinger for node '~s': ~p", [Node, _Else]),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ets:delete(sip_subscriptions),
    ets:delete(ecallmgr_channels),
    lager:debug("fs nodes termination: ~p", [ _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec add_fs_node/4 :: (atom(), atom(), proplist(), #state{}) -> {'ok', #state{}} | 
                                                                 {{'error', 'no_connection'}, #state{}} |
                                                                 {{'error', 'failed_starting_handlers'}, #state{}}.
add_fs_node(Node, Cookie, Options, #state{nodes=Nodes}=State) ->
    case [N || #node{node=Node1}=N <- Nodes, Node =:= Node1] of
        [] ->
            erlang:set_cookie(Node, Cookie),
            case net_adm:ping(Node) of
                pong ->
                    lager:debug("no node matching ~p found, adding", [Node]),
                    case ecallmgr_fs_sup:add_node(Node, Options) of
                        {ok, _} -> 
                            erlang:monitor_node(Node, true),
                            lager:info("successfully connected to node '~s'", [Node]),
                            NodeBin = amqp_util:encode(wh_util:to_binary(Node)),
                            wh_gauge:set(<<"freeswitch.nodes.", NodeBin/binary, ".up">>, 1),
                            wh_timer:update(<<"freeswitch.nodes.", NodeBin/binary, ".uptime">>),
                            wh_timer:update(<<"freeswitch.nodes.", NodeBin/binary, ".last_connected">>),
                            {ok, State#state{nodes=[#node{node=Node, cookie=Cookie, options=Options} | Nodes]}};
                        {error, {already_started, _}} ->
                            lager:info("already connected to node '~s'", [Node]),
                            NodeBin = amqp_util:encode(wh_util:to_binary(Node)),
                            wh_gauge:set(<<"freeswitch.nodes.", NodeBin/binary, ".up">>, 1),
                            wh_timer:update(<<"freeswitch.nodes.", NodeBin/binary, ".uptime">>),
                            wh_timer:update(<<"freeswitch.nodes.", NodeBin/binary, ".last_connected">>),
                            {ok, State#state{nodes=[#node{node=Node, cookie=Cookie, options=Options} | Nodes]}};
                        _Else ->
                            lager:warning("failed to add node '~s'", [Node]),
                            self() ! {nodedown, Node},
                            {{error, failed_starting_handlers}, State}
                    end;
                pang ->
                    lager:info("unable to connect to node '~s'; ensure it is reachable from this server and using cookie '~s'", [Node, Cookie]),
                    self() ! {nodedown, Node},
                    {{error, no_connection}, State}
            end;
        [#node{node=Node}] ->
            lager:info("already connected to node '~s'", [Node]),
            {ok, State}
    end.

-spec rm_fs_node/2 :: (atom(), #state{}) -> #state{}.
rm_fs_node(Node, #state{nodes=Nodes}=State) ->
    lager:debug("closing node handler for ~s", [Node]),
    _ = close_node(Node),
    NodeBin = amqp_util:encode(wh_util:to_binary(Node)),
    wh_gauge:set(<<"freeswitch.nodes.", NodeBin/binary, ".up">>, 0),
    wh_timer:delete(<<"freeswitch.nodes.", NodeBin/binary, ".uptime">>),
    State#state{nodes=lists:keydelete(Node, 2, Nodes)}.    

-spec close_node/1 :: (atom() | #node{}) -> 'ok' | {'error','not_found' | 'running' | 'simple_one_for_one'}.
close_node(#node{node=Node}) ->
    close_node(Node);
close_node(Node) ->
    catch erlang:monitor_node(Node, false), % will crash if Node is down already
    _ = ecallmgr_fs_pinger_sup:remove_node(Node),
    ecallmgr_fs_sup:remove_node(Node).

start_preconfigured_servers() ->
    put(callid, ?LOG_SYSTEM_ID),
    case ecallmgr_config:get(<<"fs_nodes">>, []) of
        [] ->
            lager:info("no preconfigured servers available. Is the sysconf whapp running?"),
            timer:sleep(5000),
            start_preconfigured_servers();
        Nodes when is_list(Nodes) ->
            lager:info("successfully retrieved FreeSWITCH nodes to connect with, doing so..."),
            [start_node_from_config(N) || N <- Nodes];
        _E ->
            lager:debug("recieved a non-list for fs_nodes: ~p", [_E]),
            timer:sleep(5000),
            start_preconfigured_servers()
    end.

start_node_from_config(MaybeJObj) ->
    case wh_json:is_json_object(MaybeJObj) of
        false -> ?MODULE:add(wh_util:to_atom(MaybeJObj, true));
        true ->
            {[Cookie], [Node]} = wh_json:get_values(MaybeJObj),
            ?MODULE:add(wh_util:to_atom(Node, true), wh_util:to_atom(Cookie, true))
    end.
            
-spec maybe_stop_preconfigured_lookup/2 :: ('ok' | {'error', _}, pid() | 'undefined') -> pid() | 'undefined'.
maybe_stop_preconfigured_lookup(_, undefined) -> undefined;
maybe_stop_preconfigured_lookup(ok, Pid) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
        true ->
            exit(Pid, kill),
            undefined;
        false ->
            undefined
    end;
maybe_stop_preconfigured_lookup(_, Pid) ->
    Pid.

-spec build_channel_record/2 :: (atom(), ne_binary()) -> {'ok', #channel{}} | {'error', _}.
build_channel_record(Node, UUID) ->
    case freeswitch:api(Node, uuid_dump, wh_util:to_list(UUID)) of
        {ok, Dump} ->
            Props = ecallmgr_util:eventstr_to_proplist(Dump),
            {ok, ?MODULE:props_to_channel_record(Props, Node)};
        {error, _}=E -> E;
        timeout -> {error, timeout}
    end.

-spec summarize_account_usage/1 :: (_) -> wh_json:json_object().
summarize_account_usage(Channels) ->
    AStats = summarize_account_usage(Channels, #astats{}),
    wh_json:from_list([{<<"Calls">>, length(AStats#astats.billing_ids)}
                       ,{<<"Channels">>,  length(Channels)}
                       ,{<<"Outbound-Flat-Rate">>, AStats#astats.outbound_fr}
                       ,{<<"Inbound-Flat-Rate">>, AStats#astats.inbound_fr}
                       ,{<<"Outbound-Per-Minute">>, AStats#astats.outbound_pm}
                       ,{<<"Inbound-Per-Minute">>, AStats#astats.inbound_pm}
                       ,{<<"Resource-Consuming-Calls">>, AStats#astats.calls_using_res}
                      ]).

-spec summarize_account_usage/2 :: (_, #astats{}) -> #astats{}.
%%summarize_account_usage([{Direction, BillingId, AuthorizingId, BridgeId, ResourceId, BillingType}|Channels], ) 

summarize_account_usage([], AStats) ->
    AStats;
summarize_account_usage([{<<"outbound">>, BillingId, _, BridgeId, ResourceId, <<"per_minute">>}|Channels], AStats) when ResourceId =/= undefined -> 
    Routines = [fun(#astats{billing_ids=I}=A) -> 
                        A#astats{billing_ids=[BillingId|lists:delete(BillingId, I)]}
                 end
                ,fun(#astats{outbound_pm=O, outbound_bridges=B}=A) ->
                         case lists:member(BridgeId, B) of
                             true -> A;
                             false -> A#astats{outbound_pm=O + 1
                                               ,outbound_bridges=[BridgeId|lists:delete(BridgeId, B)]
                                              }
                         end
                 end
                ,fun(#astats{calls_using_res=C, using_res=U}=A) ->
                         case lists:member(BillingId, U) of
                             true -> A;
                             false -> A#astats{calls_using_res=C + 1
                                               ,using_res=[BillingId|lists:delete(BillingId, U)]
                                              }
                         end
                 end
               ],
    summarize_account_usage(Channels, lists:foldr(fun(F, A) -> F(A) end, AStats, Routines));
summarize_account_usage([{<<"inbound">>, BillingId, undefined, _, _, <<"per_minute">>}|Channels], AStats) -> 
    Routines = [fun(#astats{billing_ids=I}=A) -> 
                        A#astats{billing_ids=[BillingId|lists:delete(BillingId, I)]}
                 end
                ,fun(#astats{inbound_pm=O}=A) -> A#astats{inbound_pm=O + 1} end
                ,fun(#astats{calls_using_res=C, using_res=U}=A) ->
                         case lists:member(BillingId, U) of
                             true -> A;
                             false -> A#astats{calls_using_res=C + 1
                                               ,using_res=[BillingId|lists:delete(BillingId, U)]
                                              }
                         end
                 end
               ],
    summarize_account_usage(Channels, lists:foldr(fun(F, A) -> F(A) end, AStats, Routines));

summarize_account_usage([{<<"outbound">>, BillingId, _, BridgeId, ResourceId, _}|Channels], AStats) when ResourceId =/= undefined -> 
    Routines = [fun(#astats{billing_ids=I}=A) -> 
                        A#astats{billing_ids=[BillingId|lists:delete(BillingId, I)]}
                 end
                ,fun(#astats{outbound_fr=O, outbound_bridges=B}=A) ->
                         case lists:member(BridgeId, B) of
                             true -> A;
                             false -> A#astats{outbound_fr=O + 1
                                               ,outbound_bridges=[BridgeId|lists:delete(BridgeId, B)]
                                              }
                         end
                 end
                ,fun(#astats{calls_using_res=C, using_res=U}=A) ->
                         case lists:member(BillingId, U) of
                             true -> A;
                             false -> A#astats{calls_using_res=C + 1
                                               ,using_res=[BillingId|lists:delete(BillingId, U)]
                                              }
                         end
                 end
               ],
    summarize_account_usage(Channels, lists:foldr(fun(F, A) -> F(A) end, AStats, Routines));
summarize_account_usage([{<<"inbound">>, BillingId, undefined, _, _, _}|Channels], AStats) -> 
    Routines = [fun(#astats{billing_ids=I}=A) -> 
                        A#astats{billing_ids=[BillingId|lists:delete(BillingId, I)]}
                 end
                ,fun(#astats{inbound_fr=O}=A) -> A#astats{inbound_fr=O + 1} end
                ,fun(#astats{calls_using_res=C, using_res=U}=A) ->
                         case lists:member(BillingId, U) of
                             true -> A;
                             false -> A#astats{calls_using_res=C + 1
                                               ,using_res=[BillingId|lists:delete(BillingId, U)]
                                              }
                         end
                 end
               ],
    summarize_account_usage(Channels, lists:foldr(fun(F, A) -> F(A) end, AStats, Routines));
summarize_account_usage([{_, BillingId, _, _, _, _}|Channels], AStats) ->
    Routines = [fun(#astats{billing_ids=I}=A) ->
                        A#astats{billing_ids=[BillingId|lists:delete(BillingId, I)]}
                end
               ],
    summarize_account_usage(Channels, lists:foldr(fun(F, A) -> F(A) end, AStats, Routines)).
