%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_cdr).

-behaviour(gen_server).

-export([start_link/1
         ,start_link/2
        ]).
-export([publish/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-type fs_to_whistle_map() :: [{ne_binary() | ne_binaries() | fun(), ne_binary()},...].
-define(FS_TO_WHISTLE_MAP, [{<<"FreeSWITCH-Hostname">>, <<"Handling-Server-Name">>}
                            ,{<<"Hangup-Cause">>, <<"Hangup-Cause">>}
                            ,{<<"Unique-ID">>, <<"Call-ID">>}
                            ,{<<"Event-Date-Timestamp">>, <<"Timestamp">>}
                            ,{<<"Call-Direction">>, <<"Call-Direction">>}
                            ,{<<"variable_switch_r_sdp">>, <<"Remote-SDP">>}
                            ,{<<"variable_sip_local_sdp_str">>, <<"Local-SDP">>}
                            ,{<<"variable_sip_to_uri">>, <<"To-Uri">>}
                            ,{<<"variable_sip_from_uri">>, <<"From-Uri">>}
                            ,{[<<"variable_effective_caller_id_number">>, <<"Caller-Caller-ID-Number">>], <<"Caller-ID-Number">>}
                            ,{[<<"variable_effective_caller_id_name">>, <<"Caller-Caller-ID-Name">>], <<"Caller-ID-Name">>}
                            ,{<<"Caller-Callee-ID-Name">>, <<"Callee-ID-Name">>}
                            ,{<<"Caller-Callee-ID-Number">>, <<"Callee-ID-Number">>}
                            ,{<<"Other-Leg-Unique-ID">>, <<"Other-Leg-Call-ID">>}
                            ,{<<"variable_sip_user_agent">>, <<"User-Agent">>}
                            ,{<<"variable_duration">>, <<"Duration-Seconds">>}
                            ,{<<"variable_billsec">>, <<"Billing-Seconds">>}
                            ,{<<"variable_progresssec">>, <<"Ringing-Seconds">>}
                            ,{<<"variable_digits_dialed">>, <<"Digits-Dialed">>}
                            ,{<<"ecallmgr">>, <<"Custom-Channel-Vars">>}
                            ,{fun(P) -> ecallmgr_util:get_sip_request(P) end, <<"Request">>}
                           ]).
-define(FS_TO_WHISTLE_OUTBOUND_MAP, [{<<"variable_sip_cid_type">>, <<"Caller-ID-Type">>}]).

-record(state, {node = 'undefined' :: atom()
                ,options = [] :: wh_proplist()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

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
init([Node, Options]) ->
    put('callid', Node),
    lager:info("starting new fs cdr listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_events'),
    {'ok', #state{node=Node, options=Options}}.

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
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast('bind_to_events', #state{node=Node}=State) ->
    case gproc:reg({'p', 'l', {'event', Node, <<"CHANNEL_HANGUP_COMPLETE">>}}) =:= 'true' of
        'true' -> {'noreply', State};
        'false' -> {'stop', 'gproc_badarg', State}
    end;
handle_cast(_Msg, State) ->
    {'noreply', State}.

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
handle_info({'event', [UUID | Props]}, State) ->
    spawn(?MODULE, 'publish', [UUID, Props]),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
terminate(_Reason, #state{node=Node}) ->
    lager:info("cdr listener for ~s terminating: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec publish(ne_binary(), wh_proplist()) -> 'ok'.
publish(UUID, Props) ->
    put('callid', UUID),
    CDR = create_cdr(Props),
    lager:debug("publising cdr: ~p", [CDR]),
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                        ,CDR
                        ,fun(P) -> wapi_call:publish_cdr(UUID, P) end
                       ),
    catch case props:get_value(<<"Hangup-Cause">>,Props) of
              Hangup when Hangup==<<"UNALLOCATED_NUMBER">>;
                    Hangup==<<"NO_ROUTE_DESTINATION">>;
                    Hangup==<<"RECOVERY_ON_TIMER_EXPIRE">>;
                    Hangup==<<"PROGRESS_TIMEOUT">> ->
            Realm = props:get_value(<<"Realm">>,Props,<<"unknown">>),
            whistle_stats:increment_counter(Realm,Hangup);
        _ ->
            nothing
    end.


-spec create_cdr(wh_proplist()) -> wh_proplist().
create_cdr(Props) ->
    DefProp = wh_api:default_headers(<<>>, ?APP_NAME, ?APP_VERSION),
    ApiProp = add_values(?FS_TO_WHISTLE_MAP, DefProp, Props),
    case props:get_value(<<"direction">>, ApiProp) of
        <<"outbound">> -> add_values(?FS_TO_WHISTLE_OUTBOUND_MAP, ApiProp, Props);
        _ -> ApiProp
    end.

-spec add_values(fs_to_whistle_map(), wh_proplist(), wh_proplist()) -> wh_proplist().
add_values(Mappings, BaseProp, ChannelProp) ->
    lists:foldl(fun({Fun, WK}, WApi) when is_function(Fun) ->
                        [{WK, Fun(ChannelProp)} | WApi];
                   ({<<"ecallmgr">>, <<"Custom-Channel-Vars">>=WK}, WApi) ->
                        [{WK, wh_json:from_list(ecallmgr_util:custom_channel_vars(ChannelProp))} | WApi];
                   ({<<"Event-Date-Timestamp">>=FSKey, WK}, WApi) ->
                        case props:get_value(FSKey, ChannelProp) of
                            'undefined' -> WApi;
                            V -> VUnix =  wh_util:unix_seconds_to_gregorian_seconds(wh_util:microseconds_to_seconds(V)),
                                 [{WK, wh_util:to_binary(VUnix)} | WApi]
                        end;
                   ({FSKeys, WK}, WApi) when is_list(FSKeys) ->
                        case get_first_value(FSKeys, ChannelProp) of
                            'undefined' -> WApi;
                            V -> [{WK, V} | WApi]
                        end;
                   ({FSKey, WK}, WApi) ->
                        case props:get_value(FSKey, ChannelProp) of
                            'undefined' -> WApi;
                            V -> [{WK, wh_util:to_binary(V)} | WApi]
                        end
                end, BaseProp, Mappings).

-spec get_first_value(ne_binaries(), wh_proplist()) -> api_binary().
get_first_value([], _) -> 'undefined';
get_first_value([FSKey|T], ChannelProp) ->
    case props:get_value(FSKey, ChannelProp) of
        'undefined' -> get_first_value(T, ChannelProp);
        V -> wh_util:to_binary(V)
    end.
