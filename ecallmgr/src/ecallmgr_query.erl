%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Listener for whapp requests to query the underlying switches
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_query).

-behaviour(gen_listener).

-export([start_link/0]).
-export([handle_channel_status/2]).
-export([handle_call_status/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{{?MODULE, handle_channel_status}, [{<<"call_event">>, <<"channel_status_req">>}]}
                     ,{{?MODULE, handle_call_status}, [{<<"call_event">>, <<"call_status_req">>}]}
                    ]).
-define(BINDINGS, [{call, [{restrict_to, [status_req]}]}]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(state, {}).

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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({local, ?SERVER}, ?MODULE, [{responders, ?RESPONDERS}
                                                        ,{bindings, ?BINDINGS}
                                                        ,{queue_name, ?QUEUE_NAME}
                                                        ,{queue_options, ?QUEUE_OPTIONS}
                                                        ,{consume_options, ?CONSUME_OPTIONS}
                                                       ], []).

-spec handle_channel_status(wh_json:json_object(), proplist()) -> 'ok'.
handle_channel_status(JObj, _Props) ->
    true = wapi_call:channel_status_req_v(JObj),
    _ = wh_util:put_callid(JObj),

    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("channel status request received"),

    AllNodesConnected = ecallmgr_fs_nodes:all_nodes_connected(),
    case ecallmgr_fs_nodes:fetch_channel(CallID) of
        {error, not_found} when AllNodesConnected ->
            lager:debug("no node found with channel ~s", [CallID]),
            Resp = [{<<"Call-ID">>, CallID}
                    ,{<<"Status">>, <<"terminated">>}
                    ,{<<"Error-Msg">>, <<"no node found with channel">>}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_call:publish_channel_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
        {error, not_found} ->
            lager:debug("no node found with channel ~s, but we are not authoritative", [CallID]);
        {ok, Channel} ->
            Node = wh_json:get_binary_value(<<"node">>, Channel),
            [_, Hostname] = binary:split(Node, <<"@">>),
            lager:debug("call is on ~s", [Hostname]),
            Resp = [{<<"Call-ID">>, CallID}
                    ,{<<"Status">>, <<"active">>}
                    ,{<<"Switch-Hostname">>, Hostname}
                    ,{<<"Switch-Nodename">>, wh_util:to_binary(Node)}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_call:publish_channel_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec handle_call_status(wh_json:json_object(), proplist()) -> 'ok'.
handle_call_status(JObj, _Props) ->
    true = wapi_call:call_status_req_v(JObj),
    _ = wh_util:put_callid(JObj),

    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("call status request received"),

    AllNodesConnected = ecallmgr_fs_nodes:all_nodes_connected(),
    case ecallmgr_fs_nodes:fetch_channel(CallID) of
        {error, not_found} when AllNodesConnected ->
            lager:debug("no node found with channel ~s", [CallID]),
            Resp = [{<<"Call-ID">>, CallID}
                    ,{<<"Status">>, <<"terminated">>}
                    ,{<<"Error-Msg">>, <<"no node found with call id">>}
                    ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
            wapi_call:publish_call_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
        {error, not_found} ->
            lager:debug("no node found with channel ~s, but we are not authoritative", [CallID]);
        {ok, Channel} ->
            Node = wh_json:get_atom_value(<<"node">>, Channel),
            case uuid_dump(Node, CallID) of
                error ->
                    lager:debug("failed to get channel info for ~s", [CallID]),
                    Resp = [{<<"Call-ID">>, CallID}
                            ,{<<"Status">>, <<"active">>}
                            ,{<<"Error-Msg">>, <<"uuid dump failed">>}
                            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                           ],
                    wapi_call:publish_call_status_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
                {ok, Props} ->
                    lager:debug("got channel info for ~s, forming response", [CallID]),
                    ChannelCallID = props:get_value(<<"Channel-Call-UUID">>, Props),
                    Resp = create_call_status_resp(Props, ChannelCallID =:= CallID),
                    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
                    wapi_call:publish_call_status_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                                       ,wh_json:set_value(<<"Msg-ID">>, MsgId, wh_json:from_list(Resp)))
            end
    end.

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
    {ok, #state{}}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {reply, []}.

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
    ok.

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
-spec create_call_status_resp(wh_proplist(), boolean()) -> wh_proplist().
create_call_status_resp(Props, true) ->
    {OLCIName, OLCINum} = case props:get_value(<<"Other-Leg-Direction">>, Props) of
                               <<"outbound">> ->
                                   {props:get_value(<<"Other-Leg-Callee-ID-Name">>, Props)
                                    ,props:get_value(<<"Other-Leg-Callee-ID-Number">>, Props)};
                               <<"inbound">> ->
                                   {props:get_value(<<"Other-Leg-Caller-ID-Name">>, Props)
                                    ,props:get_value(<<"Other-Leg-Caller-ID-Number">>, Props)};
                              undefined ->
                                  {undefined, undefined}
                          end,
    [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Props)}
     ,{<<"Status">>, <<"active">>}
     ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Props)}
     ,{<<"Call-ID">>, props:get_value(<<"Caller-Unique-ID">>, Props)}
     ,{<<"Call-State">>, props:get_value(<<"Channel-Call-State">>, Props)}
     ,{<<"Caller-ID-Name">>, props:get_value(<<"variable_effective_caller_id_name">>, Props
                                              ,props:get_value(<<"Caller-Caller-ID-Name">>, Props))}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"variable_effective_caller_id_number">>, Props
                                              ,props:get_value(<<"Caller-Caller-ID-Number">>, Props))}
     ,{<<"Destination-Number">>, props:get_value(<<"Caller-Destination-Number">>, Props)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Props)}
     ,{<<"Other-Leg-Caller-ID-Name">>, OLCIName}
     ,{<<"Other-Leg-Caller-ID-Number">>, OLCINum}
     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Props)}
     ,{<<"Presence-ID">>, props:get_value(<<"variable_presence_id">>, Props)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)];
create_call_status_resp(Props, false) ->
    {OLCIName, OLCINum} = case props:get_value(<<"Call-Direction">>, Props) of
                               <<"outbound">> ->
                                   {props:get_value(<<"Caller-Callee-ID-Name">>, Props)
                                    ,props:get_value(<<"Caller-Callee-ID-Number">>, Props)};
                               <<"inbound">> ->
                                   {props:get_value(<<"Caller-Caller-ID-Name">>, Props)
                                    ,props:get_value(<<"Caller-Caller-ID-Number">>, Props)};
                              undefined ->
                                  {undefined, undefined}
                           end,
    [{<<"Msg-ID">>, props:get_value(<<"Event-Date-Timestamp">>, Props)}
     ,{<<"Status">>, <<"active">>}
     ,{<<"Timestamp">>, props:get_value(<<"Event-Date-Timestamp">>, Props)}
     ,{<<"Call-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Props)}
     ,{<<"Call-State">>, props:get_value(<<"Channel-Call-State">>, Props)}
     ,{<<"Caller-ID-Name">>, props:get_value(<<"variable_effective_caller_id_name">>, Props
                                              ,props:get_value(<<"Other-Leg-Caller-ID-Name">>, Props))}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"variable_effective_caller_id_number">>, Props
                                              ,props:get_value(<<"Other-Leg-Caller-ID-Number">>, Props))}
     ,{<<"Destination-Number">>, props:get_value(<<"Other-Leg-Destination-Number">>, Props)}
     ,{<<"Other-Leg-Unique-ID">>, props:get_value(<<"Caller-Unique-ID">>, Props)}
     ,{<<"Other-Leg-Caller-ID-Name">>, OLCIName}
     ,{<<"Other-Leg-Caller-ID-Number">>, OLCINum}
     ,{<<"Other-Leg-Destination-Number">>, props:get_value(<<"Caller-Destination-Number">>, Props)}
     ,{<<"Presence-ID">>, props:get_value(<<"variable_presence_id">>, Props)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)].

-spec uuid_dump(atom(), string() | binary()) ->
                             {'ok', wh_proplist()} |
                             'error'.
uuid_dump(Node, UUID) ->
    uuid_dump(Node, UUID, wh_util:to_list(UUID)).
uuid_dump(Node, UUID, ID) ->
    case catch freeswitch:api(Node, 'uuid_dump', ID) of
        {'ok', Result} ->
            Props = ecallmgr_util:eventstr_to_proplist(Result),
            {ok, Props};
        {'EXIT', {'badarg', _}} when is_list(ID) ->
            uuid_dump(Node, UUID, wh_util:to_binary(UUID));
        _Else ->
            lager:debug("failed to get result from uuid_dump(~p) on ~p: ~p", [UUID, Node, _Else]),
            'error'
    end.
