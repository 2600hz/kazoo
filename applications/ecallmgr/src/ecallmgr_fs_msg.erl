%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Notify-type requests, like MWI updates, received and processed here
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_msg).

-behaviour(gen_listener).

-export([start_link/1, start_link/2]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([handle_message_route/2]).

-record(state, {
          node :: atom()
          ,options :: wh_proplist()
         }).

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'sms', []}
                   ,{'self', []}
                  ]).
-define(RESPONDERS, [
                     {{?MODULE, 'handle_message_route'}, [{<<"message">>, <<"route">>}]}
                    ]).

-define(QUEUE_NAME, <<"ecallmgr_fs_msg">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include_lib("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), wh_proplist()) -> startlink_ret().

start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE
                            ,[{'responders', ?RESPONDERS}
                              ,{'bindings', ?BINDINGS}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ]
                            ,[Node, Options]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put(callid, Node),
    lager:debug("starting new ecallmgr msg process"),
    gproc:reg({'p', 'l', 'fs_msg'}),
    gen_server:cast(self(), 'bind_to_msg_events'),
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
handle_cast('bind_to_msg_events', #state{node=Node}=State) ->
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"KZ::DELIVERY_REPORT">>)}),
    gproc:reg({'p', 'l', ?FS_EVENT_REG_MSG(Node, <<"KZ::MESSAGE">>)}),
    lager:debug("bound to recv_message events on node ~s", [Node]),
    {'noreply', State};
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
handle_info({'event', [_X | Props]}, #state{node=Node}=State) ->
    lager:info("fs event ~p",[_X]),
    props:to_log(Props, <<"FS_MSG">>),
    {'noreply', State, 'hibernate'};
handle_info({'EXIT', _, _}, State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("MSG UN", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {'reply', [{'node', Node}]}.

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
    lager:info("notify listener for ~s terminating: ~p", [Node, _Reason]).

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

    

get_target(To, Username) ->
    [ToUser, ToRealm] = binary:split(To , <<"@">>),
    lager:info("To Parts ~p / ~p",[ToUser, ToRealm]),
    Target = <<Username/binary, "@", ToRealm/binary>>.

get_caller_id(CID, From) ->
    [FromUser, FromRealm] = binary:split(From , <<"@">>),
    lager:info("From Parts ~p / ~p",[FromUser, FromRealm]),
    CallerID = <<CID/binary, "@", FromRealm/binary>>.
    

-spec handle_message_route(wh_json:object(), wh_proplist()) -> no_return().
handle_message_route(JObj, Props) ->
    _ = wh_util:put_callid(JObj),
    lager:info("process_message received SIP/SIMPLE Msg : ~p", [JObj]),
    Node = props:get_value('node', Props),
    From = wh_json:get_value(<<"From">>, JObj),
    CIDNumber = wh_json:get_value(<<"Caller-ID-Number">>, JObj),
    To = wh_json:get_value(<<"To">>, JObj),
    Body = wh_json:get_value(<<"Body">>, JObj),
    MessageId = wh_json:get_value(<<"Message-ID">>, JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    
    Contact = wh_json:get_value(<<"Contact">>, JObj),
    IP = wh_json:get_value(<<"Contact-IP">>, JObj),
    Port = wh_json:get_value(<<"Contact-Port">>, JObj),
    
%    case ecallmgr_registrar:lookup_original_contact(Realm, Username) of
%        {'error', 'not_found'} ->
%            lager:warning("failed to find contact for ~s@~s, dropping MWI update", [Username, Realm]);
%        {'ok', Contact} ->
%            Node = props:get_value('node', Props),
%            send_mwi_update(JObj, Node, Username, Realm, Contact)
%    end.

    
    Username = wh_json:get_value(<<"Contact-Username">>, JObj),
    [ContactUser, ContactRealm] = binary:split(Contact , <<"@">>),
 %   lager:info("Contact Parts ~p / ~p",[ContactUser, ContactRealm]),
    [FromUser, FromRealm] = binary:split(From , <<"@">>),
 %   lager:info("From Parts ~p / ~p",[FromUser, FromRealm]),
    [ToUser, ToRealm] = binary:split(To , <<"@">>),
 %   lager:info("To Parts ~p / ~p",[ToUser, ToRealm]),
    Target = get_target(To, Username),
    CallerID = get_caller_id(CIDNumber, From),
    CallerIDFull = <<"<sip:", CallerID/binary, ">">>,
    
    [User, Realm] = binary:split(To, <<"@">>),
            Header2 = [
               {"sip_profile", ?DEFAULT_FS_PROFILE}
              ,{"proto", "sip"}
              ,{"blocking", "true"}
              ,{"dest_proto", "sip"}
              ,{"to", wh_util:to_list(Target)}
              ,{"to_sip_ip", wh_util:to_list(IP)}
              ,{"to_sip_port", wh_util:to_list(Port)}
              ,{"from", wh_util:to_list(CallerID)}
              ,{"from_full", wh_util:to_list(CallerIDFull)}
              ,{"content-length", wh_util:to_list(size(Body))}
              ,{"type", "text/plain"}
              ,{"body", Body}
              ,{"Call-ID", wh_util:to_list(CallId)}
              ,{"Unique-ID", wh_util:to_list(CallId)}
              ,{"Message-ID", wh_util:to_list(MessageId)}
            ],
%    lager:info("SENDING ~p",[Header2]),
%            Resp = freeswitch:sendevent(Node, 'SEND_MESSAGE', Header),
            Resp = freeswitch:sendevent_custom(Node, 'SMS::SEND_MESSAGE', Header2),    
            lager:info("sent SIP/SIMPLE Msg to '~s' via ~s: ~p", [To, Node, Resp]).


   
