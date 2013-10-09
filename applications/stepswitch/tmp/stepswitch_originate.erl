%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_originate).

-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("skel.hrl").

-record(state, {}).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'route', []}
                   ,{'self', []}
                  ]).
-define(RESPONDERS, [
                     %% Received because of our route binding
                     {{'skel_handlers', 'handle_route_req'}, [{<<"dialplan">>, <<"route_req">>}]}

                     %% Received because of our self binding (route_wins are sent to the route_resp's Server-ID
                     %% which is usually populated with the listener's queue name
                     ,{{'skel_handlers', 'handle_route_win'}, [{<<"dialplan">>, <<"route_win">>}]}
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

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
start_link() ->
    gen_listener:start_link(?MODULE, [
                                      {'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ], []).

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
    {'ok', #state{}}.

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
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

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
-spec attempt_to_fulfill_originate_req(ne_binary(), wh_json:object(), wh_proplist()) -> originate_resp().
attempt_to_fulfill_originate_req(Number, JObj, Props) ->
    Flags = wh_json:get_value(<<"Flags">>, JObj, []),
    Resources = stepswitch_util:get_resources(JObj, Props),
    {Endpoints, _} = find_endpoints(Number, Flags, Resources, JObj),
    case {originate_to_endpoints(Endpoints, JObj),
          correct_shortdial(Number, JObj)} of
        {{'error', 'no_resources'}, 'fail'} -> {'error', 'no_resources'};
        {{'error', 'no_resources'}, CorrectedNumber} ->
            lager:debug("found no resources for number as originated, retrying number corrected for shortdial as ~s", [CorrectedNumber]),
            attempt_to_fulfill_originate_req(CorrectedNumber, JObj, Props);
        {Result, _} -> Result
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build a whistle dialplan API to bridge to the provided endpoints, and
%% block until the bridge is complete.  If the Endpoints that we are
%% attempting to use have been flagged as emergency routes then prefer
%% the emergency CID.
%% @end
%%--------------------------------------------------------------------
-spec originate_to_endpoints(wh_proplist(), wh_json:object()) ->
                                    {'error', 'no_resources'} |
                                    originate_resp().
originate_to_endpoints([], _) -> {'error', 'no_resources'};
originate_to_endpoints(Endpoints, JObj) ->
    lager:debug("found resources that can originate the number...to the cloud!"),
    Q = create_queue(),

    CIDNum = wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj
                                  ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj)),
    CIDName = wh_json:get_ne_value(<<"Outbound-Caller-ID-Name">>, JObj
                                   ,wh_json:get_ne_value(<<"Emergency-Caller-ID-Name">>, JObj)),

    {CalleeIdNumber, CalleeIdName} =
        {wh_json:get_value(<<"Outbound-Callee-ID-Number">>, JObj)
         ,wh_json:get_value(<<"Outbound-Callee-ID-Name">>, JObj)
        },
    lager:debug("set outbound callee id to ~s '~s'", [CalleeIdNumber, CalleeIdName]),


    FromURI = case whapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>, 'false') of
                  'true' ->
                      case {CIDNum, wh_json:get_value(<<"Account-Realm">>, JObj)} of
                          {'undefined', _} -> 'undefined';
                          {_, 'undefined'} -> 'undefined';
                          {FromNumber, FromRealm} -> <<"sip:", FromNumber/binary, "@", FromRealm/binary>>
                      end;
                  'false' -> 'undefined'
              end,

    lager:debug("setting from-uri to ~s", [FromURI]),

    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    CCVUpdates = props:filter_undefined(
                   [{<<"Account-ID">>, AccountId}
                   ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                   ,{<<"From-URI">>, FromURI}
                   ,{<<"Global-Resource">>, <<"true">>}
                   ]),

    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj, wh_util:rand_hex_binary(16)),
    Application = wh_json:get_value(<<"Application-Name">>, JObj, <<"park">>),
    Request = props:filter_undefined(
                [{<<"Application-Name">>, Application}
                 ,{<<"Application-Data">>, wh_json:get_value(<<"Application-Data">>, JObj)}
                 ,{<<"Msg-ID">>, MsgId}
                 ,{<<"Endpoints">>, Endpoints}
                 ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
                 ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
                 ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
                 ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
                 ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
                 ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
                 ,{<<"Outbound-Caller-ID-Name">>, CIDName}
                 ,{<<"Outbound-Callee-ID-Number">>, CalleeIdNumber}
                 ,{<<"Outbound-Callee-ID-Name">>, CalleeIdName}
                 ,{<<"Fax-Identity-Number">>, wh_json:get_value(<<"Fax-Identity-Number">>, JObj, CIDNum)}
                 ,{<<"Fax-Identity-Name">>, wh_json:get_value(<<"Fax-Identity-Name">>, JObj, CIDName)}
                 ,{<<"Caller-ID-Number">>, CIDNum}
                 ,{<<"Caller-ID-Name">>, CIDName}
                 ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
                 ,{<<"Dial-Endpoint-Method">>, <<"single">>}
                 ,{<<"Continue-On-Fail">>, <<"true">>}
                 ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
                 ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, CCVs)}
                 ,{<<"Outbound-Call-ID">>, wh_json:get_value(<<"Outbound-Call-ID">>, JObj)}
                 | wh_api:default_headers(Q, <<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
                ]),
    wapi_resource:publish_originate_req(Request),
    wait_for_originate(MsgId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume AMQP messages waiting for the originate response/error
%% @end
%%--------------------------------------------------------------------
-spec wait_for_originate(ne_binary()) ->
                                originate_resp() |
                                {'error', 'no_resources'}.
wait_for_originate(MsgId) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case get_event_type(JObj) of
                {<<"resource">>, <<"originate_resp">>, _} -> hangup_result(JObj);
                {<<"error">>, <<"originate_resp">>, _} -> {'error', JObj};
                {<<"dialplan">>, <<"originate_ready">>, _} -> {'ready', JObj};
                _  -> wait_for_originate(MsgId)
            end;
        %% if there are no FS nodes connected (or ecallmgr is down) we get the message
        %% returned so we know...
        {#'basic.return'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case wh_json:get_value(<<"Msg-ID">>, JObj) of
                MsgId -> {'error', 'no_resources'};
                _Else -> wait_for_originate(MsgId)
            end;
        _ -> wait_for_originate(MsgId)
    end.
