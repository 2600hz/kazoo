%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(stepswitch_bridge).

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
maybe_build_bridge(NumberProps, JObj, Props) ->
    Number = props:get_value('number', NumberProps),
    case stepswitch_resources:endpoints(Number, JObj) of
        [] -> {'error', 'no_resources'};
        Endpoints -> build_bridge(Endpoints, JObj, Props)
    end.

build_bridge(Endpoints, JObj, Props) ->
    IsEmergency = contains_emergency_endpoint(Endpoints),

    {CIDNum, CIDName} = bridge_caller_id(IsEmergency, JObj),
    lager:debug("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),

    {CalleeIdNumber, CalleeIdName} =
        {wh_json:get_value(<<"Outbound-Callee-ID-Number">>, JObj)
         ,wh_json:get_value(<<"Outbound-Callee-ID-Name">>, JObj)
        },
    lager:debug("set outbound callee id to ~s '~s'", [CalleeIdNumber, CalleeIdName]),

    FromURI = bridge_from_uri(CIDNum, JObj),
    lager:debug("setting from-uri to ~s", [FromURI]),

    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    CCVUpdates = props:filter_undefined([{<<"Ignore-Display-Updates">>, <<"true">>}
                                         ,{<<"Global-Resource">>, <<"true">>}
                                         ,{<<"Account-ID">>, AccountId}
                                         ,{<<"From-URI">>, FromURI}
                                         ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                                        ]),

    [{<<"Application-Name">>, <<"bridge">>}
     ,{<<"Dial-Endpoint-Method">>, <<"single">>}
     ,{<<"Continue-On-Fail">>, <<"true">>}
     ,{<<"Endpoints">>, Endpoints}
     ,{<<"Outbound-Caller-ID-Number">>, CIDNum}
     ,{<<"Outbound-Caller-ID-Name">>, CIDName}
     ,{<<"Outbound-Callee-ID-Number">>, CalleeIdNumber}
     ,{<<"Outbound-Callee-ID-Name">>, CalleeIdName}
     ,{<<"Caller-ID-Number">>, CIDNum}
     ,{<<"Caller-ID-Name">>, CIDName}
     ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
     ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj, <<"false">>)}
     ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
     ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
     ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
     ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
     ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
     ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, CCVs)}
     ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
    ].

-spec bridge_caller_id(boolean(), wh_json:object()) -> {api_binary(), api_binary()}.
bridge_caller_id('true', JObj) ->
    lager:debug("outbound call is using an emergency route, attempting to set CID accordingly"),
    {emergency_cid_number(JObj)
     ,wh_json:get_first_defined([<<"Emergency-Caller-ID-Name">>
                                 ,<<"Outbound-Caller-ID-Name">>
                                ], JObj)
    };
bridge_caller_id('false', JObj) ->
    {wh_json:get_first_defined([<<"Outbound-Caller-ID-Number">>
                                ,<<"Emergency-Caller-ID-Number">>
                               ], JObj)
     ,wh_json:get_first_defined([<<"Outbound-Caller-ID-Name">>
                                 ,<<"Emergency-Caller-ID-Name">>
                                ], JObj)
    }.
    
-spec bridge_from_uri(ne_binary(), wh_json:object()) -> api_binary().
bridge_from_uri(CIDNum, JObj) ->
    case whapps_config:get_is_true(?APP_NAME, <<"format_from_uri">>, 'false') of
        'true' ->
            case {CIDNum, wh_json:get_value(<<"Account-Realm">>, JObj)} of
                {'undefined', _} -> 'undefined';
                {_, 'undefined'} -> 'undefined';
                {FromNumber, FromRealm} ->
                    <<"sip:", FromNumber/binary, "@", FromRealm/binary>>
            end;
        'false' -> 'undefined'
    end.

-spec emergency_cid_number(wh_json:object()) -> ne_binary().
emergency_cid_number(JObj) ->
    Account = wh_json:get_value(<<"Account-ID">>, JObj),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    Candidates = [wh_json:get_ne_value(<<"Emergency-Caller-ID-Number">>, JObj)
                  ,wh_json:get_ne_value(<<"Outbound-Caller-ID-Number">>, JObj)
                 ],
    Requested = wh_json:get_first_defined([<<"Emergency-Caller-ID-Number">>
                                           ,<<"Outbound-Caller-ID-Number">>
                                          ], JObj),
    case couch_mgr:open_cache_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC) of
        {'ok', PhoneNumbers} ->
            Numbers = wh_json:get_keys(wh_json:public_fields(PhoneNumbers)),
            E911Enabled = [Number
                           || Number <- Numbers
                                  ,dash_e911_enabled(Number, PhoneNumbers)
                          ],
            emergency_cid_number(Requested, Candidates, E911Enabled);
        {'error', _R} ->
            lager:error("unable to fetch the ~s from account ~s: ~p", [?WNM_PHONE_NUMBER_DOC, Account, _R]),
            emergency_cid_number(Requested, Candidates, [])
    end.

-spec dash_e911_enabled(ne_binary(), wh_json:object()) -> boolean().
dash_e911_enabled(Number, PhoneNumbers) ->
    lists:member(<<"dash_e911">>, wh_json:get_value([Number, <<"features">>], PhoneNumbers, [])).

-spec emergency_cid_number(ne_binary(), api_binaries(), ne_binaries()) -> ne_binary().
%% if there are no e911 enabled numbers then either use the global system default
%% or the requested (if there isnt one)
emergency_cid_number(Requested, _, []) ->
    case whapps_config:get_non_empty(<<"stepswitch">>, <<"default_emergency_cid_number">>) of
        'undefined' -> Requested;
        DefaultE911 -> DefaultE911
    end;
%% If neither their emergency cid or outgoung cid is e911 enabled but their account
%% has other numbers with e911 then use the first...
emergency_cid_number(_, [], [E911Enabled|_]) -> E911Enabled;
%% due to the way we built the candidates list it can contain the atom 'undefined'
%% handle that condition (ignore)
emergency_cid_number(Requested, ['undefined'|Candidates], E911Enabled) ->
    emergency_cid_number(Requested, Candidates, E911Enabled);
%% check if the first non-atom undefined element in the list is in the list of
%% e911 enabled numbers, if so use it otherwise keep checking.
emergency_cid_number(Requested, [Candidate|Candidates], E911Enabled) ->
    case lists:member(Candidate, E911Enabled) of
        'true' -> Candidate;
        'false' -> emergency_cid_number(Requested, Candidates, E911Enabled)
    end.


execute_bridge(Command, Q, CtrlQ) ->    


    case {Result, correct_shortdial(Number, JObj)} of
        {{'error', 'no_resources'}, 'fail'} -> Result;
        {{'error', 'no_resources'}, CorrectedNumber} ->
            lager:debug("found no resources for number as dialed, retrying number corrected for shortdial as ~s", [CorrectedNumber]),
            attempt_to_fulfill_bridge_req(CorrectedNumber, CtrlQ, JObj, Props);
        _Else -> Result
    end.
    wapi_dialplan:publish_command(CtrlQ, [wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION) ++ props:filter_undefined(Command)]),
    wait_for_bridge(whapps_config:get_integer(<<"stepswitch">>, <<"bridge_timeout">>, 30000)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the provided resources contain a emergency route
%% @end
%%--------------------------------------------------------------------
-spec contains_emergency_endpoints(wh_json:objects()) -> boolean().
contains_emergency_endpoints([]) -> 'false';
contains_emergency_endpoints([Endpoint|Endpoints]) ->
    case wh_json:is_true(?CCV(<<"Emergency-Resource">>), Endpoint) of
        'true' -> 'true';
        'false' -> contains_emergency_endpoints(Endpoints)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume AMQP messages waiting for the channel to end or the
%% the bridge to complete.  However, if we receive a rate
%% response then set the CCVs accordingly.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_bridge(wh_timeout()) -> bridge_resp().
wait_for_bridge(Timeout) ->
    Start = erlang:now(),
    receive
        {#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type=CT}, payload=Payload}} ->
            JObj = wh_json:decode(Payload, CT),
            case get_event_type(JObj) of
                {<<"error">>, <<"dialplan">>, _} -> {'error', JObj};
                {<<"call_event">>, <<"CHANNEL_BRIDGE">>, _} ->
                    CallId = wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj),
                    lager:debug("outbound request bridged to call ~s", [CallId]),
                    wait_for_bridge('infinity');
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} -> hangup_result(JObj);
                {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>} -> hangup_result(JObj);
                _ -> wait_for_bridge(whapps_util:decr_timeout(Timeout, Start))
            end;
        _ -> wait_for_bridge(whapps_util:decr_timeout(Timeout, Start))
    after Timeout ->
            lager:debug("timed out after ~p ms", [Timeout]),
            {'error', 'timeout'}
    end.
