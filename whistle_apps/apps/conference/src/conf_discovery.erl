%%%============================================================================
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011 VoIP Inc
%%% @doc
%%% This module is responsible for the second stage in the conference process:
%%% 1. Determine if an arbitrary call (on an arbitrary server) is for a
%%%    conference.  If so acquire control of the call.
%%% 2. Discovery, collect enough information to determine the global identifier
%%%    of the conference, locate/start the service, and transfer control
%%% 3. Execute the conference, move new members to a conference focus, provide
%%%    in conference features, location services, and state.
%%% @end
%%% Created : 28 Jun 2011 by Karl Anderson <karl@2600hz.org>
%%%============================================================================
-module(conf_discovery).

-behaviour(gen_listener).

%% API
-export([start_link/0]).
-export([handle_discovery_req/2]).
-export([handle_search_resp/2]).
-export([handle_search_error/2]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-define(RESPONDERS, [{{?MODULE, handle_discovery_req}, [{<<"conference">>, <<"discovery_req">>}]}
                     ,{{?MODULE, handle_search_resp}, [{<<"conference">>, <<"search_resp">>}]}
                     ,{{?MODULE, handle_search_error}, [{<<"conference">>, <<"error">>}]}
                    ]).
-define(BINDINGS, [{conference, [{restrict_to, [discovery]}]}
                   ,{self, []}
                  ]).
-define(QUEUE_NAME, <<"conference_discovery">>).
-define(QUEUE_OPTIONS, [{exclusive, false}]).
-define(CONSUME_OPTIONS, [{exclusive, false}]).

-include("conference.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE,
                            [{responders, ?RESPONDERS}
                             ,{bindings, ?BINDINGS}
                             ,{queue_name, ?QUEUE_NAME}
                             ,{queue_options, ?QUEUE_OPTIONS}
                             ,{consume_options, ?CONSUME_OPTIONS}
                             ,{basic_qos, 1}
                            ], []).

-spec handle_discovery_req/2 :: (wh_json:json_object(), proplist()) -> ok.
handle_discovery_req(JObj, Props) ->
    true = wapi_conference:discovery_req_v(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    put(callid, whapps_call:call_id(Call)),
    lager:debug("received conference discovery request"),

    {ok, Srv} = conf_participant_sup:start_participant(Call),
    conf_participant:set_discovery_event(JObj, Srv),
    lager:debug("started participant process as ~p", [Srv]),

    conf_participant:consume_call_events(Srv),
    whapps_call_command:answer(Call),
    whapps_call_command:b_prompt(<<"conf-welcome">>, Call),

    Updaters = [fun(C1) ->
                        {ok, C2} = validate_conference_pin(C1, Call),
                        C2
                end
                ,fun(C) ->
                         case wh_json:is_true(<<"Moderator">>, JObj, undefined) of
                             undefined -> C;
                             true ->
                                 lager:debug("discovery request defines participant as moderator, overriding previous value"),
                                 whapps_conference:moderator(true, C);
                             false ->
                                 lager:debug("discovery request defines participant as member, overriding previous value"),
                                 whapps_conference:moderator(false, C)
                         end
                 end
                ,fun(C) -> 
                         Q = hd(props:get_value(other_queues, Props)),
                         whapps_conference:set_controller_queue(Q, C) 
                 end
                ,fun(C) -> whapps_conference:set_application_version(<<"2.0.0">>, C) end
                ,fun(C) -> whapps_conference:set_application_name(<<"conferences">>, C) end
                ,fun(_) -> 
                         {ok, C} = validate_conference_id(wh_json:get_value(<<"Conference-ID">>, JObj), Call),
                         C
                 end
               ],
    Conference = whapps_conference:update(Updaters, whapps_conference:new()),
    conf_participant:set_conference(Conference, Srv),
    SearchId = couch_mgr:get_uuid(),
    wh_cache:store_local(?CONFERENCE_CACHE, {?MODULE, discovery, SearchId}, Srv, 300),
    lager:debug("publishing conference search request ~s", [SearchId]),
    whapps_conference_command:search(SearchId, Conference),
    whapps_call_command:prompt(<<"conf-joining_conference">>, Call), 
    %% TODO: send discovery event on error
    %%    {ok, DiscoveryReq} = conf_participant:discovery_event(Srv),
    ok.

-spec handle_search_error/2 :: (wh_json:json_object(), proplist()) -> ok.
handle_search_error(JObj, _Props) ->
    true = wapi_conference:conference_error_v(JObj),
    <<"search_req">> = wh_json:get_value([<<"Request">>, <<"Event-Name">>], JObj),
    SearchId = wh_json:get_value(<<"Msg-ID">>, JObj),
    lager:debug("recieved search request error for ~s", [SearchId]),

    {ok, Srv} = wh_cache:fetch_local(?CONFERENCE_CACHE, {?MODULE, discovery, SearchId}),
    lager:debug("found participant process as ~p", [Srv]),

    {ok, Conference} = conf_participant:conference(Srv),
    {ok, Call} = conf_participant:call(Srv),
    put(callid, whapps_call:call_id(Call)),
    conf_participant:consume_call_events(Srv),
    
    {ok, Status} = whapps_call_command:b_channel_status(Call),        
    SwitchHostname = wh_json:get_value(<<"Switch-Hostname">>, Status),
    case negotiate_focus(SwitchHostname, Conference, Call) of
        {ok, _} -> 
            lager:debug("conference is not currently running but our update was accepted, starting on ~s", [SwitchHostname]),
            conf_participant:join_local(Srv);
        {error, conflict} ->
            lager:debug("conference is not currently running but our update was in conflict, searching again"),
            whapps_conference_command:search(SearchId, Conference);
        {error, _R} ->
            lager:debug("conference is not currently running but our update failed: ~p", [_R]),
            %% TODO: send discovery error
            %%    {ok, DiscoveryReq} = conf_participant:discovery_event(Srv),
            ok
    end,
    ok.

-spec handle_search_resp/2 :: (wh_json:json_object(), proplist()) -> ok.
handle_search_resp(JObj, _Props) ->    
    true = wapi_conference:search_resp_v(JObj),
    SearchId = wh_json:get_value(<<"Msg-ID">>, JObj),
    lager:debug("recieved search response for ~s", [SearchId]),

    {ok, Srv} = wh_cache:fetch_local(?CONFERENCE_CACHE, {?MODULE, discovery, SearchId}),
    lager:debug("found participant process as ~p", [Srv]),

    {ok, Call} = conf_participant:call(Srv),
    put(callid, whapps_call:call_id(Call)),

    conf_participant:consume_call_events(Srv),
    {ok, Status} = whapps_call_command:b_channel_status(Call),        
    SwitchHostname = wh_json:get_value(<<"Switch-Hostname">>, Status),
    case wh_json:get_value(<<"Switch-Hostname">>, JObj) of
        SwitchHostname -> 
            lager:debug("running conference is on the same switch, joining on ~s", [SwitchHostname]),
            conf_participant:join_local(Srv);
        _Else -> 
            lager:debug("running conference is on a different switch, bridging to ~s", [_Else]),
            conf_participant:join_remote(Srv, JObj)
    end,
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    lager:debug("starting new conference discovery process"),
    Self = self(),
    spawn(fun() ->
                  QueueName = <<>>,
                  Options = [],
                  Bindings= [{self, []}],
                  gen_listener:add_queue(Self, QueueName, Options, Bindings)
          end),
    {ok, ok}.

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
    {reply, ok, State}.
             
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
%% Handling all amqp messages
%%
%% @spec handle_event(JObj, Props) -> {reply, Props} |
%%                                    ignore
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
    lager:debug("conference discovery ~p termination", [_Reason]),
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
-spec validate_conference_id/2 :: ('undefined' | ne_binary(), whapps_call:call()) -> {ok, whapps_conference:conference()} |
                                                                                     {error, term()}.
-spec validate_conference_id/3 :: ('undefined' | ne_binary(), whapps_call:call(), pos_integer()) -> {ok, whapps_conference:conference()} |
                                                                                                    {error, term()}.
validate_conference_id(ConferenceId, Call) ->
    validate_conference_id(ConferenceId, Call, 1).

validate_conference_id(undefined, Call, Loop) when Loop > 3 ->
    lager:debug("caller has failed to provide a valid conference number to many times"),    
    whapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {error, too_many_attempts};
validate_conference_id(undefined, Call, Loop) ->
    lager:debug("requesting conference id from caller"),
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"conf-enter_conf_number">>, <<"1">>, Call) of
        {error, _}=E -> E;
        {ok, Digits} ->
            AccountDb = whapps_call:account_db(Call),
            ViewOptions = [{<<"key">>, Digits}
                           ,{<<"include_docs">>, true}
                          ],
            case couch_mgr:get_results(AccountDb, <<"conference/listing_by_number">>, ViewOptions) of
                {ok, [JObj]} -> 
                    lager:debug("caller has entered a valid conference id, building object"),
                    {ok, create_conference(wh_json:get_value(<<"doc">>, JObj), Digits)};
                _Else ->
                    lager:debug("could not find conference number ~s: ~p", [Digits, _Else]),
                    whapps_call_command:b_prompt(<<"conf-bad_conf">>, Call),
                    validate_conference_id(undefined, Call, Loop + 1)
            end
    end;
validate_conference_id(ConferenceId, Call, Loop) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, ConferenceId) of
        {ok, JObj} -> 
            lager:debug("discovery request contained a valid conference id, building object"),
            {ok, create_conference(JObj, <<"none">>)};
        _Else ->
            lager:debug("could not find conference ~s: ~p", [ConferenceId, _Else]),
            validate_conference_id(undefined, Call, Loop)
    end.

-spec validate_conference_pin/2 :: (whapps_conference:conference(), whapps_call:call()) -> {ok, whapps_conference:conference()} |
                                                                                           {error, term()}.
-spec validate_conference_pin/4 :: (undefined | boolean(), whapps_conference:conference(), whapps_call:call(), pos_integer()) 
                                   -> {ok, whapps_conference:conference()} |
                                      {error, term()}.

validate_conference_pin(Conference, Call) ->
    case whapps_conference:moderator(Conference) of
        true ->
            case whapps_conference:moderator_pins(Conference) of
                [] -> 
                    lager:debug("moderator entry requires no pin"),
                    {ok, Conference};
                _Else -> 
                    validate_conference_pin(true, Conference, Call, 1)
            end;
         false ->
            case whapps_conference:member_pins(Conference) of
                [] -> 
                    lager:debug("member entry requires no pin"),
                    {ok, Conference};
                _Else -> 
                    validate_conference_pin(false, Conference, Call, 1)
            end;
        _Else ->
            validate_conference_pin(undefined, Conference, Call, 1)            
    end.

validate_conference_pin(_, _, Call, Loop) when Loop > 3->
    lager:debug("caller has failed to provide a valid conference pin to many times"),    
    whapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {error, too_many_attempts};
validate_conference_pin(true, Conference, Call, Loop) ->
    lager:debug("requesting moderator pin from caller"),
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"conf-enter_conf_pin">>, <<"1">>, Call) of
        {error, _}=E -> E;
        {ok, Digits} ->
            Pins = whapps_conference:moderator_pins(Conference),
            case lists:member(Digits, Pins) of
                true ->            
                    lager:debug("caller entered a valid moderator pin"),
                    {ok, Conference};
                false ->   
                    lager:debug("caller entered an invalid pin"),
                    whapps_call_command:b_prompt(<<"conf-bad_pin">>, Call),
                    validate_conference_pin(true, Conference, Call, Loop + 1)
            end
    end;
validate_conference_pin(false, Conference, Call, Loop) ->
    lager:debug("requesting member pin from caller"),
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"conf-enter_conf_pin">>, <<"1">>, Call) of
        {error, _}=E -> E;
        {ok, Digits} ->
            Pins = whapps_conference:member_pins(Conference),
            case lists:member(Digits, Pins) of
                true ->            
                    lager:debug("caller entered a valid member pin"),
                    {ok, Conference};
                false ->   
                    lager:debug("caller entered an invalid pin"),
                    whapps_call_command:b_prompt(<<"conf-bad_pin">>, Call),
                    validate_conference_pin(false, Conference, Call, Loop + 1)
            end
    end;
validate_conference_pin(_, Conference, Call, Loop) ->
    lager:debug("requesting conference pin from caller, which will be used to disambiguate member/moderator"),
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"conf-enter_conf_pin">>, <<"1">>, Call) of
        {error, _}=E -> E;
        {ok, Digits} ->
            MemberPins = whapps_conference:member_pins(Conference),
            ModeratorPins = whapps_conference:moderator_pins(Conference),
            case {lists:member(Digits, MemberPins), lists:member(Digits, ModeratorPins)} of
                {true, _} ->
                    lager:debug("caller entered a pin belonging to a member"),
                    {ok, whapps_conference:set_moderator(false, Conference)};
                {false, true} ->
                    lager:debug("caller entered a pin belonging to a moderator"),
                    {ok, whapps_conference:set_moderator(true, Conference)};
                _Else ->
                    lager:debug("caller entered an invalid pin"),
                    whapps_call_command:b_prompt(<<"conf-bad_pin">>, Call),
                    validate_conference_pin(undefined, Conference, Call, Loop + 1)
            end
    end.

-spec negotiate_focus/3 :: ('undefined' | ne_binary(), whapps_conference:conference(), whapps_call:call()) -> {ok, wh_json:object()} |
                                                                                                              {error, term()}.
negotiate_focus(undefined, _, _) ->
    {error, hungup};
negotiate_focus(SwitchHostname, Conference, Call) ->
    AccountDb = whapps_call:account_db(Call),
    JObj = whapps_conference:conference_doc(Conference),
    couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"focus">>, SwitchHostname, JObj)).

-spec create_conference/2 :: (wh_json:json_object(), wh_json:json_object()) -> whapps_conference:conference().
create_conference(JObj, Digits) ->
    Conference = whapps_conference:from_conference_doc(JObj),
    ModeratorNumbers = wh_json:get_value([<<"moderator">>, <<"numbers">>], JObj, []),
    MemberNumbers = wh_json:get_value([<<"member">>, <<"numbers">>], JObj, []),
    case {lists:member(Digits, MemberNumbers), lists:member(Digits, ModeratorNumbers)} of
        {true, false} -> 
            lager:debug("the digits used to find the conference where unambiguously a member"),
            whapps_conference:set_moderator(false, Conference);
        {false, true} -> 
            lager:debug("the digits used to find the conference where unambiguously a moderator"),
            whapps_conference:set_moderator(true, Conference);
        %% the conference number is ambiguous regarding member: either both have the same number
        %%   or they joined by the discovery event having the conference id
        _Else -> Conference
    end.
