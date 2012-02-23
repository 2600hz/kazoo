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
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    {ok, Srv} = conf_participant_sup:start_participant(Call),
    conf_participant:consume_call_events(Srv),
    whapps_call_command:answer(Call),
%%    whapps_call_command:b_prompt(<<"conf-welcome">>, Call),
    case validate_conference_id(wh_json:get_value(<<"Conference-ID">>, JObj), Call) of
        {error, _} -> 
            %% TODO: send discovery error
            ok;
        {ok, #conference{id=ConferenceId}=C1} ->
            C2 = case wh_json:is_true(<<"Moderator">>, JObj, undefined) of
                     undefined -> C1;
                     true -> C1#conference{join_as_moderator=true};
                     false -> C1#conference{join_as_moderator=false}
                 end,
            %% TODO: This shouldnt crash, so we can send a discovery error back
            {ok, C3} = validate_conference_pin(C2, Call),
            conf_participant:set_conference(C3, Srv),
            conf_participant:set_discovery_event(JObj, Srv),
            {ok, Cache} = conference_sup:cache_proc(),
            SearchId = couch_mgr:get_uuid(),
            wh_cache:store_local(Cache, {?MODULE, discovery, SearchId}, Srv, 300),
            %% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            %%   TODO: Q MUST be a targeted queue...
            %% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            Q = props:get_value(queue, Props),
            Search = [{<<"Conference-ID">>, ConferenceId}
                      ,{<<"Msg-ID">>, SearchId}
                      | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                     ],
            wapi_conference:publish_search_req(Search)
    end,
    ok.

-spec handle_search_error/2 :: (wh_json:json_object(), proplist()) -> ok.
handle_search_error(JObj, Props) ->
    <<"search_req">> = wh_json:get_value([<<"Request">>, <<"Event-Name">>], JObj),
    SearchId = wh_json:get_value(<<"Msg-ID">>, JObj),
    {ok, Cache} = conference_sup:cache_proc(),
    {ok, Srv} = wh_cache:fetch_local(Cache, {?MODULE, discovery, SearchId}),
    {ok, Call} = conf_participant:call(Srv),
    conf_participant:consume_call_events(Srv),
    case whapps_call_command:b_channel_status(Call) of
        {error, _} -> 
            %% TODO: send discovery error
            %%    {ok, DiscoveryReq} = conf_participant:discovery_event(Srv),
            ok;
        {ok, Status} ->
            SwitchHostname = wh_json:get_value(<<"Switch-Hostname">>, Status),
            {ok, #conference{id=ConferenceId}=Conference} = conf_participant:conference(Srv),
            case negotiate_focus(SwitchHostname, Conference, Call) of
                {ok, _} -> 
                    io:format("JOIN CONFERENCE ON ~p~n", [SwitchHostname]),
                    conf_participant:join_local(Srv);
                {error, conflict} ->
                    io:format("TRY AGAIN...~n", []),
                    Q = props:get_value(queue, Props),
                    Search = [{<<"Conference-ID">>, ConferenceId}
                              ,{<<"Msg-ID">>, SearchId}
                              | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                             ],
                    wapi_conference:publish_search_req(Search);
                {error, _} ->
                    %% TODO: send discovery error
                    %%    {ok, DiscoveryReq} = conf_participant:discovery_event(Srv),
                    ok
            end
    end,
    ok.

-spec handle_search_resp/2 :: (wh_json:json_object(), proplist()) -> ok.
handle_search_resp(JObj, _Props) ->    
    SearchId = wh_json:get_value(<<"Msg-ID">>, JObj),
    {ok, Cache} = conference_sup:cache_proc(),
    {ok, Srv} = wh_cache:fetch_local(Cache, {?MODULE, discovery, SearchId}),
    {ok, Call} = conf_participant:call(Srv),
    conf_participant:consume_call_events(Srv),
    case whapps_call_command:b_channel_status(Call) of
        {error, _} -> 
            %% TODO: send discovery error
            %%    {ok, DiscoveryReq} = conf_participant:discovery_event(Srv),
            ok;
        {ok, Status} ->
            SwitchHostname = wh_json:get_value(<<"Switch-Hostname">>, Status),    
            case wh_json:get_value(<<"Switch-Hostname">>, JObj) of
                SwitchHostname -> 
                    io:format("JOIN EXISTING CONFERENCE ON ~p~n", [SwitchHostname]),
                    conf_participant:join_local(Srv);
                _Else -> 
                    SwitchUrl = wh_json:get_value(<<"Switch-URL">>, Status),
                    io:format("conf_participant:join_remote(~p, ~p)", [SwitchUrl, Srv])
            end
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
    ?LOG_SYS("starting new conference discovery process"),
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
    ?LOG_SYS("conference discovery ~p termination", [_Reason]),
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
-spec validate_conference_id/2 :: ('undefined' | ne_binary(), whapps_call:call()) -> {ok, #conference{}} |
                                                                                     {error, term()}.
-spec validate_conference_id/3 :: ('undefined' | ne_binary(), whapps_call:call(), pos_integer()) -> {ok, #conference{}} |
                                                                                                    {error, term()}.
validate_conference_id(ConferenceId, Call) ->
    validate_conference_id(ConferenceId, Call, 1).

validate_conference_id(undefined, Call, Loop) when Loop > 3 ->
    ?LOG_END("caller has failed to provide a valid conference number to many times"),    
    whapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {error, to_many_attempts};
validate_conference_id(undefined, Call, Loop) ->
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"conf-enter_conf_number">>, <<"1">>, Call) of
        {error, _}=E -> E;
        {ok, Digits} ->
            AccountDb = whapps_call:account_db(Call),
            ViewOptions = [{<<"key">>, Digits}
                           ,{<<"include_docs">>, true}
                          ],
            case couch_mgr:get_results(AccountDb, <<"conference/listing_by_number">>, ViewOptions) of
                {ok, [JObj]} -> 
                    {ok, create_record(wh_json:get_value(<<"doc">>, JObj), Digits)};
                _Else ->
                    ?LOG("could not find conference number ~s: ~p", [Digits, _Else]),
                    whapps_call_command:b_prompt(<<"conf-bad_conf">>, Call),
                    validate_conference_id(undefined, Call, Loop + 1)
            end
    end;
validate_conference_id(ConferenceId, Call, Loop) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, ConferenceId) of
        {ok, JObj} -> {ok, JObj};
        _Else ->
            ?LOG("could not find conference ~s: ~p", [ConferenceId, _Else]),
            validate_conference_id(undefined, Call, Loop)
    end.

-spec validate_conference_pin/2 :: (#conference{}, whapps_call:call()) -> #conference{}.
-spec validate_conference_pin/3 :: (#conference{}, whapps_call:call(), pos_integer()) -> #conference{}.

validate_conference_pin(#conference{join_as_moderator=true, moderator_pins=[]}=Conf, _) ->
    {ok, Conf};
validate_conference_pin(#conference{join_as_moderator=false, member_pins=[]}=Conf, _) ->
    {ok, Conf};
validate_conference_pin(Conference, Call) ->
    validate_conference_pin(Conference, Call, 1).

validate_conference_pin(_, Call, Loop) when Loop > 3->
    ?LOG_END("caller has failed to provide a valid conference pin to many times"),    
    whapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {error, to_many_attempts};
validate_conference_pin(#conference{join_as_moderator=true, moderator_pins=Pins}=Conf, Call, Loop) ->
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"conf-enter_conf_pin">>, <<"1">>, Call) of
        {error, _}=E -> E;
        {ok, Digits} ->
            case lists:member(Digits, Pins) of
                true ->            
                    ?LOG("caller entered a valid moderator pin"),
                    {ok, Conf};
                false ->   
                    ?LOG("caller entered an invalid pin"),
                    whapps_call_command:b_prompt(<<"conf-bad_pin">>, Call),
                    validate_conference_pin(Conf, Call, Loop + 1)
            end
    end;
validate_conference_pin(#conference{join_as_moderator=false, member_pins=Pins}=Conf, Call, Loop) ->
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"conf-enter_conf_pin">>, <<"1">>, Call) of
        {error, _}=E -> E;
        {ok, Digits} ->
            case lists:member(Digits, Pins) of
                true ->            
                    ?LOG("caller entered a valid member pin"),
                    {ok, Conf};
                false ->   
                    ?LOG("caller entered an invalid pin"),
                    whapps_call_command:b_prompt(<<"conf-bad_pin">>, Call),
                    validate_conference_pin(Conf, Call, Loop + 1)
            end
    end;
validate_conference_pin(#conference{member_pins=Member, moderator_pins=Moderator}=Conf, Call, Loop) ->
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"conf-enter_conf_pin">>, <<"1">>, Call) of
        {error, _}=E -> E;
        {ok, Digits} ->
            case {lists:member(Digits, Member), lists:member(Digits, Moderator)} of
                {true, _} ->
                    ?LOG("caller entered a pin belonging to a member"),
                    {ok, Conf#conference{join_as_moderator=false}};
                {false, true} ->
                    ?LOG("caller entered a pin belonging to a moderator"),
                    {ok, Conf#conference{join_as_moderator=true}};
                _Else ->
                    ?LOG("caller entered an invalid pin"),
                    whapps_call_command:b_prompt(<<"conf-bad_pin">>, Call),
                    validate_conference_pin(Conf, Call, Loop + 1)
            end
    end.

-spec negotiate_focus/3 :: ('undefined' | ne_binary(), #conference{}, whapps_call:call()) -> {ok, wh_json:object()} |
                                                                                             {error, term()}.
negotiate_focus(undefined, _, _) ->
    {error, hungup};
negotiate_focus(SwitchHostname, #conference{conference_doc=JObj}, Call) ->
    AccountDb = whapps_call:account_db(Call),
    couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"focus">>, SwitchHostname, JObj)).

-spec create_record/2 :: (wh_json:json_object(), wh_json:json_object()) -> #conference{}.
create_record(JObj, Digits) ->
    Conf = #conference{},
    ModeratorNumbers = wh_json:get_value([<<"moderator">>, <<"numbers">>], JObj, []),
    MemberNumbers = wh_json:get_value([<<"member">>, <<"numbers">>], JObj, []),
    Moderator = case {lists:member(Digits, MemberNumbers), lists:member(Digits, ModeratorNumbers)} of
                    {true, false} -> false;
                    {false, true} -> true;
                    %% the conference number is ambiguous regarding member: either both have the same number
                    %%   or they joined by the discovery event having the conference id
                    _Else -> undefined
                end,
    #conference{id = wh_json:get_value(<<"_id">>, JObj, Conf#conference.id)
                ,focus = wh_json:get_value(<<"focus">>, JObj, Conf#conference.focus)               
                ,bridge_password = wh_json:get_value(<<"bridge_password">>, JObj, Conf#conference.bridge_password) 
                ,bridge_username = wh_json:get_value(<<"bridge_username">>, JObj, Conf#conference.bridge_username) 
                ,member_pins = wh_json:get_value([<<"member">>, <<"pins">>], JObj, Conf#conference.member_pins)
                ,moderator_pins = wh_json:get_value([<<"moderator">>, <<"pins">>], JObj, Conf#conference.moderator_pins)
                ,join_as_moderator = Moderator
                ,member_join_muted = wh_json:is_true(<<"member_join_muted">>, JObj, Conf#conference.member_join_muted)
                ,member_join_deaf = wh_json:is_true(<<"member_join_deaf">>, JObj, Conf#conference.member_join_deaf)
                ,moderator_join_muted = wh_json:is_true(<<"moderator_join_muted">>, JObj, Conf#conference.moderator_join_muted) 
                ,moderator_join_deaf = wh_json:is_true(<<"moderator_join_deaf">>, JObj, Conf#conference.moderator_join_deaf)
                ,max_members = wh_json:get_integer_value(<<"max_members">>, JObj, Conf#conference.max_members)
                ,require_moderator = wh_json:is_true(<<"require_moderator">>, JObj, Conf#conference.require_moderator)
                ,wait_for_moderator = wh_json:is_true(<<"wait_for_moderator">>, JObj, Conf#conference.wait_for_moderator)
                ,member_play_name = wh_json:is_true(<<"member_play_name">>, JObj, Conf#conference.member_play_name)
                ,conference_doc = JObj
               }.
    
