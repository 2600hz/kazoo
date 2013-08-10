%%%============================================================================
%%% @copyright (C) 2011-2013 2600Hz Inc
%%% @doc
%%% This module is responsible for the second stage in the conference process:
%%% 1. Determine if an arbitrary call (on an arbitrary server) is for a
%%%    conference.  If so acquire control of the call.
%%% 2. Discovery, collect enough information to determine the global identifier
%%%    of the conference, locate/start the service, and transfer control
%%% 3. Execute the conference, move new members to a conference focus, provide
%%%    in conference features, location services, and state.
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%   James Aimonetti <james@2600hz.org>
%%%============================================================================
-module(conf_discovery).

-behaviour(gen_listener).

%% API
-export([start_link/0]).
-export([handle_discovery_req/2
         ,handle_config_req/2
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-define(RESPONDERS, [{{?MODULE, 'handle_discovery_req'}
                      ,[{<<"conference">>, <<"discovery_req">>}]
                     }
                     ,{{?MODULE, 'handle_config_req'}
                       ,[{<<"conference">>, <<"config_req">>}]
                      }
                    ]).
-define(BINDINGS, [{'conference', [{'restrict_to', ['discovery', 'config']}]}
                   ,{'self', []}
                  ]).
-define(QUEUE_NAME, <<"conference_discovery">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-include("conference.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE,
                            [{'responders', ?RESPONDERS}
                             ,{'bindings', ?BINDINGS}
                             ,{'queue_name', ?QUEUE_NAME}
                             ,{'queue_options', ?QUEUE_OPTIONS}
                             ,{'consume_options', ?CONSUME_OPTIONS}
                            ], []).

-spec handle_discovery_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_discovery_req(JObj, _) ->
    'true' = wapi_conference:discovery_req_v(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    put('callid', whapps_call:call_id(Call)),
    case conf_participant_sup:start_participant(Call) of
        {'ok', Srv} ->
            conf_participant:set_discovery_event(JObj, Srv),
            conf_participant:consume_call_events(Srv),
            whapps_call_command:answer(Call),
            welcome_to_conference(Call, Srv, JObj);
        _Else ->
            discovery_failed(Call, 'undefined')
    end.

handle_config_req(JObj, _Props) ->
    'true' = wapi_conference:config_req_v(JObj),
    ConfigName = wh_json:get_value(<<"Profile">>, JObj),

    fetch_config(JObj, ConfigName).

fetch_config(JObj, <<"default">> = ConfigName) ->
    Config =  whapps_config:get(<<"conferences">>, [<<"profiles">>, ConfigName], wh_json:from_list(?DEFAULT_PROFILE_CONFIG)),
    fetch_config(JObj, ConfigName, Config);
fetch_config(JObj, ConfigName) ->
    Config =  whapps_config:get(<<"conferences">>, [<<"profiles">>, ConfigName]),
    fetch_config(JObj, ConfigName, Config).

fetch_config(_JObj, _ConfigName, 'undefined') ->
    lager:debug("no profile defined for ~s", [_ConfigName]);
fetch_config(JObj, ConfigName, Profile) ->
    lager:debug("profile ~s found", [ConfigName]),
    Resp = [{<<"Profiles">>, wh_json:from_list([{ConfigName, Profile}])}
            ,{<<"Caller-Controls">>, caller_controls(ConfigName)}
            ,{<<"Advertise">>, advertise(ConfigName)}
            ,{<<"Chat-Permissions">>, chat_permissions(ConfigName)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    try
        wapi_conference:publish_config_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                            ,props:filter_undefined(Resp)
                                           )
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST)
    end.

caller_controls(<<"default">> = ConfigName) ->
    caller_controls(ConfigName, whapps_config:get(<<"conferences">>, [<<"caller-controls">>, ConfigName], ?DEFAULT_CALLER_CONTROLS_CONFIG));
caller_controls(ConfigName) ->
    caller_controls(ConfigName, whapps_config:get(<<"conferences">>, [<<"caller-controls">>, ConfigName])).

caller_controls(_ConfigName, 'undefined') -> 'undefined';
caller_controls(ConfigName, Controls) ->
    wh_json:from_list([{ConfigName, Controls}]).

advertise(<<"default">> = ConfigName) ->
    advertise(ConfigName, whapps_config:get(<<"conferences">>, [<<"advertise">>, ConfigName], ?DEFAULT_ADVERTISE_CONFIG));
advertise(ConfigName) ->
    advertise(ConfigName, whapps_config:get(<<"conferences">>, [<<"advertise">>, ConfigName])).

advertise(_ConfigName, 'undefined') -> 'undefined';
advertise(ConfigName, Advertise) -> wh_json:from_list([{ConfigName, Advertise}]).

chat_permissions(<<"default">> = ConfigName) ->
    chat_permissions(ConfigName, whapps_config:get(<<"conferences">>, [<<"chat-permissions">>, ConfigName], ?DEFAULT_CHAT_CONFIG));
chat_permissions(ConfigName) ->
    chat_permissions(ConfigName, whapps_config:get(<<"conferences">>, [<<"chat-permissions">>, ConfigName])).
chat_permissions(_ConfigName, 'undefined') -> 'undefined';
chat_permissions(ConfigName, Chat) -> wh_json:from_list([{ConfigName, Chat}]).

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
    {'ok', 'ok'}.

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
    {'reply', 'ok', State}.

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
%% Handling all amqp messages
%%
%% @spec handle_event(JObj, Props) -> {reply, Props} |
%%                                    ignore
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
    lager:debug("conference discovery ~p termination", [_Reason]).

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
-spec welcome_to_conference(whapps_call:call(), pid(), wh_json:object()) -> 'ok'.
welcome_to_conference(Call, Srv, DiscoveryJObj) ->
    whapps_call_command:prompt(<<"conf-welcome">>, Call),
    maybe_collect_conference_id(Call, Srv, DiscoveryJObj).

-spec maybe_collect_conference_id(whapps_call:call(), pid(), wh_json:object()) -> 'ok'.
maybe_collect_conference_id(Call, Srv, DiscoveryJObj) ->
    case wh_json:get_value(<<"Conference-Doc">>, DiscoveryJObj) of
        'undefined' -> collect_conference_id(Call, Srv);
        Doc ->
            N = wh_json:get_value(<<"name">>, Doc, wh_util:rand_hex_binary(8)),
            lager:debug("conf doc (~s) set instead of conf id", [N]),
            Conference = whapps_conference:set_id(N, create_conference(Doc, <<"none">>)),
            maybe_collect_conference_pin(Conference, Call, Srv)
    end.

-spec collect_conference_id(whapps_call:call(), pid()) -> 'ok'.
collect_conference_id(Call, Srv) ->
    {'ok', JObj} = conf_participant:discovery_event(Srv),
    ConferenceId = wh_json:get_value(<<"Conference-ID">>, JObj),
    case validate_conference_id(ConferenceId, Call) of
        {'ok', Conference} ->
            maybe_collect_conference_pin(Conference, Call, Srv);
        {'error', _} ->
            discovery_failed(Call, Srv)
    end.

-spec maybe_collect_conference_pin(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
maybe_collect_conference_pin(Conference, Call, Srv) ->
    case whapps_conference:moderator(Conference) of
        'true' ->
            case whapps_conference:moderator_pins(Conference) of
                [] ->
                    lager:debug("moderator entry requires no pin"),
                    prepare_whapps_conference(Conference, Call, Srv);
                _Else ->
                    collect_conference_pin('true', Conference, Call, Srv)
            end;
        'false' ->
            case whapps_conference:member_pins(Conference) of
                [] ->
                    lager:debug("member entry requires no pin"),
                    prepare_whapps_conference(Conference, Call, Srv);
                _Else ->
                    collect_conference_pin('false', Conference, Call, Srv)
            end;
        _Else ->
            case wh_util:is_empty(whapps_conference:moderator_pins(Conference))
                andalso wh_util:is_empty(whapps_conference:member_pins(Conference))
            of
                'false' ->
                    collect_conference_pin('undefined', Conference, Call, Srv);
                'true' ->
                    C = whapps_conference:set_moderator('false', Conference),
                    prepare_whapps_conference(C, Call, Srv)
            end
    end.

-spec collect_conference_pin(boolean() | 'undefined', whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
collect_conference_pin(Type, Conference, Call, Srv) ->
    case validate_conference_pin(Type, Conference, Call, 1) of
        {'ok', C} ->
            prepare_whapps_conference(C, Call, Srv);
        {'error', _} ->
            discovery_failed(Call, Srv)
    end.

-spec prepare_whapps_conference(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
prepare_whapps_conference(Conference, Call, Srv) ->
    Routines = [fun(C) ->
                        {'ok', JObj} = conf_participant:discovery_event(Srv),
                        case wh_json:is_true(<<"Moderator">>, JObj, 'false')
                            orelse wh_json:is_true([<<"Conference-Doc">>, <<"moderator">>], JObj)
                        of
                            'undefined' -> C;
                            'true' ->
                                lager:debug("discovery request defines participant as moderator, overriding previous value"),
                                whapps_conference:set_moderator('true', C);
                            'false' ->
                                lager:debug("discovery request defines participant as member, overriding previous value"),
                                whapps_conference:set_moderator('false', C)
                        end
                end
                ,fun(C) -> whapps_conference:set_application_version(<<"2.0.0">>, C) end
                ,fun(C) -> whapps_conference:set_application_name(<<"conferences">>, C) end
               ],
    C = whapps_conference:update(Routines, Conference),
    _ = case whapps_conference:play_entry_prompt(C) of
            'false' -> 'ok';
            'true' -> whapps_call_command:prompt(<<"conf-joining_conference">>, Call)
        end,
    search_for_conference(C, Call, Srv).

-spec search_for_conference(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
search_for_conference(Conference, Call, Srv) ->
    case whapps_conference_command:search(Conference) of
        {'error', _} -> handle_search_error(Conference, Call, Srv);
        {'ok', JObj} -> handle_search_resp(JObj, Conference, Call, Srv)
    end.

-spec handle_search_error(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
handle_search_error(Conference, Call, Srv) ->
    wh_amqp_channel:remove_consumer_pid(),
    Queue = whapps_conference:id(Conference),
    _ = amqp_util:new_queue(Queue),
    try amqp_util:basic_consume(Queue, [{'exclusive', 'true'}]) of
        'ok' ->
            lager:debug("initial participant creating conference on switch nodename '~p'", [whapps_call:switch_hostname(Call)]),
            conf_participant:set_conference(Conference, Srv),
            conf_participant:join_local(Srv),
            wait_for_creation(Conference)
    catch
        'exit':{{'shutdown', {'server_initiated_close', 403, <<"ACCESS_REFUSED", _/binary>>}}, _} ->
            lager:debug("conference queue ~s is exclusive, waiting for conference creation by initial participant", [Queue]),
            handle_resource_locked(Conference, Call, Srv)
    end.

-spec handle_resource_locked(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
handle_resource_locked(Conference, Call, Srv) ->
    case wait_for_creation(Conference) of
        {'ok', JObj} -> handle_search_resp(JObj, Conference, Call, Srv);
        {'error', 'timeout'} -> handle_search_error(Conference, Call, Srv)
    end.

-spec wait_for_creation(whapps_conference:conference()) -> {'ok', wh_json:object()} | {'error', 'timeout'}.
wait_for_creation(Conference) ->
    lager:debug("checking if conference ~s has been created", [whapps_conference:id(Conference)]),
    wait_for_creation(Conference, 8000).

-spec wait_for_creation(whapps_conference:conference(), non_neg_integer()) -> {'ok', wh_json:object()} | {'error', 'timeout'}.
wait_for_creation(_, After) when After =< 0 -> {'error', 'timeout'};
wait_for_creation(Conference, After) ->
    Start = erlang:now(),
    case whapps_conference_command:search(Conference) of
        {'ok', _}=Ok -> Ok;
        {'error', _} -> 
            timer:sleep(1000),
            wait_for_creation(Conference, whapps_util:decr_timeout(After, Start))
    end.

-spec handle_search_resp(wh_json:object(), whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
handle_search_resp(JObj, Conference, Call, Srv) ->
    lager:debug("participant switch nodename ~p", [whapps_call:switch_hostname(Call)]),
    SwitchHostname = whapps_call:switch_hostname(Call),
    case wh_json:get_value(<<"Switch-Hostname">>, JObj) of
        SwitchHostname ->
            lager:debug("running conference is on the same switch, joining on ~s", [SwitchHostname]),
            conf_participant:set_conference(Conference, Srv),
            conf_participant:join_local(Srv);
        _Else ->
            lager:debug("running conference is on a different switch, bridging to ~s: ~p", [_Else, JObj]),
            conf_participant:set_conference(Conference, Srv),
            conf_participant:join_remote(Srv, JObj)
    end.

-spec discovery_failed(whapps_call:call(), pid()) -> 'ok'.
discovery_failed(Call, _) -> whapps_call_command:hangup(Call).

-spec validate_conference_id(api_binary(), whapps_call:call()) ->
                                    {'ok', whapps_conference:conference()} |
                                    {'error', term()}.
validate_conference_id(ConferenceId, Call) ->
    validate_conference_id(ConferenceId, Call, 1).

-spec validate_conference_id(api_binary(), whapps_call:call(), pos_integer()) ->
                                    {'ok', whapps_conference:conference()} |
                                    {'error', term()}.
validate_conference_id('undefined', Call, Loop) when Loop > 3 ->
    lager:debug("caller has failed to provide a valid conference number to many times"),
    _ = whapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {'error', 'too_many_attempts'};
validate_conference_id('undefined', Call, Loop) ->
    lager:debug("requesting conference id from caller"),
    case whapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_number">>, 1, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            AccountDb = whapps_call:account_db(Call),
            ViewOptions = [{'key', Digits}
                           ,'include_docs'
                          ],
            case couch_mgr:get_results(AccountDb, <<"conference/listing_by_number">>, ViewOptions) of
                {'ok', [JObj]} ->
                    lager:debug("caller has entered a valid conference id, building object"),
                    {'ok', create_conference(wh_json:get_value(<<"doc">>, JObj), Digits)};
                _Else ->
                    lager:debug("could not find conference number ~s: ~p", [Digits, _Else]),
                    _ = whapps_call_command:prompt(<<"conf-bad_conf">>, Call),
                    validate_conference_id('undefined', Call, Loop + 1)
            end
    end;
validate_conference_id(ConferenceId, Call, Loop) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, ConferenceId) of
        {'ok', JObj} ->
            lager:debug("discovery request contained a valid conference id, building object"),
            {'ok', create_conference(JObj, <<"none">>)};
        _Else ->
            lager:debug("could not find conference ~s: ~p", [ConferenceId, _Else]),
            validate_conference_id('undefined', Call, Loop)
    end.

-spec validate_conference_pin('undefined' | boolean(), whapps_conference:conference(), whapps_call:call(), pos_integer()) ->
                                     {'ok', whapps_conference:conference()} |
                                     {'error', term()}.
validate_conference_pin(_, _, Call, Loop) when Loop > 3->
    lager:debug("caller has failed to provide a valid conference pin to many times"),
    _ = whapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {'error', 'too_many_attempts'};
validate_conference_pin('true', Conference, Call, Loop) ->
    lager:debug("requesting moderator pin from caller"),
    case whapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_pin">>, 1, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            Pins = whapps_conference:moderator_pins(Conference),
            case lists:member(Digits, Pins)
                orelse (Pins =:= [] andalso Digits =:= <<>>)
            of
                'true' ->
                    lager:debug("caller entered a valid moderator pin"),
                    {'ok', Conference};
                'false' ->
                    lager:debug("caller entered an invalid pin"),
                    _ = whapps_call_command:prompt(<<"conf-bad_pin">>, Call),
                    validate_conference_pin('true', Conference, Call, Loop + 1)
            end
    end;
validate_conference_pin('false', Conference, Call, Loop) ->
    lager:debug("requesting member pin from caller"),
    case whapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_pin">>, 1, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            Pins = whapps_conference:member_pins(Conference),
            case lists:member(Digits, Pins)
                orelse (Pins =:= [] andalso Digits =:= <<>>)
            of
                'true' ->
                    lager:debug("caller entered a valid member pin"),
                    {'ok', Conference};
                'false' ->
                    lager:debug("caller entered an invalid pin"),
                    _ = whapps_call_command:prompt(<<"conf-bad_pin">>, Call),
                    validate_conference_pin('false', Conference, Call, Loop + 1)
            end
    end;
validate_conference_pin(_, Conference, Call, Loop) ->
    lager:debug("requesting conference pin from caller, which will be used to disambiguate member/moderator"),
    case whapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_pin">>, 1, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            MemberPins = whapps_conference:member_pins(Conference),
            ModeratorPins = whapps_conference:moderator_pins(Conference),
            case {(lists:member(Digits, MemberPins)
                   orelse (MemberPins =:= [] andalso Digits =:= <<>>))
                  ,(lists:member(Digits, ModeratorPins)
                    orelse (MemberPins =:= [] andalso Digits =:= <<>>))}
            of
                {'true', _} ->
                    lager:debug("caller entered a pin belonging to a member"),
                    {'ok', whapps_conference:set_moderator('false', Conference)};
                {'false', 'true'} ->
                    lager:debug("caller entered a pin belonging to a moderator"),
                    {'ok', whapps_conference:set_moderator('true', Conference)};
                _Else ->
                    lager:debug("caller entered an invalid pin"),
                    _ = whapps_call_command:prompt(<<"conf-bad_pin">>, Call),
                    validate_conference_pin('undefined', Conference, Call, Loop + 1)
            end
    end.

-spec create_conference(wh_json:object(), binary()) -> whapps_conference:conference().
create_conference(JObj, Digits) ->
    Conference = whapps_conference:from_conference_doc(JObj),
    ModeratorNumbers = wh_json:get_value([<<"moderator">>, <<"numbers">>], JObj, []),
    MemberNumbers = wh_json:get_value([<<"member">>, <<"numbers">>], JObj, []),
    case {lists:member(Digits, MemberNumbers), lists:member(Digits, ModeratorNumbers)} of
        {'true', 'false'} ->
            lager:debug("the digits used to find the conference where unambiguously a member"),
            whapps_conference:set_moderator('false', Conference);
        {'false', 'true'} ->
            lager:debug("the digits used to find the conference where unambiguously a moderator"),
            whapps_conference:set_moderator('true', Conference);
        %% the conference number is ambiguous regarding member: either both have the same number
        %%   or they joined by the discovery event having the conference id
        _Else -> Conference
    end.
