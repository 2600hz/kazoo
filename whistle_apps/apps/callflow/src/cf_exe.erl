%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Callflow executioner, executes the head node and waits for further
%%% instructions
%%%
%%% @end
%%% Created:       21 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%% Last Modified: 19 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%============================================================================
-module(cf_exe).

%% API
-export([start_link/2]).
-export([init/3]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets up the call flow executioner for this call, and starts
%% execution of the first node
%% @end
%%--------------------------------------------------------------------
-spec start_link/2 :: (Call, Flow) -> tuple(ok, pid()) when
      Call :: #cf_call{},
      Flow :: json_object().
start_link(Call, Flow) ->
    proc_lib:start_link(?MODULE, init, [self(), Call, Flow]).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% initialize the new call flow execution process
%% @end
%%-----------------------------------------------------------------------------
-spec init/3 :: (Parent, Call, Flow) -> no_return() when
      Parent :: pid(),
      Call :: #cf_call{},
      Flow :: json_object().
init(Parent, #cf_call{authorizing_id=AuthId}=Call, Flow) ->
    process_flag(trap_exit, true),
    _ = call_info(Call),
    AmqpQ = init_amqp(Call),
    proc_lib:init_ack(Parent, {ok, self()}),
    _ = next(Call#cf_call{cf_pid=self()
                          ,amqp_q=AmqpQ
                          ,owner_id=cf_attributes:owner_id(AuthId, Call)}, Flow).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes the top most call flow node on a given call,
%% then waits for the modules reply, unexpected death, or timeout.
%% @end
%%--------------------------------------------------------------------
-spec next/2 :: (Call, Flow) -> no_return() when
      Call :: #cf_call{},
      Flow :: json_object().
next(#cf_call{last_action=LastAction}=Call, Flow) ->
    Module = <<"cf_", (wh_json:get_value(<<"module">>, Flow))/binary>>,
    Data = wh_json:get_value(<<"data">>, Flow),
    {Pid, Action} =
        try
            CF_Module = wh_util:to_atom(Module, true),
            ?LOG("moving to action ~s", [Module]),
            {spawn_link(CF_Module, handle, [Data, Call]), CF_Module}
        catch
            _:_ ->
                ?LOG("unknown action ~s, skipping", [Module]),
                self() ! {continue, <<"_">>},
                {undefined, LastAction}
        end,
    wait(Call#cf_call{last_action=Action}, Flow, Pid).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for the module handling the current call flow node to reply,
%% unexpectly die, or timeout and advance the call flow accordingly
%% @end
%%--------------------------------------------------------------------
-spec wait/3 :: (Call, Flow, Pid) -> no_return() when
      Call :: #cf_call{},
      Flow :: json_object(),
      Pid :: undefined | pid().
wait(Call, Flow, Pid) ->
    receive
        {'EXIT', Pid, Reason} when Reason =/= normal ->
            ?LOG("action ~w died unexpectedly, ~p", [Pid, Reason]),
            self() ! {continue, <<"_">>},
            wait(Call, Flow, Pid);
        {continue} ->
            self() ! {continue, <<"_">>},
            wait(Call, Flow, Pid);
        {continue, Key} ->
            ?LOG("continuing to child ~s", [Key]),
            case wh_json:get_value([<<"children">>, Key], Flow) of
                undefined when Key =:= <<"_">> ->
                    ?LOG_END("wild card child node doesn't exist, we are lost.."),
                    cf_call_command:hangup(Call);
                undefined ->
                    ?LOG("requested child does not exist, trying wild card", [Key]),
                    self() ! {continue},
                    wait(Call, Flow, Pid);
                ?EMPTY_JSON_OBJECT ->
                    ?LOG_END("unexpected end of callflow"),
                    cf_call_command:hangup(Call);
                NewFlow ->
                    next(Call, NewFlow)
            end;
        {get_branch_keys} ->
            {struct, Children} = wh_json:get_value(<<"children">>, Flow, ?EMPTY_JSON_OBJECT),
            Pid ! {branch_keys, lists:delete(<<"_">>, proplists:get_keys(Children))},
            wait(Call, Flow, Pid);
        {get_branch_keys, all} ->
            {struct, Children} = wh_json:get_value(<<"children">>, Flow, ?EMPTY_JSON_OBJECT),
            Pid ! {branch_keys, proplists:get_keys(Children)},
            wait(Call, Flow, Pid);
        {attempt} when not is_pid(Pid) ->
            self() ! {continue, <<"_">>},
            wait(Call, Flow, Pid);
        {attempt, Key} when not is_pid(Pid) ->
            self() ! {continue, Key},
            wait(Call, Flow, Pid);
        {attempt} ->
            self() ! {attempt, <<"_">>},
            wait(Call, Flow, Pid);
        {attempt, Key} ->
            ?LOG("attempting child ~s", [Key]),
            case wh_json:get_value([<<"children">>, Key], Flow) of
                undefined ->
                    Pid ! {attempt_resp, {error, undefined}},
                    wait(Call, Flow, Pid);
                ?EMPTY_JSON_OBJECT ->
                    Pid ! {attempt_resp, {error, empty}},
                    wait(Call, Flow, Pid);
                NewFlow ->
                    Pid ! {attempt_resp, ok},
                    next(Call, NewFlow)
            end;
        {branch, NewFlow} ->
            ?LOG("callflow has been branched"),
            next(Call, NewFlow);
        {stop} ->
            ?LOG_END("execution has been stopped"),
            wait(Call, Flow, Pid);
        %%           cf_call_command:hangup(Call)
        {_, #amqp_msg{props = Props, payload = Payload}} when
              Props#'P_basic'.content_type == <<"application/json">> ->
            JObj = mochijson2:decode(Payload),
            case whapps_util:get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_UNBRIDGE">> } ->
                    io:format("~p~n", [io_lib:format("/tmp/~s.log", [get(callid)])]),
                    R = file:write_file(io_lib:format("/tmp/~s.log", [get(callid)]),
                               io_lib:fwrite("~p~n~n", [JObj]), [append]),
                    io:format("~p~n", [R]);
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    io:format("~p~n", [io_lib:format("/tmp/~s.log", [get(callid)])]),
                    file:write_file(io_lib:format("/tmp/~s.log", [get(callid)]),
                                    io_lib:fwrite("~p~n~n", [JObj]), [append]);
                { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">> } ->
                    case wh_json:get_value(<<"Application-Name">>, JObj) of
                        <<"bridge">> ->
                            file:write_file(io_lib:format("/tmp/~s.log", [get(callid)]),
                                            io_lib:fwrite("~p~n~n", [JObj]), [append]);
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end,
            is_pid(Pid) andalso Pid ! {amqp_msg, JObj},
            wait(Call, Flow, Pid);
        _Msg ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait(Call, Flow, Pid)
    after
        60000 ->
            io:format("terminating exec ~s~n", [self()]),
            ?LOG_SYS("no call events received after 3600 seconds, shuting down")
%%            cf_call_command:hangup(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes a AMQP queue and consumer to recieve call events
%% @end
%%--------------------------------------------------------------------
-spec init_amqp/1 :: (Call) -> binary() when
      Call :: #cf_call{}.
init_amqp(#cf_call{call_id=CallId}) ->
    AmqpQ = amqp_util:new_queue(),
    _ = amqp_util:bind_q_to_callevt(AmqpQ, CallId),
    _ = amqp_util:bind_q_to_targeted(AmqpQ),
    _ = amqp_util:basic_consume(AmqpQ),
    AmqpQ.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% load the callid into to the process dictionary then log information about
%% the call that is about to be processed
%% @end
%%-----------------------------------------------------------------------------
-spec call_info/1 :: (Call) -> ok when
      Call :: #cf_call{}.
call_info(#cf_call{flow_id=FlowId, call_id=CallId, cid_name=CIDName, cid_number=CIDNumber
               ,request=Request, from=From, to=To, inception=Inception, authorizing_id=AuthorizingId }) ->
    put(callid, CallId),
    ?LOG_START("executing callflow ~s", [FlowId]),
    io:format("started exec ~p fpr ~s~n", [self(), Request]),
    ?LOG("request ~s", [Request]),
    ?LOG("to ~s", [To]),
    ?LOG("from ~s", [From]),
    ?LOG("CID ~s ~s", [CIDNumber, CIDName]),
    ?LOG("inception ~s", [Inception]),
    ?LOG("authorizing id ~s", [AuthorizingId]).
