%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_events_listener).

-behaviour(gen_listener).

-export([start_link/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([handle_conference_event/2]).

-include("blackhole.hrl").

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'handle_conference_event'}
                      ,[{<<"conference">>, <<"participant_event">>}
                        ,{<<"conference">>, <<"conference_event">>}
                       ]
                     }
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
start_link(Args) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], Args).

handle_conference_event(JObj, _Props) ->
    ConfId = wh_json:get_value(<<"Conference-ID">>, JObj),
    EventName = wh_json:get_value(<<"Event-Name">>, JObj),
    case blackhole_events_sup:get_listener(ConfId) of
        'not_found' -> 'ok';
        {ConfId, Pid, _, _} ->
            gen_server:cast(Pid, {EventName, JObj})
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
init(ConfId) ->
    State = [],
    gen_listener:add_binding(self()
                             ,'conference'
                             ,[{'restrict_to', ['participant_event'
                                                ,{'conference', ConfId}]}
                               ,{<<"Conference-ID">>, ConfId}
                              ]),
    {'ok', State}.

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
handle_call({'disconnect_socket', User}, {Pid, _}, State) ->
    case lists:delete(Pid, State) of
        [] ->
            {'stop', 'normal', 'ok', []};
        NewState ->
            blackhole_sockets:send_event(NewState, <<"user_disconnected">>, User),
            {'reply', 'ok', NewState}
    end;
handle_call('get_socket', {Pid, _}, State) ->
    Reply = lists:member(Pid, State),
    {'reply', Reply, State};
handle_call('get_state', _, State) ->
    {'reply', State, State};
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
handle_cast({'connect_socket', Pid, User}, State) ->
    blackhole_sockets:send_event([Pid|State], <<"user_connected">>, User),
    {'noreply', [Pid|State]};
handle_cast({<<"conference_event">>, JObj}, State) ->
    fw_conference_event(JObj, State),
    {'noreply', State};
handle_cast({<<"participant_event">>, JObj}, State) ->
    fw_participant_event(JObj, State),
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
fw_participant_event(JObj, Pids) ->
    Event = cleanup_binary(wh_json:get_value(<<"Event">>, JObj)),
    fw_participant_event(Event, JObj, Pids).

fw_participant_event(Event, JObj, Pids) ->
    CleanJObj = clean_participant_event(JObj),
    Id = wh_json:get_value(<<"Call-ID">>, JObj),
    blackhole_sockets:send_event(Pids
                                 ,Event
                                 ,[Id, CleanJObj]
                                ).

fw_conference_event(JObj, Pids) ->
    ConfId = wh_json:get_value(<<"Conference-ID">>, JObj),
    Event = cleanup_binary(wh_json:get_value(<<"Event">>, JObj)),
    blackhole_sockets:send_event(Pids
                                 ,Event
                                 ,[ConfId]
                                ).

clean_participant_event(JObj) ->
    RemoveKeys = [<<"Focus">>
                  ,<<"App-Version">>
                  ,<<"App-Name">>
                  ,<<"Event-Category">>
                  ,<<"Event-Name">>
                  ,<<"Msg-ID">>
                  ,<<"Node">>
                  ,<<"Server-ID">>
                  ,<<"Switch-Hostname">>
                  ,<<"Mute-Detect">>
                  ,<<"Custom-Channel-Vars">>
                 ],
    CleanKeys = [{<<"Energy-Level">>, <<"energy_level">>, fun wh_util:to_integer/1}
                 ,{<<"Current-Energy">>, <<"current_energy">>, fun wh_util:to_integer/1}
                 ,{<<"Talking">>, <<"talking">>, fun wh_util:to_boolean/1}
                 ,{<<"Speak">>, <<"mute">>, fun(X) -> not wh_util:to_boolean(X) end}
                 ,{<<"Hear">>, <<"hear">>, fun wh_util:to_boolean/1}
                 ,{<<"Video">>, <<"video">>, fun wh_util:to_boolean/1}
                 ,{<<"Floor">>, <<"floor">>, fun wh_util:to_boolean/1}
                 ,{<<"Event">>, <<"event">>, fun cleanup_binary/1}
                 ,{<<"Custom-Channel-Vars">>, <<"pin">>, fun(X) -> wh_json:get_value(<<"pin">>, X) end}
                ],
    clean_jobj(JObj, RemoveKeys, CleanKeys).


clean_jobj(JObj, RemoveKeys, []) ->
    JObj1 = wh_json:delete_keys(RemoveKeys, JObj),
    wh_json:foldl(
        fun(K, V, Acc) ->
            wh_json:set_value(cleanup_binary(K), V, Acc)
        end
        ,wh_json:new()
        ,JObj1
    );
clean_jobj(JObj, RemoveKeys, [{OldKey, NewKey} | T]) ->
    Value = wh_json:get_value(OldKey, JObj),
    J1 = wh_json:set_value(NewKey, Value, JObj),
    clean_jobj(wh_json:delete_key(OldKey, J1), RemoveKeys, T);
clean_jobj(JObj, RemoveKeys, [{OldKey, NewKey, Fun} | T]) ->
    case wh_json:get_value(OldKey, JObj) of
        'undefined' ->
            J1 = wh_json:set_value(NewKey, <<"undefined">>, JObj),
            clean_jobj(wh_json:delete_key(OldKey, J1), RemoveKeys, T);
        Value ->
            J1 = wh_json:set_value(NewKey, Fun(Value), JObj),
            clean_jobj(wh_json:delete_key(OldKey, J1), RemoveKeys, T)
    end.

cleanup_binary(Binary) ->
    String = binary:bin_to_list(Binary),
    Binary1 = binary:list_to_bin(string:to_lower(String)),
    binary:replace(Binary1, <<"-">>, <<"_">>, [global]).


