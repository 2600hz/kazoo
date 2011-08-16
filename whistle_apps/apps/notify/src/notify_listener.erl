%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle updating devices and emails about voicemails
%%% @end
%%% Created :  3 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_listener).

-behaviour(gen_server).

%% API
-export([start_link/0, add_notifier/2, rm_notifier/1, rm_notifier/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("notify.hrl").

-define(SERVER, ?MODULE).
-define(NOTIFIERS, [
		    {notify_vm, [{<<"conference">>, <<"new_voicemail">>}]}
		   ]).

-type notifier() :: {{binary(), binary()}, atom()}.
-record(state, {
	  notifications = [] :: [notifier(),...] | [] %% { {EvtCat, EvtName}, Module }
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_notifier/2 :: (Notifier, Key) -> ok when
      Notifier :: atom(),
      Key :: {binary(), binary()} | [{binary(), binary()},...].
add_notifier(Notifier, Key) when not is_list(Key) ->
    add_notifier(Notifier, [Key]);
add_notifier(Notifier, [{_,_}|_] = Keys) ->
    gen_server:cast(?SERVER, {add_notifier, Notifier, Keys}).

-spec rm_notifier/1 :: (Notifier) -> ok when
      Notifier :: atom().
-spec rm_notifier/2 :: (Notifier, Key) -> ok when
      Notifier :: atom(),
      Key :: [{binary(), binary()},...] | []. %% empty list removes all
rm_notifier(Notifier) ->
    rm_notifier(Notifier, []).
rm_notifier(Notifier, {_,_}=Key) ->
    rm_notifier(Notifier, [Key]);
rm_notifier(Notifier, Keys) ->
    gen_server:cast(?SERVER, {rm_notifier, Notifier, Keys}).

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
    ?LOG_SYS("starting new vm notify process"),
    {ok, #state{}, 0}.

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
handle_cast({add_notifier, Notifier, Keys}, #state{notifications=Notifications}=State) ->
    {noreply, State#state{notifications=add_notifier(Notifications, Notifier, Keys)}};
handle_cast({rm_notifier, Notifier, Keys}, #state{notifications=Notifications}=State) ->
    {noreply, State#state{notifications=rm_notifier(Notifications, Notifier, Keys)}}.

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
handle_info({#'basic.deliver'{}, #amqp_msg{props = #'P_basic'{content_type = <<"application/json">>}, payload = Payload}}
	    ,#state{notifications=Ns}=State) ->
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  whapps_util:put_callid(JObj),
                  _ = process_req(Ns, JObj)
          end),
    {noreply, State};

handle_info(timeout, State) ->
    %% cast shouldn't block
    spawn(fun() -> [?MODULE:add_notifier(Module, Evts) || {Module, Evts} <- ?NOTIFIERS] end),
    case start_amqp() of
	{ok, _} -> {noreply, State};
	_ -> {noreply, State, 1000}
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    {noreply, State, 1000};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State}.

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
    ?LOG_SYS("vm notify process ~p termination", [_Reason]),
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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% ensure the exhanges exist, build a queue, bind, and consume
%% @end
%%--------------------------------------------------------------------
-spec start_amqp/0 :: () -> {ok, binary()} | {error, amqp_error}.
start_amqp() ->
    try
        Q = amqp_util:new_queue(),
        amqp_util:bind_q_to_callevt(Q, ?NOTIFY_VOICEMAIL_NEW, other),
        amqp_util:basic_consume(Q),
        ?LOG_SYS("connected to AMQP"),
        {ok, Q}
    catch
        _:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
            {error, amqp_error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec process_req/2 :: (Ns, JObj) -> ok when
      Ns :: [notifier(),...] | [],
      JObj :: json_object().
process_req([], _JObj) ->
    ok;
process_req(Ns, JObj) ->
    Key = whapps_util:get_event_type(JObj),
    _ = [ catch(Module:handle_req(JObj)) || {Evt, Module} <- Ns, Key =:= Evt ],
    ok.

-spec add_notifier/3 :: (Notifications, Notifier, Keys) -> [notifier(),...] when
      Notifications :: [notifier(),...] | [],
      Notifier :: atom(),
      Keys :: [{binary(), binary()},...].
add_notifier(Notifications, Notifier, Keys) ->
    lists:foldr(fun(Evt, Acc) ->
			case lists:member({Evt, Notifier}, Notifications) of
			    true ->
				Acc;
			    false ->
				[{Evt, Notifier} | Acc]
			end
		end, Notifications, Keys).

-spec rm_notifier/3 :: (Notifications, Notifier, Keys) -> [notifier(),...] when
      Notifications :: [notifier(),...] | [],
      Notifier :: atom(),
      Keys :: [{binary(), binary()},...] | [].
%% remove all events for notifier
rm_notifier(Notifications, Notifier, []) ->
    [ N || {_, Module}=N <- Notifications, Module =/= Notifier];
%% remove events in Keys for module Notifier
rm_notifier(Notifications, Notifier, Keys) ->
    %% if Evt is in the list of Keys and Module =:= Notifier, remove it from the list of Notifications
    [ N || {Evt, Module}=N <- Notifications, (not (Module =:= Notifier andalso lists:member(Evt, Keys))) ].
