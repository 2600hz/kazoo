%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 4 Sep 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_events_srv).

-behaviour(gen_listener).

%% API
-export([start_link/2
         ,subscribe/3
         ,unsubscribe/2
         ,fetch/1
         ,stop/1
        ]).
-export([subscriptions/1
         ,set_maxevents/2
         ,get_maxevents/1
        ]).

%% gen_listener callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

%% gen_listener handler callback
-export([handle_req/2]).

-include("src/crossbar.hrl").

-define(RESPONDERS, [{?MODULE, [{<<"*">>, <<"*">>}]}]). % all matching event cat/name combos go to here
-define(BINDINGS, []).

-define(SERVER, ?MODULE).

-record(state, {
          account_id :: api_binary()
          ,user_id :: api_binary()
          ,max_events = 100 :: pos_integer()
          ,subscriptions = [] :: [queue_bindings:bind_types(),...] | []
          ,events = queue:new() :: queue() %% queue(wh_json:object())
          ,overflow = 'false' :: boolean() %% set to 'true' if events are dropped before being fetched; reset after fetch
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
-spec start_link(ne_binary(), ne_binary()) -> startlink_ret().
start_link(AccountID, UserID) ->
    gen_listener:start_link(?MODULE
                            ,[{'responders', ?RESPONDERS}
                              ,{'bindings', ?BINDINGS}]
                            ,[AccountID, UserID]
                           ).

-spec stop(pid()) -> 'ok'.
stop(Srv) -> gen_listener:stop(Srv).

-spec subscribe(pid(), atom(), wh_proplist()) ->
                       'ok' | {'error', 'unknown' | 'already_present'}.
subscribe(Srv, Sub, Options) ->
    gen_listener:call(Srv, {'subscribe', wh_util:to_atom(Sub), Options}).

-spec unsubscribe(pid(), atom()) -> 'ok'.
unsubscribe(Srv, Sub) -> gen_listener:cast(Srv, {'unsubscribe', wh_util:to_atom(Sub)}).

-spec subscriptions(pid()) -> {'ok', [queue_bindings:bind_types(),...] | []}.
subscriptions(Srv) -> gen_listener:call(Srv, 'subscriptions').

-spec fetch(pid()) -> {wh_json:objects(), boolean()}.
fetch(Srv) -> gen_listener:call(Srv, 'fetch').

-spec get_maxevents(pid()) -> integer().
get_maxevents(Srv) -> gen_listener:call(Srv, 'get_maxevents').

-spec set_maxevents(pid(), pos_integer()) -> {'ok', non_neg_integer()}.
set_maxevents(Srv, Max) when Max > 0 ->
    gen_listener:call(Srv, {'set_maxevents', Max}).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    Srv = props:get_value('server', Props),
    gen_listener:cast(Srv, {'add_event', JObj}).

%%%===================================================================
%%% gen_listener callbacks
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
init([AccountID, UserID]) ->
    lager:debug("Started for ~s / ~s", [UserID, AccountID]),
    {'ok', #state{account_id=AccountID, user_id=UserID}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call('fetch', _, #state{events=Events, overflow=Overflow}=State) ->
    {'reply', {queue:to_list(Events), Overflow}, State#state{events=queue:new()
                                                             ,overflow='false'
                                                            }};

handle_call('get_maxevents', _, #state{max_events=Max}=State) ->
    {'reply', Max, State};

handle_call({'set_maxevents', Max}, _, #state{events=Events}=State) ->
    case (Len = queue:len(Events)) > Max of
        'true' ->
            {DroppedEvents, KeptEvents} = queue:split(Len-Max, Events),
            {'reply', {'ok', queue:len(DroppedEvents)}, State#state{max_events=Max
                                                                    ,events=KeptEvents
                                                                    ,overflow='true'
                                                                   }};
        'false' ->
            {'reply', {'ok', 0}, State#state{max_events=Max}}
    end;

handle_call('subscriptions', _, #state{subscriptions=Subs}=State) ->
    {'reply', {'ok', Subs}, State};

handle_call({'subscribe', Sub, Options}, _, #state{subscriptions=Subs}=State) ->
    case lists:member(Sub, queue_bindings:known_bind_types()) of
        'false' -> {'reply', {'error', 'unknown'}, State};
        'true' ->
            case lists:member(Sub, Subs) of
                'true' -> {'reply', {'error', 'already_present'}, State};
                'false' ->
                    lager:debug("Adding binding ~s", [Sub]),
                    gen_listener:add_binding(self(), Sub, Options),
                    {'reply', 'ok', State#state{subscriptions=[Sub|Subs]}}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'unsubscribe', Sub}, #state{subscriptions=Subs}=State) ->
    lager:debug("removing binding: ~s", [Sub]),
    gen_listener:rm_binding(self(), Sub),
    {'noreply', State#state{subscriptions=lists:delete(Sub, Subs)}};
handle_cast({'add_event', JObj}, #state{events=Events, max_events=Max}=State) ->
    case (Len = queue:len(Events)) >= Max of
        'true' ->
            {_, KeptEvents} = queue:split(Len-Max, Events),
            {'noreply', State#state{events=queue:in(JObj, KeptEvents), overflow='true'}};
        'false' ->
            {'noreply', State#state{events=queue:in(JObj, Events)}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {'reply', Props}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), #state{}) -> 'ok'.
terminate(_Reason, _) -> lager:debug("cb_events_srv terminating: ~p", [_Reason]).

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
