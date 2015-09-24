%%%-------------------------------------------------------------------
%%% @copyright (C) 2014
%%% @doc
%%% This listener handles call CHANNEL_DESTROY events.
%%% It is started by cf_singular_call_hooks and will
%%% trigger when it is time to send the call end hook.
%%% @end
%%% @contributors
%%%   Benedict Chan
%%%-------------------------------------------------------------------
-module(cf_singular_call_hooks_listener).

-behaviour(gen_listener).

-export([start_link/1
         ,handle_call_event/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("callflow.hrl").

-record(state, {call :: whapps_call:call()}).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(CallID), [{'call', [{'callid', CallID}
                                     ,{'restrict_to',
                                        [ <<"CHANNEL_DESTROY">>
                                        , <<"CHANNEL_TRANSFEROR">>
                                        ]}
                                    ]}
                           ,{'self', []}
                          ]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                      ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the listener and binds to the call channel destroy events
%% @end
%%--------------------------------------------------------------------
-spec start_link(whapps_call:call()) -> startlink_ret().
start_link(Call) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS(whapps_call:call_id(Call))}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], [Call]).

%%--------------------------------------------------------------------
%% @doc
%% Handles call events (typically triggerred by a freeswitch event)
%% For the purposes of the singular hook listener, we are only interested in
%% CHANNEL_DESTROY.
%% @end
%%--------------------------------------------------------------------
-spec handle_call_event(wh_json:object(), wh_proplist()) -> _.
handle_call_event(JObj, Props) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            gen_listener:cast(props:get_value('server', Props), {'end_hook', JObj});
        {<<"call_event">>, <<"CHANNEL_TRANSFEROR">>} ->
            % stop the listener so we don't send the destroy event
            gen_listener:cast(props:get_value('server', Props), {'stop', JObj});
        {_, _Evt} -> lager:debug("ignore event ~p", [_Evt])
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the listener, and sends the init hook
%%--------------------------------------------------------------------
-spec init([whapps_call:call()]) ->
                  {'ok', state()}.
init([Call]) ->
    %% ReferredBy is interesting because we use it to tell if the call was forwarded
    ReferredBy = whapps_call:custom_channel_var(<<"Referred-By">>, Call),

    %% send the init hook only if we were not a forwarded call
    case ReferredBy of
        'undefined' -> gen_listener:cast(self(), {'init_hook'});
        _ -> 'ok'
    end,

    lager:debug("started event listener for cf_singular_hook"),
    {'ok', #state{call=Call}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(_, _, state()) ->
                         {'reply', {'error', 'not_implemented'}, state()}.
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(_, state()) -> {'noreply', state()} |
                                      {'stop', 'normal', state()}.
handle_cast({'init_hook'}, #state{call=Call}=State) ->
    cf_singular_call_hooks:send_init_hook(Call),
    {'noreply', State};
handle_cast({'end_hook', JObj}, #state{call=Call}=State) ->
    cf_singular_call_hooks:send_end_hook(Call, JObj),
    {'stop', 'normal', State};
handle_cast({'stop', _JObj}, #state{call=_Call}=State) ->
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(_, state()) -> {'noreply', state()}.
handle_info(Info, State) ->
    lager:debug("unhandled message: ~p", [Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%% @end
%%--------------------------------------------------------------------
-spec handle_event(wh_json:object(), state()) -> {'reply', []}.
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(_, state()) -> _.
terminate(_Reason, _State) ->
    lager:debug("singular call hook listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(_, state(), _) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
