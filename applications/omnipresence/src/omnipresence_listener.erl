%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(omnipresence_listener).
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

-include("omnipresence.hrl").

-define(SERVER, ?MODULE).

-record(state, {subs_pid :: kz_term:api_pid()
               ,subs_ref :: kz_term:api_reference()
               ,queue :: kz_term:api_binary()
               ,consuming = 'false' :: boolean()
               ,sync = 'false' :: boolean()
               }).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'self', []}
                  ,{'omnipresence', [{'restrict_to', ['subscribe', 'notify']}]}
                  ]).
-define(RESPONDERS, [{{'omnip_subscriptions', 'handle_kamailio_subscribe'}
                     ,[{<<"presence">>, <<"subscription">>}]
                     }
                    ,{{'omnip_subscriptions', 'handle_kamailio_notify'}
                     ,[{<<"presence">>, <<"notify">>}]
                     }
                    ]).

-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(SUBSCRIPTIONS_SYNC_ENABLED, kapps_config:get_is_true(?CONFIG_CAT, <<"subscriptions_sync_enabled">>, 'false')).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    gen_listener:cast(self(), 'find_subscriptions_srv'),
    lager:debug("omnipresence_listener started"),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('find_subscriptions_srv', #state{subs_pid=_Pid}=State) ->
    case omnipresence_sup:subscriptions_srv() of
        'undefined' ->
            lager:debug("no subs_pid"),
            gen_listener:cast(self(), 'find_subscriptions_srv'),
            {'noreply', State#state{subs_pid='undefined'}};
        P when is_pid(P) ->
            lager:debug("new subs pid: ~p", [P]),
            gen_listener:cast(self(), 'send_sync'),
            {'noreply', State#state{subs_pid=P
                                   ,subs_ref=erlang:monitor('process', P)
                                   }}
    end;
handle_cast({'gen_listener',{'created_queue',Queue}}, State) ->
    gen_listener:cast(self(), 'send_sync'),
    {'noreply', State#state{queue=Queue}};
handle_cast({'gen_listener',{'is_consuming',IsConsuming}}, State) ->
    gen_listener:cast(self(), 'send_sync'),
    {'noreply', State#state{consuming=IsConsuming}};
handle_cast('send_sync', #state{subs_pid=Pid, queue=Queue, consuming=IsConsuming} = State)
  when Pid =:= 'undefined'
       orelse Queue =:= 'undefined'
       orelse IsConsuming =:= 'false'  ->
    {'noreply', State};
handle_cast('send_sync', #state{subs_pid='undefined'}=State) ->
    {'noreply', State};
handle_cast('send_sync', #state{queue='undefined'}=State) ->
    {'noreply', State};
handle_cast('send_sync', #state{consuming='false'}=State) ->
    {'noreply', State};
handle_cast('send_sync', #state{queue=Queue, consuming='true', sync='false'} = State) ->
    maybe_sync_subscriptions(?SUBSCRIPTIONS_SYNC_ENABLED, Queue),
    {'noreply', State#state{sync='true'}};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', Ref, 'process', Pid, _R}, #state{subs_pid=Pid
                                                     ,subs_ref=Ref
                                                     }=State) ->
    gen_listener:cast(self(), 'find_subscriptions_srv'),
    {'noreply', State#state{subs_pid='undefined'
                           ,subs_ref='undefined'
                           }};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{subs_pid=S}) ->
    {'reply', [{'omnip_subscriptions', S}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_sync_subscriptions(boolean(), binary()) -> 'ok'.
maybe_sync_subscriptions('false', _) -> 'ok';
maybe_sync_subscriptions('true', Queue) ->
    Payload = kz_json:from_list(
                [{<<"Action">>, <<"Request">>}
                 | kz_api:default_headers(Queue, ?APP_NAME, ?APP_VERSION)
                ]),
    kapi_presence:publish_sync(Payload).
