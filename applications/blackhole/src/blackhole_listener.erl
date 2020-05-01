%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_listener).
-behaviour(gen_listener).

-export([start_link/0
        ,handle_amqp_event/3
        ,add_binding/1, remove_binding/1
        ,add_bindings/1, remove_bindings/1
        ,flush/0
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("blackhole.hrl").
-include_lib("kazoo_amqp/src/api/kapi_websockets.hrl").

-define(SERVER, ?MODULE).

-record(state, {bindings :: ets:tid()}).
-type state() :: #state{}.

-type mod_inited() :: 'ok' | {'error', atom()} |
                      'stopped'. %% stopped instead of inited

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'websockets', [{'restrict_to', ['get', 'module_req']}]}]).

-define(RESPONDERS, [{{?MODULE, 'handle_amqp_event'}
                     ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ]
                           ,[]
                           ).

-spec handle_amqp_event(kz_json:object(), kz_term:proplist(), gen_listener:basic_deliver() | kz_term:ne_binary()) -> 'ok'.
handle_amqp_event(EventJObj, _Props, ?MODULE_REQ_ROUTING_KEY) ->
    handle_module_req(EventJObj);
handle_amqp_event(EventJObj, _Props, <<_/binary>> = RoutingKey) ->
    Evt = kz_util:get_event_type(EventJObj),
    lager:debug("recv event ~p (~s)", [Evt, RoutingKey]),
    RK = <<"blackhole.event.", RoutingKey/binary>>,
    {Time, Res} = timer:tc(blackhole_bindings, pmap, [RK, [RoutingKey, EventJObj]]),
    lager:debug("delivered the event ~p (~s) to ~b subscriptions in ~b ms"
               ,[Evt, RoutingKey, length(Res), Time div 1000]
               );
handle_amqp_event(EventJObj, Props, BasicDeliver) ->
    handle_amqp_event(EventJObj, Props, gen_listener:routing_key_used(BasicDeliver)).

-spec handle_module_req(kz_json:object()) -> 'ok'.
handle_module_req(EventJObj) ->
    'true' = kapi_websockets:module_req_v(EventJObj),
    lager:debug("recv module_req: ~p", [EventJObj]),
    handle_module_req(EventJObj
                     ,kz_json:get_atom_value(<<"Module">>, EventJObj)
                     ,kz_json:get_binary_value(<<"Action">>, EventJObj)
                     ,kz_json:is_true(<<"Persist">>, EventJObj, 'true')
                     ).

-spec handle_module_req(kz_json:object(), atom(), kz_term:ne_binary(), boolean()) -> 'ok'.
handle_module_req(EventJObj, BHModule, <<"start">>, Persist) ->
    case code:which(BHModule) of
        'non_existing' -> send_error_module_resp(EventJObj, <<"module doesn't exist">>);
        _Path ->
            Started = start_module(BHModule),
            Persisted = maybe_persist(BHModule, Persist, Started),
            send_module_resp(EventJObj, Started, Persisted)
    end;
handle_module_req(EventJObj, BHModule, <<"stop">>, Persist) ->
    'ok' = blackhole_bindings:flush_mod(BHModule),

    Persist
        andalso blackhole_config:set_default_autoload_modules(
                  lists:delete(kz_term:to_binary(BHModule)
                              ,blackhole_config:autoload_modules()
                              )
                 ),

    send_module_resp(EventJObj, 'stopped', 'true').

-spec start_module(atom()) -> mod_inited().
start_module(BHModule) ->
    blackhole_bindings:init_mod(BHModule).

-spec maybe_persist(atom(), boolean(), mod_inited()) -> boolean().
maybe_persist(_BHModule, 'false', _Started) -> 'false';
maybe_persist(_BHModule, 'true', {'error', _}) -> 'false';
maybe_persist(BHModule, 'true', 'ok') ->
    Mods = blackhole_config:autoload_modules(),
    case lists:member(kz_term:to_binary(BHModule), Mods) of
        'true' ->
            lager:debug("module ~s persisted~n", [BHModule]),
            'true';
        'false' ->
            persist_module(BHModule, Mods)
    end.

-spec persist_module(atom(), kz_term:ne_binaries()) -> boolean().
persist_module(Module, Mods) ->
    case blackhole_config:set_default_autoload_modules(
           [kz_term:to_binary(Module)
            | lists:delete(kz_term:to_binary(Module), Mods)
           ]
          )
    of
        {'ok', _} -> 'true';
        {'error', _} -> 'false'
    end.

-spec send_module_resp(kz_json:object(), mod_inited(), boolean()) -> 'ok'.
send_module_resp(EventJObj, Started, Persisted) ->
    Resp = [{<<"Persisted">>, Persisted}
           ,{<<"Started">>, Started =:= 'ok'}
           ,{<<"Error">>, maybe_start_error(Started)}
           ,{<<"Msg-ID">>, kz_api:msg_id(EventJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = kz_api:server_id(EventJObj),
    kapi_websockets:publish_module_resp(ServerId, Resp).

-spec maybe_start_error(mod_inited()) -> kz_term:api_ne_binary().
maybe_start_error('ok') -> 'undefined';
maybe_start_error('stopped') -> 'undefined';
maybe_start_error({'error', E}) -> kz_term:to_binary(E).

-spec send_error_module_resp(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
send_error_module_resp(EventJObj, Error) ->
    Resp = [{<<"Persisted">>, 'false'}
           ,{<<"Started">>, 'false'}
           ,{<<"Error">>, Error}
           ,{<<"Msg-ID">>, kz_api:msg_id(EventJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = kz_api:server_id(EventJObj),
    kapi_websockets:publish_module_resp(ServerId, Resp).

-type bh_amqp_binding() :: {'amqp', atom(), kz_term:proplist()}.
-type bh_hook_binding() :: {'hook', kz_term:ne_binary()} | {'hook', kz_term:ne_binary(), kz_term:ne_binary()}.
-type bh_event_binding() :: bh_amqp_binding() | bh_hook_binding().
-type bh_event_bindings() :: [bh_event_binding()].

-spec add_binding(bh_event_binding()) -> 'ok'.
add_binding(Binding) ->
    gen_listener:cast(?SERVER, {'add_bh_binding', Binding}).

-spec add_bindings(bh_event_bindings()) -> 'ok'.
add_bindings(Bindings) ->
    gen_listener:cast(?SERVER, {'add_bh_bindings', Bindings}).

-spec remove_binding(bh_event_binding()) -> 'ok'.
remove_binding(Binding) ->
    gen_listener:cast(?SERVER, {'remove_bh_binding', Binding}).

-spec remove_bindings(bh_event_bindings()) -> 'ok'.
remove_bindings(Bindings) ->
    gen_listener:cast(?SERVER, {'remove_bh_bindings', Bindings}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init([]) ->
    {'ok', #state{bindings=ets:new(bindings, [])}}.

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
handle_cast({'add_bh_bindings', Bindings}, #state{bindings=ETS}=State) ->
    _ = add_bh_bindings(ETS, Bindings),
    {'noreply', State};
handle_cast({'add_bh_binding', Binding}, #state{bindings=ETS}=State) ->
    _ = add_bh_binding(ETS, Binding),
    {'noreply', State};
handle_cast({'remove_bh_bindings', Bindings}, #state{bindings=ETS}=State) ->
    _ = remove_bh_bindings(ETS, Bindings),
    {'noreply', State};
handle_cast({'remove_bh_binding', Binding}, #state{bindings=ETS}=State) ->
    _ = remove_bh_binding(ETS, Binding),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast('flush_bh_bindings', #state{bindings=ETS}=State) ->
    ets:delete_all_objects(ETS),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(?HOOK_EVT(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()), state()) ->
          {'noreply', state()}.
handle_info(?HOOK_EVT(AccountId, EventType, JObj), State) ->
    _ = kz_process:spawn(fun handle_hook_event/3, [AccountId, EventType, JObj]),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

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
-spec encode_call_id(kz_json:object()) -> kz_term:ne_binary().
encode_call_id(JObj) ->
    kz_amqp_util:encode(kz_call_event:call_id(JObj)).

-spec handle_hook_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> any().
handle_hook_event(AccountId, EventType, JObj) ->
    RK = kz_binary:join([<<"call">>
                        ,AccountId
                        ,EventType
                        ,encode_call_id(JObj)
                        ]
                       ,<<".">>
                       ),
    handle_amqp_event(JObj, [], RK).

-spec binding_key(bh_event_binding()) -> binary().
binding_key(Binding) -> base64:encode(term_to_binary(Binding)).

-spec add_bh_binding(ets:tid(), bh_event_binding()) -> 'ok'.
add_bh_binding(ETS, Binding) ->
    Key = binding_key(Binding),
    case ets:update_counter(ETS, Key, 1, {Key, 0}) of
        1 ->
            lager:debug("blackhole is creating new binding to ~p", [Binding]),
            add_bh_binding(Binding);
        0 ->
            lager:debug("listener has 0 refs after updating ? not creating new binding for ~p"
                       ,[Binding]
                       );
        _Else ->
            lager:debug("listener has ~b refs, not creating new binding for ~p"
                       ,[_Else, Binding]
                       )
    end.

-spec remove_bh_binding(ets:tid(), bh_event_binding()) -> 'ok' | 'true'.
remove_bh_binding(ETS, Binding) ->
    Key = binding_key(Binding),
    remove_bh_binding(ETS, Binding, Key, ets:update_counter(ETS, Key, -1, {Key, 0})).

-spec remove_bh_binding(ets:tid(), bh_event_binding(), binary(), integer()) -> 'ok' | 'true'.
remove_bh_binding(ETS, Binding, Key, 0) ->
    lager:debug("blackhole is deleting binding for ~p", [Binding]),
    remove_bh_binding(Binding),
    ets:delete(ETS, Key);
remove_bh_binding(ETS, Binding, Key, Neg) when Neg < 0 ->
    lager:debug("listener have ~b negative references, removing binding for ~p"
               ,[Neg, Binding]
               ),
    remove_bh_binding(Binding),
    ets:delete(ETS, Key);
remove_bh_binding(_ETS, _Binding, _Key, _Else) ->
    lager:debug("listener still have ~b references, not removing binding for ~p"
               ,[_Else, _Binding]
               ).

add_bh_binding({'hook', AccountId}) ->
    kz_hooks:register(AccountId);
add_bh_binding({'hook', <<"*">>, _Event}) ->
    kz_hooks:register();
add_bh_binding({'hook', AccountId, Event}) ->
    kz_hooks:register(AccountId, Event);
add_bh_binding({'amqp', Wapi, Options}) ->
    gen_listener:add_binding(self(), Wapi, Options).

remove_bh_binding({'hook', AccountId}) ->
    kz_hooks:deregister(AccountId);
remove_bh_binding({'hook', <<"*">>, _Event}) ->
    kz_hooks:deregister();
remove_bh_binding({'hook', AccountId, Event}) ->
    kz_hooks:deregister(AccountId, Event);
remove_bh_binding({'amqp', Wapi, Options}) ->
    gen_listener:rm_binding(self(), Wapi, Options).

add_bh_bindings(ETS, Bindings) ->
    [add_bh_binding(ETS, Binding) || Binding <- Bindings].

remove_bh_bindings(ETS, Bindings) ->
    [remove_bh_binding(ETS, Binding) || Binding <- Bindings].

-spec flush() -> 'ok'.
flush() ->
    gen_listener:cast(?SERVER, 'flush_bh_bindings').
