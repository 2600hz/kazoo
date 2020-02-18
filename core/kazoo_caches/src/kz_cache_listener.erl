%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc Bind for AMQP configuration change events and remove entries from cache
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_listener).
-behaviour(gen_listener).

-export([start_link/0
        ,add_binding/2

        ,new_channel_flush_binding/0
        ,channel_reconnect_flush_binding/0

        ,add_origin_pointers/3
        ,get_origin_pointers/2

        ,handle_change/3
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_caches.hrl").

-ifdef(TEST).
-define(BINDINGS, []).
-else.
-define(BINDINGS, [{'self', []}]).
-endif.

-define(RESPONDERS, [{{?MODULE, 'handle_change'}
                     ,[{<<"configuration">>, <<"*">>}]
                     }
                    ]
       ).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(DATABASE_BINDING, [{'type', <<"database">>}]).

-type state() :: ?MODULE.

-spec start_link() -> kz_types:startlink_ret().
-ifdef(TEST).
start_link() -> gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).
-else.
start_link() ->
    %% Bindings = [{'conf', ['federate', {'type', <<"database">>}]}],
    gen_listener:start_link({'local', ?MODULE}
                           ,?MODULE
                           ,[{'bindings', []}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).
-endif.

-spec add_binding(atom(), kz_term:proplist()) -> 'ok'.
add_binding(Name, Binding) ->
    gen_listener:add_binding(?MODULE
                            ,'conf'
                            ,['federate' | Binding]
                            ),
    RK = kapi_conf:get_routing_key(Binding, 'false'), % get a local (unoptimized) binding key
    _ = kazoo_bindings:bind(local_key(RK), 'kz_cache_conf_change', 'handle_change', Name).

-spec local_key(kz_term:ne_binary()) -> kz_term:ne_binary().
local_key(RK) ->
    <<?MODULE_STRING".", RK/binary>>.

-spec add_origin_pointers(atom(), cache_obj(), 'undefined' | origin_tuple() | origin_tuples()) -> 'ok'.
add_origin_pointers(_Name, _CacheObj, 'undefined') -> 'ok';
add_origin_pointers(Name, CacheObj, Origin) when not is_list(Origin) ->
    add_origin_pointers(Name, CacheObj, [Origin]);
add_origin_pointers(Name, CacheObj, Origins) ->
    PointerTab = kz_cache_ets:pointer_tab(Name),
    _ = [add_origin_pointer(PointerTab, CacheObj, Origin)
         || Origin <- Origins
        ],
    'ok'.

-spec add_origin_pointer(atom(), cache_obj(), origin_tuple()) -> 'true'.
add_origin_pointer(PointerTab, #cache_obj{key=Key}=CacheObj, Origin) ->
    ets:insert(PointerTab
              ,CacheObj#cache_obj{key=Key
                                 ,value=Key
                                 ,origin=Origin
                                 ,callback='undefined'
                                 ,expires_s='infinity'
                                 }
              ).

-spec get_origin_pointers(atom(), any()) -> list().
get_origin_pointers(PointerTab, Key) ->
    ets:lookup(PointerTab, Key).

-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    {'ok', ?MODULE}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> {'noreply', state()}.
handle_call(_Req, _From, State) ->
    {'noreply', State}.

-spec new_channel_flush_binding() -> kz_term:ne_binary().
new_channel_flush_binding() ->
    <<"kz_cache.channel.new">>.

-spec channel_reconnect_flush_binding() -> kz_term:ne_binary().
channel_reconnect_flush_binding() ->
    <<"kz_cache.channel.reconnect">>.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'kz_amqp_channel', {'new_channel', 'false'}}, State) ->
    _ = kazoo_bindings:map(new_channel_flush_binding(), []),
    {'noreply', State};
handle_cast({'kz_amqp_channel', {'new_channel', 'true'}}, State) ->
    _ = kazoo_bindings:map(channel_reconnect_flush_binding(), []),
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'federators_consuming', _AreFederatorsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> {'reply', []}.
handle_event(_JObj, _State) ->
    {'reply', []}.

-spec handle_change(kapi_conf:doc(), kz_term:proplist(), any()) -> 'ok'.
handle_change(JObj, _Props, <<RK/binary>>) ->
    _ = kazoo_bindings:map(local_key(RK), [JObj]),
    lager:debug("routed payload to ~s", [RK]);
handle_change(JObj, Props, Deliver) ->
    RK = gen_listener:routing_key_used(Deliver),
    handle_change(JObj, Props, RK).

-spec terminate(any(), state()) -> 'ok'.
terminate('shutdown', _State) -> 'ok';
terminate('normal', _State) -> 'ok';
terminate(_Reason, _State) ->
    lager:info("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
