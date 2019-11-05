%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_listener).
-behaviour(gen_listener).

-export([start_link/0
        ,handle_metaflow/2
        ,handle_channel_create/2
        ,handle_route_req/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("konami.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'metaflow', [{'restrict_to', ['bindings']}]}
                  ,{'route', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_metaflow'}
                     ,[{<<"metaflow">>, <<"bind">>}]
                     }
                    ,{{?MODULE, 'handle_channel_create'}
                     ,[{<<"call_event">>, <<"CHANNEL_CREATE">>}]
                     }
                    ,{{?MODULE, 'handle_route_req'}
                     ,[{<<"dialplan">>, <<"route_req">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<"konami_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

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
                                     ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                     ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                     ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], []).

-spec handle_metaflow(kz_json:object(), kz_term:proplist()) -> no_return().
handle_metaflow(JObj, Props) ->
    'true' = kapi_metaflow:binding_v(JObj),
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    kapps_call:put_callid(Call),

    _ = konami_code_statem:start(kapps_call:kvs_store('consumer_pid', props:get_value('server', Props), Call)
                                ,JObj
                                ),
    'ok'.

-spec handle_route_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_route_req(JObj, _Props) ->
    'true' = kapi_route:req_v(JObj),
    kz_log:put_callid(JObj),
    Call = kapps_call:from_route_req(JObj),

    maybe_start_metaflows(kapps_call:account_id(Call)
                         ,kapps_call:authorizing_type(Call)
                         ,kapps_call:authorizing_id(Call)
                         ,kapps_call:owner_id(Call)
                         ,Call
                         ).

-spec handle_channel_create(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_channel_create(JObj, _Props) ->
    'true' = kapi_call:event_v(JObj),
    kz_log:put_callid(JObj),
    Call = kapps_call:from_json(JObj),

    maybe_start_metaflows(kapps_call:account_id(Call)
                         ,kapps_call:authorizing_type(Call)
                         ,kapps_call:authorizing_id(Call)
                         ,kapps_call:owner_id(Call)
                         ,Call
                         ).

-spec maybe_start_metaflows(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
maybe_start_metaflows('undefined', _AuthorizingType, _AuthorizingId, _OwnerId, _CallId) ->
    lager:debug("no account id for ~s(~s) owned by ~s", [_AuthorizingId, _AuthorizingType, _OwnerId]);
maybe_start_metaflows(AccountId, <<"device">>, DeviceId, OwnerId, CallId) ->
    maybe_start_device_metaflows(AccountId, DeviceId, CallId),
    maybe_start_user_metaflows(AccountId, OwnerId, CallId);
maybe_start_metaflows(AccountId, <<"mobile">>, DeviceId, OwnerId, CallId) ->
    maybe_start_device_metaflows(AccountId, DeviceId, CallId),
    maybe_start_user_metaflows(AccountId, OwnerId, CallId);
maybe_start_metaflows(AccountId, 'undefined', DeviceId, OwnerId, CallId) ->
    maybe_start_device_metaflows(AccountId, DeviceId, CallId),
    maybe_start_user_metaflows(AccountId, OwnerId, CallId);
maybe_start_metaflows(_AccountId, _AuthorizingType, _AuthorizingId, _OwnerId, _CallId) ->
    lager:debug("unhandled channel for account ~s: ~s(~s) owned by ~s"
               ,[_AccountId, _AuthorizingId, _AuthorizingType, _OwnerId]
               ).

-spec maybe_start_device_metaflows(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
maybe_start_device_metaflows(_AccountId, 'undefined', _Call) -> 'ok';
maybe_start_device_metaflows(AccountId, DeviceId, Call) ->
    {'ok', Endpoint} = kz_datamgr:open_cache_doc(kapps_call:account_db(Call)
                                                ,DeviceId
                                                ),
    maybe_start_metaflows(AccountId, Call, kz_json:get_value(<<"metaflows">>, Endpoint)).

-spec maybe_start_user_metaflows(kz_term:ne_binary(), kz_term:api_binary(), kapps_call:call()) -> 'ok'.
maybe_start_user_metaflows(_AccountId, 'undefined', _Call) -> 'ok';
maybe_start_user_metaflows(AccountId, UserId, Call) ->
    {'ok', User} = kz_datamgr:open_cache_doc(kapps_call:account_db(Call)
                                            ,UserId
                                            ),
    maybe_start_metaflows(AccountId, Call, kz_json:get_value(<<"metaflows">>, User)).

-spec maybe_start_metaflows(kz_term:ne_binary(), kapps_call:call(), kz_term:api_object()) -> 'ok'.
maybe_start_metaflows(_AccountId, _Call, 'undefined') -> 'ok';
maybe_start_metaflows(_AccountId, _Call, Metaflows) ->
    lager:debug("starting ~p", [Metaflows]).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
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
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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
