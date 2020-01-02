%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(media_listener).
-behaviour(gen_listener).

%% API
-export([start_link/0
        ,handle_media_req/2
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

-include("media.hrl").

-record(state, {}).
-type state() :: #state{}.

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'bindings', [{'media', []}]}
                                     ,{'responders', [{{?MODULE, 'handle_media_req'}
                                                      ,[{<<"media">>, <<"media_req">>}]}
                                                     ]}
                                     ], []).

-spec handle_media_req(kz_json:object(), kz_term:proplist()) -> kz_amqp_worker:cast_return().
handle_media_req(JObj, _Props) ->
    'true' = kapi_media:req_v(JObj),
    _ = kz_log:put_callid(JObj),
    lager:debug("recv media req for msg id: ~s", [kz_api:msg_id(JObj)]),
    MediaName = kz_json:get_value(<<"Media-Name">>, JObj),
    case kz_media_url:playback(MediaName, JObj) of
        {'error', ErrorMessage} -> send_error_resp(JObj, ErrorMessage);
        StreamURL -> send_media_resp(JObj, StreamURL)
    end.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    lager:debug("starting media_mgr listener"),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_supported'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
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
    lager:debug("media listener terminating: ~p", [_Reason]).

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
-spec send_error_resp(kz_json:object(), atom() | kz_term:ne_binary()) ->
          kz_amqp_worker:cast_return().
send_error_resp(JObj, ErrMsg) ->
    MediaName = kz_json:get_value(<<"Media-Name">>, JObj),
    Error = [{<<"Media-Name">>, MediaName}
            ,{<<"Error-Code">>, <<"other">>}
            ,{<<"Error-Msg">>, kz_term:to_binary(ErrMsg)}
            ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    lager:debug("sending error reply ~s for ~s", [ErrMsg, MediaName]),
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> kapi_media:publish_error(ServerId, P) end,
    kz_amqp_worker:cast(Error, Publisher).

-spec send_media_resp(kz_json:object(), kz_term:ne_binary()) ->
          kz_amqp_worker:cast_return().
send_media_resp(JObj, StreamURL) ->
    lager:debug("media stream URL: ~s", [StreamURL]),
    Resp = [{<<"Media-Name">>, kz_json:get_value(<<"Media-Name">>, JObj)}
           ,{<<"Stream-URL">>, StreamURL}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> kapi_media:publish_resp(ServerId, P) end,
    kz_amqp_worker:cast(Resp, Publisher).
