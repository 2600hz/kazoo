%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
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
    gen_listener:start_link(?MODULE, [{bindings, [{media, []}]}
                                      ,{responders, [{{?MODULE, handle_media_req}
                                                      ,[{<<"media">>, <<"media_req">>}]}
                                                    ]}
                                     ], []).

handle_media_req(JObj, _Props) ->
    true = wapi_media:req_v(JObj),
    _ = wh_util:put_callid(JObj),
    lager:debug("recv media req for msg id: ~s", [wh_json:get_value(<<"Msg-ID">>, JObj)]),
    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
    case wh_media_url:playback(MediaName, JObj) of
        {error, ErrorMessage} ->
            send_error_resp(JObj, ErrorMessage);
        {ok, StreamURL} ->
            send_media_resp(JObj, StreamURL)
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
init([]) ->
    lager:debug("starting media_mgr listener"),
    {ok, ok}.

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
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_error_resp(JObj, ErrMsg) ->
    MediaName = wh_json:get_value(<<"Media-Name">>, JObj),
    Error = [{<<"Media-Name">>, MediaName}
             ,{<<"Error-Code">>, <<"other">>}
             ,{<<"Error-Msg">>, wh_util:to_binary(ErrMsg)}
             ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
             | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    lager:debug("sending error reply ~s for ~s", [ErrMsg, MediaName]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_media:publish_error(ServerId, P) end,
    whapps_util:amqp_pool_send(Error, Publisher).

send_media_resp(JObj, StreamURL) ->
    lager:debug("media stream URL: ~s", [StreamURL]),
    Resp = [{<<"Media-Name">>, wh_json:get_value(<<"Media-Name">>, JObj)}
            ,{<<"Stream-URL">>, StreamURL}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_media:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher).
