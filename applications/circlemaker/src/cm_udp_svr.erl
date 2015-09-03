%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is a set of different utility functions
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_udp_svr).
-behaviour(gen_server).

-include("circlemaker.hrl").
-include_lib("eradius/include/eradius_lib.hrl").

-record(state, {port :: non_neg_integer()
                ,socket
                ,secret :: ne_binary()
               }).
-type state() :: #state{}.

-define(SERVER, ?MODULE).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0, read_config/0, maybe_get_active_channels/3]).

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
-spec start_link() -> startlink_ret().
start_link() ->
    {'ok', Pid} = gen_listener:start_link({'local', ?SERVER}, ?MODULE, [], []),
    gen_server:cast(?MODULE, {'start_udp_server'}),
    {'ok', Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init([]) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {'ok', state()} |
                      {'ok', state(), non_neg_integer()} |
                      'ignore' |
                      {'stop', state()}.
init(_Args) ->
    {'ok', read_config()}.

handle_call(_Request, _From, State) ->
    {'noreply', State}.

handle_cast({'start_udp_server'}, State = #state{port = Port}) ->
    {'ok', Socket} = gen_udp:open(Port, ['binary', {'active', 'false'}]),
    inet:setopts(Socket, [{active, once}]),
    gen_udp:controlling_process(Socket, self()),
    lager:debug("UDP server opened socket: ~p", [Socket]),
    {'noreply', State#state{socket = Socket}};
handle_cast(Request, State) ->
    lager:debug("Unexpected cast request: ~p", [Request]),
    {'noreply', State}.

%'discreq' | 'discack' | 'discnak'.

handle_info(Info = {'udp', Socket, IP, InPortNo, Packet}, State = #state{socket = Socket, secret = Secret}) ->
    lager:debug("Received UDP packet: ~p", [Info]),
    case eradius_lib:decode_request(Packet, Secret) of
        'bad_pdu' ->
            lager:debug("Bad PDU format", [Packet]);
        Request = #radius_request{cmd = 'discreq'
                                  ,attrs = Attrs
                                  ,reqid = ReqId
                                  ,authenticator = ReqAuthenticator} ->
            lager:debug("Decoded Disconnect-Request PDU is: ~p", [Request]),
            NewAttrs = [{list_to_binary(AttrName), AttrValue} || {{'attribute',_,_,AttrName,_},AttrValue} <- Attrs],
            case {props:get_value(<<"Acct-Session-Id">>, NewAttrs), props:get_value(<<"User-Name">>, NewAttrs)} of
                {AcctSessionId, 'undefined'} when is_binary(AcctSessionId) ->
                    lager:debug("Disconnection by AcctSessionId: ~p", [AcctSessionId]),
                    maybe_get_active_channels('session_id'
                                              ,AcctSessionId
                                              ,{Socket, IP, InPortNo, ReqId, ReqAuthenticator, Secret}
                                             );
                {'undefined', UserName} when is_binary(UserName) ->
                    lager:debug("Disconnection by UserName: ~p", [UserName]),
                    maybe_get_active_channels('user_name'
                                              ,UserName
                                              ,{Socket, IP, InPortNo, ReqId, ReqAuthenticator, Secret}
                                             );
                {AcctSessionId, UserName} when is_binary(AcctSessionId) andalso is_binary(UserName) ->
                    lager:debug("Disconnection by AcctSessionId and UserName: ~p ~p", [AcctSessionId, UserName]),
                    maybe_get_active_channels('session_id'
                                              ,AcctSessionId
                                              ,{Socket, IP, InPortNo, ReqId, ReqAuthenticator, Secret}
                                             ),
                    maybe_get_active_channels('user_name'
                                              ,UserName
                                              ,{Socket, IP, InPortNo, ReqId, ReqAuthenticator, Secret}
                                             );
                {'undefined', 'undefined'} ->
                    lager:debug("No required fields in AVPs ~p", [NewAttrs])
            end;
        _ ->
            lager:debug("Unexpected PDU type. Bypassed.")
    end,
    inet:setopts(Socket, [{active, once}]),
    {'noreply', State};
handle_info(Info, State) ->
    lager:debug("Unexpected info request: ~p", [Info]),
    {'noreply', State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_udp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {'noreply', State}.

%%%===================================================================
%%% util functions
%%%===================================================================

read_config() ->
    ConfigPort = whapps_config:get_integer(<<"circlemaker">>, [<<"disconnection_packet">>, <<"port">>], 3799),
    ConfigSecret = whapps_config:get_binary(<<"circlemaker">>, [<<"disconnection_packet">>, <<"secret">>]),
    State = #state{port = ConfigPort, secret = ConfigSecret},
    lager:debug("Loaded configuration is ~p", [State]),
    State.

maybe_get_active_channels(Key, Value, {Socket, IP, InPortNo, ReqId, ReqAuthenticator, Secret}) ->
    Req = case Key of
            'session_id' ->
                [{<<"Call-ID">>, Value}
                 ,{<<"Fields">>, <<"all">>}
                 ,{<<"Active-Only">>, 'true'}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ];
            'user_name' ->
                [{<<"Fields">>, <<"all">>}
                 ,{<<"Active-Only">>, 'true'}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]
          end,
    % TODO: rewrite as async request? spawn?
    case wh_amqp_worker:call_collect(Req
                                     ,fun wapi_call:publish_query_channels_req/1
                                     ,{'ecallmgr', fun wapi_call:query_channels_resp_v/1}
                                    )
    of
        {'ok', []} ->
            lager:debug("No channels in response");
        {'ok', StatusJObjs} ->
            case find_channel(Key, Value, StatusJObjs) of
                [] ->
                    lager:debug("No information");
                Channels ->
                    ChannelsFlatten = lists:flatten(Channels),
                    lager:debug("Channels found: ~p", [ChannelsFlatten]),
                    hangup_channels(ChannelsFlatten, {Socket, IP, InPortNo, ReqId, ReqAuthenticator, Secret})
            end;
        {'returned', _JObj, BR} ->
            lager:debug("Return something: ~p", [BR]);
        {'timeout', Resp} ->
            lager:debug("Timeout: ~p", [Resp]);
        {'error', Error} ->
            lager:debug("Error: ~p", [Error])
    end.

-spec find_channel(ne_binary(), ne_binary(), wh_json:objects()) -> api_object().
find_channel(Key, Value, JObjs) ->
    find_channel(Key, Value, JObjs, []).

-spec find_channel(ne_binary(), ne_binary(), wh_json:objects(), wh_json:objects()) -> api_object().
find_channel(_Key, _Value, [], Acc) -> Acc;
find_channel('session_id' = Key, CallId, [StatusJObj|JObjs], Acc) ->
    lager:debug("Next StatusJObj is ~p", [StatusJObj]),
    Channel = wh_json:get_value([<<"Channels">>, CallId], StatusJObj),
    case wh_json:get_value(<<"Call-ID">>, Channel) of
        CallId -> find_channel(Key, CallId, JObjs, [Channel | Acc]);
        _AnotherCallId -> find_channel(Key, CallId, JObjs, Acc)
    end;
find_channel('user_name' = Key, UserName, [StatusJObj|JObjs], Acc) ->
    lager:debug("Next StatusJObj is ~p", [StatusJObj]),
    Channels = wh_json:get_value(<<"Channels">>, StatusJObj),
    lager:debug("Next Channels are ~p", [Channels]),
    FoundUsernameChannels = wh_json:foldl(
                                fun(_Key, JObjChannel, Acc1) ->
                                    lager:debug("Next foldl object is ~p", [{_Key, JObjChannel, Acc1}]),
                                    case wh_json:get_value(<<"Username">>, JObjChannel) of
                                        UserName -> [JObjChannel | Acc1];
                                        _ -> Acc1
                                    end
                                end,
                                [], Channels),
    lager:debug("Next FoundUsernameChannels are ~p", [FoundUsernameChannels]),
    find_channel(Key, UserName, JObjs, [FoundUsernameChannels | Acc]).

hangup_channels(Channels, {Socket, IP, InPortNo, ReqId, ReqAuthenticator, Secret}) ->
    lager:debug("Channels are hang up"),
    lists:foreach(fun(JObjChannel) -> cm_util:hangup_call(wh_json:get_value(<<"Bridge-ID">>, JObjChannel)) end, Channels),
    send_disconnect_resp({Socket, IP, InPortNo, ReqId, ReqAuthenticator, Secret}).

send_disconnect_resp({Socket, IP, InPortNo, ReqId, ReqAuthenticator, Secret}) ->
    Req = #radius_request{cmd = 'discack', reqid = ReqId, secret = Secret},
    lager:debug("Prepare md5_authenticator value from ~p and ~p", [Req, ReqAuthenticator]),
    Authenticator = eradius_lib:md5_authenticator(Req, ReqAuthenticator),
    Request = eradius_lib:set_attributes(Req#radius_request{authenticator = Authenticator}, []),
    lager:debug("Prepared disconnect response is ~p", [Request]),
    Packet = eradius_lib:encode_reply_request(Request),
    lager:debug("Sending encoded disconnect PDU ~p", [Packet]),
    gen_udp:send(Socket, IP, InPortNo, Packet).
