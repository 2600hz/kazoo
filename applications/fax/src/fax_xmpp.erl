%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(fax_xmpp).
-behaviour(gen_server).

-include("fax_cloud.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-define(XMPP_SCOPE,<<"https://www.googleapis.com/auth/googletalk">>).
-define(SCOPES,<<(?XMPP_SCOPE)/binary, " ", (?GCP_SCOPE)/binary>>).
-define(XMPP_SERVER, "talk.google.com").

-define(POLLING_INTERVAL, whapps_config:get_integer(?CONFIG_CAT, <<"xmpp_interval">> , 600000)).

-export([start/1, start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).
  
-export([send_notify/2]).


-record(state, 
        {
         faxbox_id :: ne_binary(),
         printer_id = 'undefined' :: api_binary(),
         oauth_app_id = 'undefined' :: api_binary(),
         refresh_token = 'undefined' :: api_binary(),
         connected = 'false' :: boolean(),
         session :: any(),
         jid :: any(),
         full_jid :: any()
        }).


start(PrinterId) -> 
  gen_server:start({local, wh_util:to_atom(PrinterId, 'true')}, ?MODULE, [PrinterId], []).

start_link(PrinterId) -> 
  gen_server:start_link({local, wh_util:to_atom(PrinterId, 'true')}, ?MODULE, [PrinterId], []).
  
stop(PrinterId) ->
  gen_server:cast({local, wh_util:to_atom(PrinterId, 'true')}, stop).
  
init([PrinterId]) ->
    process_flag(trap_exit, 'true'),
    gen_server:cast(self(), 'start'),    
    {ok, #state{faxbox_id=PrinterId}, ?POLLING_INTERVAL}.
  
  
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast('start', #state{faxbox_id=FaxBoxId} = State) ->
    case couch_mgr:open_doc(?WH_FAXES, FaxBoxId) of
        {'ok', JObj} ->
            JID = wh_json:get_value(<<"pvt_cloud_xmpp_jid">>, JObj),
            PrinterId = wh_json:get_value(<<"pvt_cloud_printer_id">>, JObj),
            UID = <<JID/binary,"/",PrinterId/binary>>,
            AppId = wh_json:get_value(<<"pvt_cloud_oauth_app">>, JObj),
            RefreshToken=#oauth_refresh_token{token=wh_json:get_value(<<"pvt_cloud_refresh_token">>, JObj)},
            gen_server:cast(self(), 'connect'),
            {noreply, State#state{printer_id=PrinterId,
                                  oauth_app_id=AppId,
                                  full_jid=UID,
                                  refresh_token=RefreshToken}
            , ?POLLING_INTERVAL
            };
          E ->
              {stop, E, State}
    end;

handle_cast('connect', #state{oauth_app_id=AppId, full_jid=JID, refresh_token=RefreshToken}=State) ->
    {'ok', App} = kazoo_oauth_util:get_oauth_app(AppId),
    {'ok', #oauth_token{token=Token} = OAuthToken} = kazoo_oauth_util:token(App, RefreshToken),
    case connect(wh_util:to_list(JID), wh_util:to_list(Token)) of
        {ok, {MySession, MyJID}} ->
            gen_server:cast(self(), 'subscribe'),
            {noreply, State#state{session=MySession, jid=MyJID, connected='true'}, ?POLLING_INTERVAL};
        _ -> 
            {stop, <<"Error connecting to xmpp server">>, State}
    end;

handle_cast('status', State) ->
    {noreply, State, ?POLLING_INTERVAL};

handle_cast('subscribe', #state{jid=MyJID, session=MySession}=State) ->
    {_, JFull, _JUser, _JDomain, _JResource} = MyJID,
    lager:debug("xmpp subscribe ~s",[JFull]),
    IQ = get_sub_msg(MyJID),
    PacketId = exmpp_session:send_packet(MySession, IQ),
    {noreply, State, ?POLLING_INTERVAL};

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(#received_packet{packet_type='message'}=Packet, State) ->    
    process_received_packet(Packet, State),
    {noreply, State, ?POLLING_INTERVAL};
handle_info(#received_packet{packet_type='iq'
                            ,raw_packet=Packet}, State) ->    
    lager:debug("xmpp received iq packet ~s",[exmpp_xml:document_to_binary(Packet)]),
    {noreply, State, ?POLLING_INTERVAL};
handle_info(#received_packet{}=Packet, State) ->
    lager:debug("xmpp received unknown packet type : ~p",[Packet]),
    {noreply, State, ?POLLING_INTERVAL};
handle_info(timeout, State) ->    
    gen_server:cast(self(), 'subscribe'),
    {noreply, State, ?POLLING_INTERVAL};
handle_info(_Info, State) -> 
    lager:debug("xmpp handle_info ~p",[_Info]),
  {noreply, State, ?POLLING_INTERVAL}.


terminate(_Reason, #state{jid=MyJID
                         ,session=MySession
                         ,connected='true'}) ->
    {_, JFull, _JUser, _JDomain, _JResource} = MyJID,
    lager:debug("terminating xmpp session ~s",[JFull]),
  disconnect(MySession),
  'ok';
terminate(_Reason, State) -> 
    lager:debug("terminate xmpp module with reason ~p",[_Reason]),
  'ok'.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec get_sub_msg(tuple()) -> ne_binary().
get_sub_msg({_, JFull, JUser, JDomain,JResource} = JID) ->
    BareJID = <<JUser/binary,"@",JDomain/binary>>,
    Document = <<"<iq type='set' from='", JFull/binary, "' to='",BareJID/binary,"'>"
                 ,   "<subscribe xmlns='google:push'>"
                 ,      "<item channel='cloudprint.google.com' from='cloudprint.google.com'/>"
                 ,   "</subscribe>"
                 ,"</iq>">>,
     Document.

-define(NS_PUSH, 'google:push').
-define(XML_CTX_OPTIONS,[{namespace, [{"g", "google:push"}]}]).

process_received_packet(#received_packet{raw_packet=Packet},#state{jid=JID}=State) ->
    {_, JFull, JUser, JDomain,JResource} = JID,
    BareJID = <<JUser/binary,"@",JDomain/binary>>,
    case exmpp_xml:get_element(Packet, ?NS_PUSH, 'push') of
        'undefined' -> 'undefined';
        Push -> 
            DataNode = exmpp_xml:get_element(Push, ?NS_PUSH, 'data'),
            Data64 = exmpp_xml:get_cdata(DataNode),
            PrinterId = base64:decode(Data64),
            send_notify(PrinterId, BareJID)
    end.

send_notify(PrinterId, JID) ->
    Payload = props:filter_undefined(
                [{<<"Event-Name">>, <<"push">>}
                ,{<<"Application-Name">>, <<"GCP">>}
                ,{<<"Application-Event">>, <<"Queued-Job">>}
                ,{<<"Application-Data">>, PrinterId}
                ,{<<"JID">>, JID}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    wapi_xmpp:publish_event(Payload).
    
connect(JID, Password) ->
    Session = exmpp_session:start({1,0}),
    Jid = exmpp_jid:parse(JID),
    exmpp_session:auth(Session, Jid, Password, "X-OAUTH2"),
    StreamId  = exmpp_session:connect_TCP(Session, ?XMPP_SERVER, 5222,[{starttls, enabled}]),
    
    try init_session(Session, Password)
    catch
      _:Error -> lager:debug("error connecting xmpp session: ~p", [Error]),
         {error, Error}
    end,
    {ok, {Session, Jid}}.


init_session(Session, Password) ->
  try exmpp_session:login(Session,"X-OAUTH2")
  catch
    throw:{auth_error, 'not-authorized'} ->
    exmpp_session:register_account(Session, Password),
    exmpp_session:login(Session)
  end,
  exmpp_session:send_packet(Session, exmpp_presence:set_status(exmpp_presence:available(), "Pubsubber Ready")),
  ok.

disconnect(MySession) ->
  try 
        exmpp_session:stop(MySession)
  catch
    E:R ->
        lager:debug("exception closing xmpp session ~p : ~p",[E,R])
  end.
