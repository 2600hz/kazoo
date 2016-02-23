%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(fax_xmpp).
-behaviour(gen_server).

-include("fax_cloud.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

-define(XMPP_SCOPE,<<"https://www.googleapis.com/auth/googletalk">>).
-define(SCOPES,<<(?XMPP_SCOPE)/binary, " ", (?GCP_SCOPE)/binary>>).
-define(XMPP_SERVER, <<"talk.google.com">>).

-define(POLLING_INTERVAL, whapps_config:get_integer(?CONFIG_CAT, <<"xmpp_interval">> , 600000)).

-export([start/1, start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-export([send_notify/2]).
-export([auth_xoauth2/2]).

-type xmpp_client() :: #client{}. %% escalus

-record(state, {faxbox_id :: ne_binary()
                ,printer_id :: ne_binary()
                ,oauth_app_id :: ne_binary()
                ,refresh_token :: oauth_refresh_token()
                ,connected = 'false' :: boolean()
                ,session :: xmpp_client()
                ,jid :: ne_binary()
                ,monitor :: reference()
               }).

-type state() :: #state{}.
%-type packet() :: #received_packet{}.
%-type jid() :: #jid{}.

start(PrinterId) ->
  gen_server:start({'local', wh_util:to_atom(PrinterId, 'true')}, ?MODULE, [PrinterId], []).

-spec start_link(ne_binary()) -> startlink_ret().
start_link(PrinterId) ->
  gen_server:start_link({'local', wh_util:to_atom(PrinterId, 'true')}, ?MODULE, [PrinterId], []).

stop(PrinterId) ->
  gen_server:cast({'local', wh_util:to_atom(PrinterId, 'true')}, 'stop').

init([PrinterId]) ->
    process_flag('trap_exit', 'true'),
    gen_server:cast(self(), 'start'),
    {'ok', #state{faxbox_id=PrinterId}, ?POLLING_INTERVAL}.

handle_call(_Request, _From, State) ->
  {'reply', 'ok', State}.

handle_cast('start', #state{faxbox_id=FaxBoxId} = State) ->
    case couch_mgr:open_doc(?WH_FAXES_DB, FaxBoxId) of
        {'ok', JObj} ->
            {'noreply', handle_start(JObj, State), ?POLLING_INTERVAL};
        E ->
            {'stop', E, State}
    end;

handle_cast('connect', #state{oauth_app_id=AppId
                              ,jid=JID
                              ,refresh_token=RefreshToken
                             }=State) ->
    {'ok', App} = kazoo_oauth_util:get_oauth_app(AppId),
    {'ok', #oauth_token{token=Token}} = kazoo_oauth_util:token(App, RefreshToken),
    case connect(JID, Token) of
        {'ok', #client{rcv_pid=Pid}= MySession} ->
            Monitor = erlang:monitor('process', Pid),
            gen_server:cast(self(), 'subscribe'),
            {'noreply', State#state{monitor=Monitor
                                    ,session=MySession
                                    ,connected='true'
                                   }
             ,?POLLING_INTERVAL
            };
        _ ->
            {'stop', <<"Error connecting to xmpp server">>, State}
    end;

handle_cast('status', State) ->
    {'noreply', State, ?POLLING_INTERVAL};

handle_cast('subscribe', #state{jid=MyJID
                                ,session=MySession
                               }=State) ->
    lager:debug("xmpp subscribe ~s",[MyJID]),
    IQ = escalus_stanza:from_xml(get_sub_msg(MyJID)),
    escalus_client:send(MySession, IQ),
    {'noreply', State, ?POLLING_INTERVAL};

handle_cast('stop', State) -> {'stop', 'normal', State};
handle_cast(_Msg, State) -> {'noreply', State}.

handle_info({'stanza', _Client, #xmlel{}=Packet}, State) ->
    process_received_packet(Packet, State),
    {'noreply', State, ?POLLING_INTERVAL};

handle_info('timeout', State) ->
    gen_server:cast(self(), 'subscribe'),
    {'noreply', State, ?POLLING_INTERVAL};
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}=A, #state{monitor=MonitorRef}=State) ->
  lager:debug("xmpp session down ~p",[A]),
  gen_server:cast(self(), 'start'),
  {'noreply', State, ?POLLING_INTERVAL};
handle_info(_Info, State) ->
    lager:debug("xmpp handle_info ~p",[_Info]),
    {'noreply', State, ?POLLING_INTERVAL}.

terminate(_Reason, #state{jid=JID
                          ,session=MySession
                          ,monitor=MonitorRef
                          ,connected='true'
                         }) ->
    lager:debug("terminating xmpp session ~s",[JID]),
    erlang:demonitor(MonitorRef, ['flush']),
    disconnect(MySession);
terminate(_Reason, _State) ->
    lager:debug("terminate xmpp module with reason ~p",[_Reason]).

code_change(_OldVsn, State, _Extra) -> {'ok', State}.

-spec get_sub_msg(ne_binary()) -> ne_binary().
get_sub_msg(JID) ->
    BareJID = wapi_xmpp:jid_short(JID),
    Document = <<"<iq type='set' from='", JID/binary, "' to='",BareJID/binary,"'>"
                 ,   "<subscribe xmlns='google:push'>"
                 ,      "<item channel='cloudprint.google.com' from='cloudprint.google.com'/>"
                 ,   "</subscribe>"
                 ,"</iq>"
               >>,
    Document.

-define(NS_PUSH, 'google:push').
-define(XML_CTX_OPTIONS,[{'namespace', [{"g", "google:push"}]}]).

-spec process_received_packet(exml:element(), state()) -> any().
process_received_packet(#xmlel{name = <<"message">>}=Xml
                        ,#state{jid=JID}) ->
    BareJID = wapi_xmpp:jid_short(JID),
    Push = exml_query:path(Xml, [{'element', <<"push:push">>}
                                 ,{'element', <<"push:data">>}
                                 ,'cdata'
                                ]),
    case Push of
        'undefined' -> 'undefined';
        PushData ->
            PrinterId = base64:decode(PushData),
            send_notify(PrinterId, BareJID)
    end;
process_received_packet(#xmlel{name=Type}=Xml, _State) ->
    lager:debug("received xml element ~s : ~s", [Type, exml:to_pretty_iolist(Xml)]).

-spec send_notify(ne_binary(), ne_binary()) -> any().
send_notify(PrinterId, JID) ->
    Payload = props:filter_undefined(
                [{<<"Event-Name">>, <<"push">>}
                 ,{<<"Application-Name">>, <<"GCP">>}
                 ,{<<"Application-Event">>, <<"Queued-Job">>}
                 ,{<<"Application-Data">>, PrinterId}
                 ,{<<"JID">>, JID}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    lager:debug("received xmpp push for printer ~s", [PrinterId]),
    wh_amqp_worker:cast(Payload, fun wapi_xmpp:publish_event/1).

-spec connect(ne_binary(), ne_binary()) ->
                     {'ok', xmpp_client()} |
                     {'error', any()}.
connect(JID, Password) ->
    Options = [{username, wapi_xmpp:jid_username(JID)}
               ,{server, wapi_xmpp:jid_server(JID)}
               ,{resource, wapi_xmpp:jid_resource(JID)}
               ,{password, Password}
               ,{host, ?XMPP_SERVER}
               ,{auth, {fax_xmpp, auth_xoauth2}}
               ,{starttls, optional}
              ],
    try escalus_connection:start(Options) of
        {ok, Conn, _Props, _} -> {ok, Conn};
        {error, _Error}=E -> E
    catch
        _E:_M ->
            lager:debug("exception ~p : ~p", [_E, _M]),
            {'error', _M}
    end.

-spec disconnect(xmpp_client()) -> any().
disconnect(MySession) ->
    try
        escalus_connection:stop(MySession)
    catch
        _E:_R ->
            lager:debug("exception closing xmpp session ~p : ~p", [_E, _R])
    end.

-spec handle_start(wh_json:object(), state()) -> state().
handle_start(JObj, State) ->
    JID = wh_json:get_value(<<"pvt_cloud_xmpp_jid">>, JObj),
    PrinterId = wh_json:get_value(<<"pvt_cloud_printer_id">>, JObj),
    FullJID = <<JID/binary, "/", PrinterId/binary>>,
    AppId = wh_json:get_value(<<"pvt_cloud_oauth_app">>, JObj),
    RefreshToken = #oauth_refresh_token{token=wh_json:get_value(<<"pvt_cloud_refresh_token">>, JObj)},
    gen_server:cast(self(), 'connect'),
    State#state{printer_id=PrinterId
                ,oauth_app_id=AppId
                ,jid=FullJID
                ,refresh_token=RefreshToken
               }.

auth_xoauth2(Conn, Props) ->
    Username = get_property(username, Props),
    Password = get_property(password, Props),
    Payload = <<0:8,Username/binary,0:8,Password/binary>>,
    Stanza = escalus_stanza:auth(<<"X-OAUTH2">>, [base64_cdata(Payload)]),
    ok = escalus_connection:send(Conn, Stanza),
    wait_for_success(Username, Conn).

base64_cdata(Payload) ->
    #xmlcdata{content = base64:encode(Payload)}.

get_property(PropName, Proplist) ->
    case lists:keyfind(PropName, 1, Proplist) of
        {PropName, Value} ->
            Value;
        false ->
            throw({missing_property, PropName})
    end.

wait_for_success(Username, Conn) ->
    AuthReply = escalus_connection:get_stanza(Conn, auth_reply),
    case AuthReply#xmlel.name of
        <<"success">> ->
            ok;
        R when R =:= <<"failure">> orelse R =:= <<"stream:error">> ->
            throw({auth_failed, Username, AuthReply})
    end.
