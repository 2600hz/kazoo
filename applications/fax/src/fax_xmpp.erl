%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_xmpp).
-behaviour(gen_server).

-include("fax_cloud.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

-define(XMPP_SCOPE,<<"https://www.googleapis.com/auth/googletalk">>).
-define(SCOPES,<<(?XMPP_SCOPE)/binary, " ", (?GCP_SCOPE)/binary>>).
-define(XMPP_SERVER, <<"talk.google.com">>).

-define(POLLING_INTERVAL, kapps_config:get_integer(?CONFIG_CAT, <<"xmpp_interval">> , 600000)).

-export([start_link/1, stop/1]).

-export([handle_printer_start/2
        ,handle_printer_stop/2
        ]).

-export([start_all_printers/0]).

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

-record(state, {faxbox_id :: kz_term:api_ne_binary()
               ,printer_id :: kz_term:api_ne_binary()
               ,oauth_app_id :: kz_term:api_ne_binary()
               ,refresh_token :: oauth_refresh_token() | 'undefined'
               ,connected = 'false' :: boolean()
               ,session :: xmpp_client() | 'undefined'
               ,jid :: kz_term:api_ne_binary()
               ,monitor :: kz_term:api_reference()
               }).
-type state() :: #state{}.

-define(NAME(P), kz_term:to_atom(P, 'true')).

%%-define(SERVER(P), {{'via', 'kz_globals', {'xmpp', P}}).
-define(SERVER(P), {'via', 'kz_globals', ?NAME(P)}).

-spec start_link(kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(PrinterId) ->
    lager:debug("starting new xmpp process for ~s", [PrinterId]),
    case gen_server:start_link(?SERVER(PrinterId), ?MODULE, [PrinterId], []) of
        {'error', {'already_started', Pid}} ->
            erlang:link(Pid),
            {'ok', Pid};
        Other -> Other
    end.

-spec stop(kz_term:ne_binary()) -> 'ok'.
stop(PrinterId) ->
    case kz_globals:whereis_name(?NAME(PrinterId)) of
        'undefined' -> 'ok';
        Pid -> gen_server:cast(Pid, 'stop')
    end.

-spec init(kz_term:ne_binaries()) -> {'ok', state(), pos_integer()}.
init([PrinterId]) ->
    process_flag('trap_exit', 'true'),
    gen_server:cast(self(), 'start'),
    {'ok', #state{faxbox_id=PrinterId}, ?POLLING_INTERVAL}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('start', #state{faxbox_id=FaxBoxId} = State) ->
    case kz_datamgr:open_doc(?KZ_FAXES_DB, FaxBoxId) of
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

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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

-spec terminate(any(), state()) -> 'ok'.
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

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) -> {'ok', State}.

-spec get_sub_msg(kz_term:ne_binary()) -> kz_term:ne_binary().
get_sub_msg(JID) ->
    BareJID = kapi_xmpp:jid_short(JID),
    list_to_binary(["<iq type='set' from='", JID, "' to='", BareJID, "'>"
                   ,   "<subscribe xmlns='google:push'>"
                   ,      "<item channel='cloudprint.google.com' from='cloudprint.google.com'/>"
                   ,   "</subscribe>"
                   ,"</iq>"
                   ]).

-define(NS_PUSH, 'google:push').
-define(XML_CTX_OPTIONS,[{'namespace', [{"g", "google:push"}]}]).

-spec process_received_packet(exml:element(), state()) -> any().
process_received_packet(#xmlel{name = <<"message">>}=Xml
                       ,#state{jid=JID}) ->
    BareJID = kapi_xmpp:jid_short(JID),
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

-spec send_notify(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
send_notify(PrinterId, JID) ->
    Payload = props:filter_undefined(
                [{<<"Event-Name">>, <<"push">>}
                ,{<<"Application-Name">>, <<"GCP">>}
                ,{<<"Application-Event">>, <<"Queued-Job">>}
                ,{<<"Application-Data">>, PrinterId}
                ,{<<"JID">>, JID}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    lager:debug("received xmpp push for printer ~s", [PrinterId]),
    kz_amqp_worker:cast(Payload, fun kapi_xmpp:publish_event/1).

-spec connect(kz_term:ne_binary(), kz_term:ne_binary()) ->
                     {'ok', xmpp_client()} |
                     {'error', any()}.
connect(JID, Password) ->
    Options = [{username, kapi_xmpp:jid_username(JID)}
              ,{server, kapi_xmpp:jid_server(JID)}
              ,{resource, kapi_xmpp:jid_resource(JID)}
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

-spec handle_start(kz_json:object(), state()) -> state().
handle_start(JObj, State) ->
    JID = kz_json:get_value(<<"pvt_cloud_xmpp_jid">>, JObj),
    PrinterId = kz_json:get_value(<<"pvt_cloud_printer_id">>, JObj),
    FullJID = <<JID/binary, "/", PrinterId/binary>>,
    AppId = kz_json:get_value(<<"pvt_cloud_oauth_app">>, JObj),
    RefreshToken = #oauth_refresh_token{token=kz_json:get_value(<<"pvt_cloud_refresh_token">>, JObj)},
    gen_server:cast(self(), 'connect'),
    State#state{printer_id=PrinterId
               ,oauth_app_id=AppId
               ,jid=FullJID
               ,refresh_token=RefreshToken
               }.

-spec auth_xoauth2(escalus_connection:client(), kz_term:proplist()) -> 'ok'.
auth_xoauth2(Conn, Props) ->
    Username = get_property(username, Props),
    Password = get_property(password, Props),
    Payload = <<0:8,Username/binary,0:8,Password/binary>>,
    Stanza = escalus_stanza:auth(<<"X-OAUTH2">>, [base64_cdata(Payload)]),
    'ok' = escalus_connection:send(Conn, Stanza),
    wait_for_success(Username, Conn).

base64_cdata(Payload) ->
    #xmlcdata{content = base64:encode(Payload)}.

get_property(PropName, Proplist) ->
    case lists:keyfind(PropName, 1, Proplist) of
        {PropName, Value} -> Value;
        'false' -> throw({'missing_property', PropName})
    end.

wait_for_success(Username, Conn) ->
    AuthReply = escalus_connection:get_stanza(Conn, 'auth_reply'),
    case AuthReply#xmlel.name of
        <<"success">> -> 'ok';
        R when R =:= <<"failure">>
               orelse R =:= <<"stream:error">> ->
            throw({'auth_failed', Username, AuthReply})
    end.

-spec start_all_printers() -> 'ok'.
start_all_printers() ->
    {'ok', Results} = kz_datamgr:get_results(?KZ_FAXES_DB, <<"faxbox/cloud">>),
    List = kz_term:shuffle_list(
             [{2000 + rand:uniform(4000)
              ,kz_doc:id(Result)
              ,kz_json:get_value([<<"value">>,<<"xmpp_jid">>], Result)
              }
              || Result <- Results,
                 <<"claimed">> =:= kz_json:get_ne_binary_value([<<"value">>,<<"state">>], Result)
             ]),
    [begin
         send_start_printer(Id, Jid),
         timer:sleep(Pause)
     end
     || {Pause, Id, Jid} <- List],
    'ok'.


-spec send_start_printer(kz_term:ne_binary(), kz_term:ne_binary()) -> any().
send_start_printer(PrinterId, JID) ->
    Payload = props:filter_undefined(
                [{<<"Event-Name">>, <<"start">>}
                ,{<<"Application-Name">>, <<"fax">>}
                ,{<<"Application-Event">>, <<"init">>}
                ,{<<"Application-Data">>, PrinterId}
                ,{<<"JID">>, JID}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    kz_amqp_worker:cast(Payload, fun kapi_xmpp:publish_event/1).

-spec handle_printer_start(kz_json:object(), kz_term:proplist()) -> kz_types:sup_startchild_ret().
handle_printer_start(JObj, _Props) ->
    'true' = kapi_xmpp:event_v(JObj),
    PrinterId = kz_json:get_value(<<"Application-Data">>, JObj),
    fax_xmpp_sup:start_printer(PrinterId).

-spec handle_printer_stop(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_printer_stop(JObj, _Props) ->
    'true' = kapi_xmpp:event_v(JObj),
    PrinterId = kz_json:get_value(<<"Application-Data">>, JObj),
    stop(PrinterId).
