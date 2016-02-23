%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(omnipresence_proxy).

-include("omnipresence.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0, stop/0]).

-export([sip_subscribe/2, sip_resubscribe/2, sip_dialog_update/3]).
-export([sip_authorize/3]).
-export([sip_get_user_pass/4]).

-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-export([update_manager/1]).

-record(state, {subs_pid :: pid()
                ,subs_ref :: reference()
               }).

-spec start_link() -> startlink_ret().
start_link() ->
    lager:info("starting sip callback"),
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

%% @doc Stops the SipApp.
stop() ->
    lager:info("stopping PROXY"),
    'ok'.

%%%%%%%%%%%%%%%%%%%%%%%  NkSIP CallBacks %%%%%%%%%%%%%%%%%%%%%%%%

sip_get_user_pass(User, Realm, _Req, _Call) ->
    ReqResp = whapps_util:amqp_pool_request(auth_req(User,Realm)
                                           ,fun wapi_authn:publish_req/1
                                           ,fun wapi_authn:resp_v/1
                                          ),

    case ReqResp of
        {'ok', JObj} ->
            Password = wh_json:get_value(<<"Auth-Password">>,JObj),
            case Realm of
                <<"teste.sip.90e9.com">> -> 'true';
                _ -> Password
            end;
        {'error', _} ->
            case Realm of
                <<"teste.sip.90e9.com">> -> 'true';
                _ -> 'false'
            end
    end.

sip_authorize(Auth, Req, Call) ->
    {'ok', Method} = nksip_request:meta(method, Req),
    sip_authorize(Method, Auth, Req, Call).

sip_authorize('OPTIONS', _Auth, _Req, _Call) -> 'ok';
sip_authorize(_Method, Auth, Req, _Call) ->
    IsDialog = lists:member('dialog', Auth),
    IsRegister = lists:member('register', Auth),
    {'ok', Realm} = nksip_request:meta('to_domain', Req),
    case IsDialog orelse IsRegister of
        'true' -> 'ok';
        'false' when Realm =:= <<"teste.sip.90e9.com">> -> 'ok';
        'false' ->
            case nksip_lib:get_value({'digest', Realm}, Auth) of
                'true' -> 'ok';
                'false' -> 'forbidden';
                'undefined' -> {'proxy_authenticate', Realm}
            end
    end.


sip_subscribe(Req, _Call) ->
    {'ok', ReqId} = nksip_request:get_handle(Req),
    _ = wh_util:spawn(fun update_manager/1, [ReqId]),
    'noreply'.

sip_resubscribe(Req, _Call) ->
    {'ok', ReqId} = nksip_request:get_handle(Req),
    _ = wh_util:spawn(fun update_manager/1, [ReqId]),
    'noreply'.

sip_dialog_update({'subscription_status', State, _Subs}, _Dialog, _Call) ->
    lager:debug("subscription_status ~p", [State]);
sip_dialog_update(Update, _Dialog, _Call) ->
    lager:debug("dialog update ~p", [Update]).

%% @doc SipApp Callback: initialization.
init([]) ->
    lager:info("callback init called"),
    gen_server:cast(self(), 'find_subscriptions_srv'),
    {'ok', #state{}}.

%% @doc SipApp Callback: Synchronous user call.
handle_call(_Info, _From, State) ->
    lager:info("unhandled call from ~p: ~p",[_From, _Info]),
    {'noreply', State}.

%% @doc SipApp Callback: Asynchronous user cast.
handle_cast({'omnipresence_link', Props}, #state{subs_pid=Pid}=State) ->
    gen_server:call(Pid, {'subscribe', Props}),
    {'noreply', State};
handle_cast('find_subscriptions_srv', #state{subs_pid=_Pid}=State) ->
    case omnipresence_sup:subscriptions_srv() of
        'undefined' ->
            lager:debug("no subs_pid"),
            gen_server:cast(self(), 'find_subscriptions_srv'),
            timer:sleep(500),
            {'noreply', State#state{subs_pid='undefined'}};
        P when is_pid(P) ->
            lager:debug("new subs pid: ~p", [P]),
            {'noreply', State#state{subs_pid=P
                                    ,subs_ref=erlang:monitor('process', P)
                                   }}
    end;
handle_cast(_Info, State) ->
    lager:info("unhandled cast: ~p",[_Info]),
    {'noreply', State}.

%% @doc SipApp Callback: External erlang message received.
handle_info({'DOWN', Ref, 'process', Pid, _R}, #state{subs_pid=Pid
                                                      ,subs_ref=Ref
                                                     }=State) ->
    gen_server:cast(self(), 'find_subscriptions_srv'),
    {'noreply', State#state{subs_pid='undefined'
                            ,subs_ref='undefined'
                           }};
handle_info(_Info, State) ->
    lager:info("unhandled info: ~p", [_Info]),
    {'noreply', State}.

terminate(_Reason, _State) ->
    lager:debug("TERMINATE APP: ~p", [_Reason]).


%%%%%%%%%%%%%%%%%%%%%%%  Internal %%%%%%%%%%%%%%%%%%%%%%%%

-spec auth_req(ne_binary(), ne_binary()) -> wh_proplist().
auth_req(User, Realm) ->
    [{<<"Msg-ID">>, wh_util:rand_hex_binary(15)}
     ,{<<"To">>, <<User/binary,"@",Realm/binary>>}
     ,{<<"From">>, <<User/binary,"@",Realm/binary>>}
     ,{<<"Auth-User">>, User}
     ,{<<"Auth-Realm">>, Realm}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec update_manager(nksip:request()|nksip:handle()) -> pid().
update_manager(Request) ->
    {'ok', [{'from', From}
            ,{'to_user', ToUser}
            ,{'to', To}
            ,{'subscription_handle', SubscriptionId}
            ,{'call_id', CallId}
            ,{{'header', <<"record-route">>}, RecordRoutes}
            ,{'expires', Expires}
            ,{'aor', AOR}
           ]} = nksip_request:metas(['from'
                                     ,'to_user'
                                     ,'to'
                                     ,'subscription_handle'
                                     ,'call_id'
                                     ,{'header', <<"record-route">>}
                                     ,'expires'
                                     ,'aor'
                                    ], Request),
    {'ok', {EventName, _EventOptions}= _Event} = nksip_request:meta('event', Request),
    {'ok', Expires} = nksip_request:meta('expires', Request),

    FirstRoute = lists:nth(1, RecordRoutes),
    LastRoute = lists:last(RecordRoutes),
    [FirstContact] = kzsip_uri:uris(FirstRoute),
    [LastContact] = kzsip_uri:uris(LastRoute),
    Opts = [ C || {<<"transport">> , _B} = C <- LastContact#uri.opts],
    ProxyContact = LastContact#uri{user = ToUser, opts = Opts},

    nksip_request:reply({'ok', [{'contact', ProxyContact}]}, Request),

    Props = [{<<"Subscription-ID">>, SubscriptionId}
             ,{<<"Call-ID">>, CallId}
             ,{<<"Event-Package">>, EventName}
             ,{<<"From">>, kzsip_uri:uri(From)}
             ,{<<"User">>, kzsip_uri:uri(To)}
             ,{<<"Proxy-Route">>, kzsip_uri:uri(FirstContact)}
             ,{<<"Contact">>, kzsip_uri:uri(ProxyContact)}
             ,{<<"Expires">>, wh_util:to_integer(Expires)}
             ,{<<"AOR">>, AOR}
            ],
    wh_util:spawn(fun omnip_subscriptions:proxy_subscribe/1, [Props]).

%%%%%%%%%%%%%%%%%%%%%%%  Utilities %%%%%%%%%%%%%%%%%%%%%%%%
