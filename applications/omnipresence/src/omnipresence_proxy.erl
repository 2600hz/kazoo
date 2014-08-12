
-module(omnipresence_proxy).

-include_lib("nksip/include/nksip.hrl").
-include("omnipresence.hrl").


-export([start_link/0, stop/0]).

-export([sip_subscribe/2, sip_resubscribe/2, sip_dialog_update/3]). 

-export([init/1, terminate/2]). 
-export([handle_call/3, handle_cast/2, handle_info/2]).

-export([update_manager/1]).

-record(state, {subs_pid :: pid()
                ,subs_ref :: reference()
               }).

-spec start_link() -> startlink_ret().
start_link() ->
	lager:info("starting sip callback"),
     gen_server:start_link({'local', ?MODULE},?MODULE, [], []).

%% @doc Stops the SipApp.
stop() ->
    lager:info("stopping PROXY"),
    'ok'.


%%%%%%%%%%%%%%%%%%%%%%%  NkSIP CallBacks %%%%%%%%%%%%%%%%%%%%%%%%
		

sip_subscribe(Req, _Call) ->
    {ok, ReqId} = nksip_request:get_handle(Req),
    spawn(fun() -> update_manager(ReqId) end),
    'noreply'.

sip_resubscribe(Req, _Call) ->
    {ok, ReqId} = nksip_request:get_handle(Req),
%    spawn(?MODULE, update_manager, [ReqId]),
    spawn(fun() -> update_manager(ReqId) end),
    'noreply'.


sip_dialog_update({subscription_status, State, _Subs}, _Dialog, _Call) ->
    lager:debug("subscription_status ~p", [State]);
sip_dialog_update(Update, _Dialog, _Call) ->
    lager:debug("dialog update ~p", [Update]).
    
%% TODO
%% optimize the return of sending NOTIFY and sync timers with ominp_subscriptions ?


%% @doc SipApp Callback: initialization.
init([]) ->
    lager:info("callback init ccalled"),
    gen_server:cast(self(), 'find_subscriptions_srv'),
    {ok, #state{}}.


%% @doc SipApp Callback: Synchronous user call.
handle_call(_Info, _From, State) ->
	lager:info("unhandled call : ~p : ~p",[_Info,_From]),
    {noreply, State}.


%% @doc SipApp Callback: Asynchronous user cast.
handle_cast({'omnipresence_link', Props}, #state{subs_pid=Pid}=State) ->
    gen_server:call(Pid, {'proxy_subscribe', Props}),
    {noreply, State};
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
	lager:info("unhandled cast : ~p",[_Info]),
    {noreply, State}.

%% @doc SipApp Callback: External erlang message received.
handle_info({'DOWN', Ref, 'process', Pid, _R}, #state{subs_pid=Pid
                                                      ,subs_ref=Ref
                                                     }=State) ->
    gen_server:cast(self(), 'find_subscriptions_srv'),
    {'noreply', State#state{subs_pid='undefined'
                            ,subs_ref='undefined'
                           }};
handle_info(_Info, State) ->
	lager:info("unhandled Info : ~p",[_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:debug("TERMINATE APP: ~p", [_Reason]).


%%%%%%%%%%%%%%%%%%%%%%%  Internal %%%%%%%%%%%%%%%%%%%%%%%%

-spec update_manager(nksip:request()|nksip:handle()) -> 'ok'. 
update_manager(Request) ->
    {ok, [{from, From}        
          ,{to_user, ToUser}
          ,{to, To}        
          ,{subscription_handle, SubscriptionId}
          ,{call_id, CallId}
          ,{app_id, AppId}
          ,{{header, <<"record-route">>}, RecordRoutes}
          ,{expires, Expires}
          ,{aor, AOR}
         ]} = nksip_request:metas([from
                                   ,to_user
                                   ,to
                                   ,subscription_handle
                                   ,call_id
                                   ,app_id
                                   ,{header, <<"record-route">>}
                                   ,expires
                                   ,aor
                                  ], Request),
    {'ok', {EventName, _EventOptions}= _Event} = nksip_request:meta(event, Request),
    {'ok', Expires} = nksip_request:meta(expires, Request),
    

    FirstRoute = lists:nth(1, RecordRoutes),
    LastRoute = lists:last(RecordRoutes),
    [FirstContact] = nksip_parse_uri:uris(FirstRoute),    
    [LastContact] = nksip_parse_uri:uris(LastRoute),    
    Opts = [ C || {<<"transport">> , _B} = C <- LastContact#uri.opts],
    ProxyContact = LastContact#uri{user = ToUser, opts = Opts},

    
    nksip_request:reply({ok, [{contact, ProxyContact}]}, Request),
                   

    Props = [
             {<<"Subscription-ID">>, SubscriptionId}
            ,{<<"Call-ID">>, CallId}
            ,{<<"Event-Package">>, EventName}
            ,{<<"From">>, nksip_unparse:uri(From)}
            ,{<<"User">>, nksip_unparse:uri(To)}
            ,{<<"Proxy-Route">>, nksip_unparse:uri(FirstContact)}
%            ,{<<"Contact">>, nksip_unparse:uri(To)}
            ,{<<"Contact">>, nksip_unparse:uri(ProxyContact)}
            ,{<<"Expires">>, wh_util:to_integer(Expires)}
            ,{<<"AOR">>, AOR}
             ],
    gen_server:cast(AppId, {'omnipresence_link', Props}).
    
  


%%%%%%%%%%%%%%%%%%%%%%%  Utilities %%%%%%%%%%%%%%%%%%%%%%%%

