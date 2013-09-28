-module(ipdevice_listener).
-behaviour(gen_listener).

%% API
-export([start_link/0,start/0,init/1,handle_call/3,handle_cast/2,handle_info/2
         ,terminate/2,code_change/3,handle_req/2]).

-record(state,{}).

-define(SERVER,?MODULE).
-define(RESPONDERS, [{{?MODULE,handle_req},[{<<"*">>, <<"*">>}]}]).

-define(BINDINGS, [{'self',[]}]).
-define(QUEUE_NAME, <<"ipdevice">>).
%-define(QUEUE_OPTIONS, [{exclusive, false}]).
%-define(CONSUME_OPTIONS, [{exclusive, false}]).


start() ->
    application:start(ipdevice).

start_link() ->
    lager:debug("starting new ipdevice proc"),
    [wh_util:ensure_started(A) || A <- [ 'sasl'
					 ,'whistle_amqp'
					 ,'whistle_couch'
					 ,'ibrowse'
					 ,'lager'
				       ] ],
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
                              ,{queue_name, ?QUEUE_NAME}
                              %,{queue_options, ?QUEUE_OPTIONS}
                              %,{consume_options, ?CONSUME_OPTIONS}
                             ]
                            ,[]).

init([]) ->
    {ok,#state{}}.

handle_call(_Request,_From,State) ->
    {reply,{error,not_implemented},State}.

handle_cast(_Request,State) ->
    {noreply,State}.

handle_info(_Request,State) ->
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

handle_req(JObj,_Props) ->
    lager:debug("request ~p~n",[JObj]),
    case wh_json:get_value(<<"IP">>,JObj) of
	'undefined' ->
	    'nothing';
	IP ->
	    case couch_mgr:get_results(<<"sip_auth">>
					   ,<<"credentials/lookup_by_ip">>
					   ,[{key,IP}]) of
		{error, _R} ->
		    lager:debug("unable to get view results for sip_auth resources: ~p", [_R]);
		{ok, [SipAuth]} ->
		    get_domain_and_send_reply(SipAuth
					      ,wh_json:get_value(<<"Server-ID">>,JObj)
					      ,wh_json:get_value(<<"Msg-ID">>,JObj)
					     )
	    end
    end.

get_domain_and_send_reply(SipAuth,ServerId,MsgId) ->
    lager:debug("sipauth ~p",[SipAuth]),
    AccountId = wh_json:get_value([<<"value">>, <<"account_id">>], SipAuth),
    AuthorizingId = wh_json:get_value(<<"id">>, SipAuth),
    OwnerId = wh_json:get_value([<<"value">>, <<"owner_id">>],SipAuth),
    AcctDB = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:get_results(AcctDB,<<"account/listing_by_realm">>,[]) of
	{ok, [JObj]} ->
	    Domain = wh_json:get_value(<<"key">>, JObj),
	    SendJObj = wh_json:from_list([{'account_id',AccountId}
					  ,{'id',AuthorizingId}
					  ,{'owner_id',OwnerId}
					  ,{'domain',Domain}
					  ,{<<"Msg-ID">>,MsgId}
					 ]),
	    lager:debug("sending ~p",[SendJObj]),
	    amqp_util:targeted_publish(ServerId,wh_json:encode(SendJObj));
	_ ->
	    lager:debug("cannot fetch account ~s",[AccountId])
    end.
