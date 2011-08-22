%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Listen for CDR events, send to DTH service
%%% @end
%%% Created : 15 Jun 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(dth_cdr_listener).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("dth.hrl").

-define(DTH_SUBMITCALLRECORD, <<"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">
<s:Body xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"><SubmitCallRecord xmlns=\"http://tempuri.org/\"><oCallRecord><AccountCode>~s</AccountCode><OriginatingNumber>~s</OriginatingNumber><DestinationNumber>~s</DestinationNumber><StartTime>~s</StartTime><Duration>~s</Duration><UniqueID>~s</UniqueID><BilledDuration>0</BilledDuration><CallCost>0</CallCost><CallTotal>0</CallTotal><Direction>0</Direction><WholesaleRate>0</WholesaleRate><WholesaleCost>0</WholesaleCost><RetailRate>0</RetailRate><CallTax>0</CallTax><IsIncluded>0</IsIncluded><BilledTier>0</BilledTier><PrintIndicator>0</PrintIndicator><EndTime>0001-01-01T00:00:00</EndTime><CallType>~s</CallType></oCallRecord></SubmitCallRecord></s:Body></s:Envelope>">>).
%% AccountCode: ab/12/345asdfhlj (17 chars) (add -IN if originated off-net)
%% OriginatingNumber: 2223334444
%% DestinationNumber: 2223334444
%% StartTime: 2010-05-24T01:55:00Z
%% Duration: 100 (in seconds)
%% UniqueID: callid
%% CallType: Interstate

-define(SERVER, ?MODULE).
-define(PREFIX, "p").
-record(state, {
          is_amqp_up = false :: boolean()
         ,amqp_timeout = 1000 :: pos_integer()
         ,max_timeout = 5000 :: 5000
         ,wsdl_model = undefined :: undefined | term()
	 ,dth_cdr_url = <<>> :: binary()
         }).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    process_flag(trap_exit, true),
    {ok, Configs} = file:consult([code:priv_dir(dth), "/startup.config"]),
    URL = props:get_value(dth_cdr_url, Configs),
    
    WSDLFile = [code:priv_dir(dth), "/dthsoap.wsdl"],
    WSDLHrlFile = [code:lib_dir(dth, include), "/dthsoap.hrl"],

    true = filelib:is_regular(WSDLFile),

    case filelib:is_regular(WSDLHrlFile) of
        true -> ok;
        false ->
            true = filelib:is_regular(WSDLFile),
            ok = detergent:write_hrl(WSDLFile, WSDLHrlFile, ?PREFIX)
    end,

    %% {ok, #state{wsdl_model=detergent:initModel(WSDLFile, ?PREFIX), dth_cdr_url=URL}, 0}.
    {ok, #state{dth_cdr_url=URL}, 0}.

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
handle_info(timeout, #state{is_amqp_up=false, amqp_timeout=Timeout, max_timeout=MaxTimeout}=State) when MaxTimeout =< Timeout ->
    Q = start_amqp(),
    {noreply, State#state{is_amqp_up=is_binary(Q), amqp_timeout=MaxTimeout}, MaxTimeout};
handle_info(timeout, #state{is_amqp_up=false, amqp_timeout=Timeout}=State) ->
    Q = start_amqp(),
    {noreply, State#state{is_amqp_up=is_binary(Q), amqp_timeout=Timeout*2}, Timeout};
handle_info(timeout, State) ->
    {noreply, State#state{is_amqp_up=true}};

handle_info({_, #amqp_msg{payload=Payload}}, #state{wsdl_model=WsdlModel,dth_cdr_url=URL}=State) ->
    ?LOG_SYS("Recv AMQP payload: ~s", [Payload]),
    spawn_link(fun() -> handle_amqp_msg(URL, Payload, WsdlModel) end),
    {noreply, State};

handle_info({amqp_host_down, _H}, State) ->
    ?LOG_SYS("Amqp host went down: ~s", [_H]),
    {noreply, State#state{is_amqp_up=false}, 0};

handle_info({'EXIT', From, Reason}, State) ->
    ?LOG_SYS("EXIT recv for ~p: ~p", [From, Reason]),
    {noreply, State};

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~w", [_Info]),
    {noreply, State}.

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
    ?LOG_SYS("Terminating: ~w", [_Reason]).

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
-spec(start_amqp/0 :: () -> binary() | tuple(error, amqp_error)).
start_amqp() ->
    try
        Q = amqp_util:new_queue(),
        amqp_util:bind_q_to_callevt(Q, <<"*">>, cdr),
        amqp_util:basic_consume(Q),
        Q
    catch
        _A:_B ->
            ?LOG_SYS("Failed to start amqp: ~s: ~w", [_A, _B]),
            ?LOG_SYS("Stacktrace: ~w", [erlang:get_stacktrace()]),
            {error, amqp_error}
    end.

handle_amqp_msg(Url, Payload, _WsdlModel) ->
    JObj = mochijson2:decode(Payload),

    true = wh_api:call_cdr_v(JObj),
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    CallDirection = wh_json:get_value(<<"Call-Direction">>, JObj),
    ?LOG_SYS(CallID, "Valid call cdr", []),
    ?LOG_SYS(CallID, "Call Direction: ~s", [CallDirection]),

    <<"outbound">> = CallDirection, %% b-leg only, though not the greatest way of determining this

    Timestamp = wh_util:to_integer(wh_json:get_value(<<"Timestamp">>, JObj, wh_util:current_tstamp())),
    BillingSec = wh_util:to_integer(wh_json:get_value(<<"Billing-Seconds">>, JObj, 0)),

    ?LOG(CallID, "Recv CDR: ~s", [Payload]),
    DateTime = now_to_datetime(Timestamp - BillingSec),

    ToE164 = wh_util:to_e164(get_to_user(JObj)),
    FromE164 = wh_util:to_e164(get_from_user(JObj)),

    AccountCode = get_account_code(JObj),

    ?LOG(CallID, "CDR from ~s to ~s with account code ~s", [FromE164, ToE164, AccountCode]),

    XML = iolist_to_binary(io_lib:format(?DTH_SUBMITCALLRECORD
					 ,[AccountCode
					   ,FromE164
					   ,ToE164
					   ,DateTime
					   ,wh_util:to_binary(BillingSec)
					   ,CallID
					   ,?DTH_CALL_TYPE_OTHER
					  ])),

    ?LOG(CallID, "XML sent: ~s", [XML]),

    Headers = [{"Content-Type", "text/xml; charset=utf-8"}
	       ,{"Content-Length", binary:referenced_byte_size(XML)}
	       ,{"SOAPAction", "http://tempuri.org/SubmitCallRecord"}
	      ],

    case ibrowse:send_req(Url, Headers, post, XML) of
	{ok, "200", _, RespXML} ->
	    ?LOG_END("XML sent to DTH successfully: ~s", [RespXML]);
	_Resp ->
	    ?LOG("Error with request: ~p", [_Resp])
    end.

now_to_datetime(Secs) ->
    {{YY,MM,DD},{Hour,Min,Sec}} = calendar:gregorian_seconds_to_datetime(Secs),
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
				   [YY, MM, DD, Hour, Min, Sec])).

-spec get_to_user/1 :: (json_object()) -> binary().
get_to_user(JObj) ->
    case wh_json:get_value(<<"To-Uri">>, JObj) of
	undefined ->
	    case wh_json:get_value(<<"Callee-ID-Number">>, JObj) of
		undefined -> <<"+00000000000">>;
		To -> To
	    end;
	ToUri ->
	    [To, _ToRealm] = binary:split(ToUri, <<"@">>),
	    To
    end.

-spec get_from_user/1 :: (json_object()) -> binary().
get_from_user(JObj) ->
    case wh_json:get_value(<<"From-Uri">>, JObj) of
	undefined ->
	    case wh_json:get_value(<<"Caller-ID-Number">>, JObj) of
		undefined -> <<"+00000000000">>;
		From -> From
	    end;
	FromUri ->
	    [From, _FromRealm] = binary:split(FromUri, <<"@">>),
	    From
    end.

-spec get_account_code/1 :: (json_object()) -> binary().
get_account_code(JObj) ->
    AccountID = case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
		    AID when erlang:byte_size(AID) < 18 -> AID;
		    AID -> binary:part(AID, {erlang:byte_size(AID), -17})
		end,
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Inception">>], JObj) of
	<<"off-net">> -> << AccountID/binary, "-IN">>;
	_ -> AccountID
    end.
