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

-define(DTH_URL, <<"http://173.203.64.57/dthsoapapi/dthsoap.asmx">>).


-define(SERVER, ?MODULE).
-record(state, {
          is_amqp_up = false :: boolean()
         ,amqp_timeout = 1000 :: pos_integer()
         ,max_timeout = 5000 :: 5000
         ,wsdl_model = undefined :: undefined | term()
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
    WSDLFile = [code:priv_dir(dth), "/dthsoap.wsdl"],
    WSDLHrlFile = [code:lib_dir(dth, include), "/dthsoap.hrl"],

    true = filelib:is_regular(WSDLFile),

    case filelib:is_regular(WSDLHrlFile) of
        true -> ok;
        false ->
            true = filelib:is_regular(WSDLFile),
            ok = detergent:write_hrl(WSDLFile, WSDLHrlFile)
    end,

    {ok, #state{wsdl_model=detergent:initModel(WSDLFile)}, 0}.

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

handle_info({_, #amqp_msg{payload=Payload}}, #state{wsdl_model=WsdlModel}=State) ->
    spawn(fun() -> handle_amqp_msg(Payload, WsdlModel) end),
    {noreply, State};

handle_info({amqp_host_down, _H}, State) ->
    ?LOG_SYS("Amqp host went down: ~s", [_H]),
    {noreply, State#state{is_amqp_up=false}, 0};

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

handle_amqp_msg(Payload, WsdlModel) ->
    JObj = mochijson2:decode(Payload),

    true = whistle_api:call_cdr_v(JObj),

    MicroTimestamp = whistle_util:to_integer(wh_json:get_value(<<"Timestamp">>, JObj, whistle_util:current_tstamp() * 1000000)),
    BillingSec = whistle_util:to_integer(wh_json:get_value(<<"Billing-Seconds">>, JObj, 0)),
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),

    ?LOG(CallID, "Recv CDR: ~s", [Payload]),
    DateTime = now_to_datetime( (MicroTimestamp div 1000000) - BillingSec, 1970),
    ?LOG(CallID, "DateTime: ~w ~s", [DateTime, DateTime]),

    Call = #'p:CallRecord'{
      'CustomerID' = whistle_util:to_list(wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj, <<"0000000000">>))
      ,'BatchID' = whistle_util:to_list(wh_json:get_value(<<"Msg-ID">>, JObj, now_to_date(whistle_util:current_tstamp())))
      ,'OriginatingNumber' = whistle_util:to_list(wh_json:get_value(<<"Caller-ID-Number">>, JObj))
      ,'DestinationNumber' = whistle_util:to_list(wh_json:get_value(<<"Callee-ID-Number">>, JObj))
      ,'StartTime' = whistle_util:to_list(DateTime)
      ,'Duration' = whistle_util:to_list(BillingSec)
      ,'UniqueID' = whistle_util:to_list(CallID)
      ,'CallType' = ?DTH_CALL_TYPE_OTHER
      %% READ ONLY Properties but erlsom chokes if not defined to something
      ,'BilledDuration' = "0"
      ,'CallCost' = "1"
      ,'CallTotal' = "2"
      ,'Direction' = "3"
      ,'WholesaleRate' = "4"
      ,'WholesaleCost' = "5"
      ,'RetailRate' = "6"
      ,'CallTax' = "7"
      ,'IsIncluded' = 0
      ,'BilledTier' = 0
      ,'PrintIndicator' = 0
      ,'EndTime' = "0001-01-01T00:00:00"
     },
    ?LOG(CallID, "CallRecord: ~p", [Call]),
%%     Submit = #'p:SubmitCallRecord'{'oCallRecord'=Call},
    Resp = detergent:call(WsdlModel, "SubmitCallRecord", [Call]),
    ?LOG(CallID, "Recv from DTH: ~w", [Resp]),
    io:format("Resp from call: ~p~n", [Resp]).

%% {"Other-Leg-Call-ID":"ynmhvfajysowxni@thinky64.2600hz.com","User-Agent":"PolycomSoundPointIP-SPIP_550-UA/3.3.1.0933","Callee-ID-Number":"1000","Callee-ID-Name":"Hello World","Caller-ID-Number":"1000","Caller-ID-Name":"Hello World","Local-SDP":"v=0\r\no=twinkle 138259549 1503719914 IN IP4 76.217.208.155\r\ns=-\r\nc=IN IP4 76.217.208.155\r\nt=0 0\r\nm=audio 8000 RTP/AVP 98 97 8 0 3 101\r\na=rtpmap:98 speex/16000\r\na=rtpmap:97 speex/8000\r\na=rtpmap:8 PCMA/8000\r\na=rtpmap:0 PCMU/8000\r\na=rtpmap:3 GSM/8000\r\na=rtpmap:101 telephone-event/8000\r\na=fmtp:101 0-15\r\na=ptime:20\r\n","Remote-SDP":"v=0\r\no=- 1308348029 1308348029 IN IP4 76.217.208.155\r\ns=Polycom IP Phone\r\nc=IN IP4 76.217.208.155\r\nt=0 0\r\nm=audio 2228 RTP/AVP 8 127\r\na=rtpmap:8 PCMA/8000\r\na=rtpmap:127 telephone-event/8000\r\na=oldmediaip:192.168.1.79\r\na=oldmediaip:192.168.1.79\r\n","Custom-Channel-Vars":{"Authorizing-ID":"692de45541050fab4315036c712aa208","Inception":"on-net","Account-ID":"04152ed2b428922e99ac66f3a71b0215","Realm":"kanderson.pbx.dev.2600hz.com","Username":"kanderson_1","Access-Group":"ignore","Tenant-ID":"ignore"},"Digits-Dialed":"none","Ringing-Seconds":"0","Billing-Seconds":"8","Duration-Seconds":"11","From-Uri":"1000@184.106.189.224","To-Uri":"kanderson_2@76.217.208.155:5060","Call-Direction":"outbound","Timestamp":"1308348044434992","Call-ID":"0ba7d53f-9d74-4b47-af1f-8c0eb83e32f6","Handling-Server-Name":"fs002-dev-ord.2600hz.com","Hangup-Cause":"NORMAL_CLEARING","App-Version":"0.7.2","App-Name":"ecallmgr","Event-Name":"cdr","Event-Category":"call_detail","Server-ID":""}

now_to_date(Secs) ->
    now_to_date(Secs, 0).
now_to_date(Secs, ExtraYears) ->
    {{YY,MM,DD},_} = calendar:gregorian_seconds_to_datetime(Secs),
      iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w",
                    [YY+ExtraYears, MM, DD])).

now_to_datetime(Secs) ->
    now_to_datetime(Secs, 0).
now_to_datetime(Secs, ExtraYears) ->
    {{YY,MM,DD},{Hour,Min,Sec}} = calendar:gregorian_seconds_to_datetime(Secs),
      iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                    [YY+ExtraYears, MM, DD, Hour, Min, Sec])).
