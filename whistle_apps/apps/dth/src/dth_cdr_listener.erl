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

    Timestamp = whistle_util:to_integer(wh_json:get_value(<<"Timestamp">>, JObj, erlang:now())),
    BillingSec = whistle_util:to_integer(wh_json:get_value(<<"Billing-Seconds">>, JObj, 0)),
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),

    ?LOG(CallID, "Recv CDR: ~s", [Payload]),
    DateTime = now_to_datetime(Timestamp - BillingSec),
    ?LOG(CallID, "DateTime: ~w ~s", [DateTime, DateTime]),

    Call = #'p:CallRecord'{
      'CustomerID' = whistle_util:to_list(wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj, <<"0000000000">>))
      ,'BatchID' = whistle_util:to_list(wh_json:get_value(<<"Msg-ID">>, JObj))
      ,'OriginatingNumber' = whistle_util:to_list(wh_json:get_value(<<"From-Uri">>, JObj))
      ,'DestinationNumber' = whistle_util:to_list(wh_json:get_value(<<"To-Uri">>, JObj))
      ,'StartTime' = DateTime
      ,'Duration' = whistle_util:to_list(BillingSec)
      ,'UniqueID' = whistle_util:to_list(CallID)
      ,'CallType' = ?DTH_CALL_TYPE_OTHER
     },
    ?LOG(CallID, "CallRecord: ~w", [Call]),
    Resp = detergent:call(WsdlModel, "SubmitCallRecord", [Call]),
    ?LOG(CallID, "Recv from DTH: ~w", [Resp]),
    io:format("Resp from call: ~p~n", [Resp]).

now_to_datetime(Secs) ->
    {{YY,MM,DD},{Hour,Min,Sec}} = calendar:gregorian_seconds_to_datetime(Secs),
      io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                    [YY, MM, DD, Hour, Min, Sec]).

%%           <s:element minOccurs="0" maxOccurs="1" name="CustomerID" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="BatchID" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="OriginatingNumber" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="DestinationNumber" type="s:string" />
%%           <s:element minOccurs="1" maxOccurs="1" name="StartTime" type="s:dateTime" />
%%           <s:element minOccurs="1" maxOccurs="1" name="Duration" type="s:decimal" />
%%           <s:element minOccurs="0" maxOccurs="1" name="UniqueID" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="AccountCode" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="Disposition" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="dcontext" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="Channel" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="dstChannel" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="OriginatingIPAddress" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="CallID" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="DestinationName" type="s:string" />

%%           <s:element minOccurs="1" maxOccurs="1" name="BilledDuration" type="s:decimal" />
%%           <s:element minOccurs="1" maxOccurs="1" name="CallCost" type="s:decimal" />
%%           <s:element minOccurs="1" maxOccurs="1" name="CallTotal" type="s:decimal" />
%%           <s:element minOccurs="1" maxOccurs="1" name="Direction" type="s1:char" />
%%           <s:element minOccurs="0" maxOccurs="1" name="BilledPrefix" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="RateID" type="s:string" />
%%           <s:element minOccurs="1" maxOccurs="1" name="WholesaleRate" type="s:decimal" />
%%           <s:element minOccurs="1" maxOccurs="1" name="WholesaleCost" type="s:decimal" />
%%           <s:element minOccurs="1" maxOccurs="1" name="RetailRate" type="s:decimal" />
%%           <s:element minOccurs="1" maxOccurs="1" name="CallTax" type="s:decimal" />
%%           <s:element minOccurs="1" maxOccurs="1" name="IsIncluded" type="s:int" />
%%           <s:element minOccurs="1" maxOccurs="1" name="BilledTier" type="s:int" />
%%           <s:element minOccurs="0" maxOccurs="1" name="ChargeID" type="s:string" />
%%           <s:element minOccurs="0" maxOccurs="1" name="StatementID" type="s:string" />
%%           <s:element minOccurs="1" maxOccurs="1" name="PrintIndicator" type="s:int" />
%%           <s:element minOccurs="0" maxOccurs="1" name="MasterNumber" type="s:string" />
%%           <s:element minOccurs="1" maxOccurs="1" name="EndTime" type="s:dateTime" />
%%           <s:element minOccurs="1" maxOccurs="1" name="CallType" type="tns:CallTypeValues" />

%%           <s:enumeration value="Local" />
%%           <s:enumeration value="TieredOrigination" />
%%           <s:enumeration value="Interstate" />
%%           <s:enumeration value="Intrastate" />
%%           <s:enumeration value="Other" />
