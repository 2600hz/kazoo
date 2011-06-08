%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Maintenance-type requests, like MWI updates, received and processed here
%%% @end
%%% Created :  2 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_maintenance).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("ecallmgr.hrl").

-record(state, {}).

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
init(_) ->
    {ok, #state{}, 0}.

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
    {reply, ok, State}.

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
handle_info(timeout, State) ->
    true = is_binary(start_amqp()),
    {noreply, State};

handle_info({#'basic.deliver'{}, #amqp_msg{props=#'P_basic'{content_type = <<"application/json">> }
					   ,payload = Payload}}, State) ->
    spawn(fun() -> handle_maintenance_req(Payload) end),
    {noreply, State};

handle_info(_Info, State) ->
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
    ok.

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
start_amqp() ->
    try
	true = is_binary(Q = amqp_util:new_queue()),
	_ = amqp_util:bind_q_to_callmgr(Q, ?KEY_MWI_UPDATE),
	_ = amqp_util:basic_consume(Q),
	Q
    catch
	_:_ -> {error, amqp_error}
    end.

handle_maintenance_req(Payload) ->
    JObj = mochijson2:decode(Payload),
    handle_maintenance_req(get_type(JObj), JObj).

get_type(JObj) ->
    {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj)}.

handle_maintenance_req({<<"maintenance">>, <<"mwi">>}, JObj) ->
    true = whistle_api:mwi_update_v(JObj),

    %% ecallmgr_registrar:lookup(wh_json:get_value(<<"Message-Account-Realm">>, JObj)
    %% 			      ,wh_json:get_value(<<"Message-Account-User">>, JObj)
    %% 			      ,[<<"Contact">>])

    Headers = [{"MWI-Messages-Waiting", wh_json:get_value(<<"Messages-Waiting">>, JObj)}
	       ,{"MWI-Message-Account", <<(wh_json:get_value(<<"Message-Account-User">>, JObj))/binary,
					  "@", (wh_json:get_value(<<"Message-Account-Realm">>, JObj))/binary>>}
	       ,{"MWI-Voice-Message", whistle_util:to_binary(
					io_lib:format("~B/~B (~B/~B)", [
									wh_json:get_value(<<"Message-New">>, JObj, 0)
									,wh_json:get_value(<<"Message-Saved">>, JObj, 0)
									,wh_json:get_value(<<"Message-Urgent">>, JObj, 0)
									,wh_json:get_value(<<"Message-Urgent-Saved">>, JObj, 0)
								       ])
				       )}
	       ,{"Sofia-Profile", <<"sipinterface_1">>}
	      ],
    {ok, Node} = ecallmgr_fs_handler:request_node(<<"audio">>),
    Resp = freeswitch:sendevent(Node, 'MESSAGE_WAITING', [ {whistle_util:to_list(K), whistle_util:to_list(V)} || {K, V} <- Headers ]),
    ?LOG("Sending MWI Update to ~p(~p)", [Node, Resp]).
