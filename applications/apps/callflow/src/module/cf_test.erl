%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 24 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_test).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("../callflow.hrl").

-define(SERVER, ?MODULE).
-define(APP_NAME, <<"cf_test">>).
-define(APP_VERSION, <<"1.0">>).

-record(state, {
        cmgr_q = undefined  :: binary() | undefined
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
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

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
-spec(init/1 :: (_) -> tuple(ok, #state{})).
init([]) ->
    {ok, CQ} = start_amqp(),
    {ok, #state{cmgr_q=CQ}}.

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
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    case amqp_util:is_json(Props) of
        true ->
            JObj = mochijson2:decode(Payload),            
            spawn(fun() -> 
                          Event = {whapps_json:get_value(<<"Event-Category">>, JObj), whapps_json:get_value(<<"Event-Name">>, JObj)},
                          process_req(Event, JObj) 
                  end);
        _ ->
            format_log(info, "CF_TEST(~p): Recieved non JSON AMQP msg content type~n", [self()])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    format_log(info, "CF_TEST(~p): unhandled info ~p~n", [self(), _Info]),
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
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures the monitor exchange exists, then creates a named queue
%% and places a consumer on it
%% @end
%%--------------------------------------------------------------------
-spec(start_amqp/0 :: () -> tuple(ok, binary())).
start_amqp() ->
    amqp_util:callmgr_exchange(),
    amqp_util:targeted_exchange(),
    amqp_util:callevt_exchange(),
    CQ = amqp_util:new_callmgr_queue(<<>>),
    amqp_util:bind_q_to_callmgr(CQ, ?KEY_AUTH_REQ),
    amqp_util:bind_q_to_callmgr(CQ, ?KEY_ROUTE_REQ),
    amqp_util:bind_q_to_targeted(CQ, CQ),
    amqp_util:basic_consume(CQ),
    {ok, CQ}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Process the requests recieved from AMQP
%% @end
%%--------------------------------------------------------------------
-spec(process_req/2 :: (Event :: tuple(binary(), binary()), JObj :: json_object()) -> ok|tuple(error, atom())).
process_req({<<"directory">>, <<"auth_req">>}, JObj) ->
    RespQ = whapps_json:get_value(<<"Server-ID">>, JObj),
    Resp = [
             {<<"Msg-ID">>, whapps_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Auth-Method">>, <<"password">>}
            ,{<<"Auth-Password">>, whapps_json:get_value(<<"Auth-User">>, JObj)}
            | whistle_api:default_headers(<<>>, <<"directory">>, <<"auth_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    {ok, Payload} = whistle_api:auth_resp(Resp),
    amqp_util:targeted_publish(RespQ, Payload);

process_req(_Event, _JObj) ->
    ok.
