%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_channels).

-behaviour(gen_server).

-export([start_link/0]).
-export([total_calls/1]).
-export([resource_consuming/1]).
-export([remove/1]).
-export([inbound_flat_rate/1]).
-export([outbound_flat_rate/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("jonny5.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-record(state, {}).

-define(SERVER, ?MODULE).

-record(channel, {uuid :: api_binary() | '$1' | '_'
                  ,destination :: api_binary() | '_'
                  ,direction :: api_binary() | '$1' | '_'
                  ,account_id :: api_binary() | '$1' | '$2' | '_'
                  ,account_billing :: api_binary() | '$7' | '_'
                  ,authorizing_id :: api_binary() | '$1' | '$3' | '_'
                  ,authorizing_type :: api_binary() | '_'
                  ,owner_id :: api_binary() | '_'
                  ,resource_id :: api_binary() | '$4' | '_'
                  ,fetch_id :: api_binary() | '$5' | '_'
                  ,reseller_id :: api_binary() | '_'
                  ,reseller_billing :: api_binary() | '_'
                  ,realm :: api_binary() | '_' | '$2'
                  ,username :: api_binary() | '_' | '$1'
                  ,other_leg :: api_binary() | '$2' | '_'
                  ,node :: atom() | '$1' | '$2' | '$3' | '_'
                  ,timestamp :: pos_integer() | '_'
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
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

total_calls(AccountId) ->
    io:format("j5_channels:total_calls(~s)~n", [AccountId]),
    0.

resource_consuming(AccountId) ->
    io:format("j5_channels:resource_consuming(~s)~n", [AccountId]),
    0.

remove(CallId) ->
    io:format("j5_channels:remove(~s)~n", [CallId]).

inbound_flat_rate(AccountId) ->
    io:format("j5_channels:inbound_flat_rate(~s)~n", [AccountId]),
    0.

outbound_flat_rate(AccountId) ->
    io:format("j5_channels:outbound_flat_rate(~s)~n", [AccountId]),
    0.

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
    wh_hooks_listener:register(),
    {'ok', #state{}}.

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
    {'reply', {'error', 'not_implemented'}, State}.

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
    {'noreply', State}.

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
handle_info(?HOOK_EVT(AccountId, <<"CHANNEL_CREATE">>, JObj), State) ->
    {'noreply', State};
handle_info(?HOOK_EVT(AccountId, <<"CHANNEL_DESTROY">>, JObj), State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

from_json(JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    #channel{uuid = wh_json:get_value(<<"Call-ID">>, JObj)
             ,destination = wh_json:get_value(<<"Request">>, JObj) %% TODO: strip realm
             ,direction = wh_json:get_value(<<"Call-Direction">>, JObj)
             ,account_id = wh_json:get_value(<<"Account-ID">>, CCVs)
             ,account_billing = wh_json:get_value(<<"Account-Billing">>, CCVs)
             ,authorizing_id = wh_json:get_value(<<"Authorizing-ID">>, CCVs)
             ,authorizing_type = wh_json:get_value(<<"Authorizing-Type">>, CCVs)
             ,owner_id = wh_json:get_value(<<"Owner-ID">>, CCVs)
             ,resource_id = wh_json:get_value(<<"Resource-ID">>, CCVs)
%%             ,fetch_id = wh_json:get_value(?GET_CCV(<<"Fetch-ID">>), JObj)
             ,reseller_id = wh_json:get_value(<<"Reseller-ID">>, CCVs)
             ,reseller_billing = wh_json:get_value(<<"Reseller-Billing">>, CCVs)
             ,realm = wh_json:get_value(<<"Realm">>, CCVs)
             ,username = wh_json:get_value(<<"Username">>, CCVs)
             ,node = wh_json:get_value(<<"Media-Server">>, JObj)
             ,timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj, 0)
%%             ,other_leg=get_other_leg(wh_json:get_value(<<"Unique-ID">>, JObj), JObj)
            }.
