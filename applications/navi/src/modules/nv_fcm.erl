%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voyager Internet Ltd.
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     Ben Partridge
%%%-------------------------------------------------------------------
-module(nv_fcm).

-behaviour(gen_server).

-include("navi.hrl").

-export([start_link/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ,push/3,push/4
        ]).

-record(state, {name :: atom()}).
-type state() :: #state{}.

-spec start_link(atom(), kz_json:object()) -> kz_types:startlink_ret().
start_link(MyName, ServerConfig) ->
    gen_server:start_link({'local', MyName}, ?MODULE, [MyName, ServerConfig], []).

-spec init(any()) -> {'ok', state()}.
init([MyName, ServerConfig]) ->
    kz_util:put_callid(?MODULE),
    Name = kz_term:to_atom(kz_term:to_binary(io_lib:format("~s_srv", [MyName])), 'true'),
    %% Start the fcm server and store its name in state
    fcm:start_link(Name, kz_json:get_string_value(<<"api_key">>, ServerConfig)),
    lager:debug("starting fcm push notification server: ~p", [Name]),
    {'ok', #state{name=Name}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'push', {RegistrationId, Message, Parameters}}, #state{name=Name}=State) ->
    lager:debug("Received request to push notification into fcm"),
    Data = [{<<"message">>, Message}
           ,{<<"metadata">>, kz_json:to_proplist(props:get_value(<<"metadata">>, Parameters, []))}
           ],
    FilteredData = props:filter_undefined(Data),
    MessageParams = [{<<"data">>, FilteredData}],
    fcm:push(Name, RegistrationId, MessageParams),
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Request, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{}) ->
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.


%%%===================================================================
%%% API functions
%%%===================================================================
-spec push(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
push(Srv, RegistrationId, Message) ->
    push(Srv, RegistrationId, Message, []).
-spec push(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> any().
push(Srv, RegistrationId, Message, Parameters) ->
    lager:debug("fcm module casting push request to fcm server: ~p", [Srv]),
    gen_server:cast(Srv, {'push', {RegistrationId, Message, Parameters}}).
