%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Serve up registration information
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_registrar).

-behaviour(gen_listener).

%% API
-export([start_link/0, lookup/3, handle_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{?MODULE, [{<<"directory">>, <<"reg_success">>}]}]).
-define(BINDINGS, [{registration, []}]).

-include("ecallmgr.hrl").

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
    gen_listener:start_link({local, ?SERVER}, ?MODULE, [{responders, ?RESPONDERS}
                                                        ,{bindings, ?BINDINGS}
                                                       ], []).

-spec lookup/3 :: (ne_binary(), ne_binary(), [ne_binary(),...]) -> proplist() | {'error', 'timeout'}.
lookup(Realm, User, Fields) ->
    {ok, Srv} = ecallmgr_util_sup:registrar_proc(),
    gen_server:call(Srv, {lookup, Realm, User, Fields, get(callid)}).

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> no_return().
handle_req(JObj, _Props) ->
    {ok, Cache} = ecallmgr_util_sup:cache_proc(),
    User = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    lager:debug("received successful registration for ~s@~s, erasing cache", [User, Realm]),
    wh_cache:erase_local(Cache, cache_key(Realm, User)).

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
    lager:debug("Starting up"),
    {ok, ok}.

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
handle_call({lookup, Realm, User, Fields, CallId}, From, State) ->
    spawn(fun() ->
                  put(callid, CallId),
                  gen_server:reply(From, lookup_reg(Realm, User, Fields))
          end),
    {noreply, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_info({cache_registrations, Realm, User, RegFields}, State) ->
    lager:debug("storing registration information for ~s@~s", [User, Realm]),
    {ok, Cache} = ecallmgr_util_sup:cache_proc(),
    wh_cache:store_local(Cache, cache_key(Realm, User), RegFields
                         ,wh_util:to_integer(props:get_value(<<"Expires">>, RegFields, 300)) %% 5 minute default
                        ),
    {noreply, State, hibernate};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_,_) ->
    {reply, []}.

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
%% Returns a proplist with the keys corresponding to the elements of Fields
-spec lookup_reg/3 :: (ne_binary(), ne_binary(), [ne_binary(),...] | []) -> proplist() | {'error', 'timeout'}.
lookup_reg(Realm, User, Fields) ->
    lager:debug("looking up registration information for ~s@~s", [User, Realm]),
    {ok, Cache} = ecallmgr_util_sup:cache_proc(),
    FilterFun = fun({K, _}=V, Acc) ->
                        case lists:member(K, Fields) of
                            true -> [V | Acc];
                            false -> Acc
                        end
                end,
    case wh_cache:fetch_local(Cache, cache_key(Realm, User)) of
        {error, not_found} ->
            lager:debug("valid cached registration not found, querying whapps"),
            ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                          ,[{<<"Username">>, User}
                                            ,{<<"Realm">>, Realm}
                                            ,{<<"Fields">>, []}
                                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION) 
                                           ]
                                          ,fun wapi_registration:publish_query_req/1
                                          ,fun wapi_registration:query_resp_v/1),
            case ReqResp of
                {error, _R} -> 
                    lager:debug("did not receive registrar response: ~p", [_R]),
                    {error, timeout};
                {ok, RespJObj} ->
                    RegFields = wh_json:to_proplist(wh_json:get_value(<<"Fields">>, RespJObj, wh_json:new())),
                    {ok, Srv} = ecallmgr_util_sup:registrar_proc(),
                    Srv ! {cache_registrations, Realm, User, RegFields},
                    lager:debug("received registration information"),
                    lists:foldr(FilterFun, [], RegFields)
            end;
        {ok, RegFields} ->
            lager:debug("found cached registration information"),
            lists:foldr(FilterFun, [], RegFields)
    end.

cache_key(Realm, User) ->
    {?MODULE, Realm, User}.
