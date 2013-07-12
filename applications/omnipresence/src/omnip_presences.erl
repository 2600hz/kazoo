%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnip_presences).

-behaviour(gen_server).

-export([start_link/0
         ,table_id/0
         ,table_config/0

         ,handle_presence_update_only/2
         ,handle_presence_update/2

         ,find_presence_state/1, find_presence_state/2
         ,update_presence_state/3

         %% Accessors
         ,current_state/1
         ,user/1
         ,timestamp/1
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("omnipresence.hrl").
-include_lib("kazoo_etsmgr/include/kazoo_etsmgr.hrl").

-record(state, {}).

-record(omnip_presence_state, {
          user :: ne_binary() | '_' | '$1' % who was updated
          ,state :: api_binary() | '_' % to what state
          ,timestamp = wh_util:current_tstamp() :: wh_now() | '_' % and when
         }).
-type presence_state() :: #omnip_presence_state{}.

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
-spec start_link() -> startlink_ret().
start_link() -> gen_server:start_link(?MODULE, [], []).

-spec handle_presence_update(wh_json:object(), wh_proplist()) -> any().
handle_presence_update(JObj, Props) ->
    'true' = wapi_notifications:presence_update_v(JObj),

    U = wh_json:get_value(<<"Presence-ID">>, JObj),
    S = wh_json:get_value(<<"State">>, JObj),

    %% Send to other omnipresence whapps
    lager:debug("sending others update about ~s: ~s", [U, S]),
    _E = try wapi_omnipresence:publish_presence_update(
               wh_json:set_values([{<<"To">>, U}
                                   ,{<<"From">>, U}
                                   ,{<<"State">>, S}
                                   ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ], wh_json:new()))
         of
             OK -> OK
         catch
             _:R -> R
         end,
              
    lager:debug("maybe sending update for ~s(~s): ~p", [U, S, _E]),
    omnip_subscriptions:maybe_send_update(U, U, JObj, props:get_value('omnip_presences', Props), S).

-spec handle_presence_update_only(wh_json:object(), wh_proplist()) -> any().
handle_presence_update_only(JObj, Props) ->
    'true' = wapi_omnipresence:presence_update_v(JObj),
    update_presence_state(props:get_value(?MODULE, Props)
                          ,wh_json:get_value(<<"Presence-ID">>, JObj)
                          ,wh_json:get_value(<<"State">>, JObj)
                         ).

-spec update_to_record(wh_json:object()) -> presence_state().
update_to_record(JObj) ->
    U = case wh_json:find_first_defined([<<"Presence-ID">>, <<"To">>], JObj) of
            <<"sip:", User/binary>> -> User;
            User when is_binary(User) -> User
        end,

    S = wh_json:get_value(<<"State">>, JObj),
    #omnip_presence_state{user=U
                          ,state=S
                         }.

table_id() -> 'omnipresence_presence_states'.
table_config() ->
    ['protected', 'named_table', 'set'
     ,{'keypos', #omnip_presence_state.user}
    ].

-spec update_presence_state(pid() | atom(), api_binary(), ne_binary()) -> 'ok'.
update_presence_state(_Srv, 'undefined', _Update) -> 'ok';
update_presence_state(Srv, User, Update) ->
    PS = case ?MODULE:find_presence_state(User) of
             {'error', 'not_found'} ->
                 #omnip_presence_state{user=User
                                       ,state=Update
                                      };
             {'ok', OS} -> OS#omnip_presence_state{state=Update}
         end,
    gen_listener:cast(Srv, {'update_presence_state', PS}).

-spec current_state(presence_state()) -> api_binary().
current_state(#omnip_presence_state{state=State}) -> State.

-spec user(presence_state()) -> api_binary().
user(#omnip_presence_state{user=User}) -> User.

-spec timestamp(presence_state()) -> pos_integer().
timestamp(#omnip_presence_state{timestamp=T}) -> T.

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
    put('callid', ?MODULE),
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
handle_cast({'update_presence_state', #omnip_presence_state{user=U
                                                            ,state=S
                                                            ,timestamp=T
                                                           }=PS}, State) ->
    case find_presence_state(U) of
        {'error', 'not_found'} ->
            lager:debug("creating state for ~s with ~s", [U, S]),
            ets:insert(table_id(), PS),
            {'noreply', State};
        {'ok', _} ->
            lager:debug("updating state for ~s to ~s", [U, S]),
            handle_cast({'update_presence_state', U, [{#omnip_presence_state.state, S}
                                                      ,{#omnip_presence_state.timestamp, T}
                                                     ]}, State)
    end;
handle_cast({'update_presence_state', U, Updates}, State) ->
    ets:update_element(table_id(), U, Updates),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

-spec find_presence_state(presence_state() | ne_binary()) ->
                                 {'ok', presence_state()} |
                                 {'error', 'not_found'}.
find_presence_state(#omnip_presence_state{user=U}) ->
    find_presence_state(U);
find_presence_state(U) ->
    case ets:select(table_id(), [{#omnip_presence_state{user='$1', _='_'}
                                  ,[{'=:=', '$1', {'const', U}}]
                                  ,['$_']
                                 }])
    of
        [] -> {'error', 'not_found'};
        [#omnip_presence_state{}=PS|_] -> {'ok', PS}
    end.

find_presence_state(Realm, Username) ->
    find_presence_state(<<Username/binary, "@", Realm/binary>>).


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
handle_info(?TABLE_READY(_Tbl), State) ->
    lager:debug("recv table_ready for ~p", [_Tbl]),
    {'noreply', State, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
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
