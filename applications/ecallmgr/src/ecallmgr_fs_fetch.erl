%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%% Send config commands to FS
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_fetch).
-behaviour(gen_server).

%% API
-export([start_link/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-include("ecallmgr.hrl").

-record(state, {node :: atom()
               ,section :: atom()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(atom(), kz_proplist()) -> startlink_ret().
start_link(Node, Section) ->
    gen_server:start_link(?SERVER, [Node, Section], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([atom() | ne_binary()]) -> {'ok', state()}.
init([Node, Section]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    lager:info("starting new fs fetch listener for ~s", [Node]),
    gen_server:cast(self(), 'bind'),
    {'ok', #state{node=Node, section=kz_term:to_atom(Section, 'true')}}.

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('bind', #state{node=Node, section=Section}=State) ->
    case freeswitch:bind(Node, Section) of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish bind for ~s : ~p", [Section, Reason]),
            {'stop', Reason, State}
    end;
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'fetch', JObj}, #state{node=Node}=State) ->
    _ = kz_util:spawn(fun handle_fetch_req/2, [Node, JObj]),
    {'noreply', State};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("config listener for ~s terminating: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec handle_fetch_req(atom(), kz_json:object()) -> fs_sendmsg_ret().
handle_fetch_req(Node, JObj) ->
    kz_util:put_callid(JObj),
    FetchId = kzd_fetch:fetch_uuid(JObj),
    Key = kzd_fetch:fetch_key_value(JObj),
    Tag = kzd_fetch:fetch_tag(JObj),
    Name = kzd_fetch:fetch_key_name(JObj),
    Section = kzd_fetch:fetch_section(JObj),
    Context = kzd_fetch:hunt_context(JObj),
    RKs = lists:filter(fun kz_term:is_not_empty/1, [<<"fetch">>, Section, Tag, Name, Key, Context]),
    Routing = kz_binary:join(RKs, <<".">>),
    Map = #{node => Node, section => kz_term:to_atom(Section, 'true'), fetch_id => FetchId, payload => JObj},
    case kazoo_bindings:map(Routing, Map) of
        [] -> not_found(Map);
        _ -> 'ok'
    end.

not_found(#{node := Node, fetch_id := FetchId, section := Section}) ->
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    freeswitch:fetch_reply(Node, FetchId, Section, iolist_to_binary(XmlResp)).
