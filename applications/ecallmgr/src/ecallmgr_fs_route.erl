%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Receive route(dialplan) requests from FS, request routes and respond
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_route).
-behaviour(gen_server).

-export([start_link/1, start_link/2]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-record(state, {node = 'undefined' :: atom()
               ,options = [] :: kz_proplist()
               ,switch_url :: api_binary()
               ,switch_uri :: api_binary()
               ,switch_info = 'false' :: boolean()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(atom()) -> startlink_ret().
-spec start_link(atom(), kz_proplist()) -> startlink_ret().
start_link(Node) -> start_link(Node, []).
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

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
init([Node, Options]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    lager:info("starting new fs route listener for ~s", [Node]),
    gen_server:cast(self(), 'bind_to_dialplan'),
    gen_server:cast(self(), 'bind_to_chatplan'),
    {'ok', #state{node=Node, options=Options}}.

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
handle_cast('bind_to_dialplan', #state{node=Node}=State) ->
    case freeswitch:bind(Node, 'dialplan') of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish dialplan route bindings: ~p", [Reason]),
            {'stop', Reason, State}
    end;
handle_cast('bind_to_chatplan', #state{node=Node}=State) ->
    case freeswitch:bind(Node, 'chatplan') of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish chatplan route bindings: ~p", [Reason]),
            {'stop', Reason, State}
    end;
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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

handle_info({'fetch', Section, Tag, Key, Value, FSId, FSData}
           ,#state{node=Node, switch_info='false'}=State) ->
    try ecallmgr_fs_node:sip_url(Node) of
        'undefined' ->
            lager:debug("no sip url available yet for ~s, rejecting route request", [Node]),
            {'noreply', State, 'hibernate'};
        SwitchURL ->
            [_, SwitchURIHost] = binary:split(SwitchURL, <<"@">>),
            SwitchURI = <<"sip:", SwitchURIHost/binary>>,
            handle_info({'fetch', Section, Tag, Key, Value, FSId, FSData},
                        State#state{switch_uri=SwitchURI
                                   ,switch_url=SwitchURL
                                   ,switch_info='true'
                                   })
    catch
        _E:_R ->
            lager:warning("failed to include switch_url/uri for node ~s : ~p : ~p", [Node, _E, _R]),
            {'noreply', State, 'hibernate'}
    end;
handle_info({'fetch', Section, _Tag, _Key, _Value, FSId, [CallId | FSData]}, #state{node=Node
                                                                                   ,switch_info='true'
                                                                                   ,switch_uri=SwitchURI
                                                                                   ,switch_url=SwitchURL
                                                                                   }=State) ->
    Props = props:filter_undefined([{<<"Switch-URL">>, SwitchURL}
                                   ,{<<"Switch-URI">>, SwitchURI}
                                   ,{<<"Switch-Nodename">>, kz_util:to_binary(Node)}
                                   ]) ++ FSData,
    handle_fetch(Section, FSId, CallId, Props, Node),
    {'noreply', State, 'hibernate'};
handle_info({'EXIT', _, 'noconnection'}, State) ->
    {stop, {'shutdown', 'noconnection'}, State};
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(_Other, State) ->
    lager:debug("unhandled msg: ~p", [_Other]),
    {'noreply', State}.

-spec handle_fetch(ne_binary(), ne_binary(), ne_binary(), kzd_freeswitch:data(), atom()) -> 'ok'.
handle_fetch(Section ,FSId, CallId, FSData, Node) ->
    EventName = props:get_value(<<"Event-Name">>, FSData),
    SubClass = props:get_value(<<"Event-Subclass">>, FSData),
    DefContext = props:get_value(<<"context">>, FSData, ?DEFAULT_FREESWITCH_CONTEXT),
    Context = props:get_value(<<"Hunt-Context">>, FSData, DefContext),
    Msg = {'route', Section, EventName, SubClass, Context, FSId, CallId, FSData},
    _ = gproc:send({'p', 'l', ?FS_ROUTE_MSG(Node, Section, Context)}, Msg),
    _ = gproc:send({'p', 'l', ?FS_ROUTE_MSG(Node, Section, <<"*">>)}, Msg),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
                                                % terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("route listener for ~s terminating: ~p", [Node, _Reason]).

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
