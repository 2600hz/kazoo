%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc Send config commands to FS
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Section) ->
    gen_server:start_link(?SERVER, [Node, Section], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:ne_binary()]) -> {'ok', state()}.
init([Node, Section]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(Node),
    lager:info("starting new fs fetch listener for ~s", [Node]),
    gen_server:cast(self(), 'bind'),
    {'ok', #state{node=Node, section=kz_term:to_atom(Section, 'true')}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('bind', #state{node=Node, section=Section}=State) ->
    case freeswitch:bind(Node, Section) of
        'ok' -> {'noreply', State};
        {'error', Reason} ->
            lager:critical("unable to establish bind for ~s : ~p", [Section, Reason]),
            {'stop', Reason, State}
    end;
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:info("config listener for ~s terminating: ~p", [Node, _Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_fetch_req(atom(), kz_json:object()) -> fs_sendmsg_ret().
handle_fetch_req(Node, JObj) ->
    kz_util:put_callid(JObj),
    %% lager:debug_unsafe("FETCH : ~s", [kz_json:encode(JObj, ['pretty'])]),
    FetchId = kzd_fetch:fetch_uuid(JObj),
    CoreUUID = kzd_fetch:core_uuid(JObj),
    Key = kzd_fetch:fetch_key_value(JObj),
    Section = kzd_fetch:fetch_section(JObj),
    Version = kzd_fetch:fetch_version(JObj),
    Event = kz_term:to_lower_binary(kz_api:event_name(JObj)),

    RKs = lists:filter(fun kz_term:is_not_empty/1, [<<"fetch">>, Section, Version, Event, Key]),

    Routing = kz_binary:join(RKs, <<".">>),
    lager:debug("requesting binding for ~s", [Routing]),
    Map = #{node => Node
           ,section => kz_term:to_atom(Section, 'true')
           ,fetch_id => FetchId
           ,payload => JObj
           ,version => kz_term:to_atom(Version, 'true')
           ,core_uuid => kz_term:to_atom(CoreUUID, 'true')
           ,routing => Routing
           ,call_id => kzd_fetch:call_id(JObj)
           },
    case kazoo_bindings:map(Routing, Map) of
        [] -> not_found(Map);
        _ -> 'ok'
    end.

not_found(#{node := Node, fetch_id := FetchId, section := Section}) ->
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    freeswitch:fetch_reply(Node, FetchId, Section, iolist_to_binary(XmlResp)).
