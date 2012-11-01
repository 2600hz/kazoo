%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Send config commands to FS
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_config).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]). 
-export([handle_config_req/3]).
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
                ,options = [] :: wh_proplist()
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
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

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
    put(callid, Node),
    process_flag(trap_exit, true),
    lager:debug("starting new fs config listener for ~s", [Node]),
    case freeswitch:bind(Node, configuration) of
        ok ->
            lager:debug("bound to config request on ~s", [Node]),
            {ok, #state{node=Node, options=Options}};
        {error, Reason} ->
            lager:warning("failed to bind to config requests on ~s, ~p", [Node, Reason]),
            {stop, Reason};
        timeout ->
            lager:warning("failed to bind to config requests on ~s: timeout", [Node]),
            {stop, timeout}
    end.

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
handle_info({fetch, configuration, <<"configuration">>, <<"name">>, Conf, ID, _Data}, #state{node=Node}=State) ->
    lager:debug("fetch configuration request from ~s: ~s", [Node, ID]),
    spawn(?MODULE, handle_config_req, [Node, ID, Conf]),

    {noreply, State};
handle_info({_Fetch, _Section, _Something, _Key, _Value, ID, _Data}, #state{node=Node}=State) ->
    lager:debug("unhandled fetch from section ~s for ~s:~s", [_Section, _Something, _Key]),
    _ = freeswitch:fetch_reply(Node, ID, ""),

    {noreply, State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
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
terminate(_Reason, #state{node=Node}) ->
    lager:debug("fs config ~s termination: ~p", [Node, _Reason]).

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
-spec handle_config_req/3 :: (atom(), ne_binary(), ne_binary()) -> fs_sendmsg_ret().
handle_config_req(Node, ID, FsConf) ->
    put(callid, ID),
    try fsconf_key(FsConf) of
        ConfKey ->
            SysconfResp = ecallmgr_config:fetch(ConfKey, wh_json:new()),
            ConfigXml = generate_resp_xml(ConfKey, SysconfResp),

            lager:debug("sending XML to ~s: ~s", [Node, ConfigXml]),
            _ = freeswitch:fetch_reply(Node, ID, ConfigXml),
            ecallmgr_config:flush(ConfKey) % ensure we get latest and greatest
    catch
        error:function_clause ->
            lager:debug("config file ~s not supported", [FsConf]),
            {ok, Resp} = ecallmgr_fs_xml:empty_response(),
            _ = freeswitch:fetch_reply(Node, ID, Resp)
    end.

%%% FS conf keys are not necessarily the same as we store them, remap it
-spec fsconf_key/1 :: (ne_binary()) -> ne_binary().
fsconf_key(<<"acl.conf">>) -> <<"acls">>.

generate_resp_xml(<<"acls">>, Resp) ->
    case wh_json:is_empty(Resp) of
        true ->
            lager:debug("no acls configured in sysconf, returning empty response"),
            {ok, Empty} = ecallmgr_fs_xml:empty_response(),
            Empty;
        false ->
            try ecallmgr_fs_xml:acl_xml(Resp) of
                {ok, ConfigXml} -> erlang:iolist_to_binary(ConfigXml)
            catch
                error:function_clause ->
                    lager:debug("acls resp failed to convert to XML: ~p", [Resp]),
                    {ok, Empty} = ecallmgr_fs_xml:empty_response(),
                    Empty
            end
    end.
