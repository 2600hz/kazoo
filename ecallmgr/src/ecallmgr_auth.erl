%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Handles authentication requests on the FS instance by a device
%%% @end
%%% Created : 17 Aug 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_auth).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup_user/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(proplists, [get_value/2, get_value/3]).

-define(SERVER, ?MODULE). 
-define(EMPTYRESPONSE, "<document type=\"freeswitch/xml\"></document>").
-define(REGISTERRESPONSE,
"<document type=\"freeswitch/xml\">
	<section name=\"directory\">
		<domain name=\"~s\">
			<user id=\"~s\">
				<params>
					<param name=\"a1-hash\" value=\"~s\"/>
				</params>
				<variables>
					<variable name=\"user_context\" value=\"default\"/>
				</variables>
			</user>
		</domain>
	</section>
</document>").

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

%% see lookup_user/2 after gen_server callbacks

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
    process_flag(trap_exit, true),
    Node = list_to_atom(lists:concat(["freeswitch@", net_adm:localhost()])),
    Opts = [],
    case net_adm:ping(Node) of
	pong ->
	    {ok, Pid} = freeswitch:start_fetch_handler(Node, directory, ?MODULE, lookup_user, Opts),
	    link(Pid);
	_ ->
	    io:format("Unable to find ~p to talk to freeSWITCH~n", [Node])
    end,
    {ok, #state{}}.

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

lookup_user(Node, State) ->
    receive
	{fetch, directory, "domain", "name", _Value, ID, [undefined | Data]} ->
	    io:format("fetch directory: Id: ~p Data: ~p~n", [ID, Data]),
	    User = get_value("user", Data),
	    Domain = get_value("domain", Data),
	    Hash = a1hash(User, Domain, "james"),
	    Resp = lists:flatten(io_lib:format(?REGISTERRESPONSE, [Domain, User, Hash])),
	    io:format("Sending resp: ~p~n", [Resp]),
	    freeswitch:fetch_reply(Node, ID, Resp),
	    ?MODULE:lookup_user(Node, State);
	{fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]} ->
	    io:format("fetch dir unknown: Se: ~p So: ~p, K: ~p V: ~p ID: ~p, D: ~p~n", [_Section, _Something, _Key, _Value, ID, _Data]),
	    freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
	    ?MODULE:lookup_user(Node, State);
	{nodedown, Node} ->
	    io:format("Node we were serving XML search requests to exited", []),
	    ok;
	Other ->
	    io:format("got other response: ~p", [Other]),
	    ?MODULE:lookup_user(Node, State)
    end.

a1hash(User, Realm, Password) ->
    to_hex(erlang:md5(User++":"++Realm++":"++Password)).

to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex(L) when is_list(L) ->
    lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- L]).
