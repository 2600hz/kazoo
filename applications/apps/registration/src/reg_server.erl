%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Store registrations, do user lookups for contact strings
%%% @end
%%% Created : 13 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(reg_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/amqp_client/include/amqp_client.hrl").

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).
-define(REG_DB, "registrations").

-type proplist() :: list(tuple(binary(), binary())) | [].

-record(state, {
	  amqp_host = "localhost" :: string()
	  ,my_q = undefined :: undefined | binary()
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
init([]) ->
    H = net_adm:localhost(),
    Q = start_amqp(H),
    {ok, #state{amqp_host=H, my_q = Q}}.

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
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, #state{}=State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};
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
-spec(terminate/2 :: (_, #state{}) -> no_return()).
terminate(_Reason, #state{amqp_host=Host, my_q=Q}) ->
    stop_amqp(Host, Q),
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
-spec(start_amqp/1 :: (Host :: string()) -> binary()).
start_amqp(Host) ->
    Q = amqp_util:new_queue(Host, <<>>),
    amqp_util:bind_q_to_broadcast(Host, Q),
    amqp_util:basic_consume(Host, Q),
    Q.

-spec(stop_amqp/2 :: (Host :: string(), Q :: binary()) -> no_return()).
stop_amqp(Host, Q) ->
    amqp_util:unbind_q_from_broadcast(Host, Q),
    amqp_util:delete_queue(Host, Q).

-spec(handle_req/3 :: (ContentType :: binary(), Payload :: binary(), State :: #state{}) -> no_return()).
handle_req(<<"application/json">>, Payload, State) ->
    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
    format_log(info, "REG_SRV(~p): Recv JSON~nPayload: ~p~n", [self(), Prop]),
    process_req(get_msg_type(Prop), Prop, State).

-spec(get_msg_type/1 :: (Prop :: proplist()) -> tuple(binary(), binary())).
get_msg_type(Prop) ->
    { props:get_value(<<"Event-Category">>, Prop), props:get_value(<<"Event-Name">>, Prop) }.

-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), Prop :: proplist(), State :: #state{}) -> no_return()).
process_req({<<"directory">>, <<"auth_req">>}, Prop, State) ->
    format_log(info, "REG_SRV: Lookup auth creds here for~n~p~n", [Prop]),
    ok;
process_req({<<"directory">>, <<"reg_success">>}, Prop, State) ->
    format_log(info, "REG_SRV: Reg success~n~p~n", [Prop]),
    Domain = props:get_value(<<"Realm">>, Prop),
    DomainDoc = case couch_mgr:open_doc(?REG_DB, Domain) of
		    {error, not_found} -> [{<<"_id">>, Domain}, {<<"registrations">>, {[]}}];
		    Doc when is_list(Doc) -> Doc
		end,
    Regs = props:get_value(<<"registrations">>, DomainDoc, []),
    format_log(info, "REG_SRV(~p): Domain: ~p~nRegs: ~p~n", [self(), Domain, Regs]),
    DomainDoc1 = [ {<<"registrations">>, [{struct, Prop} | Regs]} | lists:keydelete(<<"registrations">>, 1, DomainDoc)],
    {ok, _} = couch_mgr:save_doc(?REG_DB, DomainDoc1);
process_req(_,_,_) ->
    not_handled.


%% {save_doc,"registrations",
%%  {
%%    [{<<"registrations">>, [
%% 			    {[{<<"User-Agent">>,<<"Twinkle/1.4.2">>},
%% 			      {<<"Status">>,<<"Registered(UDP)">>},
%% 			      {<<"Realm">>,<<"192.168.0.104">>},
%% 			      {<<"Username">>,<<"2600pbx">>},
%% 			      {<<"Network-Port">>,<<"5065">>},
%% 			      {<<"Network-IP">>,<<"192.168.0.104">>},
%% 			      {<<"To-Host">>,<<"192.168.0.104">>},
%% 			      {<<"To-User">>,<<"2600pbx">>},
%% 			      {<<"Expires">>,<<"3600">>},
%% 			      {<<"RPid">>,<<"unknown">>},
%% 			      {<<"Contact">>,
%% 			       <<"\"4158867971\" <sip:2600pbx@192.168.0.104:5065;transport=udp>">>},
%% 			      {<<"From-Host">>,
%% 			       <<"trunks.2600hz.com">>},
%% 			      {<<"From-User">>,<<"2600pbx">>},
%% 			      {<<"Event-Timestamp">>,63462243722.0},
%% 			      {<<"App-Version">>,<<"0.5.6">>},
%% 			      {<<"App-Name">>,
%% 			       <<"ecallmgr_fs_node">>},
%% 			      {<<"Event-Name">>,<<"reg_success">>},
%% 			      {<<"Event-Category">>,<<"directory">>},
%% 			      {<<"Server-ID">>,<<>>}]}
%% 			   ]},
%%     {<<"_id">>,<<"192.168.0.104">>}]
%%  }
%% }
