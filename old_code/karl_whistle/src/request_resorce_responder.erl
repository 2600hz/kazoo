%%%-------------------------------------------------------------------
%%% File    : request_resorce_responder.erl
%%% Author  : K Anderson
%%% Description : Handles request for system resources
%%%
%%% Created : May 1 2010
%%%-------------------------------------------------------------------
-module(request_resorce_responder).
-include("../include/amqp_client.hrl").
-include("../include/common.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-vsn('1.0').

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start_link/0, start/0, stop/0]).

-record(state, {}).

-define(SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the direct exchange dispatcher
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    exit(whereis(?SERVER), shutdown).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Opens a new channel for this server, declares the
%%   exchange, declares a queue named after the localhost (with the
%%   localhost as a route_key, binds the queue to the exchange, and
%%   opens a consumer on that queue
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),

    amqp_broadcast_dispatcher:consume(),
    amqp_targeted_dispatcher:consume(),

    ?INFO("~p request_resorce_responder init complete!~n", [self()]),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State}
%% Description: Handle OTP sync messages.
%%--------------------------------------------------------------------

%% Catch all so we dont loose state
handle_call(Unhandled, _From, State) ->
    ?DEBUG("~p request_resorce_responder unknown call: ~p~n", [self(), Unhandled]),
    {reply, {error, unknown}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State}
%% Description: Handling OTP async message to publish to the direct
%%   exchange
%%--------------------------------------------------------------------

%% catch all so we dont loose state
handle_cast(Unhandled, State) ->
    ?DEBUG("~p request_resorce_responder unknown cast: ~p~n", [self(), Unhandled]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State}
%% Description: Handling all non call/cast messages.  This is how
%%   the AMQP erlang client consumer will message us, and we are
%%   traping exists so we handle those to.
%%--------------------------------------------------------------------

%% cleanly exit if we have been asked to exit
handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};

handle_info({_, #'P_basic'{reply_to = undefined}, {#xmlElement{name = request_resorce, content = ResourceReq}, _}}, State) ->
    ?WARN("~p request_resorce_responder recieved request with no reply_to: ~p~n", [self(), ResourceReq]),
    {noreply, State};

handle_info({_, Props, {#xmlElement{name = request_resorce, content = Requests}, _}}, State) ->
    process_requests(Requests, Props),
    {noreply, State};

%% catch all so we dont loose state
handle_info(_Unhandled, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> Reason
%% Description: This function is called when it is about to terminate
%%   and cleans up our open resources
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    %% cancel our consumer
    amqp_broadcast_dispatcher:cancel(),
    amqp_targeted_dispatcher:cancel(),

    ?DEBUG("~p request_resorce_responder terminated: ~p~n", [self(), Reason]),

    Reason.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% loop over all the elements in the request and process each
process_requests([], _) -> ok;
process_requests([H|T], Props) ->
    case (H#xmlElement.name) of
        voice_channels -> reply_voice_channels(H, Props);
        _ -> ?WARN("~p request_resorce_responder processed unknown request type: ~p~n", [self(), H#xmlElement.name])
    end,
    process_requests(T, Props).

%% if the request was of a voice_channel type then reply to it here
reply_voice_channels(Request, #'P_basic'{reply_to = ReplyTo}) ->
    #xmlElement{attributes = Attributes, content = _Parameters} = Request,
    ?DEBUG("~p Request for voicechannel resources! Reply to: ~p~n", [self(), ReplyTo]),

    %%% Check if we have the requested resources, for now just return as if we did

    Reply = {resource_avaliable,
                [],
                [{voice_channels,
                    [{id, get_xml_attr(id, unknown, Attributes)}, {avaliable, get_xml_attr(min_quantity, 0, Attributes)}, {cost, 10}, {hostname, net_adm:localhost()}],
                    [{contact, [], [lists:concat(["sip:", net_adm:localhost(), ":5060"])]}]
                }]
            },
    amqp_targeted_dispatcher:publish(xmerl:export_simple([Reply],xmerl_xml), ReplyTo),
    ok.

get_xml_attr(_Name, Default, []) -> Default;
get_xml_attr(Name, _Default, [#xmlAttribute{name = Name, value = Value}|_]) -> Value;
get_xml_attr(Name, Default, [_|T]) -> get_xml_attr(Name, Default, T).