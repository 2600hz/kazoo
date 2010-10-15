%%%-------------------------------------------------------------------
%%% File    : job_dispatcher.erl
%%% Author  : K Anderson
%%% Description : Dispatches job requests
%%%
%%% Created : May 1 2010
%%%-------------------------------------------------------------------
-module(job_dispatcher).
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

    ?INFO("~p job_dispatcher init complete!~n", [self()]),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State}
%% Description: Handle OTP sync messages.
%%--------------------------------------------------------------------

%% Catch all so we dont loose state
handle_call(Unhandled, _From, State) ->
    ?DEBUG("~p job_dispatcher unknown call: ~p~n", [self(), Unhandled]),
    {reply, {error, unknown}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State}
%% Description: Handling OTP async message to publish to the direct
%%   exchange
%%--------------------------------------------------------------------

%% catch all so we dont loose state
handle_cast(Unhandled, State) ->
    ?DEBUG("~p job_dispatcher unknown cast: ~p~n", [self(), Unhandled]),
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

handle_info({_, Props, {#xmlElement{name = preform_action, content = Jobs}, _}}, State) ->
    process_jobs(Jobs, Props),
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

    ?DEBUG("~p job_dispatcher terminated: ~p~n", [self(), Reason]),

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
process_jobs([], _) -> ok;
process_jobs([H|T], Props) ->
    case (H#xmlElement.name) of
        job -> preform_tasks(H#xmlElement.content, H#xmlElement.attributes, Props);
        _ -> ok
    end,
    process_jobs(T, Props).

preform_tasks([], _, _) -> ok;
preform_tasks([H|T], Attributes, Props) ->
    case (H#xmlElement.name) of
        fs_api ->
            Command = lists:flatten(["api ", get_xml_attr(application, "", H#xmlElement.attributes), " ", get_xml_attr(data, "", H#xmlElement.attributes)]),
            ?DEBUG("~p job_dispatcher message fs: ~p~n", [self(), Command]),
            try (fs_msg_sender ! {send_command, Command}) of
                _ -> ok
            catch
                _:Error -> ?WARN("~p job_dispatcher failed to message fs_msg_sender: ~p~n", [self(), Error])
            end;
        _ -> ?WARN("~p job_dispatcher unknown job type: ~p~n", [self(), H#xmlElement.name])
    end,
    preform_tasks(T, Attributes, Props).


get_xml_attr(_Name, Default, []) -> Default;
get_xml_attr(Name, _Default, [#xmlAttribute{name = Name, value = Value}|_]) -> Value;
get_xml_attr(Name, Default, [_|T]) -> get_xml_attr(Name, Default, T).
