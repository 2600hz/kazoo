%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_alert).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-include_lib("whistle/include/wh_log.hrl").

%% API
-export([start_link/0]).
-export([format/1, format/2, format/3, format/4, format/5, format/6]).
%-export([emerg/2, alert/2, crit/2, err/2, warning/2, notice/2, info/2, debug/2]).

%% gen_listener callbacks
-export([init/1, handle_call/3, handle_cast/2
         ,handle_info/2, code_change/3, terminate/2
        ]).

-define(APP_VERSION, <<"2.0">>).
-define(APP_NAME, <<"Whistle Alert">>).

-record(alert, {level=debug
                ,req_id=?LOG_SYSTEM_ID
                ,section=sys
                ,module=?MODULE
                ,line=0
                ,pid=self()
                ,msg= <<>>
                ,args=[]}).

-type msg() :: string() | binary().
%-type options() :: ['cons' | 'perror' | 'pid' | 'odelay' | 'ndelay',...].

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
-spec start_link/0 :: () -> {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% -spec emerg/2 :: (string(), list(term())) -> 'ok'.
%% emerg(Msg, Args) ->
%%     gen_server:cast(?MODULE, #alert{level=emergency, msg=Msg, args=Args}).

%% -spec alert/2 :: (string(), list(term())) -> 'ok'.
%% alert(Msg, Args) ->
%%     gen_server:cast(?MODULE, #alert{level=alert, msg=Msg, args=Args}).

%% -spec crit/2 :: (string(), list(term())) -> 'ok'.
%% crit(Msg, Args) ->
%%     gen_server:cast(?MODULE, #alert{level=critical, msg=Msg, args=Args}).

%% -spec err/2 :: (string(), list(term())) -> 'ok'.
%% err(Msg, Args) ->
%%     gen_server:cast(?MODULE, #alert{level=error, msg=Msg, args=Args}).

%% -spec warning/2 :: (string(), list(term())) -> 'ok'.
%% warning(Msg, Args) ->
%%     gen_server:cast(?MODULE, #alert{level=warning, msg=Msg, args=Args}).

%% -spec notice/2 :: (string(), list(term())) -> 'ok'.
%% notice(Msg, Args) ->
%%     gen_server:cast(?MODULE, #alert{level=notice, msg=Msg, args=Args}).

%% -spec info/2 :: (string(), list(term())) -> 'ok'.
%% info(Msg, Args) ->
%%     gen_server:cast(?MODULE, #alert{level=info, msg=Msg, args=Args}).

%% -spec debug/2 :: (string(), list(term())) -> 'ok'.
%% debug(Msg, Args) ->
%%     gen_server:cast(?MODULE, #alert{level=debug, msg=Msg, args=Args}).

-spec format/1 :: (#alert{} | msg()) -> 'ok'.
format(Msg) ->
    format(#alert{section=sys, req_id=erlang:get(callid), module=?MODULE, line=?LINE, pid=self(), msg=Msg}).

-spec format/2 :: (atom() | msg(), msg() | list()) -> 'ok'.
format(Level, Msg) when is_atom(Level) ->
    lager:level(Msg);
    %% gen_server:cast(?MODULE, #alert{section=sys, req_id=erlang:get(callid), module=?MODULE, line=?LINE, pid=self(), level=Level, msg=Msg});
format(Msg, Args) ->
    lager:debug(Msg, Args).
    %% gen_server:cast(?MODULE, #alert{section=sys, req_id=erlang:get(callid), module=?MODULE, line=?LINE, pid=self(), msg=Msg, args=Args}).

-spec format/3 :: (atom(), list(), msg()) -> 'ok'.
format(Section, [ReqId, Module, Line, Pid], Msg) ->
    lager:debug(Msg).
    %% gen_server:cast(?MODULE, #alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, msg=Msg}).

-spec format/4 :: (atom(), list(), atom() | msg(), msg() | list()) -> 'ok'.
format(Section, [undefined|_]=Defaults, undefined, Msg) ->
    format(Section, Defaults, ?LOG_SYSTEM_ID, Msg);
format(Section, [ReqId|_]=Defaults, undefined, Msg) ->
    format(Section, Defaults, ReqId, Msg);
format(Section, [ReqId, Module, Line, Pid], Level, Msg) when is_atom(Level) ->
    lager:Level(Msg);
    %% gen_server:cast(?MODULE,#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, level=Level, msg=Msg});
format(Section, [ReqId, Module, Line, Pid], Msg, Args) ->
    lager:debug(Msg, Args).
    %% gen_server:cast(?MODULE,#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, msg=Msg, args=Args}).

-spec format/5 :: (atom(), list(), undefined | atom() | binary(), msg(), list()) -> 'ok'.
format(Section, [undefined | _]=Defaults, undefined, Msg, Args) when is_atom(Section) ->
    format(Section, Defaults, ?LOG_SYSTEM_ID, Msg, Args);
format(Section, [ReqId | _]=Defaults, undefined, Msg, Args) ->
    format(Section, Defaults, ReqId, Msg, Args);
format(Section, [ReqId, Module, Line, Pid], Level, Msg, Args) when is_atom(Level) ->
    lager:Level(Msg, Args);
    %% gen_server:cast(?MODULE,#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, level=Level, msg=Msg, args=Args});
format(Section, [_, Module, Line, Pid], ReqId, Msg, Args) ->
    lager:debug(Msg, Args).
    %% gen_server:cast(?MODULE,#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, msg=Msg, args=Args}).

-spec format/6 :: (atom(), list(), atom(), undefined | binary(), msg(), list()) -> 'ok'.
format(Section, [undefined | _]=Defaults, Level, undefined, Msg, Args) ->
    format(Section, Defaults, Level, ?LOG_SYSTEM_ID, Msg, Args);
format(Section, [ReqId | _]=Defaults, Level, undefined, Msg, Args) ->
    format(Section, Defaults, Level, ReqId, Msg, Args);
format(Section, [_, Module, Line, Pid], Level, ReqId, Msg, Args) ->
    lager:Level(Msg, Args).
    %% gen_server:cast(?MODULE,#alert{section=Section, req_id=ReqId, module=Module, line=Line, pid=Pid, level=Level, msg=Msg, args=Args}).

%%%===================================================================
%%% gen_listener callbacks
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
init(_) ->
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
handle_call(_Msg, _From, State) ->
    {noreply, State}.

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
handle_cast({publish, #alert{req_id=ReqId, level=Level, section=Section, module=Module, line=Line, msg=Msg, args=Args}}, State) ->
    try
        Data = case lists:keyfind(extra_data, 1, Args) of {extra_data, D} -> D; _Else -> [] end,
        Str = list_to_binary(io_lib:format(Msg, lists:keydelete(extra_data, 1, Args))),
        AccountId = case props:get_value(account_id, Data) of undefined -> undefined; Else -> wh_util:to_binary(Else) end,
        Details = case wh_json:is_json_object(Data) of false when is_list(Data) -> wh_json:from_list(Data); _ -> Data end,
        Notify = [{<<"Line">>, wh_util:to_binary(Line)}
                  ,{<<"Module">>, wh_util:to_binary(Module)}
                  ,{<<"Section">>, wh_util:to_binary(Section)}
                  ,{<<"Request-ID">>, wh_util:to_binary(ReqId)}
                  ,{<<"Node">>, wh_util:to_binary(net_adm:localhost())}
                  ,{<<"Level">>, wh_util:to_binary(Level)}
                  ,{<<"Message">>, Str}
                  ,{<<"Account-ID">>, AccountId}
                  ,{<<"Details">>, Details}
                  | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
                 ],
        wapi_notifications:publish_system_alert(Notify)
    catch
        A:B ->
            ST = erlang:get_stacktrace(),
            lager:debug("|000000000000|debug|sys|~p:~b (~w) logger error: ~p: ~p"
                        ,[?MODULE, ?LINE, self(), A, B]),
            [lager:debug("|000000000000|debug|sys|~p:~b (~w) st line: ~p"
                         ,[?MODULE, ?LINE, self(), STLine])
             || STLine <- ST]
    end,
    {noreply, State};
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
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate/2 :: (term(), term()) -> 'ok'.
terminate(_Reason, _) ->
    lager:debug("alert listener terminating: ~p", [_Reason]).

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
