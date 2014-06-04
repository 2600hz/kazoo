%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(camper_channel_tracker).

-export([start_link/0]).
-export([init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).
-export([add_request/4
         ,available_device/2]).

-include("camper.hrl").

-record('state', {'requests' :: dict()
                  ,'requestor_queues' :: dict()
                  ,'sipnames' :: dict()
                  ,'acctdb' :: ne_binary()
                 }
       ).

-spec get_requests(#'state'{}) -> dict().
get_requests(#'state'{'requests' = Val}) ->
    Val.

-spec get_requestor_queues(#'state'{}) -> dict().
get_requestor_queues(#'state'{'requestor_queues' = Val}) ->
    Val.

-spec get_sipnames(#'state'{}) -> dict().
get_sipnames(#'state'{'sipnames' = Val}) ->
    Val.

-spec add_request(ne_binary(), ne_binary(), ne_binary(), ne_binaries()) -> 'ok'.
add_request(AcctDb, AuthorizingId, Exten, Targets) ->
    gen_server:cast(?MODULE, {'add_request', AcctDb, AuthorizingId, Exten, Targets}).

-spec available_device(ne_binary(), ne_binary()) -> 'ok'.
available_device(AcctId, SIPName) ->
    gen_server:cast(?MODULE, {'available_device', AcctId, SIPName}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

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
    lager:info("started campering..."),
    {'ok', dict:new()}.

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
handle_cast({'add_request',AcctDb, Dev, Exten, Targets}, GlobalState) ->
    AcctId = wh_util:format_account_id(AcctDb, 'raw'),
    NewGlobal = with_state(AcctId
                          ,GlobalState
                          ,fun(Local) ->
                               #'state'{'requests' = dict:merge(fun(_, _, V2) -> V2 end
                                                               ,get_requests(Local)
                                                               ,make_requests(Targets, Dev, Exten)
                                                               )
                                        ,'sipnames' = dict:store(Exten, Targets, get_sipnames(Local))
                                        ,'requestor_queues' = maybe_update_queues(Targets, Dev, get_requestor_queues(Local))
                                        ,'acctdb' = AcctDb
                                       }
                           end
                          ),
    {'noreply', NewGlobal};
handle_cast(_Msg, State) ->
    lager:info("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

-spec with_state(ne_binary(), dict(ne_binary(), #'state'{}), fun((#'state'{}) -> #'state'{})) -> #'state'{}.
with_state(AcctId, Global, F) ->
    Local = case dict:find(AcctId, Global) of
                {'ok', S} -> S;
                _ -> #'state'{'requests' = dict:new()
                              ,'requestor_queues' = dict:new()
                              ,'sipnames' = dict:new()
                             }
            end,
    dict:store(AcctId, F(Local), Global).

-spec make_requests(ne_binary(), ne_binary(), ne_binary()) -> dict().
make_requests(SIPNames, Requestor, Exten) ->
    R = lists:map(fun (SIPName) -> {SIPName, Requestor} end, SIPNames),
    lists:foldl(fun (Req, Acc) -> dict:store(Req, Exten, Acc) end, dict:new(), R).

-spec maybe_update_queues(ne_binaries(), ne_binary(), dict()) -> dict().
maybe_update_queues(SIPNames, Requestor, Queues) ->
    lists:foldl(fun (SIPName, Acc) ->
                    Q = case dict:find(SIPName, Acc) of
                            {'ok', Queue} -> Queue;
                            _ -> queue:new()
                        end,
                    case queue:member(Requestor, Q) of
                        'true' -> Acc;
                        'false' -> dict:store(SIPName,queue:in(Requestor, Q),Acc)
                    end
                end
                ,Queues
                ,SIPNames
               ).

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
    lager:info("unhandled msg: ~p", [_Info]),
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
    lager:info("camper ~p termination", [_Reason]),
    'ok'.

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
