%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Store routing keys/pid bindings. When a binding is fired,
%%% pass the payload to the pid for evaluation, accumulating 
%%% the results for the response to the running process.
%%%
%%% foo.erl -> bind("module.init").
%%% *** Later ***
%%% module.erl
%%%   init() -> run("module.init", {some, "payload", 4, <<"You">>}).
%%%                foo ! Payload,
%%%                receive -> Resp
%%%   init() <- [Resp]
%%%   init() -> Decides what to do with responses
%%% @end
%%% Created :  7 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_bindings).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/0, bind/1, run/2, flush/0, flush/1]).

%% Helper Functions for Results of a run/{1,2}
-export([any/1, all/1, succeeded/1, failed/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).
-import(props, [get_value/2, get_value/3]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).

-type binding() :: tuple(binary(), list(pid() | atom())).
-type binding_result() :: tuple(binding_result, term(), term()).

-record(state, {bindings = [] :: list(binding()) | []}).

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

-spec(bind/1 :: (Binding :: binary()) -> ok | tuple(error, term())).
bind(Binding) ->
    gen_server:call(?MODULE, {bind, Binding}, infinity).

%% return [ {Result, Payload1} ], a list of tuples, the first element of which is the result of the bound handler,
%% and the second element is the payload, possibly modified
-spec(run/2 :: (Routing :: binary(), Payload :: term()) -> list(tuple(term(), term()))).
run(Routing, Payload) ->
    gen_server:call(?MODULE, {run, Routing, Payload}, infinity).

flush() ->
    gen_server:cast(?MODULE, flush).

flush(Binding) ->
    gen_server:cast(?MODULE, {flush, Binding}).

%%-------------------------------------------------------------------
%% @doc
%% Helper functions for working on a result set of bindings
%% @end
%%-------------------------------------------------------------------
-spec(any/1 :: (Res :: list(tuple(boolean(), term()))) -> boolean()).
any(Res) when is_list(Res) -> lists:any(fun check_bool/1, Res).

-spec(all/1 :: (Res :: list(tuple(boolean(), term()))) -> boolean()).
all(Res) when is_list(Res) -> lists:all(fun check_bool/1, Res).

-spec(failed/1 :: (Res :: list(tuple(boolean(), term()))) -> list()).
failed(Res) when is_list(Res) -> lists:filter(fun filter_out_succeeded/1, Res).

-spec(succeeded/1 :: (Res :: list(tuple(boolean(), term()))) -> list()).
succeeded(Res) when is_list(Res) -> lists:filter(fun filter_out_failed/1, Res).

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
handle_call({run, Routing, Payload}, _, #state{bindings=Bs}=State) ->
    Res = lists:foldl(fun({B, Ps}, Acc) ->
			      case binding_matches(B, Routing) of
				  true -> get_bind_results(Ps, Payload, Acc, Routing);
				  false -> Acc
			      end
		      end, [], Bs),
    {reply, Res, State};
			
handle_call({bind, Binding}, {From, _Ref}, #state{bindings=Bs}=State) ->
    format_log(info, "CB_BINDING(~p): ~p binding ~p~n", [self(), From, Binding]),
    case lists:keyfind(Binding, 1, Bs) of
	false ->
	    link(From),
	    {reply, ok, State#state{bindings=[{Binding, [From]} | Bs]}};
	{_, Subscribers} ->
	    case lists:member(From, Subscribers) of
		true -> {reply, exists, State};
		false ->
		    link(From),
		    {reply, ok, State#state{bindings=[{Binding, [From|Subscribers]} | lists:keydelete(Binding, 1, Bs)]}}
	    end
    end;

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
handle_cast(flush, #state{bindings=Bs}=State) ->
    lists:foreach(fun flush_binding/1, Bs),
    {noreply, State#state{bindings=[]}};
handle_cast({flush, Binding}, #state{bindings=Bs}=State) ->
    case lists:keyfind(Binding, 1, Bs) of
	false -> {noreply, State};
	{_, _}=B ->
	    flush_binding(B),
	    {noreply, State#state{bindings=lists:keydelete(Binding, 1, Bs)}}
    end;
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
handle_info({'EXIT', Pid, _Reason}, #state{bindings=Bs}=State) ->
    format_log(info, "BINDINGS(~p): ~p went down(~p)~n", [self(), Pid, _Reason]),
    Bs1 = lists:foldr(fun({B, Subs}, Acc) ->
			      [{B, lists:delete(Pid, Subs)} | Acc]
		      end, [], Bs),
    {noreply, State#state{bindings=Bs1}};
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
terminate(_Reason, #state{bindings=Bs}) ->
    lists:foreach(fun flush_binding/1, Bs),
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
-spec(binding_matches/2 :: (B :: binary(), R :: binary()) -> boolean()).
binding_matches(B, B) -> true;
%% foo.* matches foo.bar, foo.baz, but not bip.bar
%% B is what was bound, R is the routing key being used
%% so B can be more generic, R is absolute
binding_matches(B, R) ->
    Opts = [global],
    matches(binary:split(B, <<".">>, Opts), binary:split(R, <<".">>, Opts)).

%% if both are empty, we made it!
-spec(matches/2 :: (Bs :: list(binary()), Rs :: list(binary())) -> boolean()).
matches([], []) -> true;
matches([<<"#">>], []) -> true;

matches([<<"#">>, <<"*">>], []) -> false;
matches([<<"#">>, <<"*">>], [_]) -> true; % match one item:  #.* matches foo

matches([<<"#">> | Bs], []) -> % sadly, #.# would match foo, foo.bar, foo.bar.baz, etc
    matches(Bs, []);           % so keep checking by stipping of the first #

%% if one runs out without a wildcard, no matchy
matches([], [_|_]) -> false; % foo.*   foo
matches([_|_], []) -> false;

%% * matches one segment only
matches([<<"*">> | Bs], [_|Rs]) ->
    matches(Bs, Rs); % so ignore what the routing segment is and continue

%% # can match 0 or more segments
matches([<<"#">>, B | Bs], [B | Rs]) ->
    matches(Bs, Rs); % if the proceeding values match, strip them out, as we've finished matching the #
matches([<<"#">>, <<"*">> | _]=Bs, [_ | Rs]) ->
    matches(Bs, Rs);

matches([<<"#">> | _]=Bs, [_ | Rs]) ->
    matches(Bs, Rs); % otherwise leave the # in to continue matching

%% if the segments match, continue
matches([B | Bs], [B | Rs]) ->
    matches(Bs, Rs);
%% otherwise no match
matches(_, _) -> false.

%% returns the results for each pid and their modification (if any) to the payload
-spec(get_bind_results/4 :: (Pids :: list(pid()), Payload :: term(), Results :: list(binding_result()), Route :: binary()) -> list(tuple(term() | timeout, term()))).
get_bind_results(Pids, Payload, Results, Route) ->
    lists:foldr(fun(Pid, Acc) ->
			Pid ! {binding_fired, self(), Route, Payload},
			receive
			    {binding_result, Resp, Pay1} -> [{Resp, Pay1} | Acc]
			after
			    1000 -> [{timeout, Payload} | Acc]
			end
		end, Results, Pids).

%% let those bound know their binding is flushed
-spec(flush_binding/1 :: (Binding :: binding()) -> no_return()).
flush_binding({B, Subs}) ->
    lists:foreach(fun(S) -> S ! {binding_flushed, B} end, Subs).


%%-------------------------------------------------------------------------
%% @doc
%% Helpers for the result set helpers
%% @end
%%-------------------------------------------------------------------------
-spec(check_bool/1 :: (tuple(boolean(), term()) | term()) -> boolean()).
check_bool({true, _}) -> true;
check_bool({timeout, _}) -> true;
check_bool(_) -> false.

-spec(filter_out_failed/1 :: (tuple(boolean(), _)) -> boolean()).
filter_out_failed({true, _}) -> true;
filter_out_failed({false, _}) -> false.

-spec(filter_out_succeeded/1 :: (tuple(boolean(), _)) -> boolean()).
filter_out_succeeded({true, _}) -> false;
filter_out_succeeded({false, _}) -> true.
	    
%% EUNIT TESTING

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

bindings_match_test() ->
    Routings = [ <<"foo.bar.zot">>, <<"foo.quux.zot">>, <<"foo.bar.quux.zot">>, <<"foo.zot">>, <<"foo">>, <<"xap">>],
    Bindings = [
		{<<"#">>, [true, true, true, true, true, true]}
		,{<<"foo.*.zot">>, [true, true, false, false, false, false]}
		,{<<"foo.#.zot">>, [true, true, true, true, false, false]}
		,{<<"*.bar.#">>, [true, false, true, false, false, false]}
		,{<<"*">>, [false, false, false, false, true, true]}
		,{<<"#.tow">>, [false, false, false, false, false, false]}
		,{<<"#.quux.zot">>, [false, true, true, false, false, false]}
		,{<<"xap.#">>, [false, false, false, false, false, true]}
		,{<<"#.*">>, [true, true, true, true, true, true]}
		,{<<"#.bar.*">>, [true, false, false, false, false, false]}
	       ],
    lists:foreach(fun({B, _}=Expected) ->
			  Actual = lists:foldr(fun(R, Acc) -> [binding_matches(B, R) | Acc] end, [], Routings),
			  ?assertEqual(Expected, {B, Actual})
		  end, Bindings).

bindings_server(B) ->
    ?MODULE:bind(B),
    bindings_loop().

bindings_loop() ->
    receive
	{binding_fired, Pid, _R, Payload} ->
	    Pid ! {binding_result, looks_good, Payload},
	    bindings_loop();
	{binding_flushed, _} ->
	    bindings_loop();
	shutdown -> ok
    end.

bind_and_route_test() ->
    ?MODULE:start_link(),
    ?MODULE:flush(),
    Routings = [
		%% routing, # responses
		{<<"foo.bar.zot">>, 3}
		,{<<"foo.quux.zot">>, 3}
		,{<<"foo.bar.quux.zot">>, 2}
		,{<<"foo.zot">>, 2}
		,{<<"foo">>, 2}
		,{<<"xap">>, 2}
	       ],
    Bindings = [ <<"#">>, <<"foo.*.zot">>, <<"foo.#.zot">>, <<"*">>, <<"#.quux">>],

    BoundPids = [ spawn(fun() -> bindings_server(B) end) || B <- Bindings ],
    timer:sleep(500),
    lists:foreach(fun({R, N}) ->
			  Res = ?MODULE:run(R, R),
			  ?assertEqual({R, N}, {R, length(Res)})
		  end, Routings),
    lists:foreach(fun(P) -> P ! shutdown end, BoundPids).

-endif.
