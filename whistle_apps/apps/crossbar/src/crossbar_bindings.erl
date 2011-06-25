%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
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
-export([start_link/0, bind/1, map/2, fold/2, flush/0, flush/1, stop/0]).

%% Helper Functions for Results of a map/2
-export([any/1, all/1, succeeded/1, failed/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/crossbar.hrl").
%% PropEr needs to be included before eunit. Both modules create a ?LET macro,
%% but the PropEr one is the useful one. Also needs to be included before any
%% function definition because it includes functions.
-include_lib("proper/include/proper.hrl").

-define(SERVER, ?MODULE).

-type binding_result() :: tuple(binding_result, term(), term()).
-type binding() :: tuple(binary(), queue()). %% queue(pid() | atom())
-type bindings() :: list(binding()) | [].

-record(state, {bindings = [] :: bindings()}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% return [ {Result, Payload1} ], a list of tuples, the first element
%% of which is the result of the bound handler, and the second element
%% is the payload, possibly modified
%% @end
%%--------------------------------------------------------------------
-spec(map/2 :: (Routing :: binary(), Payload :: term()) -> list(tuple(term(), term()))).
map(Routing, Payload) ->
    gen_server:call(?MODULE, {map, Routing, Payload}, infinity).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% return the modified Payload after it has been threaded through
%% all matching bindings
%% @end
%%--------------------------------------------------------------------
-spec(fold/2 :: (Routing :: binary(), Payload :: term()) -> term()).
fold(Routing, Payload) ->
    gen_server:call(?MODULE, {fold, Routing, Payload}, infinity).

%%-------------------------------------------------------------------
%% @doc
%% Helper functions for working on a result set of bindings
%% @end
%%-------------------------------------------------------------------
-spec(any/1 :: (Res :: proplist()) -> boolean()).
any(Res) when is_list(Res) -> lists:any(fun check_bool/1, Res).

-spec(all/1 :: (Res :: proplist()) -> boolean()).
all(Res) when is_list(Res) -> lists:all(fun check_bool/1, Res).

-spec(failed/1 :: (Res :: proplist()) -> proplist()).
failed(Res) when is_list(Res) -> [R || R <- Res, filter_out_succeeded(R)].

-spec(succeeded/1 :: (Res :: proplist()) -> proplist()).
succeeded(Res) when is_list(Res) -> [R || R <- Res, filter_out_failed(R)].

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

-spec(bind/1 :: (Binding :: binary()) -> ok | tuple(error, exists)).
bind(Binding) ->
    gen_server:call(?MODULE, {bind, Binding}, infinity).

-spec(flush/0 :: () -> ok).
flush() ->
    gen_server:cast(?MODULE, flush).

-spec(flush/1 :: (Binding :: binary()) -> ok).
flush(Binding) ->
    gen_server:cast(?MODULE, {flush, Binding}).

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
handle_call({map, Routing, Payload}, From , #state{bindings=Bs}=State) ->
    spawn(fun() ->
		  ?TIMER_START(list_to_binary(["bindings.map ", Routing])),
		  ?LOG_SYS("Running map: ~p", [Routing]),
                  Reply = lists:foldl(
                            fun({B, Ps}, Acc) ->
                                    case binding_matches(B, Routing) of
                                        true ->
                                            map_bind_results(Ps, Payload, Acc, Routing);
                                        false ->
                                            Acc
                                    end
                            end, [], Bs),
		  ?TIMER_STOP("bindings.map"),
                  gen_server:reply(From, Reply)
          end),
    {noreply, State};
handle_call({fold, Routing, Payload}, From , #state{bindings=Bs}=State) ->
    spawn(fun() ->
		  ?TIMER_START(list_to_binary(["bindings.fold ", Routing])),
                  ?LOG_SYS("Running fold: ~p", [Routing]),
                  Reply = lists:foldl(
                            fun({B, Ps}, Acc) ->
                                    case binding_matches(B, Routing) of
                                        true -> fold_bind_results(Ps, Acc, Routing);
                                        false -> Acc
                                    end
                            end, Payload, Bs),
		  ?TIMER_STOP("bindings.fold"),
                  gen_server:reply(From, Reply)
          end),
    {noreply, State};
handle_call({bind, Binding}, {From, _Ref}, #state{bindings=[]}=State) ->
    link(From),
    {reply, ok, State#state{bindings=[{Binding, queue:in(From, queue:new())}]}};
handle_call({bind, Binding}, {From, _Ref}, #state{bindings=Bs}=State) ->
    ?LOG_SYS("~w is binding ~s", [From, Binding]),
    case lists:keyfind(Binding, 1, Bs) of
	false ->
	    link(From),
	    {reply, ok, State#state{bindings=[{Binding, queue:in(From, queue:new())} | Bs]}};
	{_, Subscribers} ->
	    case queue:member(From, Subscribers) of
		true -> {reply, {error, exists}, State};
		false ->
		    link(From),
		    {reply, ok, State#state{bindings=[{Binding, queue:in(From, Subscribers)} | lists:keydelete(Binding, 1, Bs)]}}
	    end
    end.

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
handle_cast(stop, State) ->
    {stop, normal, State}.

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
    ?LOG_SYS("~w went down(~w)", [Pid, _Reason]),
    Bs1 = lists:foldr(fun({B, Subs}, Acc) ->
			      [{B, remove_subscriber(Pid, Subs)} | Acc]
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
    ?LOG_SYS("Terminating: ~p", [_Reason]),
    lists:foreach(fun flush_binding/1, Bs).

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
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(remove_subscriber/2 :: (Pid :: pid() | atom(), Subs :: queue()) -> queue()).
remove_subscriber(Pid, Subs) ->
    case queue:member(Pid, Subs) of
	false -> Subs;
	true ->
	    queue:filter(fun(P) when P =:= Pid -> false;
			    (_) -> true
			 end, Subs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Evaluate a binding against a routing key
%%      Break both binaries on the '.' delimiter, reverse the resulting list of
%%      symbols, and iterate through the lists until a determination is made of
%%      whether there is a match.
%%      
%% @end
%%--------------------------------------------------------------------
-spec(binding_matches/2 :: (B :: binary(), R :: binary()) -> boolean()).
binding_matches(B, B) -> true;
binding_matches(B, R) ->
    Opts = [global],
    matches(lists:reverse(binary:split(B, <<".">>, Opts))
	    ,lists:reverse(binary:split(R, <<".">>, Opts))
	   ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%
%% <<"#.6.*.1.4.*">>,<<"6.a.a.6.a.1.4.a">>
%% 
%%--------------------------------------------------------------------
%% if both are empty, we made it!
-spec(matches/2 :: (Bs :: list(binary()), Rs :: list(binary())) -> boolean()).
matches([], []) -> true;
matches([<<"#">>], []) -> true;

matches([<<"#">>, <<"*">>], []) -> false;
matches([<<"#">>, <<"*">>], [<<>>]) -> false;
matches([<<"#">>, <<"*">>], [_]) -> true; % match one item:  #.* matches foo

matches([<<"#">> | Bs], []) -> % sadly, #.# would match foo, foo.bar, foo.bar.baz, etc
    matches(Bs, []);           % so keep checking by stipping of the first #

%% if one runs out without a wildcard, no matchy
matches([], [_|_]) -> false; % foo.*   foo
matches([_|_], []) -> false;
matches([_|_], [<<>>]) -> false;

%% * matches one segment only
matches([<<"*">> | Bs], [_|Rs]) ->
    matches(Bs, Rs); % so ignore what the routing segment is and continue

%% # can match 0 or more segments
matches([<<"#">>, B | Bs], [B | Rs]) ->
    %% Since the segment in B could be repeated later in the Routing Key, we need to bifurcate here
    %% but we'll short circuit if this was indeed the end of the # matching
    %% see binding_matches(<<"#.A.*">>,<<"A.a.A.a">>)

    case lists:member(B, Rs) of
	true ->
	    matches(Bs, Rs) orelse matches([<<"#">> | Bs], Rs);
	false ->
	    matches(Bs, Rs)
    end;

matches([<<"#">>, <<"*">> | _]=Bs, [_ | Rs]) ->
    matches(Bs, Rs);

matches([<<"#">> | _]=Bs, [_ | Rs]) ->
    matches(Bs, Rs); % otherwise leave the # in to continue matching

%% if the segments match, continue
matches([B | Bs], [B | Rs]) ->
    matches(Bs, Rs);
%% otherwise no match
matches(_, _) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% returns the results for each pid and their modification (if any) to the payload
%% Results is the accumulated results list so far
%% @end
%%--------------------------------------------------------------------
-spec(map_bind_results/4 :: (Pids :: queue(), Payload :: term(), Results :: list(binding_result()), Route :: binary()) -> list(tuple(term() | timeout, term()))).
map_bind_results(Pids, Payload, Results, Route) ->
    lists:foldr(fun(Pid, Acc) ->
		      Pid ! {binding_fired, self(), Route, Payload},
                      case wait_for_map_binding() of
                          {ok,  Resp, Pay1} -> [{Resp, Pay1} | Acc];
                          timeout -> [{timeout, Payload} | Acc];
			  {error, E} -> [{error, E, Pid}|Acc]
                      end
		end, Results, queue:to_list(Pids)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Run a receive loop if we recieve hearbeat, otherwise collect binding results
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_map_binding/0 :: () -> tuple(ok, atom(), term()) | timeout | tuple(error, atom())).
wait_for_map_binding() ->
    receive
        {binding_result, Resp, Pay} -> {ok, Resp, Pay};
	{binding_error, Error} -> {error, Error};
        heartbeat -> wait_for_map_binding()
    after
        1000 -> timeout
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a binding_result uses 'eoq' for its response, the payload is
%% ignored and the subscriber is re-inserted into the queue, with the
%% previous payload being passed to the next invocation.
%% @end
%%--------------------------------------------------------------------
-spec(fold_bind_results/3 :: (Pids :: queue(), Payload :: term(), Route :: binary()) -> term()).
fold_bind_results(_, {error, _}=E, _) -> E;
fold_bind_results(Pids, Payload, Route) ->
    fold_bind_results(Pids, Payload, Route, queue:len(Pids), queue:new()).

fold_bind_results(Pids, Payload, Route, PidsLen, ReRunQ) ->
    case queue:out(Pids) of
	{empty, _} ->
	    case queue:len(ReRunQ) of
		0 -> Payload; % no one to re-run, return
		N when N < PidsLen ->
		    %% one or more pids weren't ready to operate on Payload, let them have another go
		    fold_bind_results(ReRunQ, Payload, Route, queue:len(ReRunQ), queue:new());
		PidsLen ->
		    %% If all Pids 'eoq'ed, ReRunQ will be the same queue, and Payload will be unchanged - exit the fold
		    ?LOG_SYS("Loop detected for ~s, returning", [Route]),
		    Payload
	    end;

	{{value, P}, Pids1} ->
	    P ! {binding_fired, self(), Route, Payload},
            case wait_for_fold_binding() of
                {ok, Pay1} -> fold_bind_results(Pids1, Pay1, Route);
                eoq -> fold_bind_results(queue:in(P, Pids1), Payload, Route);
                timeout -> fold_bind_results(Pids1, Payload, Route);
		{error, _}=E -> E
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Run a receive loop if we recieve hearbeat, otherwise collect binding results
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_fold_binding/0 :: () -> tuple(ok, term())|timeout|eoq|tuple(error, atom())).
wait_for_fold_binding() ->
    receive
        {binding_result, eoq, _} -> eoq;
        {binding_result, _, Pay} -> {ok, Pay};
	{binding_error, Error} -> {error, Error};
        heartbeat -> wait_for_fold_binding()
    after
        1000 -> timeout
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% let those bound know their binding is flushed
%% @end
%%--------------------------------------------------------------------
-spec(flush_binding/1 :: (Binding :: binding()) -> no_return()).
flush_binding({B, Subs}) ->
    lists:foreach(fun(S) -> S ! {binding_flushed, B} end, queue:to_list(Subs)).

%%-------------------------------------------------------------------------
%% @doc
%% Helpers for the result set helpers
%% @end
%%-------------------------------------------------------------------------
-spec(check_bool/1 :: (tuple(boolean(), term()) | term()) -> boolean()).
check_bool({true, _}) -> true;
check_bool({timeout, _}) -> true;
check_bool(_) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(filter_out_failed/1 :: (tuple(boolean(), _)) -> boolean()).
filter_out_failed({true, _}) -> true;
filter_out_failed({_, _}) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(filter_out_succeeded/1 :: (tuple(boolean(), _)) -> boolean()).
filter_out_succeeded({true, _}) -> false;
filter_out_succeeded({_, _}) -> true.

%% EUNIT and PropEr TESTING %%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(ROUTINGS, [ <<"foo.bar.zot">>, <<"foo.quux.zot">>, <<"foo.bar.quux.zot">>, <<"foo.zot">>, <<"foo">>, <<"xap">>]).

-define(BINDINGS, [
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
		  ]).

bindings_match_test() ->
    lists:foreach(fun({B, _}=Expected) ->
			  Actual = lists:foldr(fun(R, Acc) -> [binding_matches(B, R) | Acc] end, [], ?ROUTINGS),
			  ?assertEqual(Expected, {B, Actual})
		  end, ?BINDINGS).

simple_bind_test() ->
    ?MODULE:start_link(),
    logger:start(),

    Binding = <<"foo">>,
    
    BindFun = fun() ->
		      timer:sleep(500),
		      ?assertEqual(ok, ?MODULE:bind(Binding)),
		      ?assertEqual({error, exists}, ?MODULE:bind(Binding))
	      end,

    Pids = [ spawn(BindFun) || _ <- lists:seq(1,3) ],
    _ = [ erlang:monitor(process, Pid) || Pid <- Pids ],

    wait_for_all(Pids),

    ?assertEqual(ok, ?MODULE:flush(Binding)),
    ?assertEqual(ok, ?MODULE:flush(<<"non-existant">>)),

    ?MODULE:stop().

weird_bindings_test() ->
    ?assertEqual(true, binding_matches(<<"#.A.*">>,<<"A.a.A.a">>)),
    ?assertEqual(true, binding_matches(<<"#.*">>, <<"foo">>)),
    ?assertEqual(true, binding_matches(<<"#.*">>, <<"foo.bar">>)),
    ?assertEqual(false, binding_matches(<<"foo.#.*">>, <<"foo">>)),
    ?assertEqual(false, binding_matches(<<"#.*">>, <<"">>)),
    ?assertEqual(true, binding_matches(<<"#.6.*.1.4.*">>,<<"6.a.a.6.a.1.4.a">>)).

wait_for_all([]) ->
    ok;
wait_for_all([P|Ps]) ->
    receive
	{'DOWN', _, process, P, _} ->
	    wait_for_all(Ps)
    end.


fold_bindings_server(B) ->
    ?assertEqual(?MODULE:bind(B), ok),
    ?assertEqual(?MODULE:bind(B), {error, exists}),
    fold_bindings_loop(B).

fold_bindings_loop(B) ->
    receive
	{binding_fired, Pid, _R, Payload} ->
	    ?LOG_SYS("binding received: payload ~p", [Payload]),
	    Pid ! {binding_result, true, Payload+1},
	    fold_bindings_loop(B);
	{binding_flushed, _} ->
	    fold_bindings_loop(B);
	shutdown -> ok
    end.

map_bindings_server(B) ->
    ?assertEqual(?MODULE:bind(B), ok),
    ?assertEqual(?MODULE:bind(B), {error, exists}),
    map_bindings_loop(B).

map_bindings_loop(B) ->
    receive
	{binding_fired, Pid, _R, _Payload} ->
	    Pid ! {binding_result, true, B},
	    map_bindings_loop(B);
	{binding_flushed, _} ->
	    map_bindings_loop(B);
	shutdown -> ok
    end.

-define(ROUTINGS_MAP_FOLD, [
			    %% routing, # responses or count from fold
			    {<<"foo.bar.zot">>, 3}
			    ,{<<"foo.quux.zot">>, 3}
			    ,{<<"foo.bar.quux.zot">>, 2}
			    ,{<<"foo.zot">>, 2}
			    ,{<<"foo">>, 2}
			    ,{<<"xap">>, 2}
			   ]).

-define(BINDINGS_MAP_FOLD, [ <<"#">>, <<"foo.*.zot">>, <<"foo.#.zot">>, <<"*">>, <<"#.quux">>]).

start_server(Fun) ->
    logger:start(),
    ?MODULE:start_link(),
    ?assertEqual(ok, ?MODULE:flush()),

    [ spawn(fun() -> Fun(B) end) || B <- ?BINDINGS_MAP_FOLD ],

    timer:sleep(500).

stop_server() ->
    ?MODULE:stop().

fold_and_route_test() ->
    start_server(fun fold_bindings_server/1),
    lists:foreach(fun({R, N}) ->
			  Res = ?MODULE:fold(R, 0),
			  ?LOG("fold: ~s: ~w -> ~p", [R, N, Res]),
			  ?assertEqual({R, N}, {R, Res})
		  end, ?ROUTINGS_MAP_FOLD),
    stop_server().
    

map_and_route_test() ->
    start_server(fun map_bindings_server/1),
    lists:foreach(fun({R, N}) ->
			  Res = ?MODULE:map(R, R),
			  ?LOG("map: ~s: ~w -> ~p", [R, N, Res]),
			  ?assertEqual({R, N}, {R, length(Res)}),
			  ?assertEqual(?MODULE:any(Res), true),
			  ?assertEqual(?MODULE:all(Res), true),
			  ?assertEqual(?MODULE:failed(Res), []),
			  ?assertEqual(?MODULE:succeeded(Res), Res)
		  end, ?ROUTINGS_MAP_FOLD),
    stop_server().

proper_test_() ->
    {"Runs the module's PropEr tests for rebar quick commands",
     {timeout, 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{max_shrinks, 0}]))
      ]}}.

%%% PropEr tests
%% Checks that the patterns for paths (a.#.*.c) match or do not
%% match a given expanded path.
prop_expands() ->
    ?FORALL(Paths, expanded_paths(),
	    ?WHENFAIL(io:format("Failed on ~p~n",[Paths]),
		      lists:all(fun(X) -> X end, %% checks if all true
				[binding_matches(Pattern, Expanded) =:= Expected ||
				    {Pattern, Expanded, Expected} <- Paths])
		     )).

%%% GENERATORS

%% Gives a list of paths that were expanded, some of them to fail on purpose,
%% some of them not to.
expanded_paths() ->
    ?LET(P, path(),
	 begin
	     B = list_to_binary(P),
	     ?LET({{Expanded1, IsRight1},{Expanded2, IsRight2}},
		  {wrong(P), right(P)},
		  [{B, list_to_binary(Expanded1), IsRight1},
		   {B, list_to_binary(Expanded2), IsRight2}])
	 end).

%% Tries to make a pattern wrong. Will not always suceed because a pattern
%% like "#" can be anything at all.
%%
%% Returns {Str, ShouldMatchOriginal}.
wrong(Path) ->
    ?LET(P, Path, wrong(P, true, [])).

%% Will expand the patterns according to the rules so they should always match
%%
%% Returns {Str, ShouldMatchOriginal}.
right(Path) ->
    ?LET(P, Path, {right1(P), true}).

%% Here's why some patterns will always succeed even if we try to make them
%% wrong. In a given strign S, we could add segments, but some subpatterns
%% would have a chance to fix the problem we created. See a.*.#, which means
%% 'at least two segments'  but still matches (albeit wrongly) a.b if we drop
%% a section of the text, replace it by one, or add two of them. It can
%% technically be done, but we would need a strong lookahead for that.
%% This is especially the case of .#., which we will have to simply ignore.
%%
%% Returns {Str, ShouldMatchOriginal}.
wrong([], Bool, Acc) ->
    {lists:reverse(Acc), Bool};
wrong("*.#." ++ Rest, Bool, Acc) -> %% the # messes stuff up, can't invalidate
    wrong(Rest, Bool, Acc);
wrong("*.#", Bool, Acc) ->  %% same as above, end of string
    {lists:reverse(Acc), Bool};
wrong("*." ++ Rest, _Bool, Acc) ->
    wrong(Rest, false, Acc);
wrong(".*", _Bool, Acc) ->
    {lists:reverse(Acc), false};
wrong(".#." ++ Rest, Bool, Acc) -> %% can't make this one wrong
    wrong(Rest, Bool, [$.|Acc]);
wrong("#." ++ Rest, Bool, Acc) -> %% same, start of string
    wrong(Rest, Bool, Acc);
wrong(".#", Bool, Acc) -> %% same as above, end of string
    {lists:reverse(Acc), Bool};
wrong([Char|Rest], Bool, Acc) when Char =/= $*, Char =/= $# ->
    wrong(Rest, Bool, [Char|Acc]).

%% Returns an expanded string according to the rules
right1([]) -> [];
right1("*" ++ Rest) ->
    ?LET(S, segment(), S++right1(Rest));
right1(".#" ++ Rest) ->
    ?LET(X,
	 union([
		"",
		?LAZY(?LET(S, segment(), [$.]++S)),
		?LAZY(?LET({A,B}, {segment(), segment()}, [$.]++A++[$.]++B)),
		?LAZY(?LET({A,B,C}, {segment(), segment(), segment()}, [$.]++A++[$.]++B++[$.]++C))
	       ]),
	 X ++ right1(Rest));
right1("#." ++ Rest) ->
    ?LET(X,
	 union([
		"",
		?LAZY(?LET(S, segment(), S++[$.])),
		?LAZY(?LET({A,B}, {segment(), segment()}, A++[$.]++B++[$.])),
		?LAZY(?LET({A,B,C}, {segment(), segment(), segment()}, A++[$.]++B++[$.]++C++[$.]))
	       ]),
	 X ++ right1(Rest));
right1([Char|Rest]) ->
    [Char|right1(Rest)].

%% Building a basic pattern/path string
path() ->
    ?LET(Base, ?LAZY(weighted_union([{3,a()}, {1,b()}])),
	 ?LET({H,T}, {union(["*.","#.",""]), union([".*",".#",""])},
	      H ++ Base ++ T)).

a() ->
    ?LET({X,Y}, {segment(), ?LAZY(union([b(), markers()]))},
	 X ++ [$.] ++ Y). 

b() ->
    ?LET({X,Y}, {segment(), ?LAZY(union([b(), c()]))},
	 X ++ [$.] ++ Y). 

c() ->
    segment().

segment() ->
    ?SUCHTHAT(
       X,
       list(union([choose($a,$z), choose($A,$Z), choose($0,$9)])),
       length(X) =/= 0
      ).

markers() -> 
    ?LET(S, ?LAZY(union([[$#, $., c()], [$*, $., b()]])), lists:flatten(S)).

proper_test_() ->
    {"Runs the module's PropEr tests for rebar quick commands",
     {timeout, 15000,
      [
          ?_assertEqual([], proper:module(?MODULE, [{max_shrinks, 0}]))
    ]}}.

%%% PropEr tests
%% Checks that the patterns for paths (a.#.*.c) match or do not
%% match a given expanded path.
prop_expands() ->
    ?FORALL(Paths, expanded_paths(),
     ?WHENFAIL(io:format("Failed on ~p~n",[Paths]),
         lists:all(fun(X) -> X end, %% checks if all true
            [binding_matches(Pattern, Expanded) =:= Expected ||
                {Pattern, Expanded, Expected} <- Paths]))).

%%% GENERATORS

%% Gives a list of paths that were expanded, some of them to fail on purpose,
%% some of them not to.
expanded_paths() ->
    ?LET(P, path(),
        begin
            B = list_to_binary(P),
            ?LET({{Expanded1, IsRight1},{Expanded2, IsRight2}},
                {wrong(P), right(P)},
                [{B, list_to_binary(Expanded1), IsRight1},
                 {B, list_to_binary(Expanded2), IsRight2}])
        end).

%% Tries to make a pattern wrong. Will not always suceed because a pattern
%% like "#" can be anything at all.
%%
%% Returns {Str, ShouldMatchOriginal}.
wrong(Path) ->
    ?LET(P, Path,
        begin
            {Str, Bool} = wrong(P, true, []),
            {re:replace(Str, <<"\\.+">>, <<".">>, [{return, list}]), Bool}
        end).

%% Will expand the patterns according to the rules so they should always match
%%
%% Returns {Str, ShouldMatchOriginal}.
right(Path) ->
    ?LET(P, Path, {right1(P), true}).

%% Here's why some patterns will always succeed even if we try to make them
%% wrong. This is the case of "#" which we will have to simply ignore.
%%
%% Returns {Str, ShouldMatchOriginal}.
wrong([], Bool, Acc) ->
    {lists:reverse(Acc), Bool};
wrong("*.#." ++ Rest, _Bool, Acc) ->
    wrong(Rest, false, [$.|Acc]);
wrong(".*.#." ++ Rest, _Bool, Acc) ->
    wrong(Rest, false, [$.|Acc]);
wrong("*.#", _Bool, Acc) ->  %% same as above, end of string
    {lists:reverse(Acc), false};
wrong("*." ++ Rest, _Bool, Acc) ->
    wrong(Rest, false, Acc);
wrong(".*", _Bool, Acc) ->
    {lists:reverse(Acc), false};
wrong(".#." ++ Rest, Bool, Acc) -> %% can't make this one wrong
    wrong(Rest, Bool, [$.|Acc]);
wrong("#." ++ Rest, Bool, Acc) -> %% same, start of string
    wrong(Rest, Bool, Acc);
wrong(".#", Bool, Acc) -> %% same as above, end of string
    {lists:reverse(Acc), Bool};
wrong([Char|Rest], Bool, Acc) when Char =/= $*, Char =/= $# ->
    wrong(Rest, Bool, [Char|Acc]).

%% Returns an expanded string according to the rules
right1([]) -> [];
right1("*" ++ Rest) ->
    ?LET(S, segment(), S++right1(Rest));
right1(".#" ++ Rest) ->
    ?LET(X,
        union([
            "",
            ?LAZY(?LET(S, segment(), [$.]++S)),
            ?LAZY(?LET({A,B}, {segment(), segment()}, [$.]++A++[$.]++B)),
            ?LAZY(?LET({A,B,C}, {segment(), segment(), segment()}, [$.]++A++[$.]++B++[$.]++C))
        ]),
        X ++ right1(Rest));
right1("#." ++ Rest) ->
    ?LET(X,
        union([
            "",
            ?LAZY(?LET(S, segment(), S++[$.])),
            ?LAZY(?LET({A,B}, {segment(), segment()}, A++[$.]++B++[$.])),
            ?LAZY(?LET({A,B,C}, {segment(), segment(), segment()}, A++[$.]++B++[$.]++C++[$.]))
        ]),
        X ++ right1(Rest));
right1([Char|Rest]) ->
    [Char|right1(Rest)].

%% Building a basic pattern/path string
path() ->
    ?LET(Base, ?LAZY(weighted_union([{3,a()}, {1,b()}])),
        ?LET({H,T}, {union(["*.","#.",""]), union([".*",".#",""])},
            H ++ Base ++ T)).

a() ->
    ?LET({X,Y}, {segment(), ?LAZY(union([b(), markers()]))},
        X ++ [$.] ++ Y). 

b() ->
    ?LET({X,Y}, {segment(), ?LAZY(union([b(), c()]))},
        X ++ [$.] ++ Y). 

c() ->
    ?LAZY(union([segment(), markers()])).

segment() ->
    ?SUCHTHAT(
        X,
        list(union([choose($a,$z), choose($A,$Z), choose($0,$9)])),
        length(X) =/= 0
    ).

markers() -> 
    ?LET(S, ?LAZY(union([
        [$#, $., b()],
        [$#, $., c()],
        [$*, $., b()],
        [$*, $., c()]
    ])), lists:flatten(S)).
-endif.
