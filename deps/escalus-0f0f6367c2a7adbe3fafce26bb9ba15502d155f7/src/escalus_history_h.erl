-module(escalus_history_h).
-behaviour(gen_event).

-export([get_history/1]).

-export([init/1,
	 terminate/2,
	 handle_info/2,
	 handle_call/2,
	 handle_event/2,
     code_change/3]).

-ifdef(namespaced_types).
-type dict_t() :: dict:dict().
-else.
-type dict_t() :: dict().
-endif.

-record(state, {
        events :: list(),
        counters :: dict_t()
}).

get_history(Mgr) ->
    gen_event:call(Mgr, escalus_history_h, get_history).

init([]) ->
    S = #state{
            events = []
        },
    {ok, S}.

handle_event({incoming_stanza, Jid, Stanza}, State) ->
    {ok, save_stanza(incoming_stanza, Jid, Stanza, State)};
handle_event({outgoing_stanza, Jid, Stanza}, State) ->
    {ok, save_stanza(outgoing_stanza, Jid, Stanza, State)};
handle_event({pop_incoming_stanza, Jid, Stanza}, State) ->
    {ok, save_stanza(pop_incoming_stanza, Jid, Stanza, State)};
handle_event(story_start, State) ->
    {ok, save_story_event(story_start, State)};
handle_event(story_end, State) ->
    {ok, save_story_event(story_end, State)};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(get_history, State=#state{events=Events}) ->
    {ok, lists:reverse(Events), State}.

code_change(_, _, State) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%% ===================================================================
%% Helpers
%% ===================================================================

save_stanza(Type, Jid, Stanza, State=#state{events = Events}) ->
    State#state{
        events = [{stanza, Type, Jid, now(), Stanza}|Events]}.

save_story_event(Type, State=#state{events = Events}) ->
    State#state{
        events = [{story, Type, now()}|Events]}.
