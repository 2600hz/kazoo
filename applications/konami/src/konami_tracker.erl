%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_tracker).

-behaviour(gen_server).

%% API
-export([start_link/0
         ,track/3
         ,update_other_leg/1
         ,untrack/0
         ,active/0
        ]).

%% gen_server
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

%% ETS Related
-export([table_id/0
         ,table_options/0
         ,find_me_function/0
        ]).

-include("konami.hrl").

start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-record(active_fsm, {pid :: pid()
                     ,call_id :: api_binary()
                     ,other_leg :: api_binary()
                     ,listen_on :: 'a' | 'b' | 'ab'
                    }).
-type active_fsm() :: #active_fsm{}.
-type active_fsms() :: [active_fsm(),...] | [].

-spec track(api_binary(), api_binary(), 'a' | 'b' | 'ab') -> 'ok'.
track(CallId, OtherLeg, ListenOn) ->
    gen_server:cast(?MODULE, {'track', #active_fsm{pid=self()
                                                   ,call_id=CallId
                                                   ,other_leg=OtherLeg
                                                   ,listen_on=ListenOn
                                                  }}).

-spec update_other_leg(api_binary()) -> 'ok'.
update_other_leg(CallId) ->
    gen_server:cast(?MODULE, {'update', self(), {#active_fsm.other_leg, CallId}}).

-spec untrack() -> 'ok'.
untrack() ->
    gen_server:cast(?MODULE, {'untrack', self()}).

-spec active() -> {'ok', wh_json:objects() | active_fsms()} |
                  {'error', _}.
active() ->
    active('json').
active(Format) ->
    case ets:match_object(table_id(), #active_fsm{_='_'}) of
        {'error', _}=E -> E;
        Active ->
            {'ok', [record_to_format(A, Format) || A <- Active]}
    end.

record_to_format(Active, 'json') ->
    record_to_jobj(Active);
record_to_format(Active, 'record') ->
    Active.

-spec record_to_jobj(active_fsm()) -> wh_json:object().
record_to_jobj(#active_fsm{pid=Pid
                           ,call_id=CallId
                           ,other_leg=OtherLeg
                           ,listen_on=ListenOn
                          }) ->
    wh_json:from_list([{<<"pid">>, list_to_binary(pid_to_list(Pid))}
                       ,{<<"call_id">>, CallId}
                       ,{<<"other_leg">>, OtherLeg}
                       ,{<<"listen_on">>, ListenOn}
                      ]).

-spec table_id() -> ?MODULE.
table_id() -> ?MODULE.

-spec key_pos() -> integer().
key_pos() -> #active_fsm.pid.

-spec table_options() -> list().
table_options() ->
    ['protected', 'named_table'
     ,{'keypos', key_pos()}
    ].

-spec find_me_function() -> api_pid().
find_me_function() ->
    whereis(?MODULE).

init([]) ->
    wh_util:put_callid(?MODULE),
    lager:debug("starting konami_tracker"),
    {'ok', 'ok'}.

handle_call(_Request, _From, State) ->
    {'reply', 'not_implemented', State}.

handle_cast({'track', Active}, State) ->
    lager:debug("tracking ~p", [Active]),
    ets:insert_new(table_id(), Active),
    {'noreply', State};
handle_cast({'untrack', Key}, State) ->
    lager:debug("untracking ~p", [Key]),
    ets:delete(table_id(), Key),
    {'noreply', State};
handle_cast({'update', Key, Update}, State) ->
    lager:debug("updating ~p with ~p", [Key, Update]),
    ets:update_element(table_id(), Key, Update),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

handle_info({'ETS-TRANSFER', _TableId, _From, _GiftData}, State) ->
    lager:debug("control received of the ETS table"),
    {'noreply', State};
handle_info(_Msg, State) ->
    lager:debug("unhandled info: ~p", [_Msg]),
    {'noreply', State}.

terminate(_Reason, _State) ->
    lager:debug("terminating: ~p", [_Reason]).

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
