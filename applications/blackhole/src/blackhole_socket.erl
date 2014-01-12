%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_socket).

-include("blackhole.hrl").

-export([open/3
        ,recv/4
        ,close/3
        ]).

-record(state, {'listener'}).

open(_Pid, _SId, _Opts) ->
    lager:debug("opening socket ~p", [_SId]),
    {'ok', #state{}}.

%% Msg
recv(_Pid, _SId, {message, <<>>, Message}, State) ->
    lager:debug("receive message ~p on socket ~p", [Message, _SId]),
    {'ok', State};
%% Custom Events
recv(Pid, _SId, {event, <<>>, Event, Message}, State) ->
    lager:debug("receive message ~p on socket ~p with event ~p", [Message, _SId, Event]),
    {'ok', handle_event(Event, Message, Pid, State)};
%% Catch all
recv(_Pid, _SId, Message, State) ->
    lager:info("receive unknown message ~p on socket ~p", [Message, _SId]),
    {'ok', State}.

close(_Pid, _SId, #state{listener='undefined'}) ->
    lager:debug("closing socket ~p", [_SId]),
    'ok';
close(_Pid, _SId, #state{listener=PidListener}) ->
    lager:debug("closing socket ~p", [_SId]),
    'ok' = gen_server:call(PidListener, {'disconnect_socket'}).

%% Connection Event
handle_event(<<"calls.connect">>, Data, Pid, State) ->
    AccountId = wh_json:get_value(<<"account_id">>, Data),
    lager:debug("listening for call events for account: ~p", [AccountId]),
    {'ok', PidListener} = blackhole_call_events_sup:start_listener(AccountId),
    gen_server:cast(PidListener, {'connect_socket', Pid}),
    State#state{listener=PidListener};
handle_event(<<"connection">>, Data, Pid, State) ->
    lager:debug('connection happened'),
    ConfId = wh_json:get_value(<<"conference_id">>, Data),
    User = wh_json:get_value(<<"user_name">>, Data),
    {'ok', PidListener} = blackhole_events_sup:start_listener(ConfId),
    gen_server:cast(PidListener, {'connect_socket', Pid, User}),
    socketio_session:send_event(Pid, <<"connected">>, [User, ConfId]),
    State#state{listener=PidListener};
handle_event(<<"conference.disconnection">>, _, _Pid, #state{listener='undefined'}) ->
    #state{};
handle_event(<<"conference.connection_status">>, _, Pid, #state{listener='undefined'}) ->
    socketio_session:send_event(Pid, <<"connection_status">>, ['false']),
    #state{};
handle_event(<<"conference.connection_status">>, _, Pid, #state{listener=PidListener}=State) ->
    Status = gen_server:call(PidListener, 'get_socket'),
    socketio_session:send_event(Pid, <<"connection_status">>, [Status]),
    State;
%% Unknown Event
handle_event(Event, Data, Pid, State) ->
    lager:info("receive unknown event ~p", [Event]),
    Unknown = [{<<"event">>, Event}
              ,{<<"data">>, Data}
              ],
    socketio_session:send_event(Pid, <<"unknown_event">>, Unknown),
    State.
