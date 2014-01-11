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

-record(state, {listener
				,user	
			   }).


open(_Pid, _SId, _Opts) ->
    lager:debug("opening socket ~p", [_SId]),
    {'ok', #state{}}.

close(_Pid, _SId, #state{listener='undefined'}) ->
    lager:debug("closing socket ~p", [_SId]),
    'ok';
close(_Pid, _SId, #state{listener=PidListener, user=User}) ->
    lager:debug("closing socket ~p", [_SId]),
    'ok' = gen_server:call(PidListener, {'disconnect_socket', User}).


%% Msg
recv(_Pid, _SId, {message, <<>>, Message}, State) ->
	lager:debug("receive message ~p on socket ~p", [Message, _SId]),
    {'ok', State};
%% Custom Events
recv(Pid, _SId, {event, <<>>, Event, Message}, State) ->
    {'ok', handle_event(Event, Message, Pid, State)};
%% Catch all
recv(_Pid, _SId, Message, State) ->
    lager:info("receive unknown message ~p on socket ~p", [Message, _SId]),
    {'ok', State}.

%% Connection Event
handle_event(<<"connection">>, Data, Pid, State) ->
	ConfId = wh_json:get_value(<<"conference_id">>, Data),
	User = wh_json:get_value(<<"user_name">>, Data),
	{'ok', PidListener} = blackhole_events_sup:start_listener(ConfId),
	gen_server:cast(PidListener, {'connect_socket', Pid, User}),
	socketio_session:send_event(Pid, <<"connected">>, [User, ConfId]),
	State#state{listener=PidListener, user=User};
handle_event(<<"disconnection">>, _, _Pid, #state{listener='undefined'}) ->
	#state{};
handle_event(<<"disconnection">>, _, Pid, #state{listener=PidListener, user=User}) ->
	'ok' = gen_server:call(PidListener, {'disconnect_socket', User}),
	socketio_session:send_event(Pid, <<"disconnected">>, [User]),
	#state{};
handle_event(<<"connection_status">>, _, Pid, #state{listener='undefined'}) ->
	socketio_session:send_event(Pid, <<"connection_status">>, ['false']),
	#state{};
handle_event(<<"connection_status">>, _, Pid, #state{listener=PidListener}=State) ->
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








