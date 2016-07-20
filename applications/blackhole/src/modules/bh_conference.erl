%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Peter Defebvre
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(bh_conference).

-export([handle_event/2
        ,add_amqp_binding/2, rm_amqp_binding/2
        ]).

-include("blackhole.hrl").

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    lager:debug("handling conference event ~s", [get_response_key(EventJObj)]),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context)
                               ,get_response_key(EventJObj)
                               ,kz_json:normalize_jobj(EventJObj)
                               ).

-spec add_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
add_amqp_binding(<<"conference.command.", ConfId/binary>>, Context) ->
    AccountId = bh_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case ConfId of
        <<"*">> -> command_bind_all(AccountDb);
        ConfId -> maybe_command_bind(AccountDb, ConfId)
    end;
add_amqp_binding(<<"conference.event.", Binding/binary>>, Context) ->
    AccountId = bh_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case binary:split(Binding, <<".">>, ['global']) of
        [<<"*">>, <<"*">>] ->
            event_bind_all(AccountDb);
        [<<"*">>, _CallId] ->
            lager:debug("invalid conference event bind: ~s", [Binding]);
        [ConfId, CallId] ->
            maybe_event_bind(AccountDb, ConfId, CallId);
        _Else ->
            lager:debug("invalid conference event bind: ~s", [Binding])
    end;
add_amqp_binding(Binding, _Context) ->
    lager:debug("unmatched binding ~p", [Binding]).

-spec rm_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
rm_amqp_binding(<<"conference.command.", ConfId/binary>>, Context) ->
    AccountId = bh_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case ConfId of
        <<"*">> -> command_unbind_all(AccountDb);
        ConfId -> command_unbind(ConfId)
    end;
rm_amqp_binding(<<"conference.event.", Binding/binary>>, Context) ->
    AccountId = bh_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case binary:split(Binding, <<".">>, ['global']) of
        [<<"*">>, <<"*">>] ->
            event_unbind_all(AccountDb);
        [<<"*">>, _CallId] ->
            lager:debug("invalid conference event bind: ~s", [Binding]);
        [ConfId, CallId] ->
            event_unbind(ConfId, CallId);
        _Else ->
            lager:debug("invalid conference event bind: ~s", [Binding])
    end;
rm_amqp_binding(Binding, _Context) ->
    lager:debug("unmatched binding ~p", [Binding]).

%%%===================================================================
%%% Internal functions
%%%==================================================================

-spec get_response_key(kz_json:object()) -> ne_binary().
get_response_key(JObj) ->
    kz_json:get_first_defined([<<"Application-Name">>, <<"Event-Name">>], JObj).

-spec command_binding_options(ne_binary()) -> kz_proplist().
command_binding_options(ConfId) ->
    [{'conference', ConfId}
    ,{'restrict_to', ['command']}
    ,'federate'
    ].

-spec event_binding_options(ne_binary(), ne_binary()) -> kz_proplist().
event_binding_options(ConfId, CallId) ->
    [{'conference', {ConfId, CallId}}
    ,{'restrict_to', ['event']}
    ,'federate'
    ].

%% commands

command_bind(ConfId) ->
    blackhole_listener:add_binding('conference', command_binding_options(ConfId)).

command_unbind(ConfId) ->
    blackhole_listener:remove_binding('conference', command_binding_options(ConfId)).

command_bind_all(AccountDb) ->
    _ = [ command_bind(ConfId) || ConfId <- all_conferences(AccountDb) ].

command_unbind_all(AccountDb) ->
    _ = [ command_unbind(ConfId) || ConfId <- all_conferences(AccountDb) ].

maybe_command_bind(AccountDb, ConfId) ->
    case kz_datamgr:open_doc(AccountDb, ConfId) of 
        {'ok', _JObj} ->
            command_bind(ConfId);
        _ ->
            lager:error("attempt to bind to non-existent conference ~p", ConfId)
    end.

%% events

event_bind(ConfId, CallId) ->
    lager:error("bind:~p ~p", [ConfId, CallId]),
    blackhole_listener:add_binding('conference', event_binding_options(ConfId, CallId)),
    blackhole_bindings:bind(kz_util:join_binary([<<"conference.event">>, ConfId, CallId], <<".">>), ?MODULE, 'handle_event').

event_unbind(ConfId, CallId) ->
    blackhole_listener:remove_binding('conference', event_binding_options(ConfId, CallId)).

event_bind_all(AccountDb) ->
    _ = [ event_bind(ConfId, <<"*">>) || ConfId <- all_conferences(AccountDb) ].

event_unbind_all(AccountDb) ->
    _ = [ event_unbind(ConfId, <<"*">>) || ConfId <- all_conferences(AccountDb) ].

maybe_event_bind(AccountDb, ConfId, CallId) ->
    case kz_datamgr:open_doc(AccountDb, ConfId) of 
        {'ok', _JObj} ->
            event_bind(ConfId, CallId);
        _ ->
            lager:error("attempt to bind to non-existent conference ~p", ConfId)
    end.

%% other

all_conferences(AccountDb) ->
    case kz_datamgr:get_all_results(AccountDb, <<"conferences/crossbar_listing">>) of
        {'error', _R} -> [];
        {'ok', JObjs} -> [ kz_doc:id(JObj) || JObj <- JObjs ]
    end.