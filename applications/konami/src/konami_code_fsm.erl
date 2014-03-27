%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_code_fsm).

-behaviour(gen_fsm).

%% API
-export([start_fsm/2
         ,dtmf/3
        ]).

%% gen_fsm callbacks
-export([init/1

         ,unarmed/2, unarmed/3
         ,armed/2, armed/3

         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

-include("konami.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-record(state, {numbers :: api_object()
                ,patterns :: api_object()
                ,binding_key = konami_config:binding_key() :: ne_binary()
                ,digit_timeout = konami_config:timeout() :: pos_integer()
                ,collected_dtmf = <<>> :: binary()
                ,call :: whapps_call:call()
               }).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_fsm(whapps_call:call(), wh_json:object()) -> any().
start_fsm(Call, JObj) ->
    wh_hooks:register(whapps_call:account_id(Call), <<"CHANNEL_DESTROY">>),
    gen_fsm:enter_loop(?MODULE, [], 'unarmed'
                       ,#state{numbers=numbers(Call, JObj)
                               ,patterns=patterns(Call, JObj)
                               ,binding_key=binding_key(Call, JObj)
                               ,digit_timeout=digit_timeout(Call, JObj)
                               ,call=Call
                              }).

-spec dtmf(pid(), ne_binary(), ne_binary()) -> 'ok'.
dtmf(FSM, CallId, DTMF) ->
    gen_fsm:send_event(FSM, {'dtmf', CallId, DTMF}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
    {'ok', 'unarmed', #state{}}.

unarmed(_Event, State) ->
    lager:debug("unhandled unarmed/2: ~p", [_Event]),
    {'next_state', 'unarmed', State}.

unarmed(_Event, _From, State) ->
    lager:debug("unhandled unarmed/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'unarmed', State}.

armed(_Event, State) ->
    lager:debug("unhandled armed/2: ~p", [_Event]),
    {'next_state', 'armed', State}.

armed(_Event, _From, State) ->
    lager:debug("unhandled armed/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'armed', State}.

handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

handle_info(?HOOK_EVT(_AccountId, <<"CHANNEL_DESTROY">>, Evt), StateName, #state{call=Call}=State) ->
    case wh_json:get_value(<<"Call-ID">>, Evt) =:= whapps_call:call_id_direct(Call) of
        'true' ->
            lager:debug("recv channel_destroy while in ~s, going down", [StateName]),
            {'stop', 'normal', State};
        'false' ->
            {'next_state', StateName, State}
    end;
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

terminate(_Reason, _StateName, _State) ->
    lager:debug("fsm terminating while in ~s: ~p", [_StateName, _Reason]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec binding_key(whapps_call:call(), wh_json:object()) -> ne_binary().
binding_key(Call, JObj) ->
    case wh_json:get_value(<<"Binding-Key">>, JObj) of
        'undefined' ->
            konami_config:binding_key(whapps_call:account_id(Call));
        BindingKey ->
            lager:debug("using custom binding key '~s'", [BindingKey]),
            BindingKey
    end.

-spec numbers(whapps_call:call(), wh_json:object()) -> wh_json:object().
numbers(Call, JObj) ->
    case wh_json:get_value(<<"Numbers">>, JObj) of
        'undefined' -> konami_config:numbers(whapps_call:account_id(Call));
        Numbers -> Numbers
    end.

-spec patterns(whapps_call:call(), wh_json:object()) -> wh_json:object().
patterns(Call, JObj) ->
    case wh_json:get_value(<<"Patterns">>, JObj) of
        'undefined' -> konami_config:patterns(whapps_call:account_id(Call));
        Patterns -> Patterns
    end.

-spec digit_timeout(whapps_call:call(), wh_json:object()) -> pos_integer().
digit_timeout(Call, JObj) ->
    case wh_json:get_integer_value(<<"Digit-Timeout">>, JObj) of
        'undefined' -> konami_config:timeout(whapps_call:account_id(Call));
        Timeout -> Timeout
    end.

