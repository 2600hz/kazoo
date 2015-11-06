%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(milliwatt_route_req).

-export([handle_req/2]).

-include("milliwatt.hrl").

-define(CONFLICT_ACTION, 'tone').

-define(TONE, wh_json:from_list(
                [{<<"caller_id">>,[]}
                ,{<<"number">>,[<<"5555555551">>]}
                ])
       ).
-define(ECHO, wh_json:from_list(
                [{<<"caller_id">>,[]}
                ,{<<"number">>,[<<"5555555552">>]}
                ])
       ).

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3000).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
-define(ROUTE_WIN_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

handle_req(JObj, _Props) ->
    'true' = wapi_route:req_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    wh_util:put_callid(CallId),
    Call = whapps_call:from_route_req(JObj),

    %% do magic to determine if we should respond...
    case tone_or_echo(Call) of
        'undefined' ->
            lager:debug("milliwatt does not know what to do with this!", []);
        Action ->
            send_route_response(Action, JObj, Call)
    end.

-spec send_route_response(atom(), wh_json:json(), whapps_call:call()) -> 'ok'.
send_route_response(Action, JObj, Call) ->
    lager:info("milliwatt knows how to route the call! sending park response"),
    Resp = props:filter_undefined([{?KEY_MSG_ID, wh_api:msg_id(JObj)}
              ,{?KEY_MSG_REPLY_ID, whapps_call:call_id_direct(Call)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_api:server_id(JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    case wh_amqp_worker:call(Resp
                             ,Publisher
                             ,fun wapi_route:win_v/1
                             ,?ROUTE_WIN_TIMEOUT
                            )
    of
        {'ok', RouteWin} ->
            lager:info("milliwatt has received a route win"),
            execute_action(Action, whapps_call:from_route_win(RouteWin, Call));
        {'error', _E} ->
            lager:info("callflow didn't received a route win, exiting : ~p", [_E])
    end.

-spec execute_action(atom(), whapps_call:call()) -> 'ok'.
execute_action('tone', Call) ->
    lager:info("milliwatt will execute action tone", []),
    milliwatt_tone:exec(Call);
execute_action('echo', Call) ->
    lager:info("milliwatt will execute action echo", []),
    milliwatt_echo:exec(Call);
execute_action(Action, Call) ->
    lager:warning("milliwatt doesnt know action: ~p", [Action]),
    whapps_call_command:hangup(Call).



-spec tone_or_echo(whapps_call:call()) -> 'echo' | 'tone' | 'undefined'.
tone_or_echo(Call) ->
    CallJObj = whapps_call:to_json(Call),
    From = wh_json:get_binary_value(<<"Caller-ID-Number">>, CallJObj, <<>>),
    To = wh_json:get_binary_value(<<"To-User">>, CallJObj, <<>>),

    case {whapps_config:get_non_empty(<<"milliwatt">>, <<"echo">>, ?ECHO)
         ,whapps_config:get_non_empty(<<"milliwatt">>, <<"tone">>, ?TONE)} of
        {'undefined', 'undefined'} -> 'undefined';
        {Echo, 'undefined'} ->
            maybe_echo(Echo, To, From);
        {'undefined', Tone} ->
            maybe_tone(Tone, To, From);
        {Echo, Tone} ->
            maybe_echo_maybe_tone(Echo, Tone, To, From)
    end.

-spec maybe_echo(wh_json:json(), ne_binary(), ne_binary()) -> 'undefined' | 'echo'.
maybe_echo(Echo, To, From) ->
    case rule_exist(Echo, To, From) of
        'true' -> 'echo';
        'false' -> 'undefined'
    end.

-spec maybe_tone(wh_json:json(), ne_binary(), ne_binary()) -> 'undefined' | 'tone'.
maybe_tone(Tone, To, From) ->
    case rule_exist(Tone, To, From) of
        'true' -> 'tone';
        'false' -> 'undefined'
    end.

-spec maybe_echo_maybe_tone(wh_json:json(), wh_json:json(), ne_binary(), ne_binary()) -> 'undefined' | 'tone' | 'echo'.
maybe_echo_maybe_tone(Echo, Tone, To, From) ->
    case {rule_exist(Echo, To, From)
         ,rule_exist(Tone, To, From)} of
        {'true', 'true'} ->
            lager:info("conflict on milliwatt actions switching back to default action: ~p", [?CONFLICT_ACTION]),
            ?CONFLICT_ACTION;
        {'true', _} -> 'echo';
        {_, 'true'} -> 'tone';
        _ -> 'undefined'
    end.

-spec rule_exist(wh_json:json(), ne_binary(), ne_binary()) -> boolean().
rule_exist(JObj, To, From) ->
    CallerIds = wh_json:get_ne_value(<<"caller_id">>, JObj, []),
    Numbers = wh_json:get_ne_value(<<"number">>, JObj, []),
    case {lists:member(To, Numbers)
         ,lists:member(From, CallerIds)} of
        {'false', 'false'} -> 'false';
        _ -> 'true'
    end.
