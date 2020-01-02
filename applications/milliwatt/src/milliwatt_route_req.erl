%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(milliwatt_route_req).

-export([handle_req/2]).

-include("milliwatt.hrl").

-define(CONFLICT_ACTION, 'tone').

-define(ECHO, kz_json:from_list(
                [{<<"caller_id">>,[]}
                ,{<<"number">>,[<<"5555555552">>]}
                ])
       ).

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3000).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
-define(ROUTE_WIN_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_route:req_v(JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    kz_log:put_callid(CallId),
    Call = kapps_call:from_route_req(JObj),

    %% do magic to determine if we should respond...
    case tone_or_echo(Call) of
        'undefined' ->
            lager:debug("milliwatt does not know what to do with this!", []);
        Action ->
            send_route_response(Action, JObj, Call)
    end.

-spec send_route_response(atom(), kz_json:object(), kapps_call:call()) -> 'ok'.
send_route_response(Action, JObj, Call) ->
    lager:info("milliwatt knows how to route the call! sending park response"),
    Resp = props:filter_undefined([{?KEY_MSG_ID, kz_api:msg_id(JObj)}
                                  ,{?KEY_MSG_REPLY_ID, kapps_call:call_id_direct(Call)}
                                  ,{<<"Routes">>, []}
                                  ,{<<"Method">>, <<"park">>}
                                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_route:publish_resp(ServerId, P) end,
    case kz_amqp_worker:call(Resp
                            ,Publisher
                            ,fun kapi_route:win_v/1
                            ,?ROUTE_WIN_TIMEOUT
                            )
    of
        {'ok', RouteWin} ->
            lager:info("milliwatt has received a route win"),
            execute_action(Action, kapps_call:from_route_win(RouteWin, Call));
        {'error', _E} ->
            lager:info("callflow didn't received a route win, exiting : ~p", [_E])
    end.

-spec execute_action(atom(), kapps_call:call()) -> 'ok'.
execute_action('tone', Call) ->
    lager:info("milliwatt will execute action tone", []),
    milliwatt_tone:exec(Call);
execute_action('echo', Call) ->
    lager:info("milliwatt will execute action echo", []),
    milliwatt_echo:exec(Call).


-spec tone_or_echo(kapps_call:call()) -> 'echo' | 'tone' | 'undefined'.
tone_or_echo(Call) ->
    CallJObj = kapps_call:to_json(Call),
    From = kz_json:get_binary_value(<<"Caller-ID-Number">>, CallJObj, <<>>),
    To = kz_json:get_binary_value(<<"To-User">>, CallJObj, <<>>),
    case {kapps_config:get_json(?CONFIG_CAT, <<"echo">>, ?ECHO)
         ,?TONE
         }
    of
        {'undefined', 'undefined'} -> 'undefined';
        {Echo, 'undefined'} ->
            maybe_echo(Echo, To, From);
        {'undefined', Tone} ->
            maybe_tone(Tone, To, From);
        {Echo, Tone} ->
            maybe_echo_maybe_tone(Echo, Tone, To, From)
    end.

-spec maybe_echo(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'undefined' | 'echo'.
maybe_echo(Echo, To, From) ->
    case rule_exist(Echo, To, From) of
        'true' -> 'echo';
        'false' -> 'undefined'
    end.

-spec maybe_tone(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'undefined' | 'tone'.
maybe_tone(Tone, To, From) ->
    case rule_exist(Tone, To, From) of
        'true' -> 'tone';
        'false' -> 'undefined'
    end.

-spec maybe_echo_maybe_tone(kz_json:object(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'undefined' | 'tone' | 'echo'.
maybe_echo_maybe_tone(Echo, Tone, To, From) ->
    case {rule_exist(Echo, To, From)
         ,rule_exist(Tone, To, From)
         }
    of
        {'true', 'true'} ->
            lager:info("conflict on milliwatt actions switching back to default action: ~p", [?CONFLICT_ACTION]),
            ?CONFLICT_ACTION;
        {'true', _} -> 'echo';
        {_, 'true'} -> 'tone';
        _ -> 'undefined'
    end.

-spec rule_exist(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
rule_exist(JObj, To, From) ->
    CallerIds = kz_json:get_ne_value(<<"caller_id">>, JObj, []),
    Numbers = kz_json:get_ne_value(<<"number">>, JObj, []),

    (not
       (not lists:member(To, Numbers))
     andalso (not lists:member(From, CallerIds))
    ).
