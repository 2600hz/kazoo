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

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3 * ?MILLISECONDS_IN_SECOND).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
-define(ROUTE_WIN_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

-type action() :: 'tone' | 'echo'.

-spec handle_req(kapi_route:req(), kz_term:proplist()) -> 'ok'.
handle_req(RouteReq, _Props) ->
    'true' = kapi_route:req_v(RouteReq),
    kz_log:put_callid(RouteReq),
    Call = kapps_call:from_route_req(RouteReq),

    %% do magic to determine if we should respond...
    case tone_or_echo(Call) of
        'undefined' ->
            lager:debug("milliwatt does not know what to do with this!", []);
        Action ->
            send_route_response(RouteReq, Call, Action)
    end.

-spec send_route_response(kapi_route:req(), kapps_call:call(), action()) -> 'ok'.
send_route_response(RouteReq, Call, Action) ->
    lager:info("milliwatt knows how to route the call! sending park response"),
    Resp = props:filter_undefined([{?KEY_MSG_ID, kz_api:msg_id(RouteReq)}
                                  ,{?KEY_MSG_REPLY_ID, kapps_call:call_id_direct(Call)}
                                  ,{<<"Routes">>, []}
                                  ,{<<"Method">>, <<"park">>}
                                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = kz_api:server_id(RouteReq),
    Publisher = fun(P) -> kapi_route:publish_resp(ServerId, P) end,
    case kz_amqp_worker:call(Resp
                            ,Publisher
                            ,fun kapi_route:win_v/1
                            ,?ROUTE_WIN_TIMEOUT
                            )
    of
        {'ok', RouteWin} ->
            lager:info("milliwatt has received a route win"),
            execute_action(kapps_call:from_route_win(RouteWin, Call), Action);
        {'error', _E} ->
            lager:info("callflow didn't received a route win, exiting : ~p", [_E])
    end.

-spec execute_action(kapps_call:call(), action()) -> 'ok'.
execute_action(Call, 'tone') ->
    lager:info("milliwatt will execute action tone", []),
    milliwatt_tone:exec(Call);
execute_action(Call, 'echo') ->
    lager:info("milliwatt will execute action echo", []),
    milliwatt_echo:exec(Call).

-spec tone_or_echo(kapps_call:call()) -> action() | 'undefined'.
tone_or_echo(Call) ->
    From = kapps_call:caller_id_number(Call),
    To = kapps_call:to_user(Call),

    lager:debug("checking whether to handle call to ~s from ~s", [To, From]),

    case {?ECHO, ?TONE} of
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
          action() | 'undefined'.
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
rule_exist(ActionConfig, To, From) ->
    matching_number(ActionConfig, To)
        orelse matching_caller_id(ActionConfig, From).

-spec matching_number(kz_json:object(), kz_term:ne_binary()) -> boolean().
matching_number(ActionConfig, To) ->
    lists:member(To, kz_json:get_list_value(<<"number">>, ActionConfig, [])).

-spec matching_caller_id(kz_json:object(), kz_term:ne_binary()) -> boolean().
matching_caller_id(ActionConfig, From) ->
    lists:member(From, kz_json:get_list_value(<<"caller_id">>, ActionConfig, [])).
