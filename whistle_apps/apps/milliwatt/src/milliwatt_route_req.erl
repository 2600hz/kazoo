-module(milliwatt_route_req).


-export([handle_req/2]).

-include("milliwatt.hrl").

-define(CONFLICT_ACTION, 'tone').

handle_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put('callid', CallId),
    Call = whapps_call:from_route_req(JObj),

    %% do magic to determine if we should respond...
    %% update the call kvs with which module to use (tone or echo)
    case tone_or_echo(Call) of
        'undefined' ->
            lager:debug("milliwatt does not know what to do with this!", []);
        Action ->
            ControllerQ = props:get_value('queue', Props),
            send_route_response(ControllerQ, JObj),
            UpdatedCall = whapps_call:kvs_store(<<"milliwatt_action">>, Action, Call),
            whapps_call:cache(UpdatedCall)
    end.

-spec send_route_response(ne_binary(), wh_json:json()) -> 'ok'.
send_route_response(ControllerQ, JObj) ->
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("milliwatt knows how to route the call! sent park response").

-spec tone_or_echo(whapps_call:call()) -> atom().
tone_or_echo(Call) ->
    CallJObj = whapps_call:to_json(Call),
    From = wh_json:get_binary_value(<<"Caller-ID-Number">>, CallJObj, <<>>),
    To = wh_json:get_binary_value(<<"To-User">>, CallJObj, <<>>),

    case {whapps_config:get_non_empty(<<"milliwatt">>, <<"echo">>)
         ,whapps_config:get_non_empty(<<"milliwatt">>, <<"tone">>)} of
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






