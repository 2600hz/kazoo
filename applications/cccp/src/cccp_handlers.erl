%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(cccp_handlers).

-export([handle_route_req/2
        ,handle_route_win/2
        ,handle_config_change/2
        ]).

-include("cccp.hrl").

-spec handle_route_req(kz_json:object(), kz_proplist()) -> any().
handle_route_req(JObj, Props) ->
    'true' = kapi_route:req_v(JObj),

    Call = kapps_call:from_route_req(JObj),
    CB_Number = knm_converters:normalize(kapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cb_number">>)),
    CC_Number = knm_converters:normalize(kapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cc_number">>)),
    case knm_converters:normalize(kapps_call:request_user(Call)) of
        CB_Number -> park_call(JObj, Props, Call);
        CC_Number -> park_call(JObj, Props, Call);
        _ -> 'ok'
    end.

-spec park_call(kz_json:object(), kz_proplist(), kapps_call:call()) -> 'ok'.
park_call(JObj, Props, Call) ->
    Q = props:get_value('queue', Props),
    Resp = props:filter_undefined([{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
                                  ,{<<"Method">>, <<"park">>}
                                   | kz_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = kz_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> kapi_route:publish_resp(ServerId, P) end,
    kapps_util:amqp_pool_send(Resp, Publisher),
    kapps_call:cache(Call, ?APP_NAME).

-spec handle_route_win(kz_json:object(), kz_proplist()) -> 'ok'.
handle_route_win(JObj, _Props) ->
    'true' = kapi_route:win_v(JObj),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    lager:info("cccp has received a route win, taking control of the call CallId: ~p",[CallId]),
    case kapps_call:retrieve(CallId, ?APP_NAME) of
        {'ok', Call} ->
            handle_cccp_call(kapps_call:from_route_win(JObj, Call));
        {'error', _R} ->
            lager:debug("Unable to find call record during route_win")
    end.

-spec handle_config_change(kz_json:object(), kz_proplist()) -> 'ok'.
handle_config_change(_JObj, _Props) ->
    'ok'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_cccp_call(kapps_call:call()) -> 'ok'.
handle_cccp_call(Call) ->
    CB_Number = knm_converters:normalize(kapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cb_number">>)),
    CC_Number = knm_converters:normalize(kapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cc_number">>)),
    case knm_converters:normalize(kapps_call:request_user(Call)) of
        CB_Number ->
            handle_callback(Call);
        CC_Number ->
            cccp_platform_sup:new(Call)
    end.

-spec handle_callback(kapps_call:call()) -> 'ok'.
handle_callback(Call) ->
    CallerNumber = knm_converters:normalize(kapps_call:caller_id_number(Call)),
    case cccp_util:authorize(CallerNumber, <<"cccps/cid_listing">>) of
        {'ok', AuthJObj} ->
            maybe_call_back(AuthJObj, Call);
        E ->
            lager:info("No caller information found for ~p. Won't call it back. (~p)", [CallerNumber, E])
    end.

-spec maybe_call_back(kz_json:object(), kapps_call:call()) -> 'ok'.
maybe_call_back(JObj, Call) ->
    AccountId = kz_json:get_value(<<"account_id">>, JObj),
    UserId = kz_json:get_value(<<"user_id">>, JObj),
    MaxConcurentCallsPerUser = kz_json:get_integer_value(<<"max_concurent_calls_per_user">>, JObj, 1),
    case (cccp_util:count_user_legs(UserId, AccountId) >= MaxConcurentCallsPerUser * 2) of
        'true' ->
            Media = kz_media_util:get_prompt(<<"cf-move-too_many_channels">>, Call),
            kapps_call_command:response(<<"486">>, <<"User busy">>, Media, Call);
        'false' ->
            kapps_call_command:hangup(Call),
            Values = [{<<"a_leg_name">>, kapps_call:caller_id_name(Call)}
                     ,{<<"a_leg_number">>, knm_converters:normalize(kapps_call:caller_id_number(Call))}
                     ],
            cccp_callback_sup:new(kz_json:set_values(Values, JObj))
    end.
