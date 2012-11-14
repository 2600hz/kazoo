%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_authz_req).

-export([handle_req/2]).

-include("jonny5.hrl").

-type(authz_resp() :: {'ok', 'under_hard_limit' | 'flat_rate' | 'per_minute'} |
                      {'error', _}).

-spec handle_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_req(JObj, Props) ->
    true = wapi_authz:req_v(JObj),
    wh_util:put_callid(JObj),

    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Limits = j5_util:get_limits(AccountId),

    Routines = [fun maybe_ignore_limits/3
                ,fun maybe_hard_limit/3
                ,fun maybe_flat_rate/3
                ,fun maybe_per_minute/3
               ],

    send_resp(JObj
              ,props:get_value(queue, Props)
              ,Limits
              ,lists:foldl(fun(F, A) -> 
                                   F(A, Limits, JObj) end
                           ,{error, not_processed}, Routines)
             ).

-spec maybe_ignore_limits/3 :: (authz_resp(), #limits{}, wh_json:json_object()) -> authz_resp().
maybe_ignore_limits(_, #limits{enabled=true}, _) ->
    {ok, limits_enabled};
maybe_ignore_limits(_, #limits{enabled=false}, _) ->
    {ok, limits_disabled}.

-spec maybe_hard_limit/3 :: (authz_resp(), #limits{}, wh_json:json_object()) -> authz_resp().
maybe_hard_limit({ok, limits_enabled}, Limits, JObj) ->
    case j5_hard_limit:is_under(Limits, JObj) of
        true -> {ok, under_hard_limits};
        false -> {error, hard_limit}
    end;
maybe_hard_limit(Else, _, _) -> Else.

-spec maybe_flat_rate/3 :: (authz_resp(), #limits{}, wh_json:json_object()) -> authz_resp().
maybe_flat_rate({ok, under_hard_limits}, Limits, JObj) -> 
    case j5_flat_rate:is_available(Limits, JObj) of
        true -> {ok, flat_rate};
        false -> {error, flat_rate_limit}
    end;
maybe_flat_rate(Else, _, _) -> Else.

-spec maybe_per_minute/3 :: (authz_resp(), #limits{}, wh_json:json_object()) -> authz_resp().
maybe_per_minute({error, flat_rate_limit}, Limits, JObj) ->
    case j5_credit:is_available(Limits, JObj) of
        true -> {ok, per_minute};
        false -> {error, per_minute_limit}
    end;
maybe_per_minute(Else, _, _) -> Else.

-spec send_resp/4 :: (wh_json:json_object(),  ne_binary(), #limits{}, {'ok', 'credit' | 'flatrate'} | {'error', _}) -> 'ok'.
send_resp(JObj, Q, Limits, {error, _R}) ->
    lager:debug("call is unauthorize due to ~s", [_R]),
    j5_util:send_system_alert(<<"no flat rate or credit">>, JObj, Limits),
    Resp = [{<<"Is-Authorized">>, <<"false">>}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_authz:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp);
send_resp(JObj, Q, _, {ok, Type}) ->
    lager:debug("call is authorized as ~s", [Type]),
    Resp = [{<<"Is-Authorized">>, <<"true">>}
            ,{<<"Type">>, wh_util:to_binary(Type)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_authz:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).
