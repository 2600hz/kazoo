%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_telnyx_util).

-export([did/1]).
-export([req/2, req/3]).

-include("knm.hrl").

-define(MOD_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".telnyx">>).

-define(CARRIER, 'knm_telnyx').

-ifdef(TEST).
-define(DEBUG_WRITE(_Format, _Args), 'ok').
-define(DEBUG_APPEND(_Format, _Args), 'ok').
-else.
-define(DEBUG, kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"debug">>, 'false')).
-define(DEBUG_FILE, "/tmp/telnyx.json").
-define(DEBUG_WRITE(Format, Args),
        _ = ?DEBUG
        andalso file:write_file(?DEBUG_FILE, io_lib:format(Format, Args))
       ).
-define(DEBUG_APPEND(Format, Args),
        _ = ?DEBUG
        andalso file:write_file(?DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).
-endif.

-define(SHOULD_KEEP_BEST_EFFORT
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"should_keep_best_effort">>, 'false')
       ).

-define(SHOULD_FILTER_RATES
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"should_filter_rates">>, 'false')
       ).

-define(USER, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"user">>)).
-define(TOKEN, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"token">>)).

-define(DOMAIN, "api.telnyx.com").
-define(URL(Path), "https://" ?DOMAIN "/origination/" ++ filename:join(Path)).

%%------------------------------------------------------------------------------
%% @doc Turns +13129677542 into %2B13129677542.
%% @end
%%------------------------------------------------------------------------------
-spec did(knm_number:knm_number()) -> nonempty_string().
did(Number) ->
    binary_to_list(
      kz_http_util:urlencode(
        knm_phone_number:number(
          knm_number:phone_number(Number)))).


-spec req(atom(), [nonempty_string()]) -> kz_json:object().
req(Method, Path) ->
    req(Method, Path, kz_json:new()).

-ifdef(TEST).

-spec req(atom(), [nonempty_string()], kz_json:object()) -> kz_json:object().
req('post', ["number_searches"], JObj) ->
    case kz_json:get_value([<<"search_descriptor">>, <<"prefix">>], JObj) of
        <<"800">> -> rep_fixture("telnyx_tollfree_search.json");
        'undefined' ->
            case kz_json:get_value([<<"search_descriptor">>, <<"country_iso">>], JObj) of
                <<"GB">> -> rep_fixture("telnyx_international_search.json");
                'undefined' -> rep_fixture("telnyx_npa_search.json")
            end
    end;
req('post', ["e911_addresses"], Body) ->
    <<"301 MARINA BLVD">> = kz_json:get_value(<<"line_1">>, Body),
    rep_fixture("telnyx_create_e911.json");
req('post', ["number_orders"], _) ->
    rep_fixture("telnyx_order.json");
req('put', ["numbers", "%2B1"++_, "e911_settings"], _) ->
    rep_fixture("telnyx_activate_e911.json");
req('put', ["numbers", "%2B1"++_], Body) ->
    EnableCNAM = <<"enable_caller_id_name">>,
    case kz_json:is_true(EnableCNAM, Body) of
        false -> rep({ok, 200, [], kz_json:encode(kz_json:from_list([{EnableCNAM, false}]))});
        true ->
            case kz_json:get_ne_binary_value(<<"cnam_listing_details">>, Body) of
                undefined -> rep_fixture("telnyx_activate_cnam_inbound.json");
                _ -> rep_fixture("telnyx_activate_cnam_outbound.json")
            end
    end;
req('delete', ["e911_addresses", "421570676474774685"], _) ->
    rep_fixture("telnyx_delete_e911.json").

rep_fixture(Fixture) ->
    rep({'ok', 200, [], list_to_binary(knm_util:fixture(Fixture))}).

-else.

-spec req(atom(), [nonempty_string()], kz_json:object()) -> kz_json:object().
req('delete'=_Method, Path, EmptyJObj) ->
    Url = ?URL(Path),
    Headers = http_headers(EmptyJObj),
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n", [_Method, Url, Headers]),
    Resp = kz_http:delete(Url, Headers, <<>>, http_options()),
    rep(Resp);
req('post'=_Method, Path, JObj) ->
    Url = ?URL(Path),
    Headers = http_headers(JObj),
    Body = kz_json:encode(JObj),
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n~s~n", [_Method, Url, Headers, Body]),
    Resp = kz_http:post(Url, Headers, Body, http_options()),
    rep(Resp);
req('put'=_Method, Path, JObj) ->
    Url = ?URL(Path),
    Headers = http_headers(JObj),
    Body = kz_json:encode(JObj),
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n~s~n", [_Method, Url, Headers, Body]),
    Resp = kz_http:put(Url, Headers, Body, http_options()),
    rep(Resp).

http_headers(BodyJObj) ->
    [{"Accept", "application/json"}
    ,{"x-api-user", binary_to_list(?USER)}
    ,{"x-api-token", binary_to_list(?TOKEN)}
    ,{"User-Agent", ?KNM_USER_AGENT}
     | [{"Content-Type", "application/json"} || not kz_json:is_empty(BodyJObj)]
    ].

http_options() ->
    [{'ssl', [{'verify', 'verify_none'}]}
    ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
    ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
    ].
-endif.

%%% Internals

-spec rep(kz_http:ret()) -> kz_json:object().
rep({'ok', 200=_Code, _Headers, <<"{",_/binary>>=Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [_Code, _Headers, Response]),

    Routines = [fun(JObj) -> maybe_remove_best_effort(?SHOULD_KEEP_BEST_EFFORT, JObj) end
               ,fun(JObj) -> maybe_filter_rates(?SHOULD_FILTER_RATES, JObj) end
               ],

    maybe_apply_limit(
      lists:foldl(fun(F, J) -> F(J) end, kz_json:decode(Response), Routines)
     );
rep({'ok', Code, _Headers, _Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [Code, _Headers, _Response]),
    Reason = http_code(Code),
    lager:warning("request error: ~p (~s)", [Code, Reason]),
    lager:debug("response: ~s", [_Response]),
    knm_errors:by_carrier(?CARRIER, Reason, <<>>);
rep({'error', R}=_E) ->
    lager:warning("request error: ~p", [_E]),
    knm_errors:by_carrier(?CARRIER, kz_term:to_binary(R), <<>>).

-spec http_code(pos_integer()) -> atom().
http_code(400) -> 'bad_request';
http_code(401) -> 'unauthenticated';
http_code(403) -> 'unauthorized';
http_code(404) -> 'not_found';
http_code(Code) when Code >= 500 -> 'server_error';
http_code(_Code) -> 'empty_response'.

-spec maybe_remove_best_effort(boolean(), kz_json:object()) -> kz_json:object().
maybe_remove_best_effort('true', JObj) -> JObj;
maybe_remove_best_effort('false', JObj) ->
    case kz_json:is_true(<<"any_best_effort">>, JObj) of
        'false' -> JObj;
        'true' ->
            Results = [Result
                       || Result <- kz_json:get_value(<<"result">>, JObj, []),
                          'true' =/= kz_json:is_true(<<"best_effort">>, Result)
                      ],
            kz_json:set_value(<<"result">>, Results, JObj)
    end.

-spec maybe_filter_rates(boolean(), kz_json:object()) -> kz_json:object().
maybe_filter_rates('false', JObj) -> JObj;
maybe_filter_rates('true', JObj) ->
    UpfrontCost = kapps_config:get_float(?MOD_CONFIG_CAT, <<"upfront_cost">>, 1.0),
    MonthlyRecurringCost = kapps_config:get_float(?MOD_CONFIG_CAT, <<"monthly_recurring_cost">>, 1.0),
    Results = [Result
               || Result <- kz_json:get_value(<<"result">>, JObj, []),
                  kz_json:get_float_value(<<"upfront_cost">>, Result) == UpfrontCost
                      andalso kz_json:get_float_value(<<"monthly_recurring_cost">>, Result) == MonthlyRecurringCost
              ],
    kz_json:set_value(<<"result">>, Results, JObj).

-spec maybe_apply_limit(kz_json:object()) -> kz_json:object().
maybe_apply_limit(JObj) ->
    maybe_apply_limit(maybe_apply_limit(JObj, <<"result">>)
                     ,<<"inexplicit_result">>
                     ).

-spec maybe_apply_limit(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_apply_limit(JObj, ResultField) ->
    Limit = kz_json:get_integer_value(<<"limit">>, JObj, 100),
    Result = take(Limit, kz_json:get_value(ResultField, JObj, [])),
    kz_json:set_value(ResultField, Result, JObj).

-spec take(non_neg_integer(), list()) -> list().
take(0, _) -> [];
take(N, L)
  when is_integer(N), N > 0 ->
    lager:debug("asked for ~p results, got ~p", [N, length(L)]),
    lists:sublist(L, N).

%%% End of Module
