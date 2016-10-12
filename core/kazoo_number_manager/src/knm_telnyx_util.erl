%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz INC
%%% @doc
%%%
%%%
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_telnyx_util).

-export([creds/0]).
-export([req/2, req/3]).

-include("knm.hrl").

-define(CARRIER, 'knm_telnyx').

-define(USER, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"user">>)).
-define(TOKEN, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"token">>)).

-define(DOMAIN, "api.telnyx.com").
-define(URL(Path), "https://" ?DOMAIN "/origination/" ++ filename:join(Path)).

-define(SHOULD_KEEP_BEST_EFFORT,
        kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"should_keep_best_effort">>, 'false')).


-spec creds() -> {ne_binary(), ne_binary()}.
creds() ->
    {?USER, ?TOKEN}.


-spec req(atom(), [nonempty_string()]) -> kz_json:object().
-spec req(atom(), [nonempty_string()], kz_json:object()) -> kz_json:object().
req(Method, Path) ->
    req(Method, Path, kz_json:new()).

-ifdef(TEST).
req('post', ["number_searches"], JObj) ->
    case kz_json:get_value([<<"search_descriptor">>, <<"prefix">>], JObj) of
        <<"800">> -> rep_fixture("telnyx_tollfree_search_12.json");
        _ -> rep_fixture("telnyx_npa_search_12.json")
    end;
req('get', ["number_searches", "411384989406463698"], _) ->
    rep_fixture("telnyx_tollfree_search_22.json");
req('get', ["number_searches", "411381763818915536"], _) ->
    rep_fixture("telnyx_npa_search_22.json");
req('post', ["number_orders"], _) ->
    rep_fixture("telnyx_order.json").

rep_fixture(Fixture) ->
    rep({'ok', 200, [], list_to_binary(knm_util:fixture(Fixture))}).

-else.
req('get'=_Method, Path, EmptyJObj) ->
    Url = ?URL(Path),
    Headers = http_headers(EmptyJObj),
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n", [_Method, Url, Headers]),
    Resp = kz_http:get(Url, Headers, http_options()),
    rep(Resp);
req('post'=_Method, Path, JObj) ->
    Url = ?URL(Path),
    Headers = http_headers(JObj),
    Body = kz_json:encode(JObj),
    ?DEBUG_APPEND("Request:~n~s ~s~n~p~n~s~n", [_Method, Url, Headers, Body]),
    Resp = kz_http:post(Url, Headers, Body, http_options()),
    rep(Resp).

http_headers(BodyJObj) ->
    {User, Token} = knm_telnyx_util:creds(),
    [{"Accept", "application/json"}
    ,{"x-api-user", binary_to_list(User)}
    ,{"x-api-token", binary_to_list(Token)}
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
rep({'ok', 200=Code, _Headers, <<"{",_/binary>>=Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [Code, _Headers, Response]),
    lager:debug("received response"),
    maybe_apply_limit(
      maybe_remove_best_effort(?SHOULD_KEEP_BEST_EFFORT, kz_json:decode(Response))
     );
rep({'ok', Code, _Headers, _Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [Code, _Headers, _Response]),
    Reason = http_code(Code),
    lager:debug("request error: ~p (~s)", [Code, Reason]),
    knm_errors:by_carrier(?CARRIER, Reason, <<>>);
rep({'error', R}=_E) ->
    lager:debug("request error: ~p", [_E]),
    knm_errors:by_carrier(?CARRIER, kz_util:to_binary(R), <<>>).

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

-spec maybe_apply_limit(kz_json:object()) -> kz_json:object().
maybe_apply_limit(JObj) ->
    Limit = kz_json:get_integer_value(<<"limit">>, JObj, 0),
    Result = take(Limit, kz_json:get_value(<<"result">>, JObj, [])),
    kz_json:set_value(<<"result">>, Result, JObj).

-spec take(non_neg_integer(), list()) -> list().
take(0, _) -> [];
take(N, L)
  when is_integer(N), N > 0 ->
    lager:debug("asked for ~p results, got ~p", [N, length(L)]),
    lists:sublist(L, N).

%%% End of Module
