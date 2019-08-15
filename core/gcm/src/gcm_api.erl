%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gcm_api).
-export([push/3]).

-define(BASEURL, "https://android.googleapis.com/gcm/send").

-type header()  :: {string(), string()}.
-type headers() :: [header(),...].
-type regids()  :: [binary(),...].
-type message() :: [tuple(),...].
-type result()  :: {number(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [any()]}.

-spec push(regids(),message(),string()) -> {'error',any()} | {'noreply','unknown'} | {'ok',result()}.
push(RegIds, Message, Key) ->
    JObj = kz_json:set_value(<<"registration_ids">>, RegIds, Message),
    Request = kz_json:encode(JObj),
    ApiKey = string:concat("key=", Key),

    try httpc:request(post, {?BASEURL, [{"Authorization", ApiKey}], "application/json", Request}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, kz_json:decode(Body)};
        {ok, {{_, 400, _}, _, _}} -> {error, json_error};
        {ok, {{_, 401, _}, _, _}} -> {error, auth_error};
        {ok, {{_, Code, _}, Headers, _}} when Code >= 500,
                                              Code =< 599 ->
            RetryTime = retry_after_from(Headers),
            {error, {retry, RetryTime}};
        {ok, {{_StatusLine, _, _}, _, _Body}} ->
            {error, timeout};
        {error, _Reason} = Error ->
            Error;
        _OtherError ->
            {noreply, unknown}
    catch
        Exception ->
            {error, Exception}
    end.



-spec retry_after_from(headers()) -> 'no_retry' | non_neg_integer().
retry_after_from(Headers) ->
    case props:get_value("retry-after", Headers) of
        undefined ->
            no_retry;
        RetryTime ->
            case string:to_integer(RetryTime) of
                {Time, _} when is_integer(Time) ->
                    Time;
                {error, no_integer} ->
                    Date = qdate:to_unixtime(RetryTime),
                    Date - qdate:unixtime()
            end
    end.
