-module(fcm_api).

-include("logger.hrl").

-export([push/3
        ,test_retry_after_from/1
        ]).

-define(HTTP_OPTIONS, [{body_format, binary}]).
-define(BASEURL, "https://fcm.googleapis.com/fcm/send").

-define(HEADERS(ApiKey), [{"Authorization", ApiKey}]).
-define(CONTENT_TYPE, "application/json").

-define(HTTP_REQUEST(ApiKey, Message),
        {?BASEURL, ?HEADERS(ApiKey), ?CONTENT_TYPE, Message}).

push(RegIds, Message, ApiKey) when is_list(Message) ->
    MessageMap = maps:from_list(Message),
    push(RegIds, MessageMap, ApiKey);

push(RegId, Message, ApiKey) when is_binary(RegId) ->
    push([RegId], Message, ApiKey);

push([RegId], Message, ApiKey) ->
    Request = maps:put(<<"to">>, RegId, Message),
    push(jsx:encode(Request), ApiKey);

push(RegIds, Message, ApiKey) ->
    Request = maps:put(<<"registration_ids">>, RegIds, Message),
    push(jsx:encode(Request), ApiKey).

push(Message, ApiKey) ->
    try httpc:request(post, ?HTTP_REQUEST(ApiKey, Message), [], ?HTTP_OPTIONS) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Json = jsx:decode(Body),
            ?INFO_MSG("Result was: ~p~n", [Json]),
            {ok, result_from(Json)};
        {ok, {{_, 400, _}, _, Body}} ->
            ?ERROR_MSG("Error in request. Reason was: Bad Request - ~p~n", [Body]),
            {error, Body};
        {ok, {{_, 401, _}, _, _}} ->
            ?ERROR_MSG("Error in request. Reason was: authorization error~n", []),
            {error, auth_error};
        {ok, {{_, Code, _}, Headers, _}} when Code >= 500
                                              andalso Code =< 599 ->
            RetryTime = retry_after_from(Headers),
            ?ERROR_MSG("Error in request. Reason was: retry. Will retry in: ~p~n", [RetryTime]),
            {error, {retry, RetryTime}};
        {ok, {{_StatusLine, _, _}, _, _Body}} ->
            ?ERROR_MSG("Error in request. Reason was: timeout~n", []),
            {error, timeout};
        {error, Reason} ->
            ?ERROR_MSG("Error in request. Reason was: ~p~n", [Reason]),
            {error, Reason};
        OtherError ->
            ?ERROR_MSG("Error in request. Reason was: ~p~n", [OtherError]),
            {noreply, unknown}
    catch
        Exception ->
            ?ERROR_MSG("Error in request. Exception ~p while calling URL: ~p~n", [Exception, ?BASEURL]),
            {error, Exception}
    end.

result_from(Json) ->
    {
      proplists:get_value(<<"multicast_id">>, Json),
      proplists:get_value(<<"success">>, Json),
      proplists:get_value(<<"failure">>, Json),
      proplists:get_value(<<"canonical_ids">>, Json),
      proplists:get_value(<<"results">>, Json)
    }.

retry_after_from(Headers) ->
    case proplists:get_value("retry-after", Headers) of
        undefined ->
            no_retry;
        RetryTime ->
            case string:to_integer(RetryTime) of
                {Time, _} when is_integer(Time) ->
                    Time;
                {error, no_integer} ->
                    Now = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(erlang:timestamp())),
                    Date = calendar:datetime_to_gregorian_seconds(get_datetime(RetryTime)),
                    Date - Now
            end
    end.
-spec test_retry_after_from(any()) -> integer().
test_retry_after_from(RetryAfter) ->
    retry_after_from([{"retry-after",RetryAfter}]).

get_datetime(DateTime) ->
    Regex = "(?:Mon|Wed|Tue|Thu|Fri), ([0-3][0-9]) (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) ([0-9]{4}) ([0-2][0-9]):([0-6][0-9]):([0-6][0-9])",
    {'match', Capture} = re:run(DateTime, Regex, [{'capture','all_but_first','binary'}]),
    [Day, Month, Year, Hour, Minute, Second] = [kz_term:to_integer(month_to_int(DatePart)) || DatePart <- Capture],
    {{Year, Month, Day},{Hour, Minute, Second}}.

month_to_int(<<"Jan">>) -> 1;
month_to_int(<<"Feb">>) -> 2;
month_to_int(<<"Mar">>) -> 3;
month_to_int(<<"Apr">>) -> 4;
month_to_int(<<"May">>) -> 5;
month_to_int(<<"Jun">>) -> 6;
month_to_int(<<"Jul">>) -> 7;
month_to_int(<<"Aug">>) -> 8;
month_to_int(<<"Sep">>) -> 9;
month_to_int(<<"Oct">>) -> 10;
month_to_int(<<"Nov">>) -> 11;
month_to_int(<<"Dec">>) -> 12;
month_to_int(Other) -> Other.
