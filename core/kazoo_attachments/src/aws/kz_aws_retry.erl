%% @author root
%% @doc @todo Add description to kz_aws_retry.
-module(kz_aws_retry).

-include("kz_aws.hrl").

%% Helpers
-export([backoff/1,
         no_retry/1,
         default_retry/1, default_retry/2
        ]).
-export_type([should_retry/0, retry_fun/0]).

-type should_retry() :: {'retry' | 'error', #aws_request{}}.
-type retry_fun() :: fun((#aws_request{}) -> should_retry()).

%% Internal impl api
-export([request/3]).

%% Error returns maintained for backwards compatibility
-spec no_retry(#aws_request{}) -> should_retry().
no_retry(Request) ->
    {'error', Request}.

%% Sleep after an attempt
-spec backoff(pos_integer()) -> 'ok'.
backoff(1) -> 'ok';
backoff(Attempt) ->
    timer:sleep(rand:uniform((1 bsl (Attempt - 1)) * 100)).

%% Currently matches DynamoDB retry
%% It's likely this is too many retries for other services
-define(NUM_ATTEMPTS, 10).

-spec default_retry(#aws_request{}) -> should_retry().
default_retry(Request) ->
    default_retry(Request, ?NUM_ATTEMPTS).

-spec default_retry(#aws_request{}, integer()) -> should_retry().
default_retry(#aws_request{attempt = Attempt} = Request, MaxAttempts)
  when Attempt >= MaxAttempts ->
    {'error', Request};
default_retry(#aws_request{should_retry = 'false'} = Request, _) ->
    {'error', Request};
default_retry(#aws_request{attempt = Attempt} = Request, _) ->
    backoff(Attempt),
    {'retry', Request}.

-spec request(aws_config(), #aws_request{}, retry_fun()) -> #aws_request{}.
request(Config, #aws_request{attempt = 0} = Request, ResultFun) ->
    request_and_retry(Config, ResultFun, {'retry', Request}).

request_and_retry(_, _, {'error', Request}) -> Request;
request_and_retry(Config, ResultFun, {'retry', Request}) ->
    #aws_request{attempt = Attempt
                ,uri = URI
                ,method = Method
                ,request_headers = Headers
                ,request_body = Body
                } = Request,
    Request2 = Request#aws_request{attempt = Attempt + 1},
    RetryFun = Config#aws_config.retry,
    case kz_aws_httpc:request(URI, Method, Headers, Body, timeout(Config), Config) of
        {'ok', {{Status, StatusLine}, ResponseHeaders, ResponseBody}} ->
            Request3 = Request2#aws_request{
                         response_type = if Status >= 200, Status < 300 -> 'ok'; 'true' -> 'error' end,
                         error_type = 'aws',
                         response_status = Status,
                         response_status_line = StatusLine,
                         response_headers = ResponseHeaders,
                         response_body = ResponseBody},
            Request4 = ResultFun(Request3),
            case Request4#aws_request.response_type of
                'ok' -> Request4;
                'error' ->
                    request_and_retry(Config, ResultFun, RetryFun(Request4))
            end;
        {'error', Reason} ->
            Request4 = Request2#aws_request{response_type = 'error'
                                           ,error_type = 'httpc'
                                           ,httpc_error_reason = Reason
                                           },
            request_and_retry(Config, ResultFun, RetryFun(Request4))
    end.

timeout(#aws_config{timeout = 'undefined'}) -> ?DEFAULT_TIMEOUT;
timeout(#aws_config{timeout = Timeout}) -> Timeout.
