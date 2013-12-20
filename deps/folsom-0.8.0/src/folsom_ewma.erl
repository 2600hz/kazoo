%%%
%%% Copyright 2011, Boundary
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%


%%%-------------------------------------------------------------------
%%% File:      folsom_ewma.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% based on https://github.com/codahale/metrics/blob/development/src/main/java/com/yammer/metrics/stats/EWMA.java
%%% references:
%%% http://www.teamquest.com/pdfs/whitepaper/ldavg1.pdf
%%% http://www.teamquest.com/pdfs/whitepaper/ldavg2.pdf
%%% @end
%%%-----------------------------------------------------------------


-module(folsom_ewma).

-define(M1_ALPHA,   1 - math:exp(-5 / 60.0)).
-define(M5_ALPHA,   1 - math:exp(-5 / 60.0 / 5)).
-define(M15_ALPHA,  1 - math:exp(-5 / 60.0 / 15)).
-define(D1_ALPHA,   1 - math:exp(-5 / 60.0 / 1440)).

-record(ewma, {
          alpha,
          interval = 5, % seconds
          initialized = false,
          rate = 0,
          total = 0
         }).

-export([update/2,
         new/2,
         rate/1,
         tick/1,
         one_minute_ewma/0,
         five_minute_ewma/0,
         fifteen_minute_ewma/0,
         one_day_ewma/0]).


% API

one_minute_ewma() ->
    new(?M1_ALPHA, 5).

five_minute_ewma() ->
    new(?M5_ALPHA, 5).

fifteen_minute_ewma() ->
    new(?M15_ALPHA, 5).

one_day_ewma() ->
    new(?D1_ALPHA, 5).

new(Alpha, Interval) ->
    #ewma{alpha = Alpha, interval = Interval}.

update(#ewma{total = Total} = EWMA, Value) ->
    EWMA#ewma{total = Total + Value}.

tick(#ewma{total = Total, rate = Rate, initialized = Init, interval = Interval, alpha = Alpha} = EWMA) ->
    InstantRate = Total / Interval,
    Rate1 = rate_calc(Init, Alpha, Rate, InstantRate),
    EWMA#ewma{rate = Rate1, initialized = true, total = 0}.

rate(#ewma{rate = Rate}) ->
    Rate.

% Internal API

rate_calc(true, Alpha, Rate, InstantRate) ->
    Rate1 = Rate + (Alpha * (InstantRate - Rate)),
    Rate1;
rate_calc(false, _, _, InstantRate) ->
    InstantRate.
