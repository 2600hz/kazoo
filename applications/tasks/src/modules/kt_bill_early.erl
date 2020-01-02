%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Hesaam
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_bill_early).

%% behaviour: tasks_provider

-export([init/0
        ]).

%% Triggerables
-export([handle_req/1]).

-include("tasks.hrl").

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".bill_early">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_ACCOUNT, ?MODULE, 'handle_req'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_term:ne_binary()) -> 'ok'.
handle_req(AccountDb) ->
    AccountId = kzs_util:format_account_id(AccountDb),

    EarlyDays = kapps_config:get_integer(?MOD_CAT, <<"how_many_early_days">>, 5),
    {DueTimestamp, IsDaysEarlyYet} = is_days_early_yet(EarlyDays),

    ShouldBill = kapps_account_config:get_global(AccountId, ?MOD_CAT, <<"bill_early_enabled">>, 'false'),
    ShouldRemind = kapps_account_config:get_global(AccountId, ?MOD_CAT, <<"reminder_enabled">>, 'false'),

    handle_req(AccountId, DueTimestamp, IsDaysEarlyYet, ShouldBill, ShouldRemind).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_term:ne_binary(), kz_time:gregorian_seconds(), boolean(), boolean(), boolean()) -> 'ok'.
handle_req(_, _, 'false', _ShouldBill, _ShouldRemind) ->
    'ok';
handle_req(AccountId, DueTimestamp, 'true', 'true', _ShouldRemind) ->
    _ = kz_services_recurring:early_collect(AccountId, DueTimestamp),
    'ok';
handle_req(AccountId, DueTimestamp, 'true', 'false', 'true') ->
    _ = kz_services_recurring:send_early_reminder(AccountId, DueTimestamp),
    'ok';
handle_req(_, _, _, _ShouldBill, _ShouldRemind) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_days_early_yet(non_neg_integer()) -> {kz_time:gregorian_seconds(), boolean()}.
is_days_early_yet(EarlyDays) ->
    is_days_early_yet(erlang:date(), EarlyDays).

-spec is_days_early_yet(calendar:date(), non_neg_integer()) -> {kz_time:gregorian_seconds(), boolean()}.
is_days_early_yet({Year, Month, Day}, EarlyDays) ->
    LastDay = calendar:last_day_of_the_month(Year, Month),
    DueTimestamp =
        calendar:datetime_to_gregorian_seconds({kz_date:normalize({Year, Month, LastDay + 1})
                                               ,{0, 0, 1}
                                               }),
    {DueTimestamp, (LastDay - EarlyDays) < Day}.

%%% End of Module.
