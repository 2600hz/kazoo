%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(conference_maintenance).

-export([blocking_refresh/0]).
-export([refresh/0, refresh/1]).

-include("conference.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec blocking_refresh() -> 'ok'.
blocking_refresh() ->
    lists:foreach(fun(AccountDb) ->
                          refresh(AccountDb)
                  end, kapps_util:get_all_accounts()),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec refresh() -> 'started'.
refresh() ->
    _ = kz_util:spawn(fun blocking_refresh/0),
    'started'.

-spec refresh(kz_term:text()) -> 'ok'.
refresh(<<Account/binary>>) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    Views = kapps_util:get_views_json('conference', "views"),
    kapps_util:update_views(AccountDb, Views);
refresh(Account) ->
    refresh(kz_term:to_binary(Account)).
