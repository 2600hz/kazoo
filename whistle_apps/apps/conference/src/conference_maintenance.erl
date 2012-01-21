%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(conference_maintenance).

-export([blocking_refresh/0]).
-export([refresh/0, refresh/1]).

-include("conference.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec blocking_refresh/0 :: () -> 'ok'.
blocking_refresh() ->
    lists:foreach(fun(AccountDb) ->
                          refresh(AccountDb)
                  end, whapps_util:get_all_accounts()),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> 'started'.
-spec refresh/1 :: (binary() | string()) -> 'ok'.

refresh() ->
    spawn(fun() ->
                  lists:foreach(fun(AccountDb) ->
                                        refresh(AccountDb)
                                end, whapps_util:get_all_accounts())
          end),
    started.

refresh(<<Account/binary>>) ->
    AccountDb = wh_util:format_account_id(Account, encoded),
    Views = whapps_util:get_views_json(conference, "views"),
    whapps_util:update_views(AccountDb, Views);
refresh(Account) ->
    refresh(wh_util:to_binary(Account)).
