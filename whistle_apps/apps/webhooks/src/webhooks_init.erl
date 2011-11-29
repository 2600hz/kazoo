%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Search all accounts for webhooks and start the appropriate handlers
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james22600hz.org>
%%%-------------------------------------------------------------------
-module(webhooks_init).

-export([start_link/0]).

-include("webhooks.hrl").

start_link() ->
    _ = [ maybe_start_handler(Db) || Db <- whapps_util:get_all_accounts()],
    ignore.

maybe_start_handler(Db) ->
    case couch_mgr:get_results(Db, <<"webhooks/crossbar_listing">>, [{<<"include_docs">>, true}]) of
	{ok, []} -> ignore;
	{ok, WebHooks} ->
	    webhooks_acct_sup:start_acct(Db, [wh_json:get_value(<<"doc">>, Hook) || Hook <- WebHooks]);
	{error, _E} ->
	    ?LOG_SYS("Failed to load webhooks view for account ~s", [Db])
    end.
