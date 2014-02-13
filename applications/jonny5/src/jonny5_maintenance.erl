%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(jonny5_maintenance).

-export([flush/0]).
-export([limits_details/1]).
-export([reconcile/1]).
-export([stop_reconciler/0]).
-export([start_reconciler/0]).

-include("jonny5.hrl").

-spec flush() -> 'ok'.
flush() -> wh_cache:flush_local(?JONNY5_CACHE).

-spec limits_details(atom() | string() | ne_binary()) -> iolist().
limits_details(Account) when not is_binary(Account) ->
    limits_details(wh_util:to_binary(Account));
limits_details(Account) ->
    %% TODO: fix usage
    AccountId = wh_util:format_account_id(Account, 'raw'),
    Props = j5_limits:to_props(j5_limits:get(AccountId)),
    pretty_print_field("Account ID", props:get_value('account_id', Props)),
    pretty_print_field("Account DB", props:get_value('account_db', Props)),
    pretty_print_field("Enabled", props:get_value('enabled', Props)),
    pretty_print_field("Current Balance", wht_util:units_to_dollars(wht_util:current_balance(AccountId))),
    pretty_print_field("Prepay Allowed", props:get_value('allow_prepay', Props)),
    pretty_print_field("Postpay Allowed", props:get_value('allow_postpay', Props)),
    pretty_print_field("Max Postpay Amount", wht_util:units_to_dollars(props:get_value('max_postpay_amount', Props))),
    pretty_print_field("Reserve Amount", wht_util:units_to_dollars(props:get_value('reserve_amount', Props))),
    pretty_print_field("Calls Hard Limit", props:get_value('calls', Props)),
    pretty_print_field("Resources Hard Limit", props:get_value('resource_consuming_calls', Props)),
    pretty_print_field("Inbound Trunks", props:get_value('inbound_trunks', Props)),
    pretty_print_field("Bundled Inbound Trunks", props:get_value('bundled_inbound_trunks', Props)),
    pretty_print_field("Inbound Usage", 0),
    pretty_print_field("Outbound Trunks", props:get_value('outbound_trunks', Props)),
    pretty_print_field("Bundled Outbound Trunks", props:get_value('bundled_outbound_trunks', Props)),
    pretty_print_field("Outbound Usage", 0),
    pretty_print_field("Twoway Trunks", props:get_value('twoway_trunks', Props)),
    pretty_print_field("Bundled Twoway Trunks", props:get_value('bundled_twoway_trunks', Props)),
    pretty_print_field("Twoway Usage", 0),
    pretty_print_field("Soft Limit Inbound", props:get_value('soft_limit_inbound', Props)),
    pretty_print_field("Soft Limit Outbound", props:get_value('soft_limit_outbound', Props)).

-spec pretty_print_field(string(), any()) -> 'ok'.
pretty_print_field(Name, Value) when is_number(Value) ->
    io:format("~-25s: ~w~n", [Name, Value]); 
pretty_print_field(Name, Value) ->
    io:format("~-25s: ~s~n", [Name, Value]).

-spec reconcile(ne_binary()) -> 'ok' | {'error', _}.
reconcile(Account) when not is_binary(Account) ->
    reconcile(wh_util:to_binary(Account));
reconcile(Account) ->
    j5_reconciler:process_account(Account).

-spec stop_reconciler() -> any().
stop_reconciler() ->
    supervisor:terminate_child('jonny5_sup', 'j5_reconciler').

-spec start_reconciler() -> any().
start_reconciler() ->
    supervisor:start_child(jonny5_sup, j5_reconciler).
