%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kzd_service_plan).

-export([account_id/1, account_id/2
         ,overrides/1, overrides/2
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-spec account_id(doc()) -> api_binary().
-spec account_id(doc(), Default) -> ne_binary() | Default.
account_id(Plan) ->
    account_id(Plan, 'undefined').
account_id(Plan, Default) ->
    wh_json:get_value(<<"account_id">>, Plan, Default).

-spec overrides(doc()) -> wh_json:object().
-spec overrides(doc(), Default) -> wh_json:object() | Default.
overrides(Plan) ->
    overrides(Plan, wh_json:new()).
overrides(Plan, Default) ->
    wh_json:get_json_value(<<"overrides">>, Plan, Default).
