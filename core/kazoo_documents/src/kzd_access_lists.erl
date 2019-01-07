%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_access_lists).

-export([new/0]).
-export([cidrs/1, cidrs/2, set_cidrs/2]).
-export([order/1, order/2, set_order/2]).
-export([user_agent/1, user_agent/2, set_user_agent/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"access_lists">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec cidrs(doc()) -> kz_term:api_ne_binaries().
cidrs(Doc) ->
    cidrs(Doc, 'undefined').

-spec cidrs(doc(), Default) -> kz_term:ne_binaries() | Default.
cidrs(Doc, Default) ->
    kz_json:get_list_value([<<"cidrs">>], Doc, Default).

-spec set_cidrs(doc(), kz_term:ne_binaries()) -> doc().
set_cidrs(Doc, Cidrs) ->
    kz_json:set_value([<<"cidrs">>], Cidrs, Doc).

-spec order(doc()) -> kz_term:api_binary().
order(Doc) ->
    order(Doc, 'undefined').

-spec order(doc(), Default) -> binary() | Default.
order(Doc, Default) ->
    kz_json:get_binary_value([<<"order">>], Doc, Default).

-spec set_order(doc(), binary()) -> doc().
set_order(Doc, Order) ->
    kz_json:set_value([<<"order">>], Order, Doc).

-spec user_agent(doc()) -> kz_term:api_binary().
user_agent(Doc) ->
    user_agent(Doc, 'undefined').

-spec user_agent(doc(), Default) -> binary() | Default.
user_agent(Doc, Default) ->
    kz_json:get_binary_value([<<"user_agent">>], Doc, Default).

-spec set_user_agent(doc(), binary()) -> doc().
set_user_agent(Doc, UserAgent) ->
    kz_json:set_value([<<"user_agent">>], UserAgent, Doc).
