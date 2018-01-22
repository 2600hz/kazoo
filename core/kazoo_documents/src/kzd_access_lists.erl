-module(kzd_access_lists).

-export([new/0]).
-export([cidrs/1, cidrs/2, set_cidrs/2]).
-export([order/1, order/2, set_order/2]).
-export([user_agent/1, user_agent/2, set_user_agent/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec cidrs(doc()) -> api_ne_binaries().
-spec cidrs(doc(), Default) -> ne_binaries() | Default.
cidrs(Doc) ->
    cidrs(Doc, 'undefined').
cidrs(Doc, Default) ->
    kz_json:get_list_value(<<"cidrs">>, Doc, Default).

-spec set_cidrs(doc(), ne_binaries()) -> doc().
set_cidrs(Doc, Cidrs) ->
    kz_json:set_value(<<"cidrs">>, Cidrs, Doc).

-spec order(doc()) -> api_binary().
-spec order(doc(), Default) -> binary() | Default.
order(Doc) ->
    order(Doc, 'undefined').
order(Doc, Default) ->
    kz_json:get_binary_value(<<"order">>, Doc, Default).

-spec set_order(doc(), binary()) -> doc().
set_order(Doc, Order) ->
    kz_json:set_value(<<"order">>, Order, Doc).

-spec user_agent(doc()) -> api_binary().
-spec user_agent(doc(), Default) -> binary() | Default.
user_agent(Doc) ->
    user_agent(Doc, 'undefined').
user_agent(Doc, Default) ->
    kz_json:get_binary_value(<<"user_agent">>, Doc, Default).

-spec set_user_agent(doc(), binary()) -> doc().
set_user_agent(Doc, UserAgent) ->
    kz_json:set_value(<<"user_agent">>, UserAgent, Doc).
