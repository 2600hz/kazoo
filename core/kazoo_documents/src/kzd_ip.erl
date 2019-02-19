%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Accessors for `ip' document.
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_ip).

-export([new/0]).
-export([host/1, host/2, set_host/2]).
-export([ip/1, ip/2, set_ip/2]).
-export([zone/1, zone/2, set_zone/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"ip">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec host(doc()) -> kz_term:api_ne_binary().
host(Doc) ->
    host(Doc, 'undefined').

-spec host(doc(), Default) -> kz_term:ne_binary() | Default.
host(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"host">>], Doc, Default).

-spec set_host(doc(), kz_term:ne_binary()) -> doc().
set_host(Doc, Host) ->
    kz_json:set_value([<<"host">>], Host, Doc).

-spec ip(doc()) -> any().
ip(Doc) ->
    ip(Doc, 'undefined').

-spec ip(doc(), Default) -> any() | Default.
ip(Doc, Default) ->
    kz_json:get_value([<<"ip">>], Doc, Default).

-spec set_ip(doc(), any()) -> doc().
set_ip(Doc, Ip) ->
    kz_json:set_value([<<"ip">>], Ip, Doc).

-spec zone(doc()) -> kz_term:api_ne_binary().
zone(Doc) ->
    zone(Doc, 'undefined').

-spec zone(doc(), Default) -> kz_term:ne_binary() | Default.
zone(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"zone">>], Doc, Default).

-spec set_zone(doc(), kz_term:ne_binary()) -> doc().
set_zone(Doc, Zone) ->
    kz_json:set_value([<<"zone">>], Zone, Doc).
