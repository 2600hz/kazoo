%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Accessors for `tray_toggle' document.
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_tray_toggle).

-export([new/0]).
-export([enabled/1, enabled/2, set_enabled/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"tray_toggle">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec enabled(doc()) -> kz_term:api_boolean().
enabled(Doc) ->
    enabled(Doc, 'undefined').

-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc, Default) ->
    kz_json:get_boolean_value([<<"enabled">>], Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value([<<"enabled">>], Enabled, Doc).
