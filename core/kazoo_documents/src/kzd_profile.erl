-module(kzd_profile).

-export([new/0]).
-export([addresses/1, addresses/2, set_addresses/2]).
-export([assistant/1, assistant/2, set_assistant/2]).
-export([birthday/1, birthday/2, set_birthday/2]).
-export([nicknames/1, nicknames/2, set_nicknames/2]).
-export([note/1, note/2, set_note/2]).
-export([role/1, role/2, set_role/2]).
-export([sort_string/1, sort_string/2, set_sort_string/2]).
-export([title/1, title/2, set_title/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec addresses(doc()) -> api_objects().
-spec addresses(doc(), Default) -> kz_json:objects() | Default.
addresses(Doc) ->
    addresses(Doc, 'undefined').
addresses(Doc, Default) ->
    kz_json:get_list_value([<<"addresses">>], Doc, Default).

-spec set_addresses(doc(), kz_json:objects()) -> doc().
set_addresses(Doc, Addresses) ->
    kz_json:set_value([<<"addresses">>], Addresses, Doc).

-spec assistant(doc()) -> api_binary().
-spec assistant(doc(), Default) -> binary() | Default.
assistant(Doc) ->
    assistant(Doc, 'undefined').
assistant(Doc, Default) ->
    kz_json:get_binary_value([<<"assistant">>], Doc, Default).

-spec set_assistant(doc(), binary()) -> doc().
set_assistant(Doc, Assistant) ->
    kz_json:set_value([<<"assistant">>], Assistant, Doc).

-spec birthday(doc()) -> api_binary().
-spec birthday(doc(), Default) -> binary() | Default.
birthday(Doc) ->
    birthday(Doc, 'undefined').
birthday(Doc, Default) ->
    kz_json:get_binary_value([<<"birthday">>], Doc, Default).

-spec set_birthday(doc(), binary()) -> doc().
set_birthday(Doc, Birthday) ->
    kz_json:set_value([<<"birthday">>], Birthday, Doc).

-spec nicknames(doc()) -> api_ne_binaries().
-spec nicknames(doc(), Default) -> ne_binaries() | Default.
nicknames(Doc) ->
    nicknames(Doc, 'undefined').
nicknames(Doc, Default) ->
    kz_json:get_list_value([<<"nicknames">>], Doc, Default).

-spec set_nicknames(doc(), ne_binaries()) -> doc().
set_nicknames(Doc, Nicknames) ->
    kz_json:set_value([<<"nicknames">>], Nicknames, Doc).

-spec note(doc()) -> api_binary().
-spec note(doc(), Default) -> binary() | Default.
note(Doc) ->
    note(Doc, 'undefined').
note(Doc, Default) ->
    kz_json:get_binary_value([<<"note">>], Doc, Default).

-spec set_note(doc(), binary()) -> doc().
set_note(Doc, Note) ->
    kz_json:set_value([<<"note">>], Note, Doc).

-spec role(doc()) -> api_binary().
-spec role(doc(), Default) -> binary() | Default.
role(Doc) ->
    role(Doc, 'undefined').
role(Doc, Default) ->
    kz_json:get_binary_value([<<"role">>], Doc, Default).

-spec set_role(doc(), binary()) -> doc().
set_role(Doc, Role) ->
    kz_json:set_value([<<"role">>], Role, Doc).

-spec sort_string(doc()) -> api_binary().
-spec sort_string(doc(), Default) -> binary() | Default.
sort_string(Doc) ->
    sort_string(Doc, 'undefined').
sort_string(Doc, Default) ->
    kz_json:get_binary_value([<<"sort_string">>], Doc, Default).

-spec set_sort_string(doc(), binary()) -> doc().
set_sort_string(Doc, SortString) ->
    kz_json:set_value([<<"sort_string">>], SortString, Doc).

-spec title(doc()) -> api_binary().
-spec title(doc(), Default) -> binary() | Default.
title(Doc) ->
    title(Doc, 'undefined').
title(Doc, Default) ->
    kz_json:get_binary_value([<<"title">>], Doc, Default).

-spec set_title(doc(), binary()) -> doc().
set_title(Doc, Title) ->
    kz_json:set_value([<<"title">>], Title, Doc).
