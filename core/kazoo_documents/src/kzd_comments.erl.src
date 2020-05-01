%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Accessors for `comments' document.
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_comments).

-export([new/0]).
-export([comments/1, comments/2, set_comments/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"comments">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec comments(doc()) -> kz_term:api_objects().
comments(Doc) ->
    comments(Doc, 'undefined').

-spec comments(doc(), Default) -> kz_json:objects() | Default.
comments(Doc, Default) ->
    kz_json:get_list_value([<<"comments">>], Doc, Default).

-spec set_comments(doc(), kz_json:objects()) -> doc().
set_comments(Doc, Comments) ->
    kz_json:set_value([<<"comments">>], Comments, Doc).
