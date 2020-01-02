%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc Alert document
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_alert).

-export([new/0
        ,type/0
        ,id/1
        ,fetch/1
        ]).

-export([title/0, title/1, title/2, set_title/2
        ,category/0 ,category/1, category/2, set_category/2
        ,message/0, message/1, message/2, set_message/2
        ,metadata/0, metadata/1, metadata/2, set_metadata/2
        ,level/0, level/1, level/2, set_level/2
        ,from/0, from/1, from/2, set_from/2
        ,to/0, to/1, to/2, set_to/2
        ,expiration_date/0, expiration_date/1, expiration_date/2, set_expiration_date/2
        ,expired/1
        ]).

-define(ID, <<"_id">>).
-define(PVT_TYPE, <<"alert">>).

-define(TITLE, <<"title">>).
-define(CATEGORY, <<"category">>).
-define(MESSAGE, <<"message">>).
-define(METADATA, <<"metadata">>).
-define(LEVEL, <<"level">>).
-define(FROM, <<"from">>).
-define(TO, <<"to">>).
-define(EXPIRATION_DATE, <<"expiration_date">>).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json:new(), ?PVT_TYPE).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(doc()) -> kz_term:api_binary().
id(JObj) ->
    kz_doc:id(JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:api_binary()) -> {'ok', doc()} |
          {'error', any()}.
fetch('undefined') ->
    {'error', 'invalid_db_name'};
fetch(<<_/binary>> = AlertId) ->
    kz_datamgr:open_cache_doc(?KZ_ALERTS_DB, AlertId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec title() -> kz_term:ne_binary().
title() ->
    ?TITLE.

-spec title(doc()) -> kz_term:api_binary().
title(JObj) ->
    title(JObj, 'undefined').

-spec title(doc(), Default) -> kz_term:ne_binary() | Default.
title(JObj, Default) ->
    kz_json:get_value(?TITLE, JObj, Default).

-spec set_title(doc(), kz_term:ne_binary()) -> doc().
set_title(JObj, Title) ->
    kz_json:set_value(?TITLE, Title, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec category() -> kz_term:ne_binary().
category() ->
    ?CATEGORY.

-spec category(doc()) -> kz_term:api_binary().
category(JObj) ->
    category(JObj, 'undefined').

-spec category(doc(), Default) -> kz_term:ne_binary() | Default.
category(JObj, Default) ->
    kz_json:get_value(?CATEGORY, JObj, Default).

-spec set_category(doc(), kz_term:ne_binary()) -> doc().
set_category(JObj, Category) ->
    kz_json:set_value(?CATEGORY, Category, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec message() -> kz_term:ne_binary().
message() ->
    ?MESSAGE.

-spec message(doc()) -> kz_term:api_binary().
message(JObj) ->
    message(JObj, 'undefined').

-spec message(doc(), Default) -> kz_term:ne_binary() | Default.
message(JObj, Default) ->
    kz_json:get_value(?MESSAGE, JObj, Default).

-spec set_message(doc(), kz_term:ne_binary()) -> doc().
set_message(JObj, Message) ->
    kz_json:set_value(?MESSAGE, Message, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec metadata() -> kz_term:ne_binary().
metadata() ->
    ?METADATA.

-spec metadata(doc()) -> kz_json:object().
metadata(JObj) ->
    metadata(JObj, kz_json:new()).

-spec metadata(doc(), Default) ->  kz_json:object() | Default.
metadata(JObj, Default) ->
    kz_json:get_value(?METADATA, JObj, Default).

-spec set_metadata(doc(), kz_json:object()) -> doc().
set_metadata(JObj, Metadata) ->
    kz_json:set_value(?METADATA, Metadata, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec level() -> kz_term:ne_binary().
level() ->
    ?LEVEL.

-spec level(doc()) -> kz_term:api_binary().
level(JObj) ->
    level(JObj, <<"info">>).

-spec level(doc(), Default) -> kz_term:ne_binary() | Default.
level(JObj, Default) ->
    kz_json:get_value(?LEVEL, JObj, Default).

-spec set_level(doc(), kz_term:ne_binary()) -> doc().
set_level(JObj, Level) ->
    kz_json:set_value(?LEVEL, Level, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec from() -> kz_term:ne_binary().
from() ->
    ?FROM.

-spec from(doc()) -> kz_json:objects().
from(JObj) ->
    from(JObj, kz_json:new()).

-spec from(doc(), Default) ->  kz_json:objects() | Default.
from(JObj, Default) ->
    kz_json:get_value(?FROM, JObj, Default).

-spec set_from(doc(), kz_json:objects()) -> doc().
set_from(JObj, From) ->
    kz_json:set_value(?FROM, From, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec to() -> kz_term:ne_binary().
to() ->
    ?TO.

-spec to(doc()) -> kz_json:objects().
to(JObj) ->
    to(JObj, []).

-spec to(doc(), Default) ->  kz_json:objects() | Default.
to(JObj, Default) ->
    kz_json:get_value(?TO, JObj, Default).

-spec set_to(doc(), kz_json:objects()) -> doc().
set_to(JObj, To) ->
    kz_json:set_value(?TO, To, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec expiration_date() -> kz_term:ne_binary().
expiration_date() ->
    ?EXPIRATION_DATE.

-spec expiration_date(doc()) -> kz_json:object().
expiration_date(JObj) ->
    expiration_date(JObj, kz_json:new()).

-spec expiration_date(doc(), Default) ->  kz_json:object() | Default.
expiration_date(JObj, Default) ->
    kz_json:get_value(?EXPIRATION_DATE, JObj, Default).

-spec set_expiration_date(doc(), kz_json:objects()) -> doc().
set_expiration_date(JObj, ExpDate) ->
    kz_json:set_value(?EXPIRATION_DATE, ExpDate, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec expired(doc()) -> boolean().
expired(JObj) ->
    kz_json:get_value(?EXPIRATION_DATE, JObj) < kz_time:now_s().

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
