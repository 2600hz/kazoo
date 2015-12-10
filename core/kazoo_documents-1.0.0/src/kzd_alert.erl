%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Alert document
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
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

-type doc() :: wh_json:object().
-export_type([doc/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> doc().
new() ->
    wh_doc:set_type(wh_json:new(), ?PVT_TYPE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec type() -> ne_binary().
type() -> ?PVT_TYPE.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec id(doc()) -> api_binary().
id(JObj) ->
    wh_doc:id(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(api_binary()) -> {'ok', doc()} |
                            {'error', any()}.
fetch('undefined') ->
    {'error', 'invalid_db_name'};
fetch(<<_/binary>> = AlertId) ->
    couch_mgr:open_cache_doc(?WH_ALERTS_DB, AlertId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec title() -> ne_binary().
-spec title(doc()) -> api_binary().
-spec title(doc(), Default) -> ne_binary() | Default.
title() ->
    ?TITLE.

title(JObj) ->
    title(JObj, 'undefined').

title(JObj, Default) ->
    wh_json:get_value(?TITLE, JObj, Default).

set_title(JObj, Title) ->
    wh_json:set_value(?TITLE, Title, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec category() -> ne_binary().
-spec category(doc()) -> api_binary().
-spec category(doc(), Default) -> ne_binary() | Default.
category() ->
    ?CATEGORY.

category(JObj) ->
    category(JObj, 'undefined').

category(JObj, Default) ->
    wh_json:get_value(?CATEGORY, JObj, Default).

set_category(JObj, Category) ->
    wh_json:set_value(?CATEGORY, Category, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec message() -> ne_binary().
-spec message(doc()) -> api_binary().
-spec message(doc(), Default) -> ne_binary() | Default.
message() ->
    ?MESSAGE.

message(JObj) ->
    message(JObj, 'undefined').

message(JObj, Default) ->
    wh_json:get_value(?MESSAGE, JObj, Default).

set_message(JObj, Message) ->
    wh_json:set_value(?MESSAGE, Message, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec metadata() -> ne_binary().
-spec metadata(doc()) -> wh_json:object().
-spec metadata(doc(), Default) ->  wh_json:object() | Default.
metadata() ->
    ?METADATA.

metadata(JObj) ->
    metadata(JObj, wh_json:new()).

metadata(JObj, Default) ->
    wh_json:get_value(?METADATA, JObj, Default).

set_metadata(JObj, Metadata) ->
    wh_json:set_value(?METADATA, Metadata, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec level() -> ne_binary().
-spec level(doc()) -> api_binary().
-spec level(doc(), Default) -> ne_binary() | Default.
level() ->
    ?LEVEL.

level(JObj) ->
    level(JObj, <<"info">>).

level(JObj, Default) ->
    wh_json:get_value(?LEVEL, JObj, Default).

set_level(JObj, Level) ->
    wh_json:set_value(?LEVEL, Level, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from() -> ne_binary().
-spec from(doc()) -> wh_json:object().
-spec from(doc(), Default) ->  wh_json:object() | Default.
from() ->
    ?FROM.

from(JObj) ->
    from(JObj, wh_json:new()).

from(JObj, Default) ->
    wh_json:get_value(?FROM, JObj, Default).

set_from(JObj, From) ->
    wh_json:set_value(?FROM, From, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to() -> ne_binary().
-spec to(doc()) -> wh_json:object().
-spec to(doc(), Default) ->  wh_json:object() | Default.
to() ->
    ?TO.

to(JObj) ->
    to(JObj, wh_json:new()).

to(JObj, Default) ->
    wh_json:get_value(?TO, JObj, Default).

set_to(JObj, To) ->
    wh_json:set_value(?TO, To, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec expiration_date() -> ne_binary().
-spec expiration_date(doc()) -> wh_json:object().
-spec expiration_date(doc(), Default) ->  wh_json:object() | Default.
expiration_date() ->
    ?EXPIRATION_DATE.

expiration_date(JObj) ->
    expiration_date(JObj, wh_json:new()).

expiration_date(JObj, Default) ->
    wh_json:get_value(?EXPIRATION_DATE, JObj, Default).

set_expiration_date(JObj, ExpDate) ->
    wh_json:set_value(?EXPIRATION_DATE, ExpDate, JObj).

%%%===================================================================
%%% Internal functions
%%%===================================================================
