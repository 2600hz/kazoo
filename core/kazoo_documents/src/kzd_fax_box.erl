%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_fax_box).

-export([new/0
        ,type/0
        ,owner_id/1, owner_id/2
        ,timezone/1, timezone/2
        ,retries/1, retries/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(KEY_OWNER_ID, <<"owner_id">>).
-define(KEY_TIMEZONE, <<"fax_timezone">>).
-define(KEY_RETRIES, <<"retries">>).

-define(PVT_TYPE, <<"faxbox">>).

-spec new() -> doc().
new() ->
    kz_json:from_list([{<<"pvt_type">>, type()}]).

-spec type() -> ne_binary().
type() -> ?PVT_TYPE.

-spec owner_id(doc()) -> api_binary().
-spec owner_id(doc(), Default) -> ne_binary() | Default.
owner_id(Box) ->
    owner_id(Box, 'undefined').
owner_id(Box, Default) ->
    kz_json:get_value(?KEY_OWNER_ID, Box, Default).

-spec timezone(doc()) -> ne_binary().
timezone(Box) ->
    timezone(Box, 'undefined').

-spec timezone(doc(), Default) -> ne_binary() | Default.
timezone(Box, Default) ->
    case kz_json:get_value(?KEY_TIMEZONE, Box) of
        'undefined'   -> owner_timezone(Box, Default);
        <<"inherit">> -> owner_timezone(Box, Default); %% UI-1808
        TZ -> TZ
    end.

-spec owner_timezone(doc(), Default) -> ne_binary() | Default.
owner_timezone(Box, Default) ->
    case kzd_user:fetch(kz_doc:account_db(Box), owner_id(Box)) of
        {'ok', OwnerJObj} -> kzd_user:timezone(OwnerJObj, Default);
        {'error', _} -> kz_account:timezone(kz_doc:account_id(Box), Default)
    end.

-spec retries(doc()) -> api_integer().
-spec retries(doc(), Default) -> integer() | Default.
retries(Box) ->
    retries(Box, 'undefined').
retries(Box, Default) ->
    kz_json:get_integer_value(?KEY_RETRIES, Box, Default).
