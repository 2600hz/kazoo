%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
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
         ,owner/1
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

-spec owner_id(doc()) -> maybe(binary()).
-spec owner_id(doc(), Default) -> ne_binary() | Default.
owner_id(Box) ->
    owner_id(Box, 'undefined').
owner_id(Box, Default) ->
    kz_json:get_value(?KEY_OWNER_ID, Box, Default).

-spec owner(doc()) -> maybe(kzd_user:doc()).
-spec owner(doc(), ne_binary()) -> maybe(kzd_user:doc()).
owner(Box) ->
    case owner_id(Box) of
        'undefined' -> 'undefined';
        OwnerId -> owner(Box, OwnerId)
    end.

owner(Box, OwnerId) ->
    case kz_datamgr:open_cache_doc(kz_doc:account_db(Box), OwnerId) of
        {'ok', OwnerJObj} -> OwnerJObj;
        {'error', 'not_found'} -> 'undefined'
    end.

-spec timezone(doc()) -> maybe(binary()).
-spec timezone(doc(), Default) -> ne_binary() | Default.
timezone(Box) ->
    timezone(Box, 'undefined').
timezone(Box, Default) ->
    case kz_json:get_value(?KEY_TIMEZONE, Box) of
        'undefined'   -> owner_timezone(Box, Default);
        <<"inherit">> -> owner_timezone(Box, Default);
        TZ -> TZ
    end.

-spec owner_timezone(doc(), Default) -> ne_binary() | Default.
-spec owner_timezone(doc(), Default, kzd_user:doc()) -> ne_binary() | Default.
owner_timezone(Box, Default) ->
    case owner(Box) of
        'undefined' -> account_timezone(Box, Default);
        OwnerJObj -> owner_timezone(Box, Default, OwnerJObj)
    end.

owner_timezone(Box, Default, OwnerJObj) ->
    case kzd_user:timezone(OwnerJObj, 'undefined') of
        'undefined' -> account_timezone(Box, Default);
        TZ -> TZ
    end.

-spec account_timezone(doc(), Default) -> ne_binary() | Default.
account_timezone(Box, Default) ->
    case kz_doc:account_id(Box) of
        'undefined' -> Default;
        AccountId ->
            {'ok', AccountJObj} = kz_account:fetch(AccountId),
            kz_account:timezone(AccountJObj, Default)
    end.

-spec retries(doc()) -> maybe(integer()).
-spec retries(doc(), Default) -> integer() | Default.
retries(Box) ->
    retries(Box, 'undefined').
retries(Box, Default) ->
    kz_json:get_integer_value(?KEY_RETRIES, Box, Default).
