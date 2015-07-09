%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_fax_box).

-export([owner_id/1, owner_id/2
         ,owner/1
         ,timezone/1, timezone/2
         ,retries/1, retries/2
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(KEY_OWNER_ID, <<"owner_id">>).
-define(KEY_TIMEZONE, <<"fax_timezone">>).
-define(KEY_RETRIES, <<"retries">>).

-spec owner_id(doc()) -> api_binary().
-spec owner_id(doc(), Default) -> ne_binary() | Default.
owner_id(Box) ->
    owner_id(Box, 'undefined').
owner_id(Box, Default) ->
    wh_json:get_value(?KEY_OWNER_ID, Box, Default).

-spec owner(doc()) -> kzd_user:doc() | 'undefined'.
-spec owner(doc(), ne_binary()) -> kzd_user:doc() | 'undefined'.
owner(Box) ->
    case owner_id(Box) of
        'undefined' -> 'undefined';
        OwnerId -> owner(Box, OwnerId)
    end.

owner(Box, OwnerId) ->
    case couch_mgr:open_cache_doc(wh_doc:account_db(Box), OwnerId) of
        {'ok', OwnerJObj} -> OwnerJObj;
        {'error', 'not_found'} -> 'undefined'
    end.

-spec timezone(doc()) -> api_binary().
-spec timezone(doc(), Default) -> ne_binary() | Default.
timezone(Box) ->
    timezone(Box, 'undefined').
timezone(Box, Default) ->
    case wh_json:get_value(?KEY_TIMEZONE, Box) of
        'undefined' -> owner_timezone(Box, Default);
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
    {'ok', AccountJObj} = kz_account:fetch(wh_doc:account_id(Box)),
    kz_account:timezone(AccountJObj, Default).

-spec retries(doc()) -> api_integer().
-spec retries(doc(), Default) -> integer() | Default.
retries(Box) ->
    retries(Box, 'undefined').
retries(Box, Default) ->
    wh_json:get_integer_value(?KEY_RETRIES, Box, Default).
