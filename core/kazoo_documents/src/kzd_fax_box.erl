%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Device document manipulation
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec owner_id(doc()) -> kz_term:api_binary().
owner_id(Box) ->
    owner_id(Box, 'undefined').

-spec owner_id(doc(), Default) -> kz_term:ne_binary() | Default.
owner_id(Box, Default) ->
    kz_json:get_value(?KEY_OWNER_ID, Box, Default).

-spec timezone(doc()) -> kz_term:ne_binary().
timezone(Box) ->
    timezone(Box, 'undefined').

-spec timezone(doc(), Default) -> kz_term:ne_binary() | Default.
timezone(Box, Default) ->
    case kz_json:get_value(?KEY_TIMEZONE, Box) of
        'undefined'   -> owner_timezone(Box, Default);
        <<"inherit">> -> owner_timezone(Box, Default); %% UI-1808
        TZ -> TZ
    end.

-spec owner_timezone(doc(), Default) -> kz_term:ne_binary() | Default.
owner_timezone(Box, Default) ->
    case kzd_users:fetch(kz_doc:account_db(Box), owner_id(Box)) of
        {'ok', OwnerJObj} -> kzd_users:timezone(OwnerJObj, Default);
        {'error', _} -> kzd_accounts:timezone(kz_doc:account_id(Box), Default)
    end.

-spec retries(doc()) -> kz_term:api_integer().
retries(Box) ->
    retries(Box, 'undefined').

-spec retries(doc(), Default) -> integer() | Default.
retries(Box, Default) ->
    kz_json:get_integer_value(?KEY_RETRIES, Box, Default).
