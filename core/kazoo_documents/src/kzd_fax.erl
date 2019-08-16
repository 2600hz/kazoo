%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Fax document manipulation
%%% @author Luis Azedo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_fax).

-export([new/0
        ,type/0
        ,owner_id/1, owner_id/2
        ,faxbox_id/1, faxbox_id/2
        ,timezone/1, timezone/2
        ,retries/1, retries/2
        ,attempts/1, attempts/2
        ,from_number/1, from_number/2
        ,from_name/1, from_name/2
        ,to_number/1, to_number/2
        ,to_name/1, to_name/2
        ,identity_number/1, identity_number/2
        ,identity_name/1, identity_name/2
        ,folder/1, folder/2
        ,document/1, document/2
        ,document_url/1
        ,notifications/1
        ,rx_result/1, tx_result/1, result/1
        ,job_node/1, job_node/2
        ,job_status/1, job_status/2
        ,size/1, size/2
        ,pages/1, pages/2
        ,retry_after/1, retry_after/2
        ]
       ).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().

-export_type([doc/0]).

-define(KEY_FAXBOX_ID, <<"faxbox_id">>).
-define(KEY_OWNER_ID, <<"owner_id">>).
-define(KEY_TIMEZONE, <<"fax_timezone">>).
-define(KEY_RETRIES, <<"retries">>).
-define(KEY_RETRY_AFTER, <<"retry_after">>).
-define(KEY_ATTEMPTS, <<"attempts">>).
-define(KEY_FOLDER, <<"folder">>).
-define(KEY_NOTIFICATIONS, <<"notifications">>).
-define(KEY_RX_RESULT, <<"rx_result">>).
-define(KEY_TX_RESULT, <<"tx_result">>).
-define(KEY_PAGES, <<"pvt_pages">>).
-define(KEY_SIZE, <<"pvt_size">>).
-define(KEY_FROM_NAME, <<"from_name">>).
-define(KEY_FROM_NUMBER, <<"from_number">>).
-define(KEY_TO_NAME, <<"to_name">>).
-define(KEY_TO_NUMBER, <<"to_number">>).
-define(KEY_IDENTITY_NAME, <<"fax_identity_name">>).
-define(KEY_IDENTITY_NUMBER, <<"fax_identity_number">>).
-define(KEY_JOB_NODE, <<"pvt_job_node">>).
-define(KEY_JOB_STATUS, <<"pvt_job_status">>).
-define(KEY_DOCUMENT, <<"document">>).
-define(KEY_DOCUMENT_URL, [<<"document">>, <<"url">>]).

-define(PVT_TYPE, <<"fax">>).

-spec new() -> doc().
new() ->
    kz_json:from_list([{<<"pvt_type">>, type()}]).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec owner_id(doc()) -> kz_term:api_binary().
owner_id(FaxDoc) ->
    owner_id(FaxDoc, 'undefined').

-spec owner_id(doc(), Default) -> kz_term:ne_binary() | Default.
owner_id(FaxDoc, Default) ->
    kz_json:get_value(?KEY_OWNER_ID, FaxDoc, Default).

-spec faxbox_id(doc()) -> kz_term:api_binary().
faxbox_id(FaxDoc) ->
    faxbox_id(FaxDoc, 'undefined').

-spec faxbox_id(doc(), Default) -> kz_term:ne_binary() | Default.
faxbox_id(FaxDoc, Default) ->
    kz_json:get_value(?KEY_FAXBOX_ID, FaxDoc, Default).

-spec timezone(doc()) -> kz_term:api_binary().
timezone(FaxDoc) ->
    timezone(FaxDoc, 'undefined').

-spec timezone(doc(), Default) -> kz_term:ne_binary() | Default.
timezone(FaxDoc, Default) ->
    kz_json:get_value(?KEY_TIMEZONE, FaxDoc, Default).

-spec retries(doc()) -> kz_term:api_integer().
retries(FaxDoc) ->
    retries(FaxDoc, 'undefined').

-spec retries(doc(), Default) -> integer() | Default.
retries(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_RETRIES, FaxDoc, Default).

-spec attempts(doc()) -> kz_term:api_integer().
attempts(FaxDoc) ->
    attempts(FaxDoc, 'undefined').

-spec attempts(doc(), Default) -> integer() | Default.
attempts(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_ATTEMPTS, FaxDoc, Default).

-spec from_number(doc()) -> kz_term:api_binary().
from_number(FaxDoc) ->
    from_number(FaxDoc, 'undefined').

-spec from_number(doc(), Default) -> kz_term:api_binary() | Default.
from_number(FaxDoc, Default) ->
    kz_json:get_value(?KEY_FROM_NUMBER, FaxDoc, Default).

-spec to_number(doc()) -> kz_term:api_binary().
to_number(FaxDoc) ->
    to_number(FaxDoc, 'undefined').

-spec to_number(doc(), Default) -> kz_term:api_binary() | Default.
to_number(FaxDoc, Default) ->
    kz_json:get_value(?KEY_TO_NUMBER, FaxDoc, Default).

-spec from_name(doc()) -> kz_term:api_binary().
from_name(FaxDoc) ->
    from_name(FaxDoc, 'undefined').

-spec from_name(doc(), Default) -> kz_term:api_binary() | Default.
from_name(FaxDoc, Default) ->
    kz_json:get_value(?KEY_FROM_NAME, FaxDoc, Default).

-spec to_name(doc()) -> kz_term:api_binary().
to_name(FaxDoc) ->
    to_name(FaxDoc, 'undefined').

-spec to_name(doc(), Default) -> kz_term:api_binary() | Default.
to_name(FaxDoc, Default) ->
    kz_json:get_value(?KEY_TO_NAME, FaxDoc, Default).

-spec notifications(doc()) -> doc().
notifications(FaxDoc) ->
    kz_json:get_value(?KEY_NOTIFICATIONS, FaxDoc, kz_json:new()).

-spec folder(doc()) -> kz_term:api_binary().
folder(FaxDoc) ->
    folder(FaxDoc, 'undefined').

-spec folder(doc(), Default) -> kz_term:api_binary() | Default.
folder(FaxDoc, Default) ->
    kz_json:get_value(?KEY_FOLDER, FaxDoc, Default).

-spec document(doc()) -> kz_json:object().
document(FaxDoc) ->
    document(FaxDoc, kz_json:new()).

-spec document(doc(), Default) -> kz_json:object() | Default.
document(FaxDoc, Default) ->
    kz_json:get_json_value(?KEY_DOCUMENT, FaxDoc, Default).

-spec document_url(doc()) -> kz_term:api_ne_binary().
document_url(FaxDoc) ->
    kz_json:get_ne_binary_value(?KEY_DOCUMENT_URL, FaxDoc).

-spec identity_name(doc()) -> kz_term:api_binary().
identity_name(FaxDoc) ->
    identity_name(FaxDoc, 'undefined').

-spec identity_name(doc(), Default) -> kz_term:api_binary() | Default.
identity_name(FaxDoc, Default) ->
    kz_json:get_value(?KEY_IDENTITY_NAME, FaxDoc, Default).

-spec identity_number(doc()) -> kz_term:api_binary().
identity_number(FaxDoc) ->
    identity_number(FaxDoc, 'undefined').

-spec identity_number(doc(), Default) -> kz_term:api_binary() | Default.
identity_number(FaxDoc, Default) ->
    kz_json:get_value(?KEY_IDENTITY_NUMBER, FaxDoc, Default).

-spec tx_result(doc()) -> doc().
tx_result(FaxDoc) ->
    kz_json:get_value(?KEY_TX_RESULT, FaxDoc, kz_json:new()).

-spec rx_result(doc()) -> doc().
rx_result(FaxDoc) ->
    kz_json:get_value(?KEY_RX_RESULT, FaxDoc, kz_json:new()).

-spec result(doc()) -> doc().
result(FaxDoc) ->
    kz_json:get_first_defined([?KEY_RX_RESULT, ?KEY_TX_RESULT], FaxDoc, kz_json:new()).

-spec job_node(doc()) -> kz_term:api_binary().
job_node(FaxDoc) ->
    job_node(FaxDoc, 'undefined').

-spec job_node(doc(), Default) -> kz_term:api_binary() | Default.
job_node(FaxDoc, Default) ->
    kz_json:get_value(?KEY_JOB_NODE, FaxDoc, Default).

-spec job_status(doc()) -> kz_term:api_binary().
job_status(FaxDoc) ->
    job_status(FaxDoc, 'undefined').

-spec job_status(doc(), Default) -> kz_term:api_binary() | Default.
job_status(FaxDoc, Default) ->
    kz_json:get_value(?KEY_JOB_STATUS, FaxDoc, Default).

-spec size(doc()) -> kz_term:api_integer().
size(FaxDoc) ->
    size(FaxDoc, 'undefined').

-spec size(doc(), Default) -> integer() | Default.
size(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_SIZE, FaxDoc, Default).

-spec pages(doc()) -> kz_term:api_integer().
pages(FaxDoc) ->
    pages(FaxDoc, 'undefined').

-spec pages(doc(), Default) -> integer() | Default.
pages(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_PAGES, FaxDoc, Default).

-spec retry_after(doc()) -> kz_term:api_integer().
retry_after(FaxDoc) ->
    retry_after(FaxDoc, 'undefined').

-spec retry_after(doc(), Default) -> integer() | Default.
retry_after(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_RETRY_AFTER, FaxDoc, Default).
