%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_voicemail_box).

-export([notification_emails/1, notification_emails/2
         ,owner_id/1, owner_id/2

         ,set_notification_emails/2
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(KEY_NOTIFY_EMAILS, <<"notify_email_addresses">>).
-define(KEY_OWNER_ID, <<"owner_id">>).

-spec notification_emails(doc()) -> ne_binaries().
-spec notification_emails(doc(), Default) -> ne_binaries() | Default.
notification_emails(Box) ->
    notification_emails(Box, []).
notification_emails(Box, Default) ->
    wh_json:get_value(?KEY_NOTIFY_EMAILS, Box, Default).

-spec set_notification_emails(doc(), api_binaries()) -> doc().
set_notification_emails(Box, 'undefined') ->
    wh_json:delete_key(?KEY_NOTIFY_EMAILS, Box);
set_notification_emails(Box, Emails) ->
    wh_json:set_value(?KEY_NOTIFY_EMAILS, Emails, Box).

-spec owner_id(doc()) -> api_binary().
-spec owner_id(doc(), Default) -> ne_binary() | Default.
owner_id(Box) ->
    owner_id(Box, 'undefined').
owner_id(Box, Default) ->
    wh_json:get_value(?KEY_OWNER_ID, Box, Default).
