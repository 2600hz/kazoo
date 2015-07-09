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
         ,owner/1
         ,timezone/1, timezone/2
         ,skip_instructions/1, skip_instructions/2
         ,skip_greeting/1, skip_greeting/2
         ,pin/1, pin/2
         ,mailbox_number/1, mailbox_number/2
         ,pin_required/1, pin_required/2
         ,check_if_owner/1, check_if_owner/2
         ,is_setup/1, is_setup/2

         ,set_notification_emails/2
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(KEY_NOTIFY_EMAILS, <<"notify_email_addresses">>).
-define(KEY_OLD_NOTIFY_EMAILS, <<"notify_email_address">>).
-define(KEY_OWNER_ID, <<"owner_id">>).
-define(KEY_TIMEZONE, <<"timezone">>).
-define(KEY_SKIP_INSTRUCTIONS, <<"skip_instructions">>).
-define(KEY_SKIP_GREETING, <<"skip_greeting">>).
-define(KEY_PIN, <<"pin">>).
-define(KEY_MAILBOX_NUMBER, <<"mailbox">>).
-define(KEY_PIN_REQUIRED, <<"require_pin">>).
-define(KEY_CHECK_IF_OWNER, <<"check_if_owner">>).
-define(KEY_IS_SETUP, <<"is_setup">>).

-spec notification_emails(doc()) -> ne_binaries().
-spec notification_emails(doc(), Default) -> ne_binaries() | Default.
notification_emails(Box) ->
    notification_emails(Box, []).
notification_emails(Box, Default) ->
    case wh_json:get_value(?KEY_NOTIFY_EMAILS, Box) of
        'undefined' -> wh_json:get_value(?KEY_OLD_NOTIFY_EMAILS, Box, Default);
        Emails -> Emails
    end.

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

-spec skip_instructions(doc()) -> boolean().
-spec skip_instructions(doc(), Default) -> boolean() | Default.
skip_instructions(Box) ->
    skip_instructions(Box, 'false').
skip_instructions(Box, Default) ->
    wh_json:is_true(?KEY_SKIP_INSTRUCTIONS, Box, Default).

-spec skip_greeting(doc()) -> boolean().
-spec skip_greeting(doc(), Default) -> boolean() | Default.
skip_greeting(Box) ->
    skip_greeting(Box, 'false').
skip_greeting(Box, Default) ->
    wh_json:is_true(?KEY_SKIP_GREETING, Box, Default).

-spec pin(doc()) -> api_binary().
-spec pin(doc(), Default) -> ne_binary() | Default.
pin(Box) ->
    pin(Box, 'undefined').
pin(Box, Default) ->
    wh_json:get_binary_value(?KEY_PIN, Box, Default).

-spec mailbox_number(doc()) -> api_binary().
-spec mailbox_number(doc(), Default) -> ne_binary() | Default.
mailbox_number(Box) ->
    mailbox_number(Box, 'undefined').
mailbox_number(Box, Default) ->
    wh_json:get_binary_value(?KEY_MAILBOX_NUMBER, Box, Default).

-spec pin_required(doc()) -> boolean().
-spec pin_required(doc(), Default) -> boolean() | Default.
pin_required(Box) ->
    pin_required(Box, 'false').
pin_required(Box, Default) ->
    wh_json:is_true(?KEY_PIN_REQUIRED, Box, Default).

-spec check_if_owner(doc()) -> boolean().
-spec check_if_owner(doc(), Default) -> boolean() | Default.
check_if_owner(Box) ->
    check_if_owner(Box, 'false').
check_if_owner(Box, Default) ->
    wh_json:is_true(?KEY_CHECK_IF_OWNER, Box, Default).

-spec is_setup(doc()) -> boolean().
-spec is_setup(doc(), Default) -> boolean() | Default.
is_setup(Box) ->
    is_setup(Box, 'false').
is_setup(Box, Default) ->
    wh_json:is_true(?KEY_IS_SETUP, Box, Default).
