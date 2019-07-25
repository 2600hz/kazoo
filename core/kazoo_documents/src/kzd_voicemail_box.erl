%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Device document manipulation
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_voicemail_box).

-export([new/0
        ,type/0
        ,notification_emails/1, notification_emails/2
        ,owner_id/1, owner_id/2
        ,timezone/1, timezone/2
        ,skip_instructions/1, skip_instructions/2
        ,skip_greeting/1, skip_greeting/2
        ,skip_envelope/1, skip_envelope/2
        ,pin/1, pin/2
        ,mailbox_number/1, mailbox_number/2
        ,pin_required/1, pin_required/2
        ,check_if_owner/1, check_if_owner/2
        ,is_setup/1, is_setup/2

        ,set_notification_emails/2
        ,media_extension/1
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(KEY_NOTIFY_EMAILS, <<"notify_email_addresses">>).
-define(KEY_OLD_NOTIFY_EMAILS, <<"notify_email_address">>).
-define(KEY_OWNER_ID, <<"owner_id">>).
-define(KEY_TIMEZONE, <<"timezone">>).
-define(KEY_SKIP_INSTRUCTIONS, <<"skip_instructions">>).
-define(KEY_SKIP_GREETING, <<"skip_greeting">>).
-define(KEY_SKIP_ENVELOPE, <<"skip_envelope">>).
-define(KEY_PIN, <<"pin">>).
-define(KEY_MAILBOX_NUMBER, <<"mailbox">>).
-define(KEY_PIN_REQUIRED, <<"require_pin">>).
-define(KEY_IS_FF_RW_ENABLED, <<"is_voicemail_ff_rw_enabled">>).
-define(KEY_SEEK_DURATION, <<"seek_duration_ms">>).
-define(KEY_CHECK_IF_OWNER, <<"check_if_owner">>).
-define(KEY_IS_SETUP, <<"is_setup">>).

-define(PVT_TYPE, <<"vmbox">>).
-define(DEFAULT_SEEK_DURATION, 10000).

-define(ACCOUNT_VM_EXTENSION(AccountId),
        kapps_account_config:get_global(AccountId
                                       ,<<"callflow">>
                                       ,[<<"voicemail">>, <<"extension">>]
                                       ,<<"mp3">>
                                       )
       ).

-spec new() -> doc().
new() ->
    kz_json:from_list([{<<"pvt_type">>, type()}]).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec notification_emails(doc()) -> kz_term:ne_binaries().
notification_emails(Box) ->
    notification_emails(Box, []).

-spec notification_emails(doc(), Default) -> kz_term:ne_binaries() | Default.
notification_emails(Box, Default) ->
    case kz_json:get_list_value(?KEY_NOTIFY_EMAILS, Box) of
        'undefined' ->
            case kz_json:get_value(?KEY_OLD_NOTIFY_EMAILS, Box, Default) of
                Email when is_binary(Email) -> [Email];
                Emails when is_list(Emails) -> Emails;
                _ -> []
            end;
        Emails -> Emails
    end.

-spec set_notification_emails(doc(), kz_term:api_binaries()) -> doc().
set_notification_emails(Box, 'undefined') ->
    kz_json:delete_key(?KEY_NOTIFY_EMAILS, Box);
set_notification_emails(Box, Emails) ->
    kz_json:set_value(?KEY_NOTIFY_EMAILS, Emails, Box).

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
        <<"inherit">> -> owner_timezone(Box, Default);  %% UI-1808
        TZ -> TZ
    end.

-spec owner_timezone(doc(), Default) -> kz_term:ne_binary() | Default.
owner_timezone(Box, Default) ->
    owner_timezone(Box, Default, owner_id(Box)).

owner_timezone(Box, Default, 'undefined') ->
    kzd_accounts:timezone(kz_doc:account_id(Box), Default);
owner_timezone(Box, Default, OwnerId) ->
    case kzd_users:fetch(kz_doc:account_db(Box), OwnerId) of
        {'ok', OwnerJObj} -> kzd_users:timezone(OwnerJObj, Default);
        {'error', _} -> kzd_accounts:timezone(kz_doc:account_id(Box), Default)
    end.

-spec skip_instructions(doc()) -> boolean().
skip_instructions(Box) ->
    skip_instructions(Box, 'false').

-spec skip_instructions(doc(), Default) -> boolean() | Default.
skip_instructions(Box, Default) ->
    kz_json:is_true(?KEY_SKIP_INSTRUCTIONS, Box, Default).

-spec skip_greeting(doc()) -> boolean().
skip_greeting(Box) ->
    skip_greeting(Box, 'false').

-spec skip_greeting(doc(), Default) -> boolean() | Default.
skip_greeting(Box, Default) ->
    kz_json:is_true(?KEY_SKIP_GREETING, Box, Default).

-spec skip_envelope(doc()) -> boolean().
skip_envelope(Box) ->
    skip_envelope(Box, 'false').

-spec skip_envelope(doc(), Default) -> boolean() | Default.
skip_envelope(Box, Default) ->
    kz_json:is_true(?KEY_SKIP_ENVELOPE, Box, Default).

-spec pin(doc()) -> kz_term:api_binary().
pin(Box) ->
    pin(Box, 'undefined').

-spec pin(doc(), Default) -> kz_term:ne_binary() | Default.
pin(Box, Default) ->
    kz_json:get_ne_binary_value(?KEY_PIN, Box, Default).

-spec mailbox_number(doc()) -> kz_term:api_binary().
mailbox_number(Box) ->
    mailbox_number(Box, 'undefined').

-spec mailbox_number(doc(), Default) -> kz_term:ne_binary() | Default.
mailbox_number(Box, Default) ->
    kz_json:get_binary_value(?KEY_MAILBOX_NUMBER, Box, Default).

-spec pin_required(doc()) -> boolean().
pin_required(Box) ->
    pin_required(Box, 'false').

-spec pin_required(doc(), Default) -> boolean() | Default.
pin_required(Box, Default) ->
    kz_json:is_true(?KEY_PIN_REQUIRED, Box, Default).

-spec check_if_owner(doc()) -> boolean().
check_if_owner(Box) ->
    check_if_owner(Box, 'false').

-spec check_if_owner(doc(), Default) -> boolean() | Default.
check_if_owner(Box, Default) ->
    kz_json:is_true(?KEY_CHECK_IF_OWNER, Box, Default).

-spec is_setup(doc()) -> boolean().
is_setup(Box) ->
    is_setup(Box, 'false').

-spec is_setup(doc(), Default) -> boolean() | Default.
is_setup(Box, Default) ->
    kz_json:is_true(?KEY_IS_SETUP, Box, Default).

-spec media_extension(doc()) -> kz_term:ne_binary().
media_extension(Box) ->
    AccountId = kz_doc:account_id(Box),
    kz_json:get_ne_binary_value(<<"media_extension">>, Box, ?ACCOUNT_VM_EXTENSION(AccountId)).
