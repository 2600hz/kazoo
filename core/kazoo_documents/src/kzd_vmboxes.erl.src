-module(kzd_vmboxes).

-export([new/0]).
-export([check_if_owner/1, check_if_owner/2, set_check_if_owner/2]).
-export([delete_after_notify/1, delete_after_notify/2, set_delete_after_notify/2]).
-export([is_setup/1, is_setup/2, set_is_setup/2]).
-export([mailbox/1, mailbox/2, set_mailbox/2]).
-export([media/1, media/2, set_media/2]).
-export([media_extension/1, media_extension/2, set_media_extension/2]).
-export([name/1, name/2, set_name/2]).
-export([not_configurable/1, not_configurable/2, set_not_configurable/2]).
-export([notify/1, notify/2, set_notify/2]).
-export([notify_email_addresses/1, notify_email_addresses/2, set_notify_email_addresses/2]).
-export([owner_id/1, owner_id/2, set_owner_id/2]).
-export([pin/1, pin/2, set_pin/2]).
-export([require_pin/1, require_pin/2, set_require_pin/2]).
-export([save_after_notify/1, save_after_notify/2, set_save_after_notify/2]).
-export([skip_greeting/1, skip_greeting/2, set_skip_greeting/2]).
-export([skip_instructions/1, skip_instructions/2, set_skip_instructions/2]).
-export([timezone/1, timezone/2, set_timezone/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec check_if_owner(doc()) -> boolean().
-spec check_if_owner(doc(), Default) -> boolean() | Default.
check_if_owner(Doc) ->
    check_if_owner(Doc, true).
check_if_owner(Doc, Default) ->
    kz_json:get_boolean_value(<<"check_if_owner">>, Doc, Default).

-spec set_check_if_owner(doc(), boolean()) -> doc().
set_check_if_owner(Doc, CheckIfOwner) ->
    kz_json:set_value(<<"check_if_owner">>, CheckIfOwner, Doc).

-spec delete_after_notify(doc()) -> boolean().
-spec delete_after_notify(doc(), Default) -> boolean() | Default.
delete_after_notify(Doc) ->
    delete_after_notify(Doc, false).
delete_after_notify(Doc, Default) ->
    kz_json:get_boolean_value(<<"delete_after_notify">>, Doc, Default).

-spec set_delete_after_notify(doc(), boolean()) -> doc().
set_delete_after_notify(Doc, DeleteAfterNotify) ->
    kz_json:set_value(<<"delete_after_notify">>, DeleteAfterNotify, Doc).

-spec is_setup(doc()) -> boolean().
-spec is_setup(doc(), Default) -> boolean() | Default.
is_setup(Doc) ->
    is_setup(Doc, false).
is_setup(Doc, Default) ->
    kz_json:get_boolean_value(<<"is_setup">>, Doc, Default).

-spec set_is_setup(doc(), boolean()) -> doc().
set_is_setup(Doc, IsSetup) ->
    kz_json:set_value(<<"is_setup">>, IsSetup, Doc).

-spec mailbox(doc()) -> api_ne_binary().
-spec mailbox(doc(), Default) -> ne_binary() | Default.
mailbox(Doc) ->
    mailbox(Doc, 'undefined').
mailbox(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"mailbox">>, Doc, Default).

-spec set_mailbox(doc(), ne_binary()) -> doc().
set_mailbox(Doc, Mailbox) ->
    kz_json:set_value(<<"mailbox">>, Mailbox, Doc).

-spec media(doc()) -> kz_json:object().
-spec media(doc(), Default) -> kz_json:object() | Default.
media(Doc) ->
    media(Doc, {}).
media(Doc, Default) ->
    kz_json:get_json_value(<<"media">>, Doc, Default).

-spec set_media(doc(), kz_json:object()) -> doc().
set_media(Doc, Media) ->
    kz_json:set_value(<<"media">>, Media, Doc).

-spec media_extension(doc()) -> binary().
-spec media_extension(doc(), Default) -> binary() | Default.
media_extension(Doc) ->
    media_extension(Doc, <<"mp3">>).
media_extension(Doc, Default) ->
    kz_json:get_binary_value(<<"media_extension">>, Doc, Default).

-spec set_media_extension(doc(), binary()) -> doc().
set_media_extension(Doc, MediaExtension) ->
    kz_json:set_value(<<"media_extension">>, MediaExtension, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"name">>, Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value(<<"name">>, Name, Doc).

-spec not_configurable(doc()) -> boolean().
-spec not_configurable(doc(), Default) -> boolean() | Default.
not_configurable(Doc) ->
    not_configurable(Doc, false).
not_configurable(Doc, Default) ->
    kz_json:get_boolean_value(<<"not_configurable">>, Doc, Default).

-spec set_not_configurable(doc(), boolean()) -> doc().
set_not_configurable(Doc, NotConfigurable) ->
    kz_json:set_value(<<"not_configurable">>, NotConfigurable, Doc).

-spec notify(doc()) -> api_object().
-spec notify(doc(), Default) -> kz_json:object() | Default.
notify(Doc) ->
    notify(Doc, 'undefined').
notify(Doc, Default) ->
    kz_json:get_json_value(<<"notify">>, Doc, Default).

-spec set_notify(doc(), kz_json:object()) -> doc().
set_notify(Doc, Notify) ->
    kz_json:set_value(<<"notify">>, Notify, Doc).

-spec notify_email_addresses(doc()) -> ne_binaries().
-spec notify_email_addresses(doc(), Default) -> ne_binaries() | Default.
notify_email_addresses(Doc) ->
    notify_email_addresses(Doc, []).
notify_email_addresses(Doc, Default) ->
    kz_json:get_list_value(<<"notify_email_addresses">>, Doc, Default).

-spec set_notify_email_addresses(doc(), ne_binaries()) -> doc().
set_notify_email_addresses(Doc, NotifyEmailAddresses) ->
    kz_json:set_value(<<"notify_email_addresses">>, NotifyEmailAddresses, Doc).

-spec owner_id(doc()) -> api_ne_binary().
-spec owner_id(doc(), Default) -> ne_binary() | Default.
owner_id(Doc) ->
    owner_id(Doc, 'undefined').
owner_id(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"owner_id">>, Doc, Default).

-spec set_owner_id(doc(), ne_binary()) -> doc().
set_owner_id(Doc, OwnerId) ->
    kz_json:set_value(<<"owner_id">>, OwnerId, Doc).

-spec pin(doc()) -> api_ne_binary().
-spec pin(doc(), Default) -> ne_binary() | Default.
pin(Doc) ->
    pin(Doc, 'undefined').
pin(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"pin">>, Doc, Default).

-spec set_pin(doc(), ne_binary()) -> doc().
set_pin(Doc, Pin) ->
    kz_json:set_value(<<"pin">>, Pin, Doc).

-spec require_pin(doc()) -> boolean().
-spec require_pin(doc(), Default) -> boolean() | Default.
require_pin(Doc) ->
    require_pin(Doc, false).
require_pin(Doc, Default) ->
    kz_json:get_boolean_value(<<"require_pin">>, Doc, Default).

-spec set_require_pin(doc(), boolean()) -> doc().
set_require_pin(Doc, RequirePin) ->
    kz_json:set_value(<<"require_pin">>, RequirePin, Doc).

-spec save_after_notify(doc()) -> boolean().
-spec save_after_notify(doc(), Default) -> boolean() | Default.
save_after_notify(Doc) ->
    save_after_notify(Doc, false).
save_after_notify(Doc, Default) ->
    kz_json:get_boolean_value(<<"save_after_notify">>, Doc, Default).

-spec set_save_after_notify(doc(), boolean()) -> doc().
set_save_after_notify(Doc, SaveAfterNotify) ->
    kz_json:set_value(<<"save_after_notify">>, SaveAfterNotify, Doc).

-spec skip_greeting(doc()) -> boolean().
-spec skip_greeting(doc(), Default) -> boolean() | Default.
skip_greeting(Doc) ->
    skip_greeting(Doc, false).
skip_greeting(Doc, Default) ->
    kz_json:get_boolean_value(<<"skip_greeting">>, Doc, Default).

-spec set_skip_greeting(doc(), boolean()) -> doc().
set_skip_greeting(Doc, SkipGreeting) ->
    kz_json:set_value(<<"skip_greeting">>, SkipGreeting, Doc).

-spec skip_instructions(doc()) -> boolean().
-spec skip_instructions(doc(), Default) -> boolean() | Default.
skip_instructions(Doc) ->
    skip_instructions(Doc, false).
skip_instructions(Doc, Default) ->
    kz_json:get_boolean_value(<<"skip_instructions">>, Doc, Default).

-spec set_skip_instructions(doc(), boolean()) -> doc().
set_skip_instructions(Doc, SkipInstructions) ->
    kz_json:set_value(<<"skip_instructions">>, SkipInstructions, Doc).

-spec timezone(doc()) -> api_ne_binary().
-spec timezone(doc(), Default) -> ne_binary() | Default.
timezone(Doc) ->
    timezone(Doc, 'undefined').
timezone(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"timezone">>, Doc, Default).

-spec set_timezone(doc(), ne_binary()) -> doc().
set_timezone(Doc, Timezone) ->
    kz_json:set_value(<<"timezone">>, Timezone, Doc).
