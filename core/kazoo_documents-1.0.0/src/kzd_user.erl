%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_user).

-export([email/1, email/2
         ,voicemail_notification_enabled/1, voicemail_notification_enabled/2
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(KEY_EMAIL, <<"email">>).

-spec email(doc()) -> api_binary().
-spec email(doc(), Default) -> ne_binary() | Default.
email(User) ->
    email(User, 'undefined').
email(User, Default) ->
    wh_json:get_value(?KEY_EMAIL, User, Default).

-spec voicemail_notification_enabled(doc()) -> boolean().
-spec voicemail_notification_enabled(doc(), Default) -> boolean() | Default.
voicemail_notification_enabled(User) ->
    voicemail_notification_enabled(User, 'false').
voicemail_notification_enabled(User, Default) ->
    wh_json:is_true(<<"vm_to_email_enabled">>, User, Default).
