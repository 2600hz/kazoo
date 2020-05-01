%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2020, 2600Hz
%%% @doc Sends a notification for missed call.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`recipients'</dt>
%%%   <dd>A list of JSON Objects contains different recipients for notifications.
%%%   Each recipient object have to keys:
%%%     <ul>
%%%       <li>`type': Specified what kind of recipient is this. Possible values are `user' or `email'.</li>
%%%       <li>`id': Based on `type' it could be a list of emails or user IDs (to get their email address) or
%%%       just a single email or user ID (to get its email from).</li>
%%%     </ul>
%%%   </dd>
%%% </dl>
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_missed_call_alert).

-behaviour(gen_cf_action).

-export([handle/2]).
-export([handle_termination/3]).

-include("callflow.hrl").

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".missed_call_alert">>).

%%------------------------------------------------------------------------------
%% @doc maybe add termination handler then unconditionally
%%      continue the flow
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    _ = maybe_add_handle(Data, Call, kapps_call:authorizing_type(Call)),
    cf_exe:continue(Call).

%%------------------------------------------------------------------------------
%% @doc only add handler if the call is coming from another account or
%%      from a resource external to kazoo
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_handle(kz_json:object(), kapps_call:call(), kz_term:api_binary()) -> kapps_call:call().
maybe_add_handle(Data, Call, 'undefined') ->
    lager:debug("inbound call from another account, adding termination handler..."),
    add_handler(Data, Call);
maybe_add_handle(Data, Call, <<"resource">>=_T) ->
    lager:debug("inbound call from ~s, adding termination handler...", [_T]),
    add_handler(Data, Call);
maybe_add_handle(Data, Call, <<"sys_info">>=_T) ->
    lager:debug("inbound call from ~s, adding termination handler...", [_T]),
    add_handler(Data, Call);
maybe_add_handle(_Data, Call, _Wat) ->
    lager:debug("call is not inbound to account from external resource or account, ignoring..."),
    Call.

%%------------------------------------------------------------------------------
%% @doc handles call termination, only handle if neither the bridged
%%      or voicemail message left flags are set
%% @end
%%------------------------------------------------------------------------------
-spec handle_termination(kapps_call:call(), kz_json:object(), kz_json:object()) -> 'ok'.
handle_termination(Call, JObj, Data) ->
    handle_termination(Call, JObj, Data, should_handle_termination(Call)).

-spec handle_termination(kapps_call:call(), kz_json:object(), kz_json:object(), boolean()) -> 'ok'.
handle_termination(_, _, _, 'false') ->
    lager:debug("doing nothing, call has been bridged or a message was left");
handle_termination(Call, JObj, Data, 'true') ->
    lager:debug("call went unanswered and left no voicemail message"),
    case find_email_addresses(Call, kz_json:get_value(<<"recipients">>, Data, [])) of
        [] -> send_missed_alert(Call, JObj, 'undefined');
        Emails -> send_missed_alert(Call, JObj, Emails)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_handler(kz_json:object(), kapps_call:call()) -> 'ok'.
add_handler(Data, Call) -> cf_exe:add_termination_handler(Call, {?MODULE, 'handle_termination', [Data]}).

-spec should_handle_termination(kapps_call:call()) -> boolean().
should_handle_termination(Call) ->
    not kapps_call:call_bridged(Call)
        andalso not kapps_call:message_left(Call).

-spec send_missed_alert(kapps_call:call(), kz_json:object(), kz_term:api_ne_binaries()) -> 'ok'.
send_missed_alert(Call, Notify, Emails) ->
    lager:debug("trying to publish missed_call_alert for call-id ~s", [kapps_call:call_id_direct(Call)]),
    Props = props:filter_undefined(
              [{<<"From-User">>, kapps_call:from_user(Call)}
              ,{<<"From-Realm">>, kapps_call:from_realm(Call)}
              ,{<<"To-User">>, kapps_call:to_user(Call)}
              ,{<<"To-Realm">>, kapps_call:to_realm(Call)}
              ,{<<"Account-ID">>, kapps_call:account_id(Call)}
              ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
              ,{<<"Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
              ,{<<"Timestamp">>, kz_time:now_s()}
              ,{<<"Call-ID">>, kapps_call:call_id_direct(Call)}
              ,{<<"Notify">>, Notify}
              ,{<<"Call-Bridged">>, kapps_call:call_bridged(Call)}
              ,{<<"Message-Left">>, kapps_call:message_left(Call)}
              ,{<<"To">>, Emails}
               | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
              ]
             ),
    kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_missed_call/1).

%%------------------------------------------------------------------------------
%% @doc Try to find email addressed using module's data object
%% @end
%%------------------------------------------------------------------------------
-spec find_email_addresses(kapps_call:call(), kz_json:objects()) -> kz_term:ne_binaries().
find_email_addresses(Call, Recipients) ->
    AccountDb = kzs_util:format_account_db(kapps_call:account_id(Call)),
    lists:flatten(
      [Emails
       || JObj <- Recipients,
          Emails <- find_email_addresses_by_type(AccountDb, JObj, kz_json:get_ne_binary_value(<<"type">>, JObj)),
          kz_term:is_not_empty(Emails)
      ]
     ).

%%------------------------------------------------------------------------------
%% @doc Possible values for recipient type can be:
%%      * email: an email or a list of emails
%%      * user: a user id or a list of user ids to read their email
%%              addresses from
%% @end
%%------------------------------------------------------------------------------
-spec find_email_addresses_by_type(kz_term:ne_binary(), kz_json:object(), kz_term:api_binary()) -> kz_term:ne_binaries().
find_email_addresses_by_type(AccountDb, JObj, <<"email">>) ->
    get_email_addresses(AccountDb, kz_json:get_value(<<"id">>, JObj));
find_email_addresses_by_type(AccountDb, JObj, <<"user">>) ->
    find_users_addresses(AccountDb, kz_json:get_value(<<"id">>, JObj));
find_email_addresses_by_type(_AccountDb, _JObj, _) ->
    [].

%%------------------------------------------------------------------------------
%% @doc an email or a list of emails
%% @end
%%------------------------------------------------------------------------------
-spec get_email_addresses(kz_term:ne_binary(), kz_term:api_binary() | kz_term:ne_binaries()) -> kz_term:ne_binaries().
get_email_addresses(_AccountDb, 'undefined') -> [];
get_email_addresses(_AccountDb, <<_/binary>>=Email) -> [Email];
get_email_addresses(_AccountDb, Emails) when is_list(Emails) ->
    [E || E <- Emails, kz_term:is_not_empty(E)];
get_email_addresses(_AccountDb, _) -> [].

%%------------------------------------------------------------------------------
%% @doc a user id or a list of user ids to read their email
%%      addresses from
%% @end
%%------------------------------------------------------------------------------
-spec find_users_addresses(kz_term:ne_binary(), kz_term:api_binary() | kz_term:ne_binaries()) -> kz_term:ne_binaries().
find_users_addresses(_AccountDb, 'undefined') -> [];
find_users_addresses(AccountDb, <<_/binary>>=UserId) -> bulk_read_emails(AccountDb, [UserId]);
find_users_addresses(AccountDb, Ids) when is_list(Ids) -> bulk_read_emails(AccountDb, Ids);
find_users_addresses(_AccountDb, _) -> [].

-spec bulk_read_emails(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
bulk_read_emails(AccountDb, Ids) ->
    case kz_datamgr:open_docs(AccountDb, Ids) of
        {'ok', JObjs} ->
            [Email
             || J <- JObjs,
                _LogMe <- [kz_term:is_not_empty(kz_json:get_value(<<"error">>, J))
                           andalso lager:debug("failed to open ~p in db ~s: ~p"
                                              ,[kz_json:get_value(<<"key">>, J)
                                               ,AccountDb
                                               ,kz_json:get_value(<<"error">>, J)
                                               ]
                                              )
                          ],
                Email <- [kzd_users:email(kz_json:get_value(<<"doc">>, J))],
                kz_term:is_not_empty(Email)
            ];
        {'error', _R} ->
            lager:debug("failed to read users email address: ~p", [_R]),
            []
    end.
