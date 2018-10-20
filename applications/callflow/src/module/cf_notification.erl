%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2018, 2600Hz
%%% @doc Sends a notification for missed call.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`send_at'</dt>
%%%   <dd>Defines when send customer defined notification. Possible values `callflow_exec' and `channel_destroy'.
%%%   For `callflow_exec' value notifications is send during calllfow execution.
%%%   For `channel_destroy' value notification is send after channel(bridge) is destroed.</dd>
%%%
%%%   <dt>`template_id'</dt>
%%%   <dd>Defines customer defined notification template name</dd>
%%%
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
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_notification).

-behaviour(gen_cf_action).

-export([handle/2]).
-export([send_notification/3]).

-include("callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc maybe add termination handler then unconditionally
%%      continue the flow
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"send_at">>, Data, <<"channel_destroy">>) of
        <<"channel_destroy">> -> add_handler(Data, Call);
        <<"callflow_exec">> -> send_notification(Call, [], Data);
        Value -> lager:warning("Bad value of \"send_at\": \"~s\"", [Value])
    end,
    cf_exe:continue(Call).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_handler(kz_json:object(), kapps_call:call()) -> 'ok'.
add_handler(Data, Call) -> cf_exe:add_termination_handler(Call, {?MODULE, 'send_notification', [Data]}).

-spec send_notification(kapps_call:call(), kz_json:object(), kz_json:object()) -> 'ok'.
send_notification(Call, Notify, Data) ->
    case kz_json:get_value(<<"template_id">>, Data) of
        'undefined' -> lager:debug("Template_ID is not defined, ignoring");
        TemplateID ->
            lager:debug("trying to send customer defined notification \"~s\" for call-id ~s", [TemplateID, kapps_call:call_id_direct(Call)]),
            Emails = find_email_addresses(Call, kz_json:get_value(<<"recipients">>, Data, [])),
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
                      ,{<<"Template-ID">>, TemplateID}
                      ,{<<"To">>, Emails}
                       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ]
                     ),
            kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_cf_notification/1)
    end.

%%------------------------------------------------------------------------------
%% @doc Try to find email addressed using module's data object
%% @end
%%------------------------------------------------------------------------------
-spec find_email_addresses(kapps_call:call(), kz_json:objects()) -> kz_term:ne_binaries().
find_email_addresses(Call, Recipients) ->
    AccountDb = kz_util:format_account_db(kapps_call:account_id(Call)),
    Emails = lists:flatten(
               [Emails
                || JObj <- Recipients,
                   Emails <- find_email_addresses_by_type(AccountDb, JObj, kz_json:get_ne_binary_value(<<"type">>, JObj)),
                   kz_term:is_not_empty(Emails)
               ]
              ),
    case Emails of
        [] -> 'undefined';
        _ -> Emails
    end.

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
                Email <- [kzd_user:email(kz_json:get_value(<<"doc">>, J))],
                kz_term:is_not_empty(Email)
            ];
        {'error', _R} ->
            lager:debug("failed to read users email address: ~p", [_R]),
            []
    end.
