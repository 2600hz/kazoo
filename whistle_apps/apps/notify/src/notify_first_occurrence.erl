%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Craws accounts and triggers 'first' registration and call emails
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_first_occurrence).

-include("notify.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-export([init/0]).
-export([handle_req/2]).
-export([start_crawler/0]).
-export([crawler_loop/0]).

-define(SERVER, ?MODULE).

-define(DEFAULT_TEXT_TMPL, notify_init_occur_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_init_occur_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_init_occur_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".first_occurrence">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    _ = put(callid, ?LOG_SYSTEM_ID),

    %% ensure the vm template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"crawl_for_first_occurrence">>, true) of
        false -> ok;
        true ->
            Crawler = {?MODULE
                       ,{?MODULE, start_crawler, []}
                       ,permanent, 5000, worker, [?MODULE]
                      },
            Start = supervisor:start_child(notify_sup, Crawler),
            lager:debug("crawler: ~p", [Start])
    end,
    lager:debug("init done for ~s", [?MODULE]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_crawler/0 :: () -> {'ok', pid()}.
start_crawler() ->
    {ok, proc_lib:spawn_link(fun crawler_loop/0)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Handles AMQP request comming from gen_listener (reg_resp)
%% @end
%%--------------------------------------------------------------------
-spec handle_req/2 :: (wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    true = wapi_registration:query_resp_v(JObj),
    AccountId = case wh_json:is_true(<<"Multiple">>, JObj) of
                    true -> wh_json:get_value([<<"Fields">>, 1, <<"Account-ID">>], JObj);
                    false -> wh_json:get_value([<<"Fields">>, <<"Account-ID">>], JObj)
                end,
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {ok, Account} ->
            notify_initial_registration(AccountDb, Account);
        _E -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to set a flag marking the initial_registration notice as
%% sent, and if successfull then send the email. This will fail if
%% another process does it first (ie: generating a 409 conflict).
%% @end
%%--------------------------------------------------------------------
-spec notify_initial_registration/2 :: (ne_binary(), wh_json:object()) -> 'ok'.
notify_initial_registration(AccountDb, JObj) ->
    Account = wh_json:set_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>]
                                ,true
                                ,JObj),
    case couch_mgr:save_doc(AccountDb, Account) of
        {ok, _} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, Account),
            first_occurrence_notice(Account, <<"registration">>);
        _E -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to set a flag marking the initial_call notice as
%% sent, and if successfull then send the email. This will fail if
%% another process does it first (ie: generating a 409 conflict).
%% @end
%%--------------------------------------------------------------------
-spec notify_initial_call/2 :: (ne_binary(), wh_json:object()) -> any().
notify_initial_call(AccountDb, JObj) ->
    Account = wh_json:set_value([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>]
                                ,true
                                ,JObj),

    case couch_mgr:save_doc(AccountDb, Account) of
        {ok, _} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, Account),
            first_occurrence_notice(Account, <<"call">>);
        _ -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send an email notifying that a first occurrence event has happened.
%% @end
%%--------------------------------------------------------------------
first_occurrence_notice(Account, Occurrence) ->
    lager:debug("creating first occurrence notice"),

    Props = create_template_props(Account, Occurrence),

    CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"first_occurrence">>, <<"email_text_template">>], Account),
    {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"first_occurrence">>, <<"email_html_template">>], Account),
    {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"first_occurrence">>, <<"email_subject_template">>], Account),
    {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

    To = wh_json:get_value([<<"notifications">>, <<"first_occurrence">>, <<"send_to">>], Account
                           ,whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<"">>)),
    RepEmail = notify_util:get_rep_email(Account),

    _ = build_and_send_email(TxtBody, HTMLBody, Subject, To, Props),
    build_and_send_email(TxtBody, HTMLBody, Subject, RepEmail, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props/2 :: (wh_json:object(), ne_binary()) -> proplist().
create_template_props(Account, Occurrence) ->
    Admin = notify_util:find_admin(Account),
    [{<<"event">>, Occurrence}
     ,{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
     ,{<<"service">>, notify_util:get_service_props(wh_json:new(), Account, ?MOD_CONFIG_CAT)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email/5 :: (iolist(), iolist(), iolist(), api_binary() | ne_binaries(), wh_proplist()) -> any().
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To) ->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To];
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    Service = props:get_value(<<"service">>, Props),
    From = props:get_value(<<"send_from">>, Service),
    %% Content Type, Subtype, Headers, Parameters, Body
    Email = {<<"multipart">>, <<"mixed">>
                 ,[{<<"From">>, From}
                   ,{<<"To">>, To}
                   ,{<<"Subject">>, Subject}
                  ]
             ,[]
             ,[{<<"multipart">>, <<"alternative">>, [], []
                ,[{<<"text">>, <<"plain">>, [{<<"Content-Type">>, <<"text/plain">>}], [], iolist_to_binary(TxtBody)}
                  ,{<<"text">>, <<"html">>, [{<<"Content-Type">>, <<"text/html">>}], [], iolist_to_binary(HTMLBody)}
                 ]
               }
              ]
            },
    notify_util:send_email(From, To, Email).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Occasionally loop over accounts that still require first occurence
%% notifications and test if any should be sent.
%% @end
%%--------------------------------------------------------------------
-spec crawler_loop/0 :: () -> no_return().
crawler_loop() ->
    _ = case couch_mgr:get_all_results(?WH_ACCOUNTS_DB, <<"notify/first_occurance">>) of
            {ok, Results} ->
                [test_for_initial_occurrences(Result)
                 || Result <- Results
                ];
            _ -> ok
        end,

    Cycle = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"crawler_sleep_time">>, 300000),
    erlang:send_after(Cycle, self(), wakeup),
    flush(),
    proc_lib:hibernate(?MODULE, crawler_loop, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the account has yet to send an initial call/registration
%% notification.
%% If the account has not sent the notification for calls then check
%% if there are any cdrs, and send the notice if so.
%% If the account has not sent the notification for registrations
%% then request the current registrations for the realm.  When/if
%% the response comes in a notification will be sent.
%% @end
%%--------------------------------------------------------------------
-spec test_for_initial_occurrences/1 :: (wh_json:object()) -> 'ok'.
test_for_initial_occurrences(Result) ->
    Realm = wh_json:get_value([<<"value">>, <<"realm">>], Result),
    {ok, Srv} = notify_sup:listener_proc(),
    lager:debug("testing realm '~s' for intial occurrences", [Realm]),
    case wh_json:is_true([<<"value">>, <<"sent_initial_registration">>], Result) orelse
        wh_util:is_empty(Realm) of
        true -> ok;
        false ->
            Q = gen_listener:queue_name(Srv),
            Req = [{<<"Realm">>, Realm}
                   ,{<<"Fields">>, [<<"Account-ID">>]}
                   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                  ],
            wapi_registration:publish_query_req(Req)
    end,
    case wh_json:is_true([<<"value">>, <<"sent_initial_call">>], Result) of
        true -> ok;
        false ->
            AccountDb = wh_json:get_value([<<"value">>, <<"account_db">>], Result),
            ViewOptions = [{key, <<"cdr">>}
                           ,{limit, <<"1">>}
                          ],
            case wh_util:is_empty(AccountDb) orelse
                couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) 
            of
                {ok, [_|_]} ->
                    AccountId = wh_json:get_value(<<"id">>, Result),
                    case couch_mgr:open_doc(AccountDb, AccountId) of
                        {ok, JObj} -> notify_initial_call(AccountDb, JObj);
                        _ -> ok
                    end;                    
                _ ->
                    ok
            end
    end,
    Delay = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"crawler_interaccount_delay">>, 1000),
    timer:sleep(Delay).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensure there are no messages in the process queue
%% @end
%%--------------------------------------------------------------------
-spec flush/0 :: () -> 'true'.
flush() ->
    receive _ -> flush()
    after 0 -> true
    end.
