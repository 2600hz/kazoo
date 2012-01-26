%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%% James Aimonetti <james@2600hz.org>
%%% Karl Anderson <karl@2600hz.org>
%%%
%%% Created : 22 Dec 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_new_account).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_new_account_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_new_account_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_new_account_subj_tmpl).

-define(NOTIFY_NEW_ACCT_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".new_account">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% initialize the module
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, ?DEFAULT_TEXT_TMPL} = erlydtl:compile(whapps_config:get(?NOTIFY_NEW_ACCT_CONFIG_CAT, default_text_template), ?DEFAULT_TEXT_TMPL),
    {ok, ?DEFAULT_HTML_TMPL} = erlydtl:compile(whapps_config:get(?NOTIFY_NEW_ACCT_CONFIG_CAT, default_html_template), ?DEFAULT_HTML_TMPL),
    {ok, ?DEFAULT_SUBJ_TMPL} = erlydtl:compile(whapps_config:get(?NOTIFY_NEW_ACCT_CONFIG_CAT, default_subject_template), ?DEFAULT_SUBJ_TMPL),
    ?LOG_SYS("init done for new account notify").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_notifications:new_account_v(JObj),
    whapps_util:put_callid(JObj),

    ?LOG_START("a new account has been created, sending email notification"),

    {AcctDb, AcctId} = case {wh_json:get_value(<<"Account-DB">>, JObj), wh_json:get_value(<<"Account-ID">>, JObj)} of
                           {undefined, undefined} -> undefined;
                           {undefined, Id1} ->
                               {wh_util:format_account_id(Id1, encoded), Id1};
                           {Id2, undefined} ->
                               {Id2, wh_util:format_account_id(Id2, raw)};
                           Else -> Else 
                       end,

    ?LOG("attempting to load account doc ~s from ~s", [AcctId, AcctDb]),
    {ok, AllDocs} = couch_mgr:all_docs(AcctDb, [{<<"include_docs">>, true}]),
    Account = find_account(AllDocs),
    Admin = find_admin(AllDocs), 

    To = wh_json:get_value(<<"Email">>, Admin, whapps_config:get(?NOTIFY_NEW_ACCT_CONFIG_CAT, <<"default_to">>, <<"">>)),

    DefaultFrom = list_to_binary([<<"no_reply@">>, wh_util:to_binary(net_adm:localhost())]),
    From = wh_json:get_value([<<"notifications">>, <<"new_account">>, <<"send_from">>], Account
                             ,whapps_config:get(?NOTIFY_NEW_ACCT_CONFIG_CAT, <<"default_from">>, DefaultFrom)),

    Props = [{<<"To">>, To}
             ,{<<"From">>, From}
             |get_template_props(JObj, Admin, Account, AllDocs)
            ],
    ?LOG("creating new account notice"),
    
    CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"new_account">>, <<"email_text_template">>], Account),
    {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"new_account">>, <<"email_html_template">>], Account),
    {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"new_account">>, <<"email_subject_template">>], Account),
    {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),
    
    send_new_account_email(TxtBody, HTMLBody, Subject, Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec get_template_props/4 :: (wh_json:json_object(), wh_json:json_object(), wh_json:json_object(), wh_json:json_object()) -> proplist().
get_template_props(Event, Admin, Account, AllDocs) ->
    Owners = [{wh_json:get_value([<<"doc">>, <<"_id">>], J1), wh_json:get_value(<<"doc">>, J1)}
              || J1 <- AllDocs
                     ,wh_json:get_value([<<"doc">>, <<"pvt_type">>], J1) =:= <<"user">>
             ],
    Devices = [wh_json:get_value(<<"doc">>, J2)
               || J2 <- AllDocs
                      ,wh_json:get_value([<<"doc">>, <<"pvt_type">>], J2) =:= <<"device">>
              ],
    DevicesWithOwners = [wh_json:set_value(<<"user">>, props:get_value(wh_json:get_value(<<"owner_id">>, J3), Owners), J3)
                         || J3 <- Devices
                        ],
    [{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"devices">>, notify_util:json_to_template_props(DevicesWithOwners)}
     ,{<<"admin">>, notify_util:json_to_template_props(Admin)}
     ,{<<"request">>, notify_util:json_to_template_props(Event)}
     ,{<<"service">>, notify_util:get_service_props(Event, Account, ?NOTIFY_NEW_ACCT_CONFIG_CAT)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send_new_account_email/4 :: (iolist(), iolist(), iolist(), proplist()) -> 'ok'.
send_new_account_email(TxtBody, HTMLBody, Subject, Props) ->
    From = props:get_value(<<"From">>, Props),
    To = props:get_value(<<"To">>, Props),
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
    ?LOG("sending password recovery notice to: ~p", [To]),
    notify_util:send_email(From, To, Email),
    ok.                

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_account/1 :: ([] | [wh_json:json_object(),...]) -> wh_json:json_object().
find_account([]) ->
    wh_json:new();
find_account([Doc|Docs]) ->
    JObj = wh_json:get_value(<<"doc">>, Doc),
    case wh_json:get_value(<<"pvt_type">>, JObj) of
        <<"account">> -> JObj;
        _ -> find_account(Docs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_admin/1 :: ([] | [wh_json:json_object(),...]) -> wh_json:json_object().
find_admin([]) ->
    wh_json:new();
find_admin([Doc|Docs]) ->
    JObj = wh_json:get_value(<<"doc">>, Doc),
    case wh_json:get_value(<<"pvt_type">>, JObj) =:= <<"user">> andalso wh_json:get_value(<<"priv_level">>, JObj) =:= <<"admin">> of
        true -> JObj;
        false -> find_admin(Docs)
    end.
