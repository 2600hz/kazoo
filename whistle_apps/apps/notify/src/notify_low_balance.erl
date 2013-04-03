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
-module(notify_low_balance).

-export([init/0, send/2]).
-export([collect_recipients/1]).


-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, notify_low_balance_text_tmpl).
-define(DEFAULT_HTML_TMPL, notify_low_balance_html_tmpl).
-define(DEFAULT_SUBJ_TMPL, notify_low_balance_subj_tmpl).

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".low_balance">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% initialize the module
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {ok, _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {ok, _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec send(integer(), wh_json:object()) -> any().
send(CurrentBalance, Account) ->
    AccountId = wh_json:get_value(<<"_id">>, Account),
    case collect_recipients(AccountId) of
        [] -> 'ok';
        To ->
            lager:debug("sending low balance alert for account ~s", [AccountId]),
            Props = create_template_props(CurrentBalance, Account),

            CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"low_balance">>, <<"email_text_template">>], Account),
            {ok, TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

            CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"low_balance">>, <<"email_html_template">>], Account),
            {ok, HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

            CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"low_balance">>, <<"email_subject_template">>], Account),
            {ok, Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),

            build_and_send_email(TxtBody, HTMLBody, Subject, To, Props)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(integer(), wh_json:object()) -> wh_proplist().
create_template_props(CurrentBalance, Account) ->
    AccountDb = wh_util:format_account_id(wh_json:get_value(<<"_id">>, Account), 'encoded'),
    Threshold = notify_account_crawler:low_balance_threshold(AccountDb),
    [{<<"account">>, notify_util:json_to_template_props(Account)}
     ,{<<"service">>, notify_util:get_service_props(wh_json:new(), Account, ?MOD_CONFIG_CAT)}
     ,{<<"current_balance">>, pretty_print_dollars(wht_util:units_to_dollars(CurrentBalance))}
     ,{<<"threshold">>, pretty_print_dollars(Threshold)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec pretty_print_dollars(float()) -> ne_binary().
pretty_print_dollars(Amount) ->
    wh_util:to_binary(io_lib:format("$~.2f", [Amount])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | ne_binaries(), wh_proplist()) -> any().
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
%%
%% @end
%%--------------------------------------------------------------------
-spec collect_recipients(ne_binary()) -> ne_binaries().
collect_recipients(AccountId) ->
    Routines = [fun(T) -> maybe_send_to_account_admins(AccountId, T) end
                ,fun(T) -> maybe_send_to_master_support(T) end
               ],
    To = lists:foldl(fun(F, T) -> F(T) end, [], Routines),
    sets:to_list(sets:from_list(To)).

-spec maybe_send_to_account_admins(ne_binary(), ne_binaries()) -> ne_binaries().
maybe_send_to_account_admins(AccountId, To) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),
    maybe_send_to_account_admins(MasterAccountId, AccountId, To).

-spec maybe_send_to_account_admins(ne_binary(), ne_binary(), ne_binaries()) -> ne_binaries().
maybe_send_to_account_admins(MasterAccountId, AccountId, To) ->
    ResellerId = wh_services:find_reseller_id(AccountId),
    maybe_send_to_account_admins(ResellerId, MasterAccountId, AccountId, To).

-spec maybe_send_to_account_admins(ne_binary(), ne_binary(), ne_binary(), ne_binaries()) -> ne_binaries().
maybe_send_to_account_admins(ResellerId, ResellerId, AccountId, To) ->
    send_to_account_admins(AccountId, To);
maybe_send_to_account_admins(ResellerId, MasterAccountId, AccountId, To) ->
    lager:debug("following reseller tree for ~s to ~s", [AccountId, ResellerId]),
    maybe_send_to_account_admins(MasterAccountId, ResellerId, To).

-spec send_to_account_admins(ne_binary(), ne_binaries()) -> ne_binaries().
send_to_account_admins(AccountId, To) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    ViewOptions = [{'key', <<"user">>}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {'ok', JObjs} ->
            find_account_admins(JObjs, To);
        {'error', _R} ->
            lager:debug("faild to find users in ~s: ~p", [AccountId, _R]),
            To
    end.

-spec find_account_admins(wh_json:objects(), ne_binaries()) -> ne_binaries().
find_account_admins([], To) ->
    To;
find_account_admins([JObj|JObjs], To) ->
    Email = wh_json:get_ne_value([<<"doc">>, <<"email">>], JObj),
    case wh_json:get_value([<<"doc">>, <<"priv_level">>], JObj) =:= <<"admin">>
        andalso Email =/= 'undefined'
    of
        'false' -> find_account_admins(JObjs, To);
        'true' ->
            find_account_admins(JObjs, [Email|To])
    end.

-spec maybe_send_to_master_support(ne_binaries()) -> ne_binaries().
maybe_send_to_master_support(To) ->
    case whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<"">>) of
        'undefined' -> To;
        DefaultTo -> [DefaultTo|To]
    end.
