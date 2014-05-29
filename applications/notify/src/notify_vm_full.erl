%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600hz INC
%%% @doc
%%% Renders a custom account email template, or the system default,
%%% and sends the email with voicemail attachment to the user.
%%% @end
%%%
%%% @contributors
%%% Peter Defebvre
%%%
%%%-------------------------------------------------------------------
-module(notify_vm_full).

-export([init/0, handle_req/2]).

-include("notify.hrl").

-define(DEFAULT_TEXT_TMPL, 'notify_vm_full_text_tmpl').
-define(DEFAULT_HTML_TMPL, 'notify_vm_full_html_tmpl').
-define(DEFAULT_SUBJ_TMPL, 'notify_vm_full_subj_tmpl').

-define(MOD_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".voicemail_full">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% initialize the module
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    %% ensure the vm template can compile, otherwise crash the processes
    {'ok', _} = notify_util:compile_default_text_template(?DEFAULT_TEXT_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_html_template(?DEFAULT_HTML_TMPL, ?MOD_CONFIG_CAT),
    {'ok', _} = notify_util:compile_default_subject_template(?DEFAULT_SUBJ_TMPL, ?MOD_CONFIG_CAT),
    lager:debug("init done for ~s", [?MODULE]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_full_v(JObj),
    whapps_util:put_callid(JObj),

    lager:debug("a vm_full notice has been received, sending email notification"),

    {'ok', Account} = notify_util:get_account_doc(JObj),

    Props = create_template_props(JObj),

    CustomTxtTemplate = wh_json:get_value([<<"notifications">>, <<"vm_full">>, <<"email_text_template">>], Account),
    {'ok', TxtBody} = notify_util:render_template(CustomTxtTemplate, ?DEFAULT_TEXT_TMPL, Props),

    CustomHtmlTemplate = wh_json:get_value([<<"notifications">>, <<"vm_full">>, <<"email_html_template">>], Account),
    {'ok', HTMLBody} = notify_util:render_template(CustomHtmlTemplate, ?DEFAULT_HTML_TMPL, Props),

    CustomSubjectTemplate = wh_json:get_value([<<"notifications">>, <<"vm_full">>, <<"email_subject_template">>], Account),
    {'ok', Subject} = notify_util:render_template(CustomSubjectTemplate, ?DEFAULT_SUBJ_TMPL, Props),


    case get_vm_owner_email(JObj) of
        'undefined' ->
            case notify_util:get_rep_email(Account) of
                'undefined' ->
                    SysAdminEmail = whapps_config:get(?MOD_CONFIG_CAT, <<"default_to">>, <<"">>),
                    build_and_send_email(TxtBody, HTMLBody, Subject, SysAdminEmail, Props);
                RepEmail ->
                    build_and_send_email(TxtBody, HTMLBody, Subject, RepEmail, Props)
            end;
        Email ->
            build_and_send_email(TxtBody, HTMLBody, Subject, Email, Props)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create the props used by the template render function
%% @end
%%--------------------------------------------------------------------
-spec create_template_props(wh_json:object()) -> proplist().
create_template_props(JObj) ->
    Admin = notify_util:find_admin(wh_json:get_value(<<"Authorized-By">>, JObj)),
    [{<<"send_from">>, get_send_from(Admin)}
     ,{<<"voicemail">>, [{<<"name">>, get_vm_name(JObj)}]}
    ].


-spec get_vm_owner_email(wh_json:object()) -> ne_binary() | 'error'.
get_vm_owner_email(JObj) ->
    case get_vm_doc(JObj) of
        'error' -> 'error';
        VMDoc ->
            AccoundDB = wh_json:get_value(<<"Account-DB">>, JObj),
            OwnerId = wh_json:get_value(<<"owner_id">>, VMDoc),
            case couch_mgr:open_cache_doc(AccoundDB, OwnerId) of
                {'ok', UserDoc} ->
                    wh_json:get_first_defined([<<"email">>, <<"username">>], UserDoc);
                {'error', _E} ->
                    lager:info("could not open doc ~p in ~p", [OwnerId, AccoundDB]),
                    'error'
            end
    end.

-spec get_vm_name(wh_json:object()) -> binary().
get_vm_name(JObj) ->
    case get_vm_doc(JObj) of
        'error' -> <<>>;
        VMDoc -> wh_json:get_binary_value(<<"name">>, VMDoc)
    end.

-spec get_vm_doc(wh_json:object()) -> wh_json:object() | 'error'.
get_vm_doc(JObj) ->
    VMId = wh_json:get_value(<<"Voicemail-Box">>, JObj),
    AccoundDB = wh_json:get_value(<<"Account-DB">>, JObj),
    case couch_mgr:open_cache_doc(AccoundDB, VMId) of
        {'ok', VMDoc} -> VMDoc;
        {'error', _E} ->
            lager:info("could not open doc ~p in ~p", [VMId, AccoundDB]),
            'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_send_from(wh_json:object()) -> ne_binary().
get_send_from(Admin) ->
    DefaultFrom = wh_util:to_binary(node()),
    case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"send_from_admin_email">>, 'true') of
        'false' -> DefaultFrom;
        'true' -> wh_json:get_ne_value(<<"email">>, Admin, DefaultFrom)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec build_and_send_email(iolist(), iolist(), iolist(), ne_binary() | [ne_binary(),...], proplist()) -> 'ok'.
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) when is_list(To)->
    _ = [build_and_send_email(TxtBody, HTMLBody, Subject, T, Props) || T <- To],
    'ok';
build_and_send_email(TxtBody, HTMLBody, Subject, To, Props) ->
    From = props:get_value(<<"send_from">>, Props),
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
    notify_util:send_email(From, To, Email),
    'ok'.
