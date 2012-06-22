%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_util).

-export([send_email/3]).
-export([render_template/3]).
-export([normalize_proplist/1]).
-export([json_to_template_props/1]).
-export([get_service_props/2, get_service_props/3]).
-export([get_rep_email/1]).
-export([compile_default_text_template/2]).
-export([compile_default_html_template/2]).
-export([compile_default_subject_template/2]).
-export([compile_default_template/3]).
-export([find_admin/1]).
-export([get_account_doc/1]).

-include("notify.hrl").
-include_lib("whistle/include/wh_databases.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send_email/3 :: (ne_binary(), 'undefined' | binary(), term()) -> 'ok'.
send_email(_, undefined, _) ->
    ok;
send_email(_, <<>>, _) ->
    ok;
send_email(From, To, Email) ->
    Encoded = mimemail:encode(Email),
    Relay = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"relay">>, <<"localhost">>)),
    lager:debug("sending email to ~s from ~s via ~s", [To, From, Relay]),
    ReqId = get(callid),
    gen_smtp_client:send({From, [To], Encoded}, [{relay, Relay}]
                         ,fun(X) -> put(callid, ReqId),
                                    lager:debug("email relay responded: ~p", [X])
                          end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec json_to_template_props/1 :: ('undefined' | wh_json:json_object()) -> 'undefined' | proplist().
json_to_template_props(undefined) ->
    undefined;
json_to_template_props(JObj) ->
    normalize_proplist(wh_json:recursive_to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_proplist/1 :: (proplist()) -> proplist().
normalize_proplist(Props) ->
    [normalize_proplist_element(Elem) || Elem <- Props].

normalize_proplist_element({K, V}) when is_list(V) ->
    {normalize_value(K), normalize_proplist(V)};
normalize_proplist_element({K, V}) when is_binary(V) ->
    {normalize_value(K), mochiweb_html:escape(V)};
normalize_proplist_element({K, V}) ->
    {normalize_value(K), V};
normalize_proplist_element(Else) ->
    Else.

normalize_value(Value) ->
    binary:replace(wh_util:to_lower_binary(Value), <<"-">>, <<"_">>, [global]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_default_text_template/2 :: (atom(), ne_binary()) -> {'ok', atom()}.
-spec compile_default_html_template/2 :: (atom(), ne_binary()) -> {'ok', atom()}.
-spec compile_default_subject_template/2 :: (atom(), ne_binary()) -> {'ok', atom()}.
-spec compile_default_template/3 :: (atom(), ne_binary(), atom()) -> {'ok', atom()}.

compile_default_text_template(TemplateModule, Category) ->
    compile_default_template(TemplateModule, Category, default_text_template).

compile_default_html_template(TemplateModule, Category) ->
    compile_default_template(TemplateModule, Category, default_html_template).

compile_default_subject_template(TemplateModule, Category) ->
    compile_default_template(TemplateModule, Category, default_subject_template).

compile_default_template(TemplateModule, Category, Key) ->
    {ok, TemplateModule} = erlydtl:compile(whapps_config:get(Category, Key), TemplateModule).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec render_template/3 :: (ne_binary() | 'undefined', atom(), proplist()) -> {'ok', iolist()} | {'error', term()}.
render_template(undefined, DefaultTemplate, Props) ->
    lager:debug("rendering default ~s template", [DefaultTemplate]),
    DefaultTemplate:render(Props);
render_template(Template, DefaultTemplate, Props) ->
    try
        CustomTemplate = wh_util:to_atom(list_to_binary([couch_mgr:get_uuid(), "_"
                                                        ,wh_util:to_binary(DefaultTemplate)
                                                        ])
                                         ,true),
        lager:debug("compiling custom ~s template", [DefaultTemplate]),
        {ok, CustomTemplate} = erlydtl:compile(Template, CustomTemplate),

        lager:debug("rendering custom template ~s", [CustomTemplate]),
        Result = CustomTemplate:render(Props),

        code:purge(CustomTemplate),
        code:delete(CustomTemplate),
        Result
    catch
        _:_E ->
            lager:debug("error compiling custom ~s template: ~p", [DefaultTemplate, _E]),
            render_template(undefined, DefaultTemplate, Props)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% determine the service name, provider, and url. Hunts (in order)
%% in the event, parent account notification object, and then default.
%% @end
%%--------------------------------------------------------------------
-spec get_service_props/2 :: (wh_json:json_object(), ne_binary()) -> proplist().
-spec get_service_props/3 :: (wh_json:json_object(), wh_json:json_object(), ne_binary()) -> proplist().

get_service_props(Account, ConfigCat) ->
    get_service_props(wh_json:new(), Account, ConfigCat).

get_service_props(Request, Account, ConfigCat) ->
    DefaultUrl = wh_json:get_ne_value(<<"service_url">>, Request
                                      ,whapps_config:get(ConfigCat, <<"default_service_url">>, <<"http://apps.2600hz.com">>)),
    DefaultName = wh_json:get_ne_value(<<"service_name">>, Request
                                       ,whapps_config:get(ConfigCat, <<"default_service_name">>, <<"VOIP Services">>)),
    DefaultProvider = wh_json:get_ne_value(<<"service_provider">>, Request
                                           ,whapps_config:get(ConfigCat, <<"default_service_provider">>, <<"2600hz">>)),
    DefaultNumber = wh_json:get_ne_value(<<"support_number">>, Request
                                           ,whapps_config:get(ConfigCat, <<"default_support_number">>, <<"(415) 886-7900">>)),
    DefaultEmail = wh_json:get_ne_value(<<"support_email">>, Request
                                        ,whapps_config:get(ConfigCat, <<"default_support_email">>, <<"support@2600hz.com">>)),
    UnconfiguredFrom = list_to_binary([<<"no_reply@">>, wh_util:to_binary(net_adm:localhost())]),
    DefaultFrom = wh_json:get_ne_value(<<"send_from">>, Request
                                       ,whapps_config:get(ConfigCat, <<"default_from">>, UnconfiguredFrom)),
    Tree = wh_json:get_value(<<"pvt_tree">>, Account, []),
    [_, Module] = binary:split(ConfigCat, <<".">>),
    case Tree =/= [] andalso couch_mgr:open_doc(?WH_ACCOUNTS_DB, lists:last(Tree)) of
        {ok, JObj} ->
            lager:debug("looking for notifications '~s' service info in: ~s", [Module, wh_json:get_value(<<"_id">>, JObj)]),
            [{<<"url">>, wh_json:get_value([<<"notifications">>, Module, <<"service_url">>], JObj, DefaultUrl)}
             ,{<<"name">>, wh_json:get_value([<<"notifications">>, Module, <<"service_name">>], JObj, DefaultName)}
             ,{<<"provider">>, wh_json:get_value([<<"notifications">>, Module, <<"service_provider">>], JObj, DefaultProvider)}
             ,{<<"support_number">>, wh_json:get_value([<<"notifications">>, Module, <<"support_number">>], JObj, DefaultNumber)}
             ,{<<"support_email">>, wh_json:get_value([<<"notifications">>, Module, <<"support_email">>], JObj, DefaultEmail)}
             ,{<<"send_from">>, wh_json:get_value([<<"notifications">>, Module, <<"send_from">>], JObj, DefaultFrom)}
             ,{<<"host">>, wh_util:to_binary(net_adm:localhost())}
            ];
        _E ->
            lager:debug("failed to find parent for notifications '~s' service info: ~p", [Module, _E]),
            [{<<"url">>, DefaultUrl}
             ,{<<"name">>, DefaultName}
             ,{<<"provider">>, DefaultProvider}
             ,{<<"support_number">>, DefaultNumber}
             ,{<<"support_email">>, DefaultEmail}
             ,{<<"send_from">>, DefaultFrom}
             ,{<<"host">>, wh_util:to_binary(net_adm:localhost())}
            ]
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Try to find the email address of a sub_account_rep for a given
%% account object
%% @end
%%--------------------------------------------------------------------
-spec get_rep_email/1 :: (wh_json:json_object()) -> 'undefined' | ne_binary().
get_rep_email(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    case wh_json:get_value(<<"pvt_tree">>, JObj, []) of
        [] -> undefined;
        Tree -> get_rep_email(lists:reverse(Tree), AccountId)
    end.

get_rep_email([], _) ->
    undefined;
get_rep_email([Parent|Parents], AccountId) ->
    ParentDb = wh_util:format_account_id(Parent, encoded),
    ViewOptions = [include_docs
                   ,{key, AccountId}
                  ],
    lager:debug("attempting to find sub account rep for ~s in parent account ~s", [AccountId, Parent]),
    case couch_mgr:get_results(ParentDb, <<"sub_account_reps/find_assignments">>, ViewOptions) of
        {ok, [Result|_]} ->
            case wh_json:get_value([<<"doc">>, <<"email">>], Result) of
                undefined ->
                    lager:debug("found rep but they have no email, attempting to get email of admin"),
                    wh_json:get_value(<<"email">>, find_admin(ParentDb));
                Else ->
                    lager:debug("found rep but email: ~s", [Else]),
                    Else
            end;
        _E ->
            lager:debug("failed to find rep for sub account, attempting next parent"),
            get_rep_email(Parents, Parents)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% try to find the first user with admin privileges and an email given
%% a sub account object or sub account db name.
%% @end
%%--------------------------------------------------------------------
-spec find_admin/1 :: ('undefined' | ne_binary() | wh_json:json_object()) -> wh_json:json_object().
find_admin(undefined) ->
    wh_json:new();
find_admin(AccountDb) when is_binary(AccountDb) ->
    ViewOptions = [{key, <<"user">>}
                   ,include_docs
                  ],
    case couch_mgr:get_results(AccountDb, <<"maintenance/listing_by_type">>, ViewOptions) of
        {ok, Users} ->
            Admins = [User || User <- Users
                                  ,wh_json:get_value([<<"doc">>, <<"priv_level">>], User) =:= <<"admin">>
                                  ,wh_json:get_ne_value([<<"doc">>, <<"email">>], User) =/= undefined
                     ],
            case Admins of
                [] ->
                    lager:debug("failed to find any admins with email addresses in ~s", [AccountDb]),
                    wh_json:new();
                Else ->
                    wh_json:get_value(<<"doc">>, hd(Else))
            end;
        _E ->
            lager:debug("faild to find users in ~s: ~p", [AccountDb, _E]),
            wh_json:new()
    end;
find_admin(Account) ->
    find_admin(wh_json:get_value(<<"pvt_account_db">>, Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% given a notification event try to open the account definition doc
%% @end
%%--------------------------------------------------------------------
-spec get_account_doc/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} |
                                                      {'error', term()} |
                                                      'undefined'.
get_account_doc(JObj) ->
    case {wh_json:get_value(<<"Account-DB">>, JObj), wh_json:get_value(<<"Account-ID">>, JObj)} of
        {undefined, undefined} -> undefined;
        {undefined, Id1} ->
            couch_mgr:open_doc(wh_util:format_account_id(Id1, encoded), Id1);
        {Id2, undefined} ->
            couch_mgr:open_doc(Id2, wh_util:format_account_id(Id2, raw));
        {Db, AccountId} ->
            couch_mgr:open_doc(Db, AccountId)
    end.
