%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Signup module
%%%
%%% Handle client requests for new account on-boarding.  This is a
%%% special, one-off module because:
%%%
%%% * it authenticates and authorizes itself
%%% * it has a completely unique role
%%% * it operates without an account id (or account db)
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_signup).

-export([init/0
        ,allowed_methods/0, allowed_methods/1 %% only accept 0 or 1 path token
        ,resource_exists/0, resource_exists/1
        ,authorize/1
        ,authenticate/1
        ,validate/1, validate/2
        ,put/1, post/2
        ]).

%% cleanup process
-export([start_link/0, init_it/0]).

-include("crossbar.hrl").

-define(SIGNUP_DB, <<"signups">>).

-define(VIEW_FILE, <<"views/signup.json">>).
-define(VIEW_ACTIVATION_KEYS, <<"signups/listing_by_key">>).
-define(VIEW_ACTIVATION_REALM, <<"signups/listing_by_realm">>).
-define(VIEW_ACTIVATION_CREATED, <<"signups/listing_by_created">>).

-define(SIGNUP_CONF, [code:priv_dir('crossbar'), "/signup/signup.conf"]).

-record(state, {cleanup_interval = 5 * ?SECONDS_IN_HOUR :: integer() %% once every 5 hours (in seconds)
               ,signup_lifespan = ?SECONDS_IN_DAY :: integer() %% 24 hours (in seconds)
               ,register_cmd = 'undefined' :: api_atom()
               ,activation_email_plain = 'undefined' :: api_atom()
               ,activation_email_html = 'undefined' :: api_atom()
               ,activation_email_from = 'undefined' :: api_atom()
               ,activation_email_subject = 'undefined' :: api_atom()
               }).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.signup">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.signup">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.signup">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.signup">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.put.signup">>, ?MODULE, 'put'),

    _ = kz_datamgr:db_create(?SIGNUP_DB),

    _ = case kz_datamgr:update_doc_from_file(?SIGNUP_DB, 'crossbar', ?VIEW_FILE) of
            {'error', _} -> kz_datamgr:load_doc_from_file(?SIGNUP_DB, 'crossbar', ?VIEW_FILE);
            {'ok', _} -> 'ok'
        end,

    _ = supervisor:start_child('crossbar_sup', crossbar_sup:child_spec(?MODULE)),
    ok.

-spec start_link() -> {ok, pid()}.
start_link() ->
    {'ok', proc_lib:spawn_link(?MODULE, 'init_it', [])}.

-spec init_it() -> no_return().
init_it() ->
    kz_util:put_callid(?LOG_SYSTEM_ID),
    State = init_state(),
    cleanup_loop(State).

cleanup_loop(#state{cleanup_interval=CleanupInterval}=State) ->
    Wait = CleanupInterval * ?MILLISECONDS_IN_SECOND,
    receive
        {'send_activation_email', Context} ->
            _ = send_activation_email(Context, State),
            cleanup_loop(State);
        {'register', Context} ->
            _ = exec_register_command(Context, State),
            cleanup_loop(State);
        _ ->
            cleanup_loop(State)
    after
        Wait ->
            cleanup_signups(State),
            cleanup_loop(State)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() -> [?HTTP_PUT].
allowed_methods(_Thing) -> [?HTTP_POST].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    (?HTTP_PUT) = cb_context:req_verb(Context),
    validate_new_signup(cb_context:set_account_db(Context, ?SIGNUP_DB)).

validate(Context, ActivationKey) ->
    (?HTTP_POST) = cb_context:req_verb(Context),
    check_activation_key(ActivationKey, cb_context:set_account_db(Context, ?SIGNUP_DB)).

-spec authorize(cb_context:context()) -> 'true'.
-spec authorize_nouns(req_nouns()) -> 'true'.
authorize(Context) ->
    authorize_nouns(cb_context:req_nouns(Context)).

authorize_nouns([{<<"signup">>,[]}]) -> 'true';
authorize_nouns([{<<"signup">>,[_]}]) -> 'true'.

-spec authenticate(cb_context:context()) -> 'true'.
-spec authenticate_nouns(req_nouns()) -> 'true'.
authenticate(Context) ->
    authenticate_nouns(cb_context:req_nouns(Context)).

authenticate_nouns([{<<"signup">>,[]}]) -> 'true';
authenticate_nouns([{<<"signup">>,[_]}]) -> 'true'.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    JObj = cb_context:doc(Context),
    case activate_signup(JObj) of
        {'ok', Account, User} ->
            _ = kz_datamgr:del_doc(?SIGNUP_DB, JObj),
            NewRespData = kz_json:from_list([{<<"account">>, Account}
                                            ,{<<"user">>, User}
                                            ]),
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, NewRespData}
                                        ]);
        _Else ->
            cb_context:add_system_error('datastore_fault', Context)
    end.

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context1 = crossbar_doc:save(cb_context:set_account_db(Context, ?SIGNUP_DB)),
    case cb_context:resp_status(Context1) of
        'success' ->
            P = crossbar_sup:find_proc(?MODULE),
            P ! {'send_activation_email', Context1},
            P ! {'register', Context},
            cb_context:set_resp_data(Context1, []);
        _ ->
            cb_context:add_system_error('datastore_fault', Context)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new signup document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec validate_new_signup(cb_context:context()) -> cb_context:context().
validate_new_signup(Context) ->
    JObj = cb_context:req_data(Context),
    {AccountErrors, Account} = validate_account(kz_json:get_value(<<"account">>, JObj), Context),
    {UserErrors, User} = validate_user(kz_json:get_value(<<"user">>, JObj), Context),
    case {AccountErrors, UserErrors} of
        {[], []} ->
            NewDoc = kz_json:from_list([{<<"pvt_user">>, User}
                                       ,{<<"pvt_account">>, Account}
                                       ,{<<"pvt_activation_key">>, create_activation_key()}
                                       ]),
            cb_context:setters(Context, [{fun cb_context:set_doc/2, NewDoc}
                                        ,{fun cb_context:set_resp_status/2, 'success'}
                                        ]);
        _Else ->
            crossbar_util:response_invalid_data(kz_json:from_list([{<<"account">>, AccountErrors}
                                                                  ,{<<"user">>, UserErrors}
                                                                  ]), Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the account realm is unique and if the account is valid
%% @end
%%--------------------------------------------------------------------
-spec validate_account(api_object(), cb_context:context()) ->
                              {path_tokens(), api_object()}.
validate_account('undefined', _) ->
    lager:debug("signup did not contain an account definition"),
    {[<<"account">>], 'undefined'};
validate_account(Account, Context) ->
    case is_unique_realm(kz_json:get_value(<<"realm">>, Account))
        andalso crossbar_maintenance:create_account(cb_context:set_req_data(Context, Account)) of
        'false' ->
            {[<<"duplicate realm">>], 'undefined'};
        {'ok', Context1} ->
            'success' = cb_context:resp_status(Context1),
            lager:debug("signup account is valid"),
            {[], cb_context:doc(Context1)};  %% doc = AccountJObj
        {'error', Errors} ->
            {Errors, 'undefined'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the user object is valid
%% @end
%%--------------------------------------------------------------------
-spec validate_user(api_object(), cb_context:context()) ->
                           {path_tokens(), api_object()}.
validate_user('undefined', _) ->
    lager:debug("signup did not contain an user definition"),
    {[<<"user">>], 'undefined'};
validate_user(User, Context) ->
    Context1 = cb_users_v1:create_user(cb_context:set_req_data(Context, User)),
    case cb_context:resp_status(Context1) of
        'success' ->
            lager:debug("signup user is valid"),
            {[], cb_context:doc(Context1)};
        _ ->
            Errors = cb_context:resp_data(Context1),
            lager:debug("signup user definition is not valid"),
            {Errors, 'undefined'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% generates a random activation key
%% @end
%%--------------------------------------------------------------------
-spec create_activation_key() -> ne_binary().
create_activation_key() ->
    ActivationKey = kz_util:to_hex_binary(crypto:strong_rand_bytes(32)),
    lager:debug("created new activation key ~s", [ActivationKey]),
    ActivationKey.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a signup document from the database
%% @end
%%--------------------------------------------------------------------
-spec check_activation_key(ne_binary(), cb_context:context()) -> cb_context:context().
check_activation_key(ActivationKey, Context) ->
    case kz_datamgr:get_results(?SIGNUP_DB, ?VIEW_ACTIVATION_KEYS, [{'key', ActivationKey}
                                                                   ,'include_docs'
                                                                   ])
    of
        {'ok', []} ->
            lager:debug("activation key not found"),
            crossbar_util:response('error', <<"invalid activation key">>, 403, Context);
        {'ok', [JObj|_]} ->
            lager:debug("activation key is valid"),
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_doc/2, kz_json:get_value(<<"doc">>, JObj)}
                                        ]);
        _ ->
            lager:debug("db error while looking up activation key"),
            crossbar_util:response('error', <<"invalid activation key">>, 403, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Activate signup document by creating an account and user
%% @end
%%--------------------------------------------------------------------
-spec activate_signup(kz_json:object()) ->
                             {'ok', kz_json:object(), kz_json:object()} |
                             {'error', 'creation_failed' | 'account_undefined' | 'user_undefined'}.
activate_signup(JObj) ->
    case activate_account(kz_json:get_value(<<"pvt_account">>, JObj)) of
        {'ok', Account} ->
            activate_user(Account, kz_json:get_value(<<"pvt_user">>, JObj));
        {'error', _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create the account defined on the signup document
%% @end
%%--------------------------------------------------------------------
-spec activate_account(api_object()) ->
                              {'ok', kz_json:object()} |
                              {'error', 'creation_failed' | 'account_undefined'}.
activate_account('undefined') ->
    {'error', 'account_undefined'};
activate_account(Account) ->
    Event = <<"*.execute.put.accounts">>,
    Payload = [cb_context:set_doc(cb_context:new(), Account)],
    Context1 = crossbar_bindings:fold(Event, Payload),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            AccountId = kz_doc:id(JObj),
            lager:debug("created new account ~s", [AccountId]),
            {'ok', JObj};
        _ ->
            lager:debug("could not create a new account"),
            {'error', 'creation_failed'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a remote host authenticates the shared token it will return
%% an account and user, ensure the user exists locally (creating if not)
%% @end
%%--------------------------------------------------------------------
-spec activate_user(kz_json:object(), api_object()) ->
                           {'ok', kz_json:object(), kz_json:object()} |
                           {'error', 'user_undefined' | 'creation_failed'}.
activate_user(_, 'undefined') ->
    {'error', 'user_undefined'};
activate_user(Account, User) ->
    AccountId = kz_doc:id(Account),
    Db = kz_util:format_account_id(AccountId, 'encoded'),
    Event = <<"*.execute.put.users">>,
    Payload = [cb_context:setters(cb_context:new(), [{fun cb_context:set_doc/2, User}
                                                    ,{fun cb_context:set_account_db/2, Db}
                                                    ])],
    Context1 = crossbar_bindings:fold(Event, Payload),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:resp_data(Context1),
            UserId = kz_doc:id(JObj),
            lager:debug("created new user ~s in account ~s", [UserId, AccountId]),
            {'ok', Account, JObj};
        _ ->
            lager:debug("could not create a new user in account ~s", [AccountId]),
            {'error', 'creation_failed'}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% if the system has a register command defined in priv/signup/signup.conf
%% then exectute it now.
%% @end
%%--------------------------------------------------------------------
-spec exec_register_command(cb_context:context(), #state{}) ->
                                   'ok' | string().
exec_register_command(_, #state{register_cmd='undefined'}) -> 'ok';
exec_register_command(Context, #state{register_cmd=CmdTmpl}) ->
    Props = template_props(Context),
    {'ok', Cmd} = kz_template:render(CmdTmpl, Props),
    lager:debug("executing register command ~s", [Cmd]),
    os:cmd(binary_to_list(iolist_to_binary(Cmd))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_activation_email(cb_context:context(), #state{}) ->
                                   {'ok', pid()} |
                                   {'error', term()}.
send_activation_email(Context
                     ,#state{activation_email_subject=SubjectTmpl
                            ,activation_email_from=FromTmpl
                            }=State) ->
    JObj = cb_context:doc(Context),
    ReqId = cb_context:req_id(Context),
    Props = template_props(Context),
    To = kz_json:get_value([<<"pvt_user">>, <<"email">>], JObj),
    Subject = case kz_template:render(SubjectTmpl, Props) of
                  {'ok', S} -> S;
                  _ -> <<"Confirm your account activation">>
              end,
    From = case kz_template:render(FromTmpl, Props) of
               {'ok', F} -> F;
               _ ->
                   <<"no_reply@", (kz_util:to_binary(net_adm:localhost()))/binary>>
           end,
    Email = {<<"multipart">>, <<"alternative">> %% Content Type / Sub Type
            ,[ %% Headers
               {<<"From">>, From},
               {<<"To">>, To},
               {<<"Subject">>, Subject}
             ]
            ,[] %% Parameters
            ,create_body(State, Props, [])
            },
    Encoded = mimemail:encode(Email),
    lager:debug("sending activation email to ~s", [To]),
    gen_smtp_client:send({From, [To], Encoded}
                        ,[{'relay', "localhost"}]
                        ,fun(X) -> kz_util:put_callid(ReqId),
                                   lager:debug("sending email to ~s resulted in ~p", [To, X])
                         end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a multipart body from the HTML and plain text templates
%% if they have been provided
%% @end
%%--------------------------------------------------------------------
-spec create_body(#state{}, kz_proplist(), [mail_message_body()]) -> [mail_message_body()].
create_body(#state{activation_email_html=Tmpl}=State, Props, Body) when Tmpl =/= 'undefined' ->
    case kz_template:render(Tmpl, Props) of
        {'ok', Content} ->
            Part = {<<"text">>, <<"html">>
                   ,[{<<"Content-Type">>, <<"text/html">>}]
                   ,[]
                   ,iolist_to_binary(Content)
                   },
            create_body(State#state{activation_email_html='undefined'}, Props, [Part|Body]);
        _ ->
            create_body(State#state{activation_email_html='undefined'}, Props, Body)
    end;
create_body(#state{activation_email_plain=Tmpl}=State, Props, Body) when Tmpl =/= 'undefined' ->
    case kz_template:render(Tmpl, Props) of
        {'ok', Content} ->
            Part = {<<"text">>, <<"plain">>
                   ,[{<<"Content-Type">>, <<"text/plain">>}]
                   ,[]
                   ,iolist_to_binary(Content)
                   },
            create_body(State#state{activation_email_plain='undefined'}, Props, [Part|Body]);
        _ ->
            create_body(State#state{activation_email_plain='undefined'}, Props, Body)
    end;
create_body(_, _, Body) ->
    Body.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a proplist to provide to the templates during render
%% @end
%%--------------------------------------------------------------------
-spec template_props(cb_context:context()) -> [{ne_binary(), kz_proplist() | ne_binary()},...].
template_props(Context) ->
    JObj = cb_context:doc(Context),
    Data = cb_context:req_data(Context),
    RawHost = Context#cb_context.raw_host,
    Port = Context#cb_context.port,
    ApiHost = list_to_binary(["http://", RawHost, ":", kz_util:to_list(Port), "/"]),
    %% remove the redundant request data
    Req = kz_json:delete_keys([<<"account">>, <<"user">>], Data),

    %% create props to expose to the template
    props:filter_undefined(
      [{<<"account">>, kz_json:to_proplist(<<"pvt_account">>, JObj)}
      ,{<<"user">>, kz_json:to_proplist(<<"pvt_user">>, JObj)}
      ,{<<"request">>, kz_json:to_proplist(Req)}
      ,{<<"api_url">>, [{<<"host">>, ApiHost}
                       ,{<<"path">>, <<"v1/signup/">>}
                       ]}
      ,{<<"host">>, RawHost}
      ,{<<"activation_key">>, kz_json:get_value(<<"pvt_activation_key">>, JObj, <<>>)}
      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determines if the given account realm is unique amoung the existing
%% accounts and completed signups
%% @end
%%--------------------------------------------------------------------
-spec is_unique_realm(binary() | 'undefined') -> boolean().
is_unique_realm('undefined') -> 'false';
is_unique_realm(<<>>) -> 'false';
is_unique_realm(Realm) ->
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', _} -> 'false';
        {'error', _} ->
            case kz_datamgr:get_results(?SIGNUP_DB, ?VIEW_ACTIVATION_REALM, [{'key', Realm}]) of
                {'ok', []} -> 'true';
                _Else -> 'false'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Periodically runs and cleans expired signups.  The delete function
%% will wait up to 5 minutes randomly, as an experiment to distribute
%% the db load.
%% @end
%%--------------------------------------------------------------------
-spec cleanup_signups(#state{}) -> 'ok'.
cleanup_signups(#state{signup_lifespan=Lifespan}) ->
    lager:debug("cleaning up signups"),
    Expiration = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Lifespan,
    case kz_datamgr:get_results(?SIGNUP_DB, ?VIEW_ACTIVATION_CREATED, [{'startkey', 0}
                                                                      ,{'endkey', Expiration}
                                                                      ,'include_docs'
                                                                      ])
    of
        {'ok', Expired} ->
            _ = kz_datamgr:del_docs(?SIGNUP_DB
                                   ,[kz_json:get_value(<<"doc">>, JObj) || JObj <- Expired]
                                   ),
            'ok';
        _Else -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init_state() -> #state{}.
init_state() ->
    case get_configs() of
        {'ok', Terms} ->
            lager:debug("loaded config from ~s", [?SIGNUP_CONF]),
            Defaults = #state{},
            #state{cleanup_interval =
                       props:get_integer_value('cleanup_interval', Terms, Defaults#state.cleanup_interval)
                  ,signup_lifespan =
                       props:get_integer_value('signup_lifespan', Terms, Defaults#state.signup_lifespan)
                  ,register_cmd =
                       compile_template(props:get_value('register_cmd', Terms), 'cb_signup_register_cmd')
                  ,activation_email_plain =
                       compile_template(props:get_value('activation_email_plain', Terms), 'cb_signup_email_plain')
                  ,activation_email_html =
                       compile_template(props:get_value('activation_email_html', Terms), 'cb_signup_email_html')
                  ,activation_email_from =
                       compile_template(props:get_value('activation_email_from', Terms), 'cb_signup_email_from')
                  ,activation_email_subject =
                       compile_template(props:get_value('activation_email_subject', Terms), 'cb_signup_email_subject')
                  };
        {'error', _} ->
            lager:debug("could not read config from ~s", [?SIGNUP_CONF]),
            #state{}
    end.

-spec get_configs() -> {'ok', proplist()} |
                       {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'
                        | {integer(), module(), any()}
                       }.
get_configs() ->
    file:consult(lists:flatten(?SIGNUP_CONF)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compiles a template string or path, correcting relative paths
%% to the priv directory of this module
%% @end
%%--------------------------------------------------------------------
-type template_name() :: 'cb_signup_email_from' |
                         'cb_signup_email_html' |
                         'cb_signup_email_plain' |
                         'cb_signup_email_subject' |
                         'cb_signup_register_cmd'.
-spec compile_template(string() | api_binary(), template_name()) ->
                              'undefined' | template_name().
compile_template('undefined', _) -> 'undefined';
compile_template(Template, Name) when not is_binary(Template) ->
    Path =
        case string:substr(Template, 1, 1) of
            "/" -> Template;
            _ ->
                lists:concat([code:priv_dir('crossbar'), "/signup/", Template])
        end,
    lager:debug("sourcing template from file at ~s", [Path]),
    do_compile_template(Path, Name);
compile_template(Template, Name) ->
    do_compile_template(Template, Name).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compiles template string or path, normalizing the return
%% @end
%%--------------------------------------------------------------------
-spec do_compile_template(kz_template:template(), template_name()) ->
                                 'undefined' | template_name().
do_compile_template(Template, Name) ->
    case kz_template:compile(Template, Name) of
        {'ok', Name} -> Name;
        _ -> 'undefined'
    end.
