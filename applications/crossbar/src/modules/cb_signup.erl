%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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

-include("../crossbar.hrl").

-define(SIGNUP_DB, <<"signups">>).

-define(VIEW_FILE, <<"views/signup.json">>).
-define(VIEW_ACTIVATION_KEYS, <<"signups/listing_by_key">>).
-define(VIEW_ACTIVATION_REALM, <<"signups/listing_by_realm">>).
-define(VIEW_ACTIVATION_CREATED, <<"signups/listing_by_created">>).

-define(SIGNUP_CONF, [code:lib_dir('crossbar', 'priv'), "/signup/signup.conf"]).

-record(state, {cleanup_interval = 18000 :: integer() %% once every 5 hours (in seconds)
                ,signup_lifespan = ?SECONDS_IN_DAY :: integer() %% 24 hours (in seconds)
                ,register_cmd = 'undefined' :: 'undefined' | atom()
                ,activation_email_plain = 'undefined' :: 'undefined' | atom()
                ,activation_email_html = 'undefined' :: 'undefined' | atom()
                ,activation_email_from = 'undefined' :: 'undefined' | atom()
                ,activation_email_subject = 'undefined' :: 'undefined' | atom()
               }).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.signup">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.signup">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.signup">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.signup">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.put.signup">>, ?MODULE, 'put'),

    _ = couch_mgr:db_create(?SIGNUP_DB),

    _ = case couch_mgr:update_doc_from_file(?SIGNUP_DB, 'crossbar', ?VIEW_FILE) of
            {'error', _} -> couch_mgr:load_doc_from_file(?SIGNUP_DB, 'crossbar', ?VIEW_FILE);
            {'ok', _} -> 'ok'
        end,

    supervisor:start_child('crossbar_sup', crossbar_sup:child_spec(?MODULE)).

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, 'init_it', [])}.

init_it() ->
    put('callid', ?LOG_SYSTEM_ID),
    State = init_state(),
    cleanup_loop(State).

cleanup_loop(#state{cleanup_interval=CleanupInterval}=State) ->
    Wait = CleanupInterval * 1000,
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
allowed_methods(_) -> [?HTTP_POST].

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
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    validate_new_signup(Context#cb_context{db_name=?SIGNUP_DB}).

validate(#cb_context{req_verb = ?HTTP_POST}=Context, ActivationKey) ->
    check_activation_key(ActivationKey, Context#cb_context{db_name=?SIGNUP_DB}).

-spec authorize(cb_context:context()) -> 'true'.
authorize(#cb_context{req_nouns=[{<<"signup">>,[]}]}) -> 'true';
authorize(#cb_context{req_nouns=[{<<"signup">>,[_]}]}) -> 'true'.

-spec authenticate(cb_context:context()) -> 'true'.
authenticate(#cb_context{req_nouns=[{<<"signup">>,[]}]}) -> 'true';
authenticate(#cb_context{req_nouns=[{<<"signup">>,[_]}]}) -> 'true'.

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(#cb_context{doc=JObj}=Context, _) ->
    case activate_signup(JObj) of
        {'ok', Account, User} ->
            _ = couch_mgr:del_doc(?SIGNUP_DB, JObj),
            Context#cb_context{resp_status='success'
                               ,resp_data=wh_json:from_list([{<<"account">>, Account}
                                                             ,{<<"user">>, User}
                                                            ])
                              };
        _Else ->
            cb_context:add_system_error('datastore_fault', Context)
    end.

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    case crossbar_doc:save(Context#cb_context{db_name=?SIGNUP_DB}) of
        #cb_context{resp_status='success'}=Context1 ->
            P = crossbar_sup:find_proc(?MODULE),
            P ! {'send_activation_email', Context1},
            P ! {'register', Context},
            Context1#cb_context{resp_data=[]};
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
validate_new_signup(#cb_context{req_data=JObj}=Context) ->
    {AccountErrors, Account} = validate_account(wh_json:get_value(<<"account">>, JObj), Context),
    {UserErrors, User} = validate_user(wh_json:get_value(<<"user">>, JObj), Context),
    case {AccountErrors, UserErrors} of
        {[], []} ->
            Context#cb_context{doc=wh_json:from_list([{<<"pvt_user">>, User}
                                                      ,{<<"pvt_account">>, Account}
                                                      ,{<<"pvt_activation_key">>, create_activation_key()}
                                                     ])
                               ,resp_status='success'
                              };
        _Else ->
            crossbar_util:response_invalid_data(wh_json:from_list([{<<"account">>, AccountErrors}
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
    case is_unique_realm(wh_json:get_value(<<"realm">>, Account))
        andalso crossbar_maintenance:create_account(Context#cb_context{req_data=Account}) of
        'false' ->
            {[<<"duplicate realm">>], 'undefined'};
        {'ok', #cb_context{resp_status='success', doc=Acct}} ->
            lager:debug("signup account is valid"),
            {[], Acct};
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
    case cb_users:create_user(Context#cb_context{req_data=User}) of
        #cb_context{resp_status='success', doc=Usr} ->
            lager:debug("signup user is valid"),
            {[], Usr};
        #cb_context{resp_data=Errors} ->
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
    ActivationKey =
        wh_util:to_hex_binary(crypto:rand_bytes(32)),
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
    case couch_mgr:get_results(?SIGNUP_DB, ?VIEW_ACTIVATION_KEYS, [{<<"key">>, ActivationKey}
                                                                   ,{<<"include_docs">>, true}]) of
        {ok, []} ->
            lager:debug("activation key not found"),
            crossbar_util:response(error, <<"invalid activation key">>, 403, Context);
        {ok, [JObj|_]} ->
            lager:debug("activation key is valid"),
            Context#cb_context{resp_status=success, doc=wh_json:get_value(<<"doc">>, JObj)};
        _ ->
            lager:debug("db error while looking up activation key"),
            crossbar_util:response(error, <<"invalid activation key">>, 403, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Activate signup document by creating an account and user
%% @end
%%--------------------------------------------------------------------
-spec activate_signup(wh_json:object()) ->
                                   {'ok', wh_json:object(), wh_json:object()} |
                                   {'error', 'creation_failed' | 'account_undefined' | 'user_undefined'}.
activate_signup(JObj) ->
    case activate_account(wh_json:get_value(<<"pvt_account">>, JObj)) of
        {ok, Account} ->
            activate_user(Account, wh_json:get_value(<<"pvt_user">>, JObj));
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create the account defined on the signup document
%% @end
%%--------------------------------------------------------------------
-spec activate_account('undefined' | wh_json:object()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', 'creation_failed' | 'account_undefined'}.
activate_account(undefined) ->
    {error, account_undefined};
activate_account(Account) ->
    Event = <<"*.execute.put.accounts">>,
    Payload = [#cb_context{doc=Account}],
    case crossbar_bindings:fold(Event, Payload) of
        #cb_context{resp_status=success, resp_data=JObj} ->
            AccountId = wh_json:get_value(<<"id">>, JObj),
            lager:debug("created new account ~s", [AccountId]),
            {ok, JObj};
        _ ->
            lager:debug("could not create a new account"),
            {error, creation_failed}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a remote host authenticates the shared token it will return
%% an account and user, ensure the user exists locally (creating if not)
%% @end
%%--------------------------------------------------------------------
-spec activate_user(wh_json:object(), 'undefined' | wh_json:object()) ->
                                 {'ok', wh_json:object(), wh_json:object()} |
                                 {'error', 'user_undefined' | 'creation_failed'}.
activate_user(_, undefined) ->
    {error, user_undefined};
activate_user(Account, User) ->
    AccountId = wh_json:get_value(<<"id">>, Account),
    Db = wh_util:format_account_id(AccountId, encoded),
    Event = <<"*.execute.put.users">>,
    Payload = [#cb_context{doc=User, db_name=Db}],
    case crossbar_bindings:fold(Event, Payload) of
        #cb_context{resp_status=success, resp_data=JObj} ->
            UserId = wh_json:get_value(<<"id">>, JObj),
            lager:debug("created new user ~s in account ~s", [UserId, AccountId]),
            {ok, Account, JObj};
        _ ->
            lager:debug("could not create a new user in account ~s", [AccountId]),
            {error, creation_failed}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% if the system has a register command defined in priv/signup/signup.conf
%% then exectute it now.
%% @end
%%--------------------------------------------------------------------
-spec exec_register_command(cb_context:context(), #state{}) -> 'ok' | string().
exec_register_command(_, #state{register_cmd=undefined}) ->
    ok;
exec_register_command(Context, #state{register_cmd=CmdTmpl}) ->
    Props = template_props(Context),
    {ok, Cmd} = CmdTmpl:render(Props),
    lager:debug("executing register command ~s", [Cmd]),
    os:cmd(binary_to_list(iolist_to_binary(Cmd))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_activation_email(cb_context:context(), #state{}) -> {'ok', pid()} | {'error', term()}.
send_activation_email(#cb_context{doc=JObj, req_id=ReqId}=Context, #state{activation_email_subject=SubjectTmpl
                                                                          ,activation_email_from=FromTmpl}=State) ->
    Props = template_props(Context),
    To = wh_json:get_value([<<"pvt_user">>, <<"email">>], JObj),
    Subject = case SubjectTmpl:render(Props) of
                  {ok, S} -> S;
                  _ -> <<"Confirm your account activation">>
              end,
    From = case FromTmpl:render(Props) of
               {ok, F} -> F;
               _ ->
                   <<"no_reply@", (wh_util:to_binary(net_adm:localhost()))/binary>>
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
    gen_smtp_client:send({From, [To], Encoded}, [{relay, "localhost"}]
                         ,fun(X) -> put(callid, ReqId), lager:debug("sending email to ~s resulted in ~p", [To, X]) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a multipart body from the HTML and plain text templates
%% if they have been provided
%% @end
%%--------------------------------------------------------------------
-spec create_body(#state{}, proplist(), [] | [mail_message_body(),...]) -> [] | [mail_message_body(),...].
create_body(#state{activation_email_html=Tmpl}=State, Props, Body) when Tmpl =/= undefined ->
    case Tmpl:render(Props) of
        {ok, Content} ->
            Part = {<<"text">>, <<"html">>
                    ,[{<<"Content-Type">>, <<"text/html">>}]
                    ,[]
                    ,iolist_to_binary(Content)
                   },
             create_body(State#state{activation_email_html=undefined}, Props, [Part|Body]);
        _ ->
             create_body(State#state{activation_email_html=undefined}, Props, Body)
    end;
create_body(#state{activation_email_plain=Tmpl}=State, Props, Body) when Tmpl =/= undefined ->
    case Tmpl:render(Props) of
        {ok, Content} ->
            Part = {<<"text">>, <<"plain">>
                    ,[{<<"Content-Type">>, <<"text/plain">>}]
                    ,[]
                    ,iolist_to_binary(Content)
                   },
             create_body(State#state{activation_email_plain=undefined}, Props, [Part|Body]);
        _ ->
             create_body(State#state{activation_email_plain=undefined}, Props, Body)
    end;
create_body(_, _, Body) ->
    Body.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a proplist to provide to the templates during render
%% @end
%%--------------------------------------------------------------------
-spec template_props(cb_context:context()) -> [{ne_binary(), proplist() | ne_binary()},...].
template_props(#cb_context{doc=JObj
                           ,req_data=Data
                           ,raw_host=RawHost
                           ,port=Port
                          }) ->

    ApiHost = list_to_binary(["http://", RawHost, ":", wh_util:to_list(Port), "/"]),
    %% remove the redundant request data
    Req = wh_json:delete_keys([<<"account">>, <<"user">>], Data),

    %% create props to expose to the template
    [{<<"account">>, wh_json:to_proplist(<<"pvt_account">>, JObj)}
     ,{<<"user">>, wh_json:to_proplist(<<"pvt_user">>, JObj)}
     ,{<<"request">>, wh_json:to_proplist(Req)}
     ,{<<"api_url">>, [{<<"host">>, ApiHost}
                       ,{<<"path">>, <<"v1/signup/">>}
                      ]}
     ,{<<"host">>, RawHost}
     ,{<<"activation_key">>, wh_json:get_value(<<"pvt_activation_key">>, JObj, <<>>)}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determines if the given account realm is unique amoung the existing
%% accounts and completed signups
%% @end
%%--------------------------------------------------------------------
-spec is_unique_realm(binary() | 'undefined') -> boolean().
is_unique_realm(undefined) -> false;
is_unique_realm(<<>>) -> false;
is_unique_realm(Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
        {ok, _} -> false;
        {error, _} ->
            case couch_mgr:get_results(?SIGNUP_DB, ?VIEW_ACTIVATION_REALM, [{<<"key">>, Realm}]) of
                {ok, []} -> true;
                _Else -> false
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
    case couch_mgr:get_results(?SIGNUP_DB, ?VIEW_ACTIVATION_CREATED, [{<<"startkey">>, 0}
                                                                      ,{<<"endkey">>, Expiration}
                                                                      ,{<<"include_docs">>, true}
                                                                     ]) of
        {ok, Expired} ->
            _ = couch_mgr:del_docs(?SIGNUP_DB, [wh_json:get_value(<<"doc">>, JObj, wh_json:new()) || JObj <- Expired]),
            ok;
        _Else ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init_state() -> #state{}.
init_state() ->
    case get_configs() of
        {ok, Terms} ->
            lager:debug("loaded config from ~s", [?SIGNUP_CONF]),
            Defaults = #state{},
            #state{cleanup_interval =
                       props:get_integer_value(cleanup_interval, Terms, Defaults#state.cleanup_interval)
                   ,signup_lifespan =
                       props:get_integer_value(signup_lifespan, Terms, Defaults#state.signup_lifespan)
                   ,register_cmd =
                       compile_template(props:get_value(register_cmd, Terms), cb_signup_register_cmd)
                   ,activation_email_plain =
                       compile_template(props:get_value(activation_email_plain, Terms), cb_signup_email_plain)
                   ,activation_email_html =
                       compile_template(props:get_value(activation_email_html, Terms), cb_signup_email_html)
                   ,activation_email_from =
                       compile_template(props:get_value(activation_email_from, Terms), cb_signup_email_from)
                   ,activation_email_subject =
                       compile_template(props:get_value(activation_email_subject, Terms), cb_signup_email_subject)
                  };
        {error, _} ->
            lager:debug("could not read config from ~s", [?SIGNUP_CONF]),
            #state{}
    end.

-spec get_configs() -> {'ok', proplist()} | {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'
                                                   | {integer(), module(), term()}}.
get_configs() ->
    file:consult(lists:flatten(?SIGNUP_CONF)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compiles a template string or path, correcting relative paths
%% to the priv directory of this module
%% @end
%%--------------------------------------------------------------------
-type template_name() :: 'cb_signup_email_from' | 'cb_signup_email_html' | 'cb_signup_email_plain' | 'cb_signup_email_subject' | 'cb_signup_register_cmd'.
-spec compile_template('undefined' | string() | ne_binary(), template_name()) -> 'undefined' | template_name().
compile_template(undefined, _) ->
    undefined;
compile_template(Template, Name) when not is_binary(Template) ->
    Path = case string:substr(Template, 1, 1) of
               "/" ->
                   Template;
               _ ->
                   BasePath = code:lib_dir(crossbar, priv),
                   lists:concat([BasePath, "/signup/", Template])
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
-spec do_compile_template(nonempty_string() | ne_binary(), template_name()) -> 'undefined' | template_name().
do_compile_template(Template, Name) ->
    case erlydtl:compile(Template, Name) of
        {ok, Name} ->
            lager:debug("compiled ~s template", [Name]),
            Name;
        ok ->
            lager:debug("compiled ~s template file", [Name]),
            Name;
        _E ->
            lager:debug("could not compile ~s template, ignoring", [Name]),
            undefined
    end.
