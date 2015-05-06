%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------

-module(fax_smtp).
-behaviour(gen_smtp_server_session).

-export([init/4
         ,handle_HELO/2
         ,handle_EHLO/3
         ,handle_MAIL/2
         ,handle_MAIL_extension/2
         ,handle_RCPT/2
         ,handle_RCPT_extension/2
         ,handle_DATA/4
         ,handle_RSET/1
         ,handle_VRFY/2
         ,handle_other/3
         ,handle_AUTH/4
         ,handle_STARTTLS/1
         ,code_change/3
         ,terminate/2
        ]).

-include("fax.hrl").

-define(RELAY, 'true').
-define(SMTP_MAX_SESSIONS, whapps_config:get_integer(?CONFIG_CAT, <<"smtp_sessions">>, 50)).

-record(state, {
          options = [] :: list()
          ,from :: binary()
          ,to :: binary()
          ,docs = [] :: wh_json:objects()
          ,filename :: api_binary()
          ,content_type :: binary()
          ,peer_ip :: peer()
          ,owner_id :: api_binary()
          ,owner_email :: api_binary()
          ,faxbox_email :: api_binary()
          ,faxbox :: api_object()
          ,errors = [] :: ne_binaries()
          ,original_number :: api_binary()
          ,number :: api_binary()
          ,account_id :: api_binary()
         }).

-type state() :: #state{}.
-type error_message() :: {'error', string(), #state{}}.

-type peer() :: {inet:ip_address(), non_neg_integer()}.

-spec init(ne_binary(), non_neg_integer(), peer(), wh_proplist()) ->
                  {'ok', string(), #state{}} |
                  {'stop', any(), string()}.
init(Hostname, SessionCount, Address, Options) ->
    case SessionCount > ?SMTP_MAX_SESSIONS  of
        'false' ->
            Banner = [Hostname, " Kazoo Email to Fax Server"],
            State = #state{options = Options, peer_ip = Address},
            {'ok', Banner, State};
        'true' ->
            lager:warning("connection limit exceeded ~p", [Address]),
            {'stop', 'normal', ["421 ", Hostname, " is too busy to accept mail right now"]}
    end.

-spec handle_HELO(binary(), state()) ->
                         {'ok', pos_integer(), state()} |
                         {'ok', state()} |
                         error_message().
handle_HELO(<<"invalid">>, State) ->
    %% contrived example
    {'error', "554 invalid hostname", State};
handle_HELO(<<"trusted_host">>, State) ->
    {'ok', State}; %% no size limit because we trust them.
handle_HELO(Hostname, State) ->
    lager:debug("HELO from ~s", [Hostname]),
    {'ok', 655360, State}. % 640kb of HELO should be enough for anyone.
%% If {ok, State} was returned here, we'd use the default 10mb limit

-spec handle_EHLO(binary(), list(), state()) ->
                         {'ok', list(), state()} |
                         error_message().
handle_EHLO(<<"invalid">>, _Extensions, State) ->
    %% contrived example
    {'error', "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, #state{options=Options}=State) ->
    lager:debug("EHLO from ~s", [Hostname]),
    %% You can advertise additional extensions, or remove some defaults
    MyExtensions = case props:get_is_true('auth', Options, 'false') of
                       'false' -> Extensions;
                       'true' ->
                           %% auth is enabled, so advertise it
                           [{"AUTH", "PLAIN LOGIN CRAM-MD5"}
                            ,{"STARTTLS", 'true'}
                            | Extensions
                           ]
                   end,
    {'ok', MyExtensions, State}.

-spec handle_MAIL(binary(), state()) ->
                         {'ok', state()} |
                         error_message().
handle_MAIL(From, State) ->
    lager:debug("Checking Mail from ~s", [From]),
    {'ok', State#state{from=From}}.

-spec handle_MAIL_extension(binary(), state()) ->
                                   {'ok', state()} |
                                   'error'.
handle_MAIL_extension(Extension, State) ->
    Error = io_lib:format("554 Unknown MAIL FROM extension ~s", [Extension]),
    lager:debug(Error),
    {'error', Error, State}.

-spec handle_RCPT(binary(), state()) ->
                         {'ok', state()} |
                         {'error', string(), state()}.
handle_RCPT(To, State) ->
    lager:debug("Checking Mail to ~s", [To]),
    check_faxbox((reset(State))#state{to=To}).

-spec handle_RCPT_extension(binary(), state()) ->
                                   {'ok', state()} |
                                   'error'.
handle_RCPT_extension(Extension, State) ->
    Error = io_lib:format("554 Unknown RCPT TO extension ~s", [Extension]),
    lager:debug(Error),
    {'error', Error, State}.

-spec handle_DATA(binary(), ne_binaries(), binary(), state()) ->
                         {'ok', string(), state()} |
                         {'error', string(), state()}.
handle_DATA(From, To, <<>>, State) ->
    lager:debug("552 Message too small. From ~p to ~p", [From,To]),
    {'error', "552 Message too small", State};
handle_DATA(From, To, Data, #state{options=Options}=State) ->
    lager:debug("Handle Data From ~p to ~p", [From,To]),

    %% JMA: Can this be done with wh_util:rand_hex_binary() ?
    Reference = lists:flatten(
                  [io_lib:format("~2.16.0b", [X])
                   || <<X>> <= erlang:md5(term_to_binary(erlang:now()))
                  ]),

    try mimemail:decode(Data) of
        {Type, SubType, Headers, Parameters, Body} ->
            lager:debug("Message decoded successfully!"),
            case process_message(Type, SubType, Headers, Parameters, Body, State) of
                {ProcessResult, #state{errors=[]}=NewState} ->
                    {ProcessResult, Reference, NewState};
                {ProcessResult, #state{errors=[Error | _]}=NewState} ->
                    {ProcessResult, <<"554 ",Error/binary>>, NewState}
            end;
        Other ->
            lager:debug("mime decode other ~p", [Other]),
            {'error', "554 Message decode failed", State#state{errors=[<<"Message decode failed">>]}}
    catch
        _What:_Why ->
            lager:debug("Message decode FAILED with ~p:~p", [_What, _Why]),
            case props:get_is_true('dump', Options, 'false') of
                'false' -> 'ok';
                'true' ->
                    %% optionally dump the failed email somewhere for analysis
                    File = "/tmp/"++Reference,
                    case filelib:ensure_dir(File) of
                        'ok' -> file:write_file(File, Data);
                        _ -> 'ok'
                    end
            end,
            {'error', "554 Message decode failed", State#state{errors=[<<"Message decode failed">>]}}
    end.

-spec handle_RSET(state()) -> state().
handle_RSET(State) ->
    lager:debug("RSET Called"),
    %% reset any relevant internal state
    State.

-spec handle_VRFY(binary(), state()) ->
                         {'ok', string(), state()} |
                         {'error', string(), state()}.
handle_VRFY(_Address, State) ->
    lager:debug("252 VRFY disabled by policy, just send some mail"),
    {'error', "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(binary(), binary(), state()) ->
                          {iolist(), state()} |
                          {state()}.
handle_other(<<"PROXY">>, Args, State) ->
    lager:debug("PROXY : ~p",[Args]),
    {State};
handle_other(Verb, Args, State) ->
    %% You can implement other SMTP verbs here, if you need to
    lager:debug("500 Error: command not recognized : ~p / ~p",[Verb,Args]),
    {["500 Error: command not recognized : '", wh_util:to_list(Verb), "'"], State}.

-spec handle_AUTH('login' | 'plain' | 'cram-md5', binary(), binary() | {binary(), binary()}, state()) ->
                         'error'.
handle_AUTH(_Type, _Username, _Password, _State) ->
    'error'.

-spec handle_STARTTLS(state()) -> state().
handle_STARTTLS(State) ->
    lager:debug("SMTP TLS Started"),
    State.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec terminate(any(), state()) ->  {'ok', any(), state()}.
terminate('normal', State) ->
    spawn(fun()-> handle_message(State) end),
    {'ok', 'normal', State};
terminate(Reason, State) ->
    lager:debug("terminate ~p", [Reason]),
    {'ok', Reason, State}.

%%% Internal Functions %%%

-spec handle_message(state()) -> 'ok'.
handle_message(#state{errors=[_Error | _Errors]
                      ,faxbox='undefined'
                     }=State) ->
    maybe_system_report(State);
handle_message(#state{errors = []
                      ,faxbox='undefined'
                     }=State) ->
    maybe_system_report(State#state{errors=[<<"no faxbox">>]});
handle_message(State) -> maybe_faxbox_save(State).

-spec maybe_faxbox_save(state()) -> 'ok'.
maybe_faxbox_save(#state{errors=[_Error | _Errors]}=State) ->
    maybe_faxbox_log(State);
maybe_faxbox_save(#state{docs=[]}=State) ->
    maybe_faxbox_log(State#state{errors=[<<"no fax documents to save">>]});
maybe_faxbox_save(#state{filename='undefined'}=State) ->
    maybe_faxbox_log(State#state{errors=[<<"no fax attachment to save">>]});
maybe_faxbox_save(#state{filename=Filename
                         ,content_type=CT
                         ,docs=Docs
                         ,errors=[]
                        }=State) ->
    case file:read_file(Filename) of
        {'ok', FileContents} ->
            case fax_util:save_fax_docs(Docs, FileContents, CT) of
                'ok' ->
                    file:delete(Filename),
                    'ok';
                {'error', Error} ->
                    maybe_faxbox_log(State#state{errors=[Error]})
            end;
        _Else ->
            Error = wh_util:to_binary(io_lib:format("error reading attachment ~s", [Filename])),
            maybe_faxbox_log(State#state{errors=[Error]})
    end.

-spec maybe_system_report(state()) -> 'ok'.
maybe_system_report(#state{faxbox='undefined'}=State) ->
    case whapps_config:get(?CONFIG_CAT, <<"report_anonymous_system_errors">>, 'false') of
        'true' -> system_report(State);
        'false' -> 'ok'
    end;
maybe_system_report(State) ->
    case whapps_config:get(?CONFIG_CAT, <<"report_faxbox_system_errors">>, 'true') of
        'true' -> system_report(State);
        'false' -> 'ok'
    end.

-spec maybe_faxbox_log(state()) -> 'ok'.
maybe_faxbox_log(#state{faxbox=JObj}=State) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    case wh_json:is_true(<<"log_errors">>, JObj, 'false')
        orelse ( whapps_account_config:get_global(AccountId, ?CONFIG_CAT, <<"log_faxbox_errors">>, 'true')
               andalso wh_json:is_true(<<"log_errors">>, JObj, 'true') )
        of
        'true' -> faxbox_log(State);
        'false' -> maybe_system_report(State)
    end.

-spec system_report(state()) -> 'ok'.
system_report(#state{errors=[Error | _]}=State) ->
    Props = to_proplist(State),
    Notify = props:filter_undefined(
               [{<<"Subject">>, <<"fax smtp error">>}
                ,{<<"Message">>, Error}
                ,{<<"Details">>, wh_json:from_list(Props)}
                ,{<<"Account-ID">>, props:get_value(<<"Account-ID">>, Props)}
                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
               ]),
    wh_amqp_worker:cast(Notify, fun wapi_notifications:publish_system_alert/1).

-spec faxbox_log(state()) -> 'ok'.
faxbox_log(#state{faxbox=JObj}=State) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    AccountDb = kazoo_modb:get_modb(AccountId),
    Doc = wh_json:normalize(
            wh_json:from_list(
              [{<<"pvt_account_id">>, AccountId}
               ,{<<"pvt_account_db">>, AccountDb}
               ,{<<"pvt_type">>, <<"fax_smtp_log">>}
               ,{<<"pvt_created">>, wh_util:current_tstamp()}
               ,{<<"_id">>, error_doc()}
               | to_proplist(State)
              ]
             )
           ),
    couch_mgr:save_doc(AccountDb, Doc),
    maybe_system_report(State).

-spec error_doc() -> binary().
error_doc() ->
    {Year, Month, _} = erlang:date(),
    <<(wh_util:to_binary(Year))/binary,(wh_util:pad_month(Month))/binary
      ,"-",(wh_util:rand_hex_binary(16))/binary
    >>.

-spec to_proplist(state() | api_object()) -> wh_proplist().
to_proplist(#state{}=State) ->
    props:filter_undefined(
      [{<<"To">>, State#state.to}
       ,{<<"From">>, State#state.from}
       ,{<<"Original-Number">>, State#state.original_number}
       ,{<<"Translated-Number">>, State#state.number}
       ,{<<"FaxBox-Domain">>, State#state.faxbox_email}
       ,{<<"FaxBox-Owner-ID">>, State#state.owner_id}
       ,{<<"FaxBox-Owner-Email">>, State#state.owner_email}
       ,{<<"Content-Type">>, State#state.content_type}
       ,{<<"Filename">>, State#state.filename}
       ,{<<"Errors">>, State#state.errors}
       | to_proplist(State#state.faxbox)
      ]);
to_proplist('undefined') -> [];
to_proplist(JObj) ->
    props:filter_undefined(
      [{<<"FaxBox-ID">>, wh_json:get_value(<<"_id">>, JObj)}
       ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
      ]
     ).

-spec reset(state()) -> state().
reset(State) ->
    State#state{owner_id = 'undefined'
                ,owner_email = 'undefined'
                ,faxbox_email = 'undefined'
                ,faxbox = 'undefined'
                ,original_number = 'undefined'
                ,number = 'undefined'
               }.

-spec check_faxbox(state()) ->
                          {'ok', state()} |
                          {'error', string(), state()}.
check_faxbox(#state{to=To}= State) ->
    case binary:split(wh_util:to_lower_binary(To), <<"@">>) of
        [FaxNumber, Domain] ->
            Number = fax_util:filter_numbers(FaxNumber),
            check_number(
              maybe_faxbox(State#state{faxbox_email=Domain
                                       ,original_number=FaxNumber
                                       ,number=Number
                                      }
                          )
             );
        _ ->
            lager:debug("invalid address"),
            {'error', "554 Not Found", State#state{errors=[<<"invalid address">>]}}
    end.

-spec check_number(state()) ->
                          {'ok', state()} |
                          {'error', string(), state()}.
check_number(#state{number= <<>>, original_number=Number, faxbox='undefined'}=State) ->
    Error = wh_util:to_binary(
              io_lib:format("fax number ~s is empty, no faxbox to report to", [Number])),
    lager:debug(Error),
    {'error', "554 Not Found", State#state{errors=[Error]}};
check_number(#state{number= <<>>, original_number=Number, errors=Errors}=State) ->
    Error = wh_util:to_binary(io_lib:format("fax number ~s is empty", [Number])),
    lager:debug(Error),
    {'error', "554 Not Found", State#state{errors=[ Error | Errors]}};
check_number(#state{}=State) ->
    check_permissions(State).

-spec check_permissions(state()) ->
                               {'ok', state()} |
                               {'error', string(), state()}.
-spec check_permissions(state(), ne_binaries()) ->
                               {'ok', state()} |
                               {'error', string(), state()}.
check_permissions(#state{from=_From
                         ,owner_email=OwnerEmail
                         ,faxbox=FaxBoxDoc
                        }=State) ->
    lager:debug("checking if ~s can send to ~p."
                ,[_From, wh_json:get_value(<<"name">>, FaxBoxDoc)]
               ),
    case wh_json:get_value(<<"smtp_permission_list">>, FaxBoxDoc, []) of
        [] when OwnerEmail =:= 'undefined' ->
            check_empty_permissions(State);
        Permissions ->
            check_permissions(State, Permissions)
    end.

check_permissions(#state{from=From
                         ,owner_email=OwnerEmail
                         ,faxbox=FaxBoxDoc
                         ,errors=Errors
                        }=State, Permissions) ->
    case lists:any(fun(A) -> match(From, A) end, Permissions)
        orelse From =:= OwnerEmail
    of
        'true' -> add_fax_document(State);
        'false' ->
            Error = wh_util:to_binary(
                      io_lib:format("address ~s is not allowed on faxbox ~s",
                                    [From, wh_json:get_value(<<"_id">>, FaxBoxDoc)])),
            lager:debug(Error),
            {'error', "554 not allowed", State#state{errors=[Error | Errors]}}
    end.

-spec check_empty_permissions(state()) ->
                                     {'ok', state()} |
                                     {'error', string(), state()}.
check_empty_permissions(#state{errors=Errors}=State) ->
    case whapps_config:get_is_true(<<"fax">>, <<"allow_all_addresses_when_empty">>, 'false') of
        'true' -> add_fax_document(State);
        'false' ->
            Error = <<"faxbox permissions is empty and policy doesn't allow it">>,
            lager:debug(Error),
            {'error', "554 not allowed", State#state{errors=[Error | Errors]}}
    end.

-spec match(binary(), binary()) -> boolean().
match(Address, Element) ->
    re:run(Address, Element) =/= 'nomatch'.

-spec maybe_faxbox(state()) -> state().
maybe_faxbox(#state{faxbox_email=Domain}=State) ->
    ViewOptions = [{'key', Domain}, 'include_docs'],
    case couch_mgr:get_results(?WH_FAXES_DB, <<"faxbox/email_address">>, ViewOptions) of
        {'ok', [JObj]} -> maybe_faxbox_owner(State#state{faxbox=wh_json:get_value(<<"doc">>,JObj)});
        _ -> maybe_faxbox_domain(State)
    end.

-spec maybe_faxbox_owner(state()) -> state().
maybe_faxbox_owner(#state{faxbox=FaxBoxDoc}=State) ->
    case wh_json:get_value(<<"owner_id">>, FaxBoxDoc) of
        'undefined' -> State;
        OwnerId ->
            AccountId = wh_json:get_value(<<"pvt_account_id">>, FaxBoxDoc),
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
                {'ok', OwnerDoc} ->
                    OwnerEmail = wh_json:get_value(<<"email">>, OwnerDoc),
                    State#state{owner_id=OwnerId, owner_email=OwnerEmail};
                _ -> State
            end
    end.

-spec maybe_faxbox_domain(state()) -> state().
maybe_faxbox_domain(#state{faxbox_email=Domain}=State) ->
    ViewOptions = [{'key', Domain}],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_realm">>, ViewOptions) of
        {'ok', []} ->
            Error = <<"realm ", Domain/binary, " not found in accounts db">>,
            lager:debug(Error),
            State#state{errors=[Error]};
        {'ok', [JObj]} ->
            AccountId = wh_json:get_value(<<"id">>, JObj),
            maybe_faxbox_by_owner_email(AccountId, State);
        {'ok', [_JObj | _JObjs]} ->
            Error = <<"accounts query by realm ", Domain/binary, " return more than one document">>,
            lager:debug(Error),
            State#state{errors=[Error]};
        {'error', _E} ->
            Error = <<"error searching realm ", Domain/binary>>,
            lager:debug("error ~p querying realm in accounts database",[_E]),
            State#state{errors=[Error]}
    end.

-spec maybe_faxbox_by_owner_email(ne_binary(), state()) -> state().
maybe_faxbox_by_owner_email(AccountId, #state{from=From}=State) ->
    ViewOptions = [{'key', From}],
    AccountDb = wh_util:format_account_db(AccountId),
    case couch_mgr:get_results(AccountDb, <<"users/list_by_email">>, ViewOptions) of
        {'ok', []} ->
            lager:debug("user ~s does not exist in account ~s, trying by rules",[From, AccountId]),
            maybe_faxbox_by_rules(AccountId, State);
        {'ok', [JObj]} ->
            OwnerId = wh_json:get_value(<<"id">>, JObj),
            maybe_faxbox_by_owner_id(AccountId, OwnerId, State);
        {'ok', [_JObj | _JObjs]} ->
            lager:debug("more then one user with email ~s in account ~s, trying by rules", [From, AccountId]),
            maybe_faxbox_by_rules(AccountId, State);
        {'error', _E} ->
            lager:debug("error ~p getting user by email ~s  in account ~s, trying by rules",[_E, From, AccountId]),
            maybe_faxbox_by_rules(AccountId, State)
    end.

-spec maybe_faxbox_by_owner_id(ne_binary(), ne_binary(),state()) -> state().
maybe_faxbox_by_owner_id(AccountId, OwnerId, #state{from=From}=State) ->
    ViewOptions = [{'key', OwnerId}, 'include_docs'],
    AccountDb = wh_util:format_account_db(AccountId),
    case couch_mgr:get_results(AccountDb, <<"faxbox/list_by_ownerid">>, ViewOptions) of
        {'ok', [JObj]} ->
            State#state{faxbox=wh_json:get_value(<<"doc">>,JObj)
                        ,owner_id=OwnerId
                        ,owner_email=From
                       };
        _ ->
            lager:debug("user ~s : ~s from account ~s does not have a faxbox, trying by rules"
                        ,[OwnerId, From, AccountId]
                       ),
            maybe_faxbox_by_rules(AccountId
                                  ,State#state{owner_id=OwnerId
                                               ,owner_email=From
                                              }
                                 )
    end.

-spec maybe_faxbox_by_rules(ne_binary() | wh_json:objects(), state()) -> state().
maybe_faxbox_by_rules(AccountId, State)
  when is_binary(AccountId) ->
    ViewOptions = ['include_docs'],
    AccountDb = wh_util:format_account_db(AccountId),
    case couch_mgr:get_results(AccountDb, <<"faxbox/email_permissions">>, ViewOptions) of
        {'ok', []} ->
            Error = <<"no faxboxes for account ", AccountId/binary>>,
            lager:debug(Error),
            State#state{errors=[Error]};
        {'ok', JObjs} -> maybe_faxbox_by_rules(JObjs, State#state{account_id=AccountId});
        {'error', _E} ->
            Error = <<"error getting faxbox email permissions for account ", AccountId/binary>>,
            lager:debug(Error),
            lager:debug("error ~p", [_E]),
            State#state{errors=[Error]}
    end;
maybe_faxbox_by_rules([], #state{account_id=AccountId, from=From}=State) ->
    Error = <<"no mathing rules in account ", AccountId/binary, " for ", From/binary >>,
    lager:debug(Error),
    State#state{errors=[Error]};
maybe_faxbox_by_rules([JObj | JObjs], #state{from=From}=State) ->
    Key = wh_json:get_value(<<"key">>, JObj),
    case match(From, Key) of
        'true' -> State#state{faxbox=wh_json:get_value(<<"doc">>, JObj)};
        'false' -> maybe_faxbox_by_rules(JObjs, State)
    end.

-spec add_fax_document(state()) ->
                              {'ok', state()} |
                              {'error', string(), state()}.
add_fax_document(#state{docs=Docs
                        ,from=From
                        ,owner_email=OwnerEmail
                        ,number=FaxNumber
                        ,faxbox=FaxBoxDoc
                       }=State) ->
    FaxBoxId = wh_json:get_value(<<"_id">>, FaxBoxDoc),
    AccountId = wh_json:get_value(<<"pvt_account_id">>, FaxBoxDoc),
    AccountDb = ?WH_FAXES_DB,
    ResellerId = wh_json:get_value(<<"pvt_reseller_id">>, FaxBoxDoc, wh_services:find_reseller_id(AccountId)),

    FaxBoxEmailNotify = wh_json:get_value([<<"notifications">>
                                           ,<<"outbound">>
                                           ,<<"email">>
                                           ,<<"send_to">>
                                          ], FaxBoxDoc, []),
    FaxBoxNotify = wh_json:set_value([<<"notifications">>
                                      ,<<"outbound">>
                                      ,<<"email">>
                                      ,<<"send_to">>
                                     ]
                                     ,fax_util:notify_email_list(From, OwnerEmail , FaxBoxEmailNotify)
                                     ,FaxBoxDoc
                                    ),
    Notify = wh_json:get_value([<<"notifications">>, <<"outbound">>], FaxBoxNotify),

    Props = props:filter_undefined(
              [{<<"from_name">>, wh_json:get_value(<<"caller_name">>, FaxBoxDoc)}
               ,{<<"fax_identity_name">>, wh_json:get_value(<<"fax_header">>, FaxBoxDoc)}
               ,{<<"from_number">>, wh_json:get_value(<<"caller_id">>, FaxBoxDoc)}
               ,{<<"fax_identity_number">>, wh_json:get_value(<<"fax_identity">>, FaxBoxDoc)}
               ,{<<"fax_timezone">>, wh_json:get_value(<<"fax_timezone">>, FaxBoxDoc)}
               ,{<<"to_name">>, FaxNumber}
               ,{<<"to_number">>, FaxNumber}
               ,{<<"retries">>, wh_json:get_value(<<"retries">>, FaxBoxDoc, 3)}
               ,{<<"notifications">>, Notify}
               ,{<<"faxbox_id">>, FaxBoxId}
               ,{<<"folder">>, <<"outbox">>}
              ]),

    Doc = wh_json:set_values([{<<"pvt_type">>, <<"fax">>}
                              ,{<<"pvt_job_status">>, <<"attaching files">>}
                              ,{<<"pvt_created">>, wh_util:current_tstamp()}
                              ,{<<"attempts">>, 0}
                              ,{<<"pvt_account_id">>, AccountId}
                              ,{<<"pvt_account_db">>, AccountDb}
                              ,{<<"pvt_reseller_id">>, ResellerId}
                             ]
                             ,wh_json_schema:add_defaults(wh_json:from_list(Props), <<"faxes">>)
                            ),
    {'ok', State#state{docs=[Doc | Docs]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
process_message(<<"multipart">>, <<"mixed">>, _Headers, _Parameters, Body, #state{errors=Errors}=State) ->
    lager:debug("processing multipart/mixed"),
    case Body of
        {Type, SubType, HeadersPart, ParametersPart, BodyPart} ->
            lager:debug("processing ~s/~s", [Type, SubType]),
            process_part(<<Type/binary, "/", SubType/binary>>
                        ,HeadersPart
                        ,ParametersPart
                        ,BodyPart
                        ,State);
        [{Type, SubType, _HeadersPart, _ParametersPart, _BodyParts}|_OtherParts]=Parts ->
            lager:debug("processing multiple parts, first is ~s/~s", [Type, SubType]),
            process_parts(Parts, State);
        A ->
            lager:debug("missed processing ~p", [A]),
            {'ok', State#state{errors=[<<"invalid body">> | Errors]}}
    end;
process_message(_Type, _SubType, _Headers, _Parameters, _Body, State) ->
    lager:debug("skipping ~s/~s",[_Type, _SubType]),
    {'ok', State}.

process_parts([], #state{filename='undefined', errors=Errors}=State) ->
    {'ok', State#state{errors=[<<"no valid attachment">> | Errors]}};
process_parts([], State) ->
    {'ok', State};
process_parts([Part|Parts], State) ->
    case Part of
        {Type, SubType, Headers, Parameters, BodyPart} ->
            {_ , NewState}
                = process_part(<<Type/binary, "/", SubType/binary>>
                              ,Headers
                              ,Parameters
                              ,BodyPart
                              ,State),
            process_parts(Parts, NewState);
        A ->
            lager:debug("missed parts processing ~p", [A]),
            {'ok', State}
    end.

process_part(<<"application/pdf">>=CT, _Headers, _Parameters, Body, State) ->
    lager:debug("part is application/pdf"),
    Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".pdf">>,
    file:write_file(Filename, Body),
    {'ok', State#state{filename=Filename
                       ,content_type=CT
                      }};
process_part(<<"image/tiff">>=CT, _Headers, _Parameters, Body, State) ->
    lager:debug("Part is image/tiff"),
    Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".tiff">>,
    file:write_file(Filename, Body),
    {'ok', State#state{filename=Filename
                       ,content_type=CT
                      }};
process_part(<<"application/octet-stream">>, _Headers, Parameters, Body, State) ->
    lager:debug("part is application/octet-stream, try check attachemnt filename extension"),
    case props:get_value(<<"disposition">>, Parameters) of
        <<"attachment">> ->
            Props = props:get_value(<<"disposition-params">>, Parameters),
            maybe_process_part_attachment(Props, Body, State);
         _Else ->
            lager:debug("part is not attachment"),
            {'ok', State}
    end;
process_part(_ContentType, _Headers, _Parameters, _Body, State) ->
    lager:debug("ignoring Part ~s",[_ContentType]),
    {'ok', State}.

-spec maybe_process_part_attachment(wh_proplist(), iolist(), state()) -> {'ok', state()}.
maybe_process_part_attachment(Props, Body, State) ->
    case props:get_value(<<"filename">>, Props) of
        'undefined' ->
            lager:debug("attachment without filename"),
            {'ok', State};
        Filename -> process_part_attachment(Filename, Body, State)
    end.

-spec process_part_attachment(ne_binary(), iolist(), state()) -> {'ok', state()}.
process_part_attachment(AttchFilename, Body, State) ->
    case filename:extension(wh_util:to_lower_binary(AttchFilename)) of
        <<".pdf">> ->
            lager:debug("found pdf filename extension, set content-type to application/pdf"),
            Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".pdf">>,
            file:write_file(Filename, Body),
            {'ok', State#state{filename=Filename
                               ,content_type = <<"application/pdf">>
                              }};
        <<".tiff">> ->
            lager:debug("found tiff filename extension, set content-type to image/tiff"),
            Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".tiff">>,
            file:write_file(Filename, Body),
            {'ok', State#state{filename=Filename
                               ,content_type = <<"image/tiff">>
                              }};
        <<".tif">> ->
            lager:debug("found tif filename extension, set content-type to image/tiff"),
            Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".tiff">>,
            file:write_file(Filename, Body),
            {'ok', State#state{filename=Filename
                               ,content_type = <<"image/tiff">>
                              }};
        _Else ->
            lager:debug("not acceptable filename extension ~p", [_Else]),
            {'ok', State}
    end.
