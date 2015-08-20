%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(ananke_vm_callback).

-export([init/0
         ,handle_req/2
        ]).

-include("ananke.hrl").

-record(args, {account_id
               ,user_id
               ,vm_box_id
               ,callback_number
               ,is_callback_disabled
               ,vm_number
               ,attempts
               ,interval
               ,call_timeout
               ,realm
              }).

-spec init() -> 'ok'.
init() ->
    lager:debug("starting supervisor"),
    {'ok', _} = ananke_sup:start_supervisor('ananke_callback_sup'),
    'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_v(JObj),
    _ = wh_util:put_callid(JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AccountDb = wh_json:get_value(<<"Account-DB">>, JObj),
    VMBoxId = wh_json:get_value(<<"Voicemail-Box">>, JObj),
    lager:debug("handling new voicemail in ~p", [VMBoxId]),
    {'ok', VMBoxJObj} = couch_mgr:open_cache_doc(AccountDb, VMBoxId),
    case kzd_voicemail_box:owner_id(VMBoxJObj) of
        'undefined' ->
            lager:debug("no owner"),
            'ok';
        UserId ->
            lager:debug("voicemail owner is ~p", [UserId]),
            {'ok', UserJObj} = couch_mgr:open_cache_doc(AccountDb, UserId),
            {'ok', AccountJObj} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId),

            Realm = wh_json:get_value(<<"To-Realm">>, JObj),

            Mailbox = wh_json:get_value(<<"mailbox">>, VMBoxJObj),
            VMNumber = get_voicemail_number(AccountDb, Mailbox),
            Number = get_first_defined([{<<"notify_callback_number">>, VMBoxJObj}
                                        ,{<<"vm_notify_callback_number">>, UserJObj}]),
            IsDisabled = wh_util:is_true(
                              get_first_defined([{<<"notify_callback_disabled">>, VMBoxJObj}
                                                 ,{<<"vm_notify_callback_disabled">>, UserJObj}])),
            Interval = get_interval(VMBoxJObj, UserJObj, AccountJObj),
            Attempts = get_attempts(VMBoxJObj, UserJObj, AccountJObj),
            CallTimeout = get_callback_timeout(VMBoxJObj, UserJObj, AccountJObj),

            StartArgs = #args{account_id = AccountId
                              ,user_id = UserId
                              ,vm_box_id = VMBoxId
                              ,callback_number = Number
                              ,is_callback_disabled = IsDisabled
                              ,vm_number = VMNumber
                              ,attempts = Attempts
                              ,interval = Interval
                              ,call_timeout = CallTimeout
                              ,realm = Realm
                             },
            maybe_start_caller(StartArgs)
    end.

-spec get_voicemail_number(ne_binary(), ne_binary()) -> ne_binary() | 'undefined'.
get_voicemail_number(AccountDb, Mailbox) ->
    {'ok', Callflows} = couch_mgr:get_results(AccountDb
                                              ,<<"callflows/crossbar_listing">>
                                             ,[{<<"include_docs">>, 'true'}]),
    case [Cf || Cf <- Callflows, is_voicemail_cf(Cf)] of
        [VMCallflow | _] ->
            get_callflow_number(VMCallflow, Mailbox);
        [] ->
            'undefined'
    end.

-spec is_voicemail_cf(wh_json:object()) -> boolean().
is_voicemail_cf(JObj) ->
    FlowJObj = get_cf_flow(JObj),
    IsFlow = wh_json:is_json_object(FlowJObj) andalso not wh_json:is_empty(FlowJObj),
    case {IsFlow
          ,IsFlow
            andalso wh_json:get_value(<<"module">>, FlowJObj) =:= <<"voicemail">>
            andalso wh_json:get_value([<<"data">>, <<"action">>], FlowJObj) =:= <<"check">>}
    of
        {'false', _} -> 'false';
        {'true', 'true'} -> 'true';
        _ -> is_voicemail_cf(FlowJObj)
    end.

-spec get_cf_flow(wh_json:object()) -> wh_json:object() | 'undefined'.
get_cf_flow(JObj) ->
    case wh_json:get_value([<<"children">>, <<"_">>], JObj) of
        'undefined' ->
            wh_json:get_value([<<"doc">>, <<"flow">>], JObj);
        FlowJObj -> FlowJObj
    end.

-spec get_callflow_number(wh_json:object(), ne_binary()) -> 'undefined' | ne_binary().
get_callflow_number(Callflow, _Mailbox) ->
    case wh_json:get_value([<<"doc">>, <<"numbers">>], Callflow, ['undefined']) of
        [] -> 'undefined';
        [Number | _] -> Number
    end.

-spec maybe_start_caller(#args{}) -> 'ok'.
maybe_start_caller(#args{callback_number = 'undefined'}) ->
    lager:debug("no callback number"),
    'ok';
maybe_start_caller(#args{vm_number = 'undefined', account_id = AccountId}) ->
    lager:info("cannot find voicemail number in account ~p", [AccountId]),
    'ok';
maybe_start_caller(#args{is_callback_disabled = 'true', user_id = UserId}) ->
    lager:debug("disabled for user ~p", [UserId]),
    'ok';
maybe_start_caller(StartArgs) ->
    start_caller(StartArgs).

-spec start_caller(#args{}) -> 'ok'.
start_caller(#args{callback_number = Number
                   ,vm_box_id = VMBoxId
                   ,attempts = Attempts
                   ,interval = Interval
                  } = StartArgs) ->
    lager:info("starting caller to number ~p", [Number]),
    OriginateReq = build_originate_req(StartArgs),
    case ananke_callback_sup:start_child({Number, VMBoxId}, OriginateReq, Attempts, Interval) of
        {'ok', _} -> lager:debug("started");
        {'ok', _, _} -> lager:debug("started");
        {'error', {'already_started', _}} -> lager:info("already started");
        {'error', Error} -> lager:warning("error: ~p", [Error])
    end,
    'ok'.

-spec build_originate_req(#args{}) -> wh_proplist().
build_originate_req(#args{callback_number = CallbackNumber
                          ,vm_number = VMNumber
                          ,account_id = AccountId
                          ,user_id = UserId
                          ,call_timeout = Timeout
                          ,realm = Realm
                         }) ->

    CustomChannelVars = wh_json:from_list([{<<"Account-ID">>, AccountId}
                                           ,{<<"Owner-ID">>, UserId}
                                           ,{<<"AutoAnswer">>, 'true'}
                                           ,{<<"Authorizing-ID">>, UserId}
                                           ,{<<"Inherit-Codec">>, <<"false">>}
                                           ,{<<"Authorizing-Type">>, <<"user">>}
                                           ,{<<"Realm">>, Realm}
                                           ,{<<"Account-Realm">>, Realm}
                                           ,{<<"From-Realm">>, Realm}
                                           ,{<<"Format-From-URI">>, <<"true">>}
                                           ,{<<"From-URI-Realm">>, Realm}
                                          ]),

    Endpoint = [{<<"Invite-Format">>, <<"loopback">>}
                ,{<<"Route">>, CallbackNumber}
                ,{<<"Custom-Channel-Vars">>, CustomChannelVars}
               ],

    ApplicationName = <<"transfer">>,
    ApplicationData = wh_json:from_list([{<<"Route">>, VMNumber}
                                         ,{<<"Custom-Channel-Vars">>, CustomChannelVars}
                                        ]),

    R=[{<<"Timeout">>, Timeout}
       ,{<<"Application-Name">>, ApplicationName}
       ,{<<"Application-Data">>, ApplicationData}
       ,{<<"Originate-Immediate">>, 'true'}
       ,{<<"Ignore-Early-Media">>, 'true'}
       ,{<<"Endpoints">>, [wh_json:from_list(Endpoint)]}
       ,{<<"Dial-Endpoint-Method">>, <<"single">>}
       ,{<<"Continue-On-Fail">>, 'false'}
       ,{<<"Custom-Channel-Vars">>, CustomChannelVars}
       ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Account-Realm">>
                                           ,<<"Authorizing-ID">>, <<"Authorizing-Type">>
                                           ,<<"Owner-ID">>]}
       | wh_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
      ],
    props:filter_undefined(R).

-spec get_first_defined([{ne_binary(), wh_json:object()}]) -> 'undefined' | binary().
get_first_defined(Props) ->
    get_first_defined(Props, 'undefined').

-spec get_first_defined([{ne_binary(), wh_json:object()}], any()) -> binary() | any().
get_first_defined([{Keys, JObj} | Rest], Default) ->
    case wh_json:get_value(Keys, JObj) of
        'undefined' ->
            get_first_defined(Rest, Default);
        Val ->
            Val
    end;
get_first_defined([], Default) ->
    Default.

-spec get_interval(wh_json:object(), wh_json:object(), wh_json:object()) -> integer().
get_interval(VMBoxJObj, UserJObj, AccountJObj) ->
    DefaultInterval = whapps_config:get_binary(?CONFIG_CAT
                                               ,<<"vm_notify_callback_interval">>
                                               ,5*60),
    wh_util:to_integer(
      get_first_defined([{<<"notify_callback_interval">>, VMBoxJObj}
                         ,{<<"vm_notify_callback_interval">>, UserJObj}
                         ,{<<"vm_notify_callback_interval">>, AccountJObj}
                        ]
                        ,DefaultInterval)).

-spec get_attempts(wh_json:object(), wh_json:object(), wh_json:object()) -> integer().
get_attempts(VMBoxJObj, UserJObj, AccountJObj) ->
    DefaultTries = whapps_config:get_binary(?CONFIG_CAT
                                            ,<<"vm_notify_callback_tries">>
                                            ,5),
    wh_util:to_integer(
      get_first_defined([{<<"notify_callback_attempts">>, VMBoxJObj}
                         ,{<<"vm_notify_callback_attempts">>, UserJObj}
                         ,{<<"vm_notify_callback_attempts">>, AccountJObj}
                        ]
                        ,DefaultTries)).

-spec get_callback_timeout(wh_json:object(), wh_json:object(), wh_json:object()) -> integer().
get_callback_timeout(VMBoxJObj, UserJObj, AccountJObj) ->
    DefaultCallTimeout = whapps_config:get_binary(?CONFIG_CAT
                                                  ,<<"vm_notify_callback_timeout">>
                                                  ,20),
    wh_util:to_integer(
      get_first_defined([{<<"notify_callback_timeout">>, VMBoxJObj}
                         ,{<<"vm_notify_callback_timeout">>, UserJObj}
                         ,{<<"vm_notify_callback_timeout">>, AccountJObj}
                        ]
                        ,DefaultCallTimeout)).
