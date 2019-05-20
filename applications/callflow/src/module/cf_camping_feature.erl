%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Sends request to start the call to recipient when they available.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`timeout'</dt>
%%%   <dd>Timeout in minutes.</dd>
%%%
%%%   <dt>`tries'</dt>
%%%   <dd>Number of retries.</dd>
%%%
%%%   <dt>`try_interval'</dt>
%%%   <dd>Intervals in minutes between each retries.</dd>
%%%
%%%   <dt>`stop_after'</dt>
%%%   <dd>Timeout to stop retries in minutes</dd>
%%% </dl>
%%%
%%% `cf_capture_group' is used to extract extension number.
%%% `timeout', `tries', `try_interval' and `stop_after' will correct system
%%% defaults if present.
%%%
%%% <strong>Usage example:</strong>
%%% <ol>
%%%   <li>Create a "pattern callflow" with `"patterns": ["^\\*7([0-9]*)$"]'</li>
%%%   <li>Create simple callflow with `number = 401'</li>
%%%   <li>Dial 401 to start ringing the phones in group, in another phone dial *7401 to make call camping</li>
%%% </ol>
%%%
%%%
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_camping_feature).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

-record(state, {callflow :: kz_json:object()
               ,is_no_match :: boolean()
               ,id :: kz_term:api_ne_binary()
               ,type :: kz_term:api_ne_binary()
               ,number :: kz_term:ne_binary()
               ,channels = [] :: kz_json:objects()
               ,config :: kz_json:object()
               }).
-type state() :: #state{}.

-type maybe_m(X) :: 'Nothing' | {'Just', X}.

-spec '>>='(maybe_m(A), fun((A) -> maybe_m(B))) -> maybe_m(B).
'>>='('Nothing', _) ->
    'Nothing';
'>>='({'Just', X}, Fun) ->
    Fun(X).

-spec just(A) -> maybe_m(A).
just(X) ->
    {'Just', X}.

-spec nothing() -> maybe_m(any()).
nothing() ->
    'Nothing'.

init([Data, Call]) ->
    kapps_call_command:answer(Call),
    lager:info("camping feature started"),
    Number = kapps_call:kvs_fetch('cf_capture_group', Call),
    CF = cf_flow:lookup(Number, kapps_call:account_id(Call)),
    case CF of
        {'ok', Callflow, IsNoMatch} -> just(#state{callflow = Callflow
                                                  ,is_no_match = IsNoMatch
                                                  ,number = Number
                                                  ,config = Data
                                                  });
        _ -> nothing()
    end.

-spec get_target(state()) -> maybe_m(state()).
get_target(#state{callflow = Callflow} = S) ->
    lager:debug("getting target"),
    TargetId = kz_json:get_ne_value([<<"flow">>, <<"data">>, <<"id">>], Callflow),
    TargetType = kz_json:get_ne_value([<<"flow">>, <<"module">>], Callflow),
    case {TargetType, TargetId} of
        {<<"offnet">>, _} -> just(S#state{type = TargetType});
        {'undefined', _} -> nothing();
        {_, 'undefined'} -> nothing();
        {_, _} -> just(S#state{id = TargetId, type = TargetType})
    end.

-spec check_target_type(state()) -> maybe_m(state()).
check_target_type(#state{type = TargetType} = S) ->
    lager:debug("checking target type"),
    case lists:member(TargetType, [<<"offnet">>, <<"user">>, <<"device">>]) of
        'true' -> just(S);
        'false' -> nothing()
    end.

-spec get_channels(state(), kapps_call:call()) -> maybe_m(state()).
get_channels(#state{type = TargetType, id = TargetId} = S, Call) ->
    lager:debug("exploring channels"),
    Usernames = case TargetType of
                    <<"device">> -> cf_util:sip_users_from_device_ids([TargetId], Call);
                    <<"user">> ->
                        EPs = cf_util:find_user_endpoints([TargetId], [], Call),
                        cf_util:sip_users_from_device_ids(EPs, Call);
                    <<"offnet">> ->
                        []
                end,
    just(S#state{channels = cf_util:find_channels(Usernames, Call)}).

-spec check_self(state(), kapps_call:call()) -> maybe_m(state()).
check_self(State, Call) ->
    lager:debug("check on self"),
    case {kapps_call:authorizing_id(Call), kapps_call:authorizing_type(Call)} of
        {'undefined', _} -> nothing();
        {_, 'undefined'} -> nothing();
        _ -> just(State)
    end.

-spec send_request(state(), kapps_call:call()) -> maybe_m('accepted') |
                                                  maybe_m('connected').
send_request(#state{channels = Channels} = S, Call) ->
    lager:debug("sending request"),
    case Channels of
        [] -> no_channels(S, Call);
        _ -> has_channels(S, Call)
    end.

-spec do(maybe_m(A), [fun((A) -> maybe_m(B))]) -> maybe_m(B).
do(Monad, Actions) ->
    lists:foldl(fun(Action, Acc) -> '>>='(Acc, Action) end, Monad, Actions).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, creates the parameters and branches
%% to {@link cf_group_pickup}.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Ok = do(just([Data, Call])
           ,[fun init/1
            ,fun get_target/1
            ,fun check_target_type/1
            ,fun (State) -> check_self(State, Call) end
            ,fun (State) -> get_channels(State, Call) end
            ,fun (State) -> send_request(State, Call) end
            ]),
    case Ok of
        {'Just', 'accepted'} ->
            _ = kapps_call_command:b_prompt(<<"camper-queue">>, Call),
            cf_exe:stop(Call);
        {'Just', 'connected'} -> 'ok';
        'Nothing' ->
            _ = kapps_call_command:b_prompt(<<"camper-deny">>, Call),
            cf_exe:stop(Call)
    end.

-spec get_sip_usernames_for_target(kz_term:ne_binary(), kz_term:ne_binary(), kapps_call:call()) ->
                                          kz_term:ne_binaries().
get_sip_usernames_for_target(TargetId, TargetType, Call) ->
    Targets = case TargetType of
                  <<"user">> -> kz_attributes:owned_by(TargetId, <<"device">>, Call);
                  <<"device">> -> [TargetId];
                  _Else ->
                      lager:debug("can't found camping target's type. May be wrong extension number?"),
                      []
              end,
    AccountDb = kapps_call:account_db(Call),
    [SIPUsername
     || DeviceId <- Targets,
        SIPUsername <- [get_device_sip_username(AccountDb, DeviceId)],
        'undefined' =/= SIPUsername
    ].

-spec get_device_sip_username(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
get_device_sip_username(AccountDb, DeviceId) ->
    {'ok', JObj} = kz_datamgr:open_cache_doc(AccountDb, DeviceId),
    kzd_devices:sip_username(JObj).

-spec no_channels(state(), kapps_call:call()) -> maybe_m('accepted') |
                                                 maybe_m('connected').
no_channels(#state{id = TargetId
                  ,type = TargetType
                  ,is_no_match = 'false'
                  }
           ,Call) ->
    Flow = kz_json:from_list([{<<"module">>, TargetType}
                             ,{<<"data">>, kz_json:from_list([{<<"id">>, TargetId}])}
                             ]),
    cf_exe:branch(Flow, Call),
    just('connected');
no_channels(#state{type = <<"offnet">>
                  ,is_no_match = 'true'
                  ,number = Number
                  ,config = CFG
                  }
           ,Call) ->
    MsgProps = props:filter_undefined(
                 [{<<"Number">>, Number}
                 ,{<<"Call">>, kapps_call:to_json(Call)}
                 ,{<<"Tries">>, kz_json:get_value(<<"tries">>, CFG)}
                 ,{<<"Stop-After">>, kz_json:get_value(<<"stop_after">>, CFG)}
                 ,{<<"Try-Interval">>, kz_json:get_value(<<"try_interval">>, CFG)}
                 ]),
    JObj = kz_json:from_list([{<<"Delegate-Message">>, kz_json:from_list(MsgProps)}
                              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]),
    kapi_delegate:publish_delegate(<<"camper">>, JObj, <<"offnet">>),
    just('accepted').

-spec has_channels(state(), kapps_call:call()) -> maybe_m('accepted').
has_channels(#state{id = TargetId
                   ,type = TargetType
                   ,number = Number
                   ,config = CFG
                   }, Call) ->
    Targets = get_sip_usernames_for_target(TargetId, TargetType, Call),
    MsgProps = props:filter_undefined(
                 [{<<"Account-DB">>, kapps_call:account_db(Call)}
                 ,{<<"Authorizing-ID">>, kapps_call:authorizing_id(Call)}
                 ,{<<"Authorizing-Type">>, kapps_call:authorizing_type(Call)}
                 ,{<<"Number">>, Number}
                 ,{<<"Targets">>, Targets}
                 ,{<<"Timeout">>, kz_json:get_value(<<"timeout">>, CFG)}
                 ]),
    JObj = kz_json:from_list([{<<"Delegate-Message">>, kz_json:from_list(MsgProps)}
                              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]),
    kapi_delegate:publish_delegate(<<"camper">>, JObj, <<"onnet">>),
    just('accepted').
