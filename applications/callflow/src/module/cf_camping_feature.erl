%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz INC
%%% @doc
%%% Sends request to start the call to recepient when he's available
%%%
%%% data: {
%%%   "timeout": "_minutes_timeout"
%%%   ,"tries": "_count"
%%%   ,"try_interval": "_minutes_interval"
%%%   ,"stop_after": "_minutes_timeout"
%%% }
%%%
%%% uses cf_capture_group to extract extension number
%%%
%%% `timeout`, `tries`, `try_interval` & `stop_after` will correct system
%%% defaults if present
%%%
%%% usage example
%%%
%%% 1) create a "pattern callflow" with "patterns": ["^\\*7([0-9]*)$"]
%%% 2) create simple callflow with number = 401
%%% 3) dial 401 to start ringing the phones in group, in another phone dial *7401 to make call camping
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_camping_feature).

-include("callflow.hrl").

-export([handle/2]).

-record(state, {callflow :: wh_json:object()
                ,is_no_match :: boolean()
                ,id :: ne_binary()
                ,type :: ne_binary()
                ,number :: ne_binary()
                ,channels :: wh_json:objects()
                ,config :: wh_json:object()
               }).
-type state() :: #state{}.

-type maybe(X) :: 'Nothing' | {'Just', X}.

-spec '>>='(maybe(A), fun((A) -> maybe(B))) -> maybe(B).
'>>='('Nothing', _) ->
    'Nothing';
'>>='({'Just', X}, Fun) ->
    Fun(X).

-spec just(A) -> maybe(A).
just(X) ->
    {'Just', X}.

-spec nothing() -> maybe(any()).
nothing() ->
    'Nothing'.

init([Data, Call]) ->
    whapps_call_command:answer(Call),
    lager:info("Camping feature started"),
    Number = whapps_call:kvs_fetch('cf_capture_group', Call),
    CF = cf_util:lookup_callflow(Number, whapps_call:account_id(Call)),
    case CF of
        {'ok', Callflow, IsNoMatch} -> just(#state{callflow = Callflow
                                                   ,is_no_match = IsNoMatch
                                                   ,number = Number
                                                   ,config = Data
                                                  });
        _ -> nothing()
    end.

-spec get_target(state()) -> maybe(state()).
get_target(#state{callflow = Callflow} = S) ->
    lager:debug("Getting target"),
    TargetId = wh_json:get_ne_value([<<"flow">>, <<"data">>, <<"id">>], Callflow),
    TargetType = wh_json:get_ne_value([<<"flow">>, <<"module">>], Callflow),
    case {TargetType, TargetId} of
        {<<"offnet">>, _} -> just(S#state{type = TargetType});
        {'undefined', _} -> nothing();
        {_, 'undefined'} -> nothing();
        {_, _} -> just(S#state{id = TargetId, type = TargetType})
    end.

-spec check_target_type(state()) -> maybe(state()).
check_target_type(#state{type = TargetType} = S) ->
    lager:debug("Checking target type"),
    case lists:member(TargetType, [<<"offnet">>, <<"user">>, <<"device">>]) of
        'true' -> just(S);
        'false' -> nothing()
    end.

-spec get_channels(state(), whapps_call:call()) -> maybe(state()).
get_channels(#state{type = TargetType, id = TargetId} = S, Call) ->
    lager:debug("Exlpoing channels"),
    Usernames = case TargetType of
                   <<"device">> -> cf_util:sip_users_from_device_ids([TargetId], Call);
                   <<"user">> ->
                       EPs = cf_util:find_user_endpoints([TargetId], [], Call),
                       cf_util:sip_users_from_device_ids(EPs, Call);
                   <<"offnet">> ->
                       []
               end,
    just(S#state{channels = cf_util:find_channels(Usernames, Call)}).

-spec check_self(state(), whapps_call:call()) -> maybe(state()).
check_self(State, Call) ->
    lager:debug("Check on self"),
    case {whapps_call:authorizing_id(Call), whapps_call:authorizing_type(Call)} of
        {'undefined', _} -> nothing();
        {_, 'undefined'} -> nothing();
        _ -> just(State)
    end.

-spec send_request(state(), whapps_call:call()) -> maybe('ok').
send_request(#state{channels = Channels} = S, Call) ->
    lager:debug("Sending request"),
    case Channels of
        [] -> no_channels(S, Call);
        _ -> has_channels(S, Call)
    end.

-spec do(maybe(A), [fun((A) -> maybe(B))]) -> maybe(B).
do(Monad, Actions) ->
    lists:foldl(fun(Action, Acc) -> '>>='(Acc, Action) end, Monad, Actions).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, creates the parameters and branches
%% to cf_group_pickup.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Ok = do(just([Data, Call]),[fun init/1
                                ,fun get_target/1
                                ,fun check_target_type/1
                                ,fun (State) -> check_self(State, Call) end
                                ,fun (State) -> get_channels(State, Call) end
                                ,fun (State) -> send_request(State, Call) end
                               ]),
    case Ok of
        {'Just', 'accepted'} ->
            whapps_call_command:b_prompt(<<"camper-queue">>, Call),
            cf_exe:stop(Call);
        {'Just', 'connected'} -> 'ok';
        'Nothing' ->
            whapps_call_command:b_prompt(<<"camper-deny">>, Call),
            cf_exe:stop(Call)
    end.

-spec get_sip_usernames_for_target(ne_binary(), ne_binary(), whapps_call:call()) ->
                                          ne_binaries().
get_sip_usernames_for_target(TargetId, TargetType, Call) ->
    Targets = case TargetType of
                  <<"user">> -> cf_attributes:owned_by(TargetId, <<"device">>, Call);
                  <<"device">> -> [TargetId];
                  _Else ->
                      lager:debug("Can't found camping target's type. May be wrong extension number?"),
                      []
              end,
    AccountDb = whapps_call:account_db(Call),
    props:filter_undefined(
      [get_device_sip_username(AccountDb, DeviceId)
       || DeviceId <- Targets
      ]).

-spec get_device_sip_username(ne_binary(), ne_binary()) -> api_binary().
get_device_sip_username(AccountDb, DeviceId) ->
    {'ok', JObj} = kz_datamgr:open_cache_doc(AccountDb, DeviceId),
    kz_device:sip_username(JObj).

-spec no_channels(state(), whapps_call:call()) -> maybe('accepted') |
                                                  maybe('connected').
no_channels(#state{id = TargetId
                   ,type = TargetType
                   ,is_no_match = 'false'
                  }
            ,Call) ->
    Flow = wh_json:from_list([{<<"module">>, TargetType}
                              ,{<<"data">>, wh_json:from_list([{<<"id">>, TargetId}])}
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
                  ,{<<"Call">>, whapps_call:to_json(Call)}
                  ,{<<"Tries">>, wh_json:get_value(<<"tries">>, CFG)}
                  ,{<<"Stop-After">>, wh_json:get_value(<<"stop_after">>, CFG)}
                  ,{<<"Try-Interval">>, wh_json:get_value(<<"try_interval">>, CFG)}
                 ]),
    JObj = wh_json:from_list([{<<"Delegate-Message">>, wh_json:from_list(MsgProps)}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]),
    wapi_delegate:publish_delegate(<<"camper">>, JObj, <<"offnet">>),
    just('accepted').

-spec has_channels(state(), whapps_call:call()) -> maybe('accepted').
has_channels(#state{id = TargetId
                    ,type = TargetType
                    ,number = Number
                    ,config = CFG
                   }, Call) ->
    Targets = get_sip_usernames_for_target(TargetId, TargetType, Call),
    MsgProps = props:filter_undefined(
                 [{<<"Account-DB">>, whapps_call:account_db(Call)}
                  ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
                  ,{<<"Authorizing-Type">>, whapps_call:authorizing_type(Call)}
                  ,{<<"Number">>, Number}
                  ,{<<"Targets">>, Targets}
                  ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, CFG)}
                 ]),
    JObj = wh_json:from_list([{<<"Delegate-Message">>, wh_json:from_list(MsgProps)}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]),
    wapi_delegate:publish_delegate(<<"camper">>, JObj, <<"onnet">>),
    just('accepted').
