%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_phone_number).

-export([fetch/1, fetch/2
         ,save/1
         ,delete/1
         ,release/1
         ,new/1, new/2
         ,newly_found/4
        ]).

-export([to_json/1
         ,to_public_json/1
         ,from_json/1
         ,is_phone_number/1
        ]).

-export([setters/2
         ,new/0
         ,number/1, set_number/2
         ,number_db/1 ,set_number_db/2
         ,assign_to/1, set_assign_to/2
         ,assigned_to/1 ,set_assigned_to/2
         ,prev_assigned_to/1 ,set_prev_assigned_to/2
         ,used_by/1 ,set_used_by/2
         ,features/1 ,set_features/2
         ,feature/2 ,set_feature/3
         ,state/1 ,set_state/2
         ,reserve_history/1 ,set_reserve_history/2
         ,add_reserve_history/2, unwind_reserve_history/1
         ,ported_in/1 ,set_ported_in/2
         ,module_name/1 ,set_module_name/2
         ,carrier_data/1, set_carrier_data/2, update_carrier_data/2
         ,region/1 ,set_region/2
         ,auth_by/1 ,set_auth_by/2, is_authorized/1
         ,dry_run/1 ,set_dry_run/2
         ,locality/1 ,set_locality/2
         ,doc/1, set_doc/2, update_doc/2
        ]).

-export([list_attachments/2]).

-include("knm.hrl").
-include_lib("kazoo/src/kz_json.hrl").

-record(knm_phone_number, {number :: ne_binary()
                           ,number_db :: ne_binary()
                           ,assign_to :: api_binary()
                           ,assigned_to :: api_binary()
                           ,prev_assigned_to :: api_binary()
                           ,used_by :: api_binary()
                           ,features = kz_json:new() :: kz_json:object()
                           ,state :: ne_binary()
                           ,reserve_history = [] :: ne_binaries()
                           ,ported_in = 'false' :: boolean()
                           ,module_name :: ne_binary()
                           ,carrier_data = kz_json:new() :: kz_json:object()
                           ,region :: ne_binary()
                           ,auth_by :: api_binary()
                           ,dry_run = 'false' :: boolean()
                           ,locality :: kz_json:object()
                           ,doc = kz_json:new() :: kz_json:object()
                          }).
-opaque knm_phone_number() :: #knm_phone_number{}.

-type knm_phone_numbers() :: [knm_phone_number(), ...].

-export_type([knm_phone_number/0
              ,knm_phone_numbers/0
              ,set_function/0
              ,set_functions/0
             ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary()) -> knm_phone_number().
new(DID) ->
    new(DID, knm_number_options:default()).

-spec new(ne_binary(), knm_number_options:options()) -> knm_phone_number().
new(DID, Options) ->
    NormalizedNum = knm_converters:normalize(DID),
    {'ok', PhoneNumber} =
        setters(new(),
                [{fun set_number/2, NormalizedNum}
                ,{fun set_number_db/2, knm_converters:to_db(NormalizedNum)}
                ,{fun set_assign_to/2, knm_number_options:assign_to(Options)}
                ,{fun set_state/2, ?NUMBER_STATE_DISCOVERY}
                ,{fun set_module_name/2, ?CARRIER_OTHER}
                ,{fun set_carrier_data/2, kz_json:new()}
                ,{fun set_auth_by/2, knm_number_options:auth_by(Options)}
                ,{fun set_dry_run/2, knm_number_options:dry_run(Options)}
                ,{fun set_doc/2, knm_number_options:public_fields(Options)}
                ]),
    PhoneNumber.

-spec newly_found(ne_binary(), module(), ne_binary(), kz_json:object()) ->
                         knm_phone_number_return().
newly_found(Num=?NE_BINARY, Carrier, AssignTo=?NE_BINARY, Data=?JSON_WRAPPER(_))
  when is_atom(Carrier) ->
    NormalizedNum = knm_converters:normalize(Num),
    setters(new(),
            [{fun set_number/2, NormalizedNum}
            ,{fun set_number_db/2, knm_converters:to_db(NormalizedNum)}
            ,{fun set_module_name/2, kz_util:to_binary(Carrier)}
            ,{fun set_carrier_data/2, Data}
            ,{fun set_state/2, ?NUMBER_STATE_DISCOVERY}
            ,{fun set_assign_to/2, kz_util:format_account_id(AssignTo)}
            ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) -> knm_phone_number_return().
-spec fetch(ne_binary(), knm_number_options:options()) -> knm_phone_number_return().

fetch(Num) ->
    fetch(Num, knm_number_options:default()).

-ifdef(TEST).
fetch(?TEST_CREATE_NUM, _Options) ->
    {'error', 'not_found'};
fetch(?TEST_AVAILABLE_NUM, Options) ->
    handle_fetched_result(?AVAILABLE_NUMBER, Options);
fetch(?TEST_IN_SERVICE_NUM, Options) ->
    handle_fetched_result(?IN_SERVICE_NUMBER, Options);
fetch(?TEST_IN_SERVICE_WITH_HISTORY_NUM, Options) ->
    handle_fetched_result(?IN_SERVICE_WITH_HISTORY_NUMBER, Options);
fetch(?BW_EXISTING_DID, Options) ->
    handle_fetched_result(?BW_EXISTING_JSON, Options);
fetch(_DID, _Options) ->
    {'error', 'not_found'}.
-else.
fetch(Num, Options) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    case kz_datamgr:open_cache_doc(NumberDb, NormalizedNum) of
        {'error', _R}=Error ->
            lager:error("failed to open ~s in ~s: ~p", [NormalizedNum, NumberDb, _R]),
            Error;
        {'ok', JObj} ->
            handle_fetched_result(JObj, Options)
    end.
-endif.

-spec handle_fetched_result(kz_json:object(), knm_number_options:options()) ->
                                   {'ok', knm_phone_number()}.
handle_fetched_result(JObj, Options) ->
    PhoneNumber = set_options(from_json(JObj), Options),
    case is_authorized(PhoneNumber) of
        'true' -> {'ok', PhoneNumber};
        'false' -> knm_errors:unauthorized()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_phone_number()) -> knm_phone_number().
save(#knm_phone_number{dry_run='true'}=PhoneNumber) ->
    lager:debug("dry_run-ing btw"),
    PhoneNumber;
save(#knm_phone_number{dry_run='false'}=PhoneNumber) ->
    Routines = [fun save_to_number_db/1
                ,fun handle_assignment/1
               ],
    {'ok', NewPhoneNumber} = setters(PhoneNumber, Routines),
    NewPhoneNumber.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_phone_number()) -> knm_phone_number_return().
delete(#knm_phone_number{dry_run='true'}=Number) ->
    lager:debug("dry_run-ing btw"),
    {'ok', Number};
delete(#knm_phone_number{dry_run='false'}=Number) ->
    Routines = [fun(PhoneNumber) ->
                        knm_number:phone_number(
                          knm_providers:delete(
                            knm_number:set_phone_number(
                              knm_number:new(), PhoneNumber
                             )
                           )
                         )
                end
                ,fun delete_number_doc/1
                ,fun maybe_remove_number_from_account/1
               ],
    {'ok', NewPhoneNumber} = setters(Number, Routines),
    NewPhoneNumber.

-spec release(knm_phone_number()) -> knm_phone_number().
-spec release(knm_phone_number(), ne_binary()) -> knm_phone_number().
release(PhoneNumber) ->
    release(PhoneNumber, state(PhoneNumber)).

release(PhoneNumber, ?NUMBER_STATE_RELEASED) ->
    PhoneNumber;
release(PhoneNumber, ?NUMBER_STATE_DELETED) ->
    PhoneNumber;
release(PhoneNumber, ?NUMBER_STATE_RESERVED) ->
    authorize_release(PhoneNumber);
release(PhoneNumber, ?NUMBER_STATE_PORT_IN) ->
    authorize_release(PhoneNumber);
release(PhoneNumber, ?NUMBER_STATE_IN_SERVICE) ->
    authorize_release(PhoneNumber);
release(PhoneNumber, FromState) ->
    knm_errors:invalid_state_transition(PhoneNumber
                                        ,FromState
                                        ,?NUMBER_STATE_RELEASED
                                       ).

-spec authorize_release(knm_phone_number()) -> knm_phone_number().
-spec authorize_release(knm_phone_number(), ne_binary()) -> knm_phone_number().
authorize_release(PhoneNumber) ->
    authorize_release(PhoneNumber, auth_by(PhoneNumber)).

-ifdef(TEST).
authorize_release(PhoneNumber, ?KNM_DEFAULT_AUTH_BY) ->
    authorized_release(PhoneNumber);
authorize_release(PhoneNumber, ?MASTER_ACCOUNT_ID) ->
    authorized_release(PhoneNumber);
authorize_release(_PhoneNumber, _AuthBy) ->
    knm_errors:unauthorized().
-else.
authorize_release(PhoneNumber, ?KNM_DEFAULT_AUTH_BY) ->
    lager:info("bypassing auth"),
    authorized_release(PhoneNumber);
authorize_release(PhoneNumber, AuthBy) ->
    AssignedTo = assigned_to(PhoneNumber),
    case kz_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true') of
        'false' -> knm_errors:unauthorized();
        'true' -> authorized_release(PhoneNumber)
    end.
-endif.

-spec authorized_release(knm_phone_number()) -> knm_phone_number().
authorized_release(PhoneNumber) ->
    ReleasedState = knm_config:released_state(?NUMBER_STATE_AVAILABLE),
    Routines =
        [{fun set_features/2, kz_json:new()}
         ,{fun set_doc/2, kz_json:private_fields(doc(PhoneNumber))}
         ,{fun set_prev_assigned_to/2, assigned_to(PhoneNumber)}
         ,{fun set_assigned_to/2, 'undefined'}
         ,{fun set_state/2, ReleasedState}
        ],
    {'ok', NewPhoneNumber} = setters(PhoneNumber, Routines),
    NewPhoneNumber.

%%--------------------------------------------------------------------
%% @public
%% @doc Returns same fields view phone_numbers.json returns.
%%--------------------------------------------------------------------
-spec to_public_json(knm_phone_number()) -> kz_json:object().
to_public_json(Number) ->
    JObj = to_json(Number),
    State = {<<"state">>, state(Number)},
    UsedBy = {<<"used_by">>, used_by(Number)},
    ReadOnly =
        kz_json:from_list(
          props:filter_empty(
            [ {<<"created">>, kz_doc:created(JObj)}
            , {<<"modified">>, kz_doc:modified(JObj)}
            , State
            , UsedBy
            ])
         ),
    Root =
        kz_json:from_list(
          props:filter_empty(
            [ State
            , UsedBy
              | kz_json:to_proplist(kz_json:public_fields(JObj))
            ])
         ),
    kz_json:set_value(<<"_read_only">>, ReadOnly, Root).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_json(knm_phone_number()) -> kz_json:object().
to_json(#knm_phone_number{doc=JObj}=N) ->
    Now = kz_util:current_tstamp(),
    kz_json:from_list(
      props:filter_empty(
        [{<<"_id">>, number(N)}
         ,{?PVT_DB_NAME, number_db(N)}
         ,{?PVT_ASSIGNED_TO, assigned_to(N)}
         ,{?PVT_PREVIOUSLY_ASSIGNED_TO, prev_assigned_to(N)}
         ,{?PVT_USED_BY, used_by(N)}
         ,{?PVT_FEATURES, features(N)}
         ,{?PVT_STATE, state(N)}
         ,{?PVT_RESERVE_HISTORY, reserve_history(N)}
         ,{?PVT_PORTED_IN, ported_in(N)}
         ,{?PVT_MODULE_NAME, module_name(N)}
         ,{?PVT_CARRIER_DATA, carrier_data(N)}
         ,{?PVT_REGION, region(N)}
         ,{?PVT_MODIFIED, Now}
         ,{?PVT_CREATED, kz_doc:created(JObj, Now)}
         ,{?PVT_TYPE, <<"number">>}
         | kz_json:to_proplist(
             kz_json:delete_key(<<"id">>, kz_json:public_fields(JObj))
            )
        ])
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_json(kz_json:object()) -> knm_phone_number().
from_json(JObj) ->
    #knm_phone_number{
       number = kz_doc:id(JObj)
       ,number_db = kz_json:get_value(?PVT_DB_NAME, JObj)
       ,assigned_to = kz_json:get_value(?PVT_ASSIGNED_TO, JObj)
       ,prev_assigned_to = kz_json:get_value(?PVT_PREVIOUSLY_ASSIGNED_TO, JObj)
       ,used_by = kz_json:get_value(?PVT_USED_BY, JObj)
       ,features = kz_json:get_value(?PVT_FEATURES, JObj, kz_json:new())
       ,state = kz_json:get_first_defined([?PVT_STATE, ?PVT_STATE_LEGACY], JObj)
       ,reserve_history = kz_json:get_value(?PVT_RESERVE_HISTORY, JObj, [])
       ,ported_in = kz_json:is_true(?PVT_PORTED_IN, JObj, 'false')
       ,module_name = kz_json:get_value(?PVT_MODULE_NAME, JObj)
       ,carrier_data = kz_json:get_value(?PVT_CARRIER_DATA, JObj)
       ,region = kz_json:get_value(?PVT_REGION, JObj)
       ,auth_by = kz_json:get_value(?PVT_AUTH_BY, JObj)
       ,doc = kz_json:delete_key(<<"id">>, kz_json:public_fields(JObj))
      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> knm_phone_number().
new() ->
    #knm_phone_number{}.

-spec is_phone_number(any()) -> boolean().
is_phone_number(#knm_phone_number{}) -> 'true';
is_phone_number(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setters(knm_phone_number(), set_functions()) -> knm_phone_number_return().
setters(Number, Routines) ->
    try lists:foldl(fun setters_fold/2, Number, Routines) of
        {'ok', _N}=Ok -> Ok;
        {'error', _R}=Error -> Error;
        Result -> {'ok', Result}
    catch
        'throw':{'stop', Error} -> Error;
        'error':Reason -> {'error', Reason}
    end.

-type set_function() :: fun((knm_phone_number()) -> setter_acc()) |
                        fun((knm_phone_number(), V) -> setter_acc()) |
                        {fun((knm_phone_number(), V) -> setter_acc()), V} |
                        {fun((knm_phone_number(), K, V) -> setter_acc()), [K | V,...]}.
-type set_functions() :: [set_function()].

-type setter_acc() :: knm_phone_number_return() |
                      knm_phone_number().

-spec setters_fold(set_function(), setter_acc()) -> setter_acc().
setters_fold(_, {'error', _R}=Error) ->
    throw({'stop', Error});
setters_fold({_Fun, 'undefined'}, PhoneNumber) ->
    lager:debug("skipping ~p", [_Fun]),
    PhoneNumber;
setters_fold({Fun, [_|_]=Value}, PhoneNumber) when is_function(Fun) ->
    setters_fold_apply(Fun, [PhoneNumber | Value]);
setters_fold({Fun, Value}, PhoneNumber) when is_function(Fun, 2) ->
    setters_fold_apply(Fun, [PhoneNumber, Value]);
setters_fold(Fun, PhoneNumber) when is_function(Fun, 1) ->
    setters_fold_apply(Fun, [PhoneNumber]).

-spec setters_fold_apply(set_function(), nonempty_list()) -> setter_acc().
setters_fold_apply(Fun, [{'ok',PhoneNumber}|Args]) ->
    setters_fold_apply(Fun, [PhoneNumber|Args]);
setters_fold_apply(Fun, Args) ->
    lager:debug("applying ~p", [Fun]),
    erlang:apply(Fun, Args).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec number(knm_phone_number()) -> ne_binary().
number(#knm_phone_number{number=Num}) -> Num.

-spec set_number(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_number(N, Number=?NE_BINARY) ->
    N#knm_phone_number{number=Number}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec number_db(knm_phone_number()) -> ne_binary().
number_db(#knm_phone_number{number_db=NumberDb}) ->
    NumberDb.

-spec set_number_db(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_number_db(N, NumberDb=?NE_BINARY) ->
    N#knm_phone_number{number_db=NumberDb}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_to(knm_phone_number()) -> api_binary().
assign_to(#knm_phone_number{assign_to=AssignTo}) ->
    AssignTo.

-spec set_assign_to(knm_phone_number(), api_binary()) -> knm_phone_number().
set_assign_to(N, 'undefined') ->
    N#knm_phone_number{assign_to = 'undefined'};
set_assign_to(N, AssignTo=?MATCH_ACCOUNT_RAW(_)) ->
    N#knm_phone_number{assign_to=AssignTo}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assigned_to(knm_phone_number()) -> api_binary().
assigned_to(#knm_phone_number{assigned_to=AssignedTo}) ->
    AssignedTo.

-spec set_assigned_to(knm_phone_number(), api_binary()) -> knm_phone_number().
set_assigned_to(N, 'undefined') ->
    N#knm_phone_number{assigned_to = 'undefined'};
set_assigned_to(N, AssignedTo=?MATCH_ACCOUNT_RAW(_)) ->
    N#knm_phone_number{assigned_to=AssignedTo}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prev_assigned_to(knm_phone_number()) -> api_binary().
prev_assigned_to(#knm_phone_number{prev_assigned_to=PrevAssignedTo}) ->
    PrevAssignedTo.

-spec set_prev_assigned_to(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_prev_assigned_to(N, PrevAssignedTo=?MATCH_ACCOUNT_RAW(_)) ->
    N#knm_phone_number{prev_assigned_to=PrevAssignedTo}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec used_by(knm_phone_number()) -> api_binary().
used_by(#knm_phone_number{used_by=UsedBy}) -> UsedBy.

-spec set_used_by(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_used_by(N, UsedBy=?NE_BINARY) ->
    N#knm_phone_number{used_by=UsedBy}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec features(knm_phone_number()) -> kz_json:object().
features(#knm_phone_number{features=Features}) -> Features.

-spec set_features(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_features(N, Features=?JSON_WRAPPER(_)) ->
    N#knm_phone_number{features=Features}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec feature(knm_phone_number(), ne_binary()) ->
                     kz_json:json_term() | 'undefined'.
feature(Number, Feature) ->
    kz_json:get_value(Feature, features(Number)).

-spec set_feature(knm_phone_number(), ne_binary(), kz_json:json_term()) ->
                         knm_phone_number().
set_feature(N, Feature=?NE_BINARY, Data) ->
    Features = kz_json:set_value(Feature, Data, features(N)),
    N#knm_phone_number{features=Features}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec state(knm_phone_number()) -> api_binary().
state(#knm_phone_number{state=State}) -> State.

-spec set_state(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_state(N, State=?NE_BINARY) ->
    N#knm_phone_number{state=State}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reserve_history(knm_phone_number()) -> ne_binaries().
reserve_history(#knm_phone_number{reserve_history=History}) -> History.

-spec set_reserve_history(knm_phone_number(), ne_binaries()) -> knm_phone_number().
set_reserve_history(N, History) when is_list(History) ->
    N#knm_phone_number{reserve_history=History}.

-spec add_reserve_history(knm_phone_number(), ne_binary()) -> knm_phone_number().
add_reserve_history(#knm_phone_number{reserve_history=[AccountId|_]}=PN
                    ,AccountId=?NE_BINARY
                   ) ->
    PN;
add_reserve_history(PN, AccountId=?NE_BINARY) ->
    PN#knm_phone_number{reserve_history=[AccountId | reserve_history(PN)]}.

-spec unwind_reserve_history(knm_phone_number()) -> knm_phone_number().
unwind_reserve_history(PN) ->
    H = reserve_history(PN),
    Prev = prev_assigned_to(PN),
    set_reserve_history(PN, lists:delete(Prev, H)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ported_in(knm_phone_number()) -> boolean().
ported_in(#knm_phone_number{ported_in=Ported}) -> Ported.

-spec set_ported_in(knm_phone_number(), boolean()) -> knm_phone_number().
set_ported_in(N, Ported) when is_boolean(Ported) ->
    N#knm_phone_number{ported_in=Ported}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec module_name(knm_phone_number()) -> api_binary().
module_name(#knm_phone_number{module_name = ?CARRIER_LOCAL_LEGACY}) -> ?CARRIER_LOCAL;
module_name(#knm_phone_number{module_name = Name}) -> Name.

-spec set_module_name(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_module_name(N, Name=?NE_BINARY) ->
    N#knm_phone_number{module_name=Name}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec carrier_data(knm_phone_number()) -> kz_json:object().
carrier_data(#knm_phone_number{carrier_data=Data}) -> Data.

-spec set_carrier_data(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_carrier_data(N, Data=?JSON_WRAPPER(_)) ->
    N#knm_phone_number{carrier_data=Data}.

-spec update_carrier_data(knm_phone_number(), kz_json:object()) -> knm_phone_number().
update_carrier_data(N=#knm_phone_number{carrier_data = Data}, JObj=?JSON_WRAPPER(_)) ->
    Updated = kz_json:merge_jobjs(JObj, Data),
    N#knm_phone_number{carrier_data = Updated}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec region(knm_phone_number()) -> ne_binary().
region(#knm_phone_number{region=Region}) -> Region.

-spec set_region(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_region(N, Region=?NE_BINARY) ->
    N#knm_phone_number{region=Region}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec auth_by(knm_phone_number()) -> api_binary().
auth_by(#knm_phone_number{auth_by=AuthBy}) -> AuthBy.

-spec set_auth_by(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_auth_by(N, AuthBy=?NE_BINARY) ->
    N#knm_phone_number{auth_by=AuthBy}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dry_run(knm_phone_number()) -> boolean().
dry_run(#knm_phone_number{dry_run=DryRun}) -> DryRun.

-spec set_dry_run(knm_phone_number(), boolean()) -> knm_phone_number().
set_dry_run(N, DryRun) when is_boolean(DryRun) ->
    N#knm_phone_number{dry_run=DryRun}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec locality(knm_phone_number()) -> kz_json:object().
locality(#knm_phone_number{locality=Locality}) -> Locality.

-spec set_locality(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_locality(N, JObj=?JSON_WRAPPER(_)) ->
    N#knm_phone_number{locality=JObj}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec doc(knm_phone_number()) -> kz_json:object().
doc(#knm_phone_number{doc=Doc}) -> Doc.

-spec set_doc(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_doc(N, JObj=?JSON_WRAPPER(_)) ->
    N#knm_phone_number{doc=JObj}.

-spec update_doc(knm_phone_number(), kz_json:object()) -> knm_phone_number().
update_doc(N=#knm_phone_number{doc = Doc}, JObj=?JSON_WRAPPER(_)) ->
    Updated = kz_json:merge_jobjs(JObj, Doc),
    N#knm_phone_number{doc = Updated}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list_attachments(knm_phone_number(), ne_binary()) -> {'ok', kz_json:object()} |
                                                           {'error', any()}.
list_attachments(PhoneNumber, AuthBy) ->
    AssignedTo = assigned_to(PhoneNumber),
    case
        state(PhoneNumber) == ?NUMBER_STATE_PORT_IN
        andalso kz_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true')
    of
        'true' -> {'ok', kz_doc:attachments(doc(PhoneNumber), kz_json:new())};
        'false' -> {'error', 'unauthorized'}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_options(knm_phone_number(), knm_number_options:options()) -> knm_phone_number().
set_options(Number, Options) when is_list(Options) ->
    Updates = [{fun set_assign_to/2, knm_number_options:assign_to(Options)}
               %% See knm_number_options:default/0 for these 2.
              ,{fun set_dry_run/2, knm_number_options:dry_run(Options, 'false')}
              ,{fun set_auth_by/2, knm_number_options:auth_by(Options, ?KNM_DEFAULT_AUTH_BY)}
              ],
    {'ok', PhoneNumber} = setters(Number, Updates),
    PhoneNumber.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(knm_phone_number()) -> boolean().
-ifdef(TEST).
is_authorized(#knm_phone_number{auth_by = ?KNM_DEFAULT_AUTH_BY}) -> 'true';
is_authorized(#knm_phone_number{assigned_to = AssignedTo
                               ,auth_by = AuthBy
                               }) ->
    ?LOG_DEBUG("is authz ~s ~s", [AuthBy, AssignedTo]),
    (AssignedTo =:= ?RESELLER_ACCOUNT_ID orelse AssignedTo =:= ?MASTER_ACCOUNT_ID)
        andalso (AuthBy =:= ?RESELLER_ACCOUNT_ID orelse AuthBy =:= ?MASTER_ACCOUNT_ID).
-else.
is_authorized(#knm_phone_number{auth_by = ?KNM_DEFAULT_AUTH_BY}) ->
    lager:info("bypassing auth"),
    'true';
is_authorized(#knm_phone_number{assigned_to = AssignedTo
                               ,auth_by = AuthBy
                               }) ->
    ?LOG_DEBUG("is authz ~s ~s", [AuthBy, AssignedTo]),
    kz_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true').
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_to_number_db(knm_phone_number()) -> knm_phone_number().
-ifdef(TEST).
save_to_number_db(PhoneNumber) -> PhoneNumber.
-else.
save_to_number_db(PhoneNumber) ->
    NumberDb = number_db(PhoneNumber),
    JObj = to_json(PhoneNumber),
    case kz_datamgr:ensure_saved(NumberDb, JObj) of
        {'ok', Doc} -> from_json(Doc);
        {'error', 'not_found'} ->
            lager:debug("creating new db '~s' for number '~s'", [NumberDb, number(PhoneNumber)]),
            'true' = kz_datamgr:db_create(NumberDb),
            kz_datamgr:revise_views_from_folder(NumberDb, kz_util:to_atom(?APP_NAME)),
            save_to_number_db(PhoneNumber);
        {'error', E} ->
            lager:error("failed to save ~s in ~s: ~p"
                        ,[number(PhoneNumber), NumberDb, E]
                       ),
            knm_errors:database_error(E, PhoneNumber)
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_assignment(knm_phone_number()) -> knm_phone_number().
handle_assignment(PhoneNumber) ->
    ?LOG_DEBUG("handling assignment for ~s", [number(PhoneNumber)]),
    unassign(assign(PhoneNumber)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign(knm_phone_number()) -> knm_phone_number().
-spec assign(knm_phone_number(), ne_binary()) -> knm_phone_number().
assign(PhoneNumber) ->
    AssignedTo = assigned_to(PhoneNumber),
    case kz_util:is_empty(AssignedTo) of
        'true' -> PhoneNumber;
        'false' -> assign(PhoneNumber, AssignedTo)
    end.

-ifdef(TEST).
assign(PhoneNumber, _AssignedTo) ->
    PhoneNumber.
-else.
assign(PhoneNumber, AssignedTo) ->
    AccountDb = kz_util:format_account_db(AssignedTo),
    case kz_datamgr:ensure_saved(AccountDb, to_json(PhoneNumber)) of
        {'error', E} ->
            lager:error("failed to assign number ~s to ~s"
                        ,[number(PhoneNumber), AccountDb]
                       ),
            knm_errors:assign_failure(PhoneNumber, E);
        {'ok', JObj} ->
            lager:debug("assigned number ~s to ~s"
                        ,[number(PhoneNumber), AccountDb]
                       ),
            from_json(JObj)
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec unassign(knm_phone_number()) -> knm_phone_number().
-spec unassign(knm_phone_number(), ne_binary()) -> knm_phone_number().
unassign(PhoneNumber) ->
    PrevAssignedTo = prev_assigned_to(PhoneNumber),
    case kz_util:is_empty(PrevAssignedTo) of
        'true' ->
            lager:debug("prev_assigned_to is is empty for ~s, ignoring"
                        ,[number(PhoneNumber)]
                       ),
            PhoneNumber;
        'false' ->
            unassign(PhoneNumber, PrevAssignedTo)
    end.

-ifdef(TEST).
unassign(PhoneNumber, _PrevAssignedTo) ->
    PhoneNumber.
-else.
unassign(PhoneNumber, PrevAssignedTo) ->
    Num = number(PhoneNumber),
    case get_number_in_account(PrevAssignedTo, Num) of
        {'error', 'not_found'} ->
            lager:debug("number ~s was not found in ~s, no need to unassign"
                        ,[Num, PrevAssignedTo]
                       ),
            PhoneNumber;
        {'ok', _} -> do_unassign(PhoneNumber, PrevAssignedTo);
        {'error', _R} -> do_unassign(PhoneNumber, PrevAssignedTo)
    end.

-spec do_unassign(knm_phone_number(), ne_binary()) -> knm_phone_number().
do_unassign(PhoneNumber, PrevAssignedTo) ->
    AccountDb = kz_util:format_account_db(PrevAssignedTo),
    case kz_datamgr:del_doc(AccountDb, to_json(PhoneNumber)) of
        {'error', E} ->
            lager:error("failed to unassign number ~s from ~s"
                        ,[number(PhoneNumber), PrevAssignedTo]
                       ),
            knm_errors:assign_failure(PhoneNumber, E);
        {'ok', _} ->
            lager:debug("unassigned number ~s from ~s"
                        ,[number(PhoneNumber), PrevAssignedTo]
                       ),
            PhoneNumber
    end.

-spec get_number_in_account(ne_binary(), ne_binary()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', any()}.
get_number_in_account(AccountId, Num) ->
    AccountDb = kz_util:format_account_db(AccountId),
    kz_datamgr:open_cache_doc(AccountDb, Num).
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete_number_doc(knm_phone_number()) -> knm_phone_number_return().
delete_number_doc(Number) ->
    NumberDb = number_db(Number),
    JObj = to_json(Number),
    case kz_datamgr:del_doc(NumberDb, JObj) of
        {'error', _R}=E -> E;
        {'ok', _} -> {'ok', Number}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_remove_number_from_account(knm_phone_number()) -> knm_phone_number_return().
maybe_remove_number_from_account(Number) ->
    AssignedTo = assigned_to(Number),
    case kz_util:is_empty(AssignedTo) of
        'true' ->
            lager:debug("assigned_to is is empty for ~s, ignoring", [number(Number)]),
            {'ok', Number};
        'false' ->
            case kz_datamgr:del_doc(kz_util:format_account_db(AssignedTo), to_json(Number)) of
                {'error', _R}=E -> E;
                {'ok', _} -> {'ok', Number}
            end
    end.
