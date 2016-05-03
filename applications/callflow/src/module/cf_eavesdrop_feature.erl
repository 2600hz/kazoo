%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600hz, INC
%%% @doc
%%% Eacesdrop feature code
%%%
%%% data: {
%%%   "group_id":"_group_id_"
%%%   ,"approved_device_id":"_user_id_"
%%%   ,"appoved_user_id":"_device_id_"
%%%   ,"approved_group_id":"_group_id_"
%%% }
%%%
%%% group_id defines list of eavesdrop's targets. If group_id is
%%% undefuned then anybody can be eavesdroped.
%%% One of the approved_device_id, appoved_user_id, approved_group_id
%%% must be defined to access feature code.
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_eavesdrop_feature).

-include("callflow.hrl").

-export([handle/2
         ,get_target_for_extension/2
        ]).

-export_type([target/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    Exten = kapps_call:kvs_fetch('cf_capture_group', Call),
    Target = get_target_for_extension(Exten, Call),
    Table = fields_to_check(),

    case cf_util:check_value_of_fields(Table, 'false', Data, Call)
        andalso maybe_correct_target(Target, kz_json:get_value(<<"group_id">>, Data), Call)
    of
        'true' ->
            Flow = kz_json:from_list([{<<"data">>, build_data(Target, Call)}
                                      ,{<<"module">>, <<"eavesdrop">>}
                                      ,{<<"children">>, kz_json:new()}
                                     ]),
            cf_exe:branch(Flow, Call);
        'false' ->
            cf_eavesdrop:no_permission_to_eavesdrop(Call),
            cf_exe:stop(Call)
    end.

-spec fields_to_check() -> kz_proplist().
fields_to_check() ->
    [{<<"approved_device_id">>, fun(Id, Call) -> Id == kapps_call:authorizing_id(Call) end}
     ,{<<"approved_user_id">>, fun cf_util:caller_belongs_to_user/2}
     ,{<<"approved_group_id">>, fun cf_util:caller_belongs_to_group/2}
    ].

-type target() :: {'ok', ne_binary(), ne_binary()} | 'error'.

-spec build_data(target(), kapps_call:call()) -> kz_json:object().
build_data('error', _Call) -> kz_json:new();
build_data({'ok', TargetId, <<"device">>}, Call) ->
    build_flow_data(Call, [{<<"device_id">>, TargetId}]);
build_data({'ok', TargetId, <<"user">>}, Call) ->
    build_flow_data(Call, [{<<"user_id">>, TargetId}]);
build_data({'ok', TargetId, _TargetType}, Call) ->
    lager:debug("unknown target type ~s for ~s", [TargetId, _TargetType]),
    build_flow_data(Call, []).

-spec build_flow_data(kapps_call:call(), kz_proplist()) -> kz_json:object().
-spec build_flow_data(kapps_call:call(), kz_proplist(), api_binary()) -> kz_json:object().
build_flow_data(Call, Data) ->
    build_flow_data(Call, Data, kapps_call:authorizing_type(Call)).
build_flow_data(Call, Data, <<"device">>) ->
    kz_json:from_list([{<<"approved_device_id">>, kapps_call:authorizing_id(Call)}
                       | Data
                      ]);
build_flow_data(Call, Data, <<"user">>) ->
    kz_json:from_list([{<<"approved_user_id">>, kapps_call:authorizing_id(Call)}
                       | Data
                      ]);
build_flow_data(_Call, Data, _AuthorizingType) ->
    lager:debug("unhandled authorizing type ~s", [_AuthorizingType]),
    kz_json:from_list(Data).

-spec get_target_for_extension(ne_binary(), kapps_call:call()) ->
                                      target().
get_target_for_extension(Exten, Call) ->
    case cf_util:lookup_callflow(Exten, kapps_call:account_id(Call)) of
        {'ok', Callflow, _} ->
            lookup_endpoint(kz_json:get_value(<<"flow">>, Callflow));
        {'error', _} -> 'error'
    end.

-spec maybe_correct_target(target(), api_binary(), kapps_call:call()) ->
                                  boolean().
maybe_correct_target(_Target, 'undefined', _Call) ->
    'true';
maybe_correct_target('error', _GroupId, _Call) ->
    'false';
maybe_correct_target({'ok', TargetId, _}, GroupId, Call) ->
    lists:member(TargetId, find_group_members(GroupId, Call)).

-spec find_group_members(ne_binary(), kapps_call:call()) -> ne_binaries().
find_group_members(GroupId, Call) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), GroupId) of
        {'error', _E} -> [];
        {'ok', GroupJObj} ->
            kz_json:get_keys(<<"endpoints">>, GroupJObj)
    end.

-spec lookup_endpoint(api_object()) -> target().
-spec lookup_endpoint(kz_json:object(), api_binary()) -> target().
lookup_endpoint('undefined') -> 'error';
lookup_endpoint(Flow) ->
    lookup_endpoint(Flow, kz_json:get_value(<<"module">>, Flow)).

lookup_endpoint(Flow, <<"device">> = TargetType) ->
    {'ok', kz_json:get_value([<<"data">>, <<"id">>], Flow), TargetType};
lookup_endpoint(Flow, <<"user">> = TargetType) ->
    {'ok', kz_json:get_value([<<"data">>, <<"id">>], Flow), TargetType};
lookup_endpoint(Flow, _TargetType) ->
    Child = kz_json:get_value([<<"children">>, <<"_">>], Flow),
    lookup_endpoint(Child).
