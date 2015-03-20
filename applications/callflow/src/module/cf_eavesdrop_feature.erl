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

-include("../callflow.hrl").

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
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    Exten = whapps_call:kvs_fetch('cf_capture_group', Call),
    Target = get_target_for_extension(Exten, Call),
    Table = fields_to_check(),

    case cf_util:check_value_of_fields(Table, 'false', Data, Call)
        andalso maybe_correct_target(Target, Data, Call)
    of
        'true' ->
            Flow = wh_json:from_list([{<<"data">>, build_data(Target, Call)}
                                      ,{<<"module">>, <<"eavesdrop">>}
                                      ,{<<"children">>, wh_json:new()}
                                     ]),
            cf_exe:branch(Flow, Call);
        'false' ->
            cf_eavesdrop:no_permission_to_eavesdrop(Call),
            cf_exe:stop(Call)
    end.

-spec fields_to_check() -> wh_proplist().
fields_to_check() ->
    [{<<"approved_device_id">>, fun(Id, Call) -> Id == whapps_call:authorizing_id(Call) end}
     ,{<<"approved_user_id">>, fun cf_util:caller_belongs_to_user/2}
     ,{<<"approved_group_id">>, fun cf_util:caller_belongs_to_group/2}
    ].

-type target() :: {'ok', ne_binary(), ne_binary()} | 'error'.

-spec build_data(target(), whapps_call:call()) -> wh_json:object().
build_data('error', _Call) -> wh_json:new();
build_data({'ok', TargetId, <<"device">>}, Call) ->
    build_flow_data(Call, [{<<"device_id">>, TargetId}]);
build_data({'ok', TargetId, <<"user">>}, Call) ->
    build_flow_data(Call, [{<<"user_id">>, TargetId}]);
build_data({'ok', TargetId, _TargetType}, Call) ->
    lager:debug("unknown target type ~s for ~s", [TargetId, _TargetType]),
    build_flow_data(Call, []).

-spec build_flow_data(whapps_call:call(), wh_proplist()) -> wh_json:object().
-spec build_flow_data(whapps_call:call(), wh_proplist(), api_binary()) -> wh_json:object().
build_flow_data(Call, Data) ->
    build_flow_data(Call, Data, whapps_call:authorizing_type(Call)).
build_flow_data(Call, Data, <<"device">>) ->
    wh_json:from_list([{<<"approved_device_id">>, whapps_call:authorizing_id(Call)}
                       | Data
                      ]);
build_flow_data(Call, Data, <<"user">>) ->
    wh_json:from_list([{<<"approved_user_id">>, whapps_call:authorizing_id(Call)}
                       | Data
                      ]);
build_flow_data(_Call, Data, _AuthorizingType) ->
    lager:debug("unhandled authorizing type ~s", [_AuthorizingType]),
    wh_json:from_list(Data).

-spec get_target_for_extension(ne_binary(), whapps_call:call()) ->
                                      target().
get_target_for_extension(Exten, Call) ->
    case cf_util:lookup_callflow(Exten, whapps_call:account_id(Call)) of
        {'ok', Callflow, _} ->
            lookup_endpoint(wh_json:get_value(<<"flow">>, Callflow));
        {'error', _} -> 'error'
    end.

-spec lookup_endpoint(api_object()) -> target().
-spec lookup_endpoint(wh_json:object(), api_binary()) -> target().
lookup_endpoint('undefined') -> 'error';
lookup_endpoint(Callflow) ->
    lookup_endpoint(Callflow, wh_json:get_value(<<"module">>, Callflow)).

lookup_endpoint(Callflow, <<"device">> = TargetType) ->
    {'ok', wh_json:get_value([<<"data">>, <<"id">>], Callflow), TargetType};
lookup_endpoint(Callflow, <<"user">> = TargetType) ->
    {'ok', wh_json:get_value([<<"data">>, <<"id">>], Callflow), TargetType};
lookup_endpoint(Callflow, _TargetType) ->
    Child = wh_json:get_value([<<"children">>, <<"_">>], Callflow),
    lookup_endpoint(Child).

-spec maybe_correct_target(target(), wh_json:object(), whapps_call:call()) ->
                                  boolean().
-spec maybe_correct_target(target(), wh_json:object(), whapps_call:call(), api_binary()) ->
                                  boolean().
maybe_correct_target(Target, Data, Call) ->
    maybe_correct_target(Target, Data, Call, wh_json:get_value(<<"group_id">>, Data)).

maybe_correct_target(_Target, _Data, _Call, 'undefined') -> 'true';
maybe_correct_target('error', _Data, _Call, _GroupId) -> 'false';
maybe_correct_target({'ok', TargetId, _}, _Data, Call, GroupId) ->
    cf_util:maybe_belongs_to_group(TargetId, GroupId, Call).
