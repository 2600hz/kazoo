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

-export([handle/2]).

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

    case  cf_util:check_value_of_fields(Table, 'false', Data, Call)
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
build_data({'ok', TargetId, TargetType}, Call) ->
    Target =
        case TargetType of
            <<"device">> -> [{<<"device_id">>, TargetId}];
            <<"user">> -> [{<<"user_id">>, TargetId}];
            _ ->
                lager:info("Unknown target's type(~s)", [TargetType]),
                []
        end,
    Permission =
        case whapps_call:authorizing_type(Call) of
            <<"device">> ->
                [{<<"approved_device_id">>, whapps_call:authorizing_id(Call)}
                 | Target
                ];
            <<"user">> ->
                [{<<"approved_user_id">>, whapps_call:authorizing_id(Call)}
                 | Target
                ]
        end,
    wh_json:from_list(Permission);
build_data('error', _Call) ->
    wh_json:new().

-spec get_target_for_extension(ne_binary(), whapps_call:call()) ->
                                      target().
get_target_for_extension(Exten, Call) ->
    case cf_util:lookup_callflow(Exten, whapps_call:account_id(Call)) of
        {'ok', Callflow, _} ->
            TargetId = wh_json:get_ne_value([<<"flow">>, <<"data">>, <<"id">>], Callflow),
            TargetType = wh_json:get_ne_value([<<"flow">>, <<"module">>], Callflow),
            {'ok', TargetId, TargetType};
        _ ->
            'error'
    end.

-spec maybe_correct_target(target(), wh_json:object(), whapps_call:call()) ->
                                  boolean().
maybe_correct_target(Target, Data, Call) ->
    case wh_json:get_value(<<"group_id">>, Data) of
        'undefined' ->
            'true';
        GroupId ->
            case Target of
                {'ok', TargetId, _} -> cf_util:maybe_belongs_to_group(TargetId, GroupId, Call);
                'error' -> 'false'
            end
    end.
