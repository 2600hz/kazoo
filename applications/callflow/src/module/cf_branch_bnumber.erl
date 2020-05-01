%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc Branch the call to the capture group if found, otherwise
%%% continue with original callee number.
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_branch_bnumber).
-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    handle(Data, Call, cf_util:normalize_capture_group(CaptureGroup, Call)).

handle(_Data, Call, 'undefined') ->
    lager:info("no capture group value to use, skipping"),
    cf_exe:continue(Call);
handle(_Data, Call, <<>>) ->
    lager:info("no capture group value to use, skipping"),
    cf_exe:continue(Call);
handle(Data, Call, CaptureGroup) ->
    handle(Data, Call, CaptureGroup, kz_json:is_true(<<"hunt">>, Data)).

handle(_Data, Call, CaptureGroup, 'false') ->
    lager:info("trying to branch to child ~s", [CaptureGroup]),
    cf_exe:continue(CaptureGroup, Call);
handle(Data, Call, CaptureGroup, 'true') ->
    maybe_hunt(Data, Call, CaptureGroup).

maybe_hunt(Data, Call, CaptureGroup) ->
    HuntDeny = kz_json:get_ne_binary_value(<<"hunt_deny">>, Data),
    HuntAllow = kz_json:get_ne_binary_value(<<"hunt_allow">>, Data),

    maybe_hunt(Data, Call, CaptureGroup, is_hunt_allowed(CaptureGroup, HuntDeny, HuntAllow)).

is_hunt_allowed(CaptureGroup, HuntDeny, HuntAllow) ->
    is_allowed(CaptureGroup, HuntAllow)
        andalso not is_denied(CaptureGroup, HuntDeny).

is_allowed(_CaptureGroup, 'undefined') -> 'true';
is_allowed(_CaptureGroup, <<>>) -> 'true';
is_allowed(CaptureGroup, Regex) ->
    try re:run(CaptureGroup, Regex) of
        {'match', _} -> 'true';
        'nomatch' ->
            lager:info("capture group ~s does not match allowed regex ~s", [CaptureGroup, Regex]),
            'false'
    catch
        _:_ ->
            lager:info("regex ~s is invalid, not allowing", [Regex]),
            'false'
    end.

is_denied(_CaptureGroup, 'undefined') -> 'false';
is_denied(_CaptureGroup, <<>>) -> 'false';
is_denied(CaptureGroup, Regex) ->
    try re:run(CaptureGroup, Regex) of
        'nomatch' -> 'false';
        {'match', _} ->
            lager:info("capture group ~s does matches denied regex ~s, not allowing hunt", [CaptureGroup, Regex]),
            'true'
    catch
        _:_ ->
            lager:info("regex ~s is invalid, allowing hunt", [Regex]),
            'false'
    end.

maybe_hunt(_Data, Call, CaptureGroup, 'true') ->
    AccountId = kapps_call:account_id(Call),
    lager:info("hunting for ~s in account ~s", [CaptureGroup, AccountId]),

    AllowNoMatch = cf_route_req:allow_no_match(Call),
    case cf_flow:lookup(CaptureGroup, AccountId) of
        {'ok', Flow, NoMatch} when (not NoMatch)
                                   orelse AllowNoMatch ->
            Props = [{'cf_capture_group', kz_json:get_ne_value(<<"capture_group">>, Flow)}
                    ,{'cf_capture_groups', kz_json:get_value(<<"capture_groups">>, Flow, kz_json:new())}
                    ],
            UpdatedCall = kapps_call:kvs_store_proplist(Props, Call),
            cf_exe:set_call(UpdatedCall),
            lager:info("branching to ~s", [kz_doc:id(Flow)]),
            cf_exe:branch(kzd_callflows:flow(Flow, kz_json:new()), UpdatedCall);
        _Else ->
            lager:info("hunt failed to find a callflow"),
            cf_exe:continue(Call)
    end.
