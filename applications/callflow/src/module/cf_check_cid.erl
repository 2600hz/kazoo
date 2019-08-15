%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handles inspection of incoming caller id and branching to a child
%%% callflow node accordingly.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`use_absolute_mode'</dt>
%%%   <dd>A boolean, if `true', direct call down a branch that matches the
%%%   incoming caller ID. If `false', use regex to determine whether incoming call
%%%   should directed down the "match" or "nomatch" branch.  Default is `false'.
%%%   </dd>
%%%
%%%   <dt>`regex'</dt>
%%%   <dd>A regular expression (like `^\\+15558881111') used to determine match/nomatch
%%%   when `use_absolute_mode' is `false'. Default matches all incoming caller IDs.
%%%   </dd>
%%%
%%%   <dt>`caller_id'</dt>
%%%   <dd><strong>Optional: </strong>Caller ID to applied to incoming call.
%%%     <dl>
%%%       <dt>`external'</dt>
%%%       <dd>Applied to external caller ID. Matches branch, if specified.
%%%         <dl>
%%%           <dt>`name'</dt><dd>A string for caller ID name, e.g. "Joseph"</dd>
%%%           <dt>`number'</dt><dd>A string for caller ID number, e.g. "+15558881122"</dd>
%%%         </dl>
%%%       </dd>
%%%
%%%       <dt>`internal'</dt>
%%%       <dd>Applied to internal caller ID.
%%%         <dl>
%%%           <dt>`name'</dt><dd>A string for caller ID name, e.g. "Joseph"</dd>
%%%           <dt>`number'</dt><dd>A string for caller ID number, e.g. "+15558881122"</dd>
%%%         </dl>
%%%       </dd>
%%%     </dl>
%%%   </dd>
%%%
%%%   <dt>`user_id'</dt>
%%%   <dd><strong>Optional: </strong>User Id applied as owner of incoming call when
%%%   a call goes down the match branch, if specified.
%%%   </dd>
%%% </dl>
%%%
%%% Sample for children section of Callflow to branch into based on the variable:
%%% ```
%%%    "children": {
%%%        "match": { // callflow node to branch to when absolute mode is false and regex matches },
%%%        "nomatch": { // callflow node to branch to when regex does not match or no child node defined for incoming caller id },
%%%        "+15558881111": { // callflow node to branch to absolute mode is true and caller id matches +15558881111) }
%%%    }
%%% '''
%%%
%%% @author Brian Davis
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_check_cid).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CallerIdNumber = kapps_call:caller_id_number(Call),
    Regex = kz_json:get_ne_binary_value(<<"regex">>, Data, <<".*">>),

    lager:debug("comparing caller id ~s against regex ~s", [CallerIdNumber, Regex]),

    case re:run(CallerIdNumber, Regex) of
        {'match', _} -> handle_match(Data, Call, CallerIdNumber);
        'nomatch' -> handle_no_match(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Handle a caller id "match" condition
%% @end
%%------------------------------------------------------------------------------
-spec handle_match(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
handle_match(Data, Call, CallerIdNumber) ->
    case kz_json:is_true(<<"use_absolute_mode">>, Data, 'false') of
        'true' -> maybe_branch_on_caller_id(Data, Call, CallerIdNumber);
        'false' -> maybe_branch_on_regex(Data, Call)
    end.

-spec maybe_branch_on_caller_id(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
maybe_branch_on_caller_id(Data, Call, CallerIdNumber) ->
    case is_callflow_child(CallerIdNumber, Call) of
        'true' -> update_caller_identity(Data, Call);
        'false' -> cf_exe:continue(Call)
    end.

-spec maybe_branch_on_regex(kz_json:object(), kapps_call:call()) -> 'ok'.
maybe_branch_on_regex(Data, Call) ->
    case is_callflow_child(<<"match">>, Call) of
        'true' -> update_caller_identity(Data, Call);
        'false' -> cf_exe:continue(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Handle a caller id "no match" condition
%% @end
%%------------------------------------------------------------------------------
-spec handle_no_match(kapps_call:call()) -> 'ok'.
handle_no_match(Call) ->
    case is_callflow_child(<<"nomatch">>, Call) of
        'true' -> 'ok';
        'false' -> cf_exe:continue(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Check if the given node name is a callflow child
%% @end
%%------------------------------------------------------------------------------
-spec is_callflow_child(kz_term:ne_binary(), kapps_call:call()) -> boolean().
is_callflow_child(Name, Call) ->
    lager:debug("looking for callflow child ~s", [Name]),
    case cf_exe:attempt(Name, Call) of
        {'attempt_resp', 'ok'} ->
            lager:debug("found callflow child"),
            'true';
        {'attempt_resp', {'error', _}} ->
            lager:debug("failed to find callflow child"),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc update the caller id and owner information for this call
%% @end
%%------------------------------------------------------------------------------
-spec update_caller_identity(kz_json:object(), kapps_call:call()) -> 'ok'.
update_caller_identity(Data, Call) ->
    Name = kz_json:get_ne_binary_value([<<"caller_id">>, <<"external">>, <<"name">>], Data),
    Number = kz_json:get_ne_binary_value([<<"caller_id">>, <<"external">>, <<"number">>], Data),
    UserId =  kz_json:get_ne_binary_value([<<"user_id">>], Data),
    case is_valid_caller_identity(Name, Number, UserId) of
        'true' ->
            lager:info("setting caller id to ~s <~s>", [Number, Name]),
            lager:info("setting owner id to ~s", [UserId]),
            Updates = [fun(C) -> kapps_call:set_caller_id_number(Number, C) end
                      ,fun(C) -> kapps_call:set_caller_id_name(Name, C) end
                      ,fun(C) -> kapps_call:kvs_store('owner_id', UserId, C) end
                      ],
            {'ok', C} = cf_exe:get_call(Call),
            cf_exe:set_call(kapps_call:exec(Updates, C));
        'false' -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc validate that all required parameters are defined
%% @end
%%------------------------------------------------------------------------------
-spec is_valid_caller_identity(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) -> boolean().
is_valid_caller_identity('undefined', _Number, _UserId) -> 'false';
is_valid_caller_identity(_Name, 'undefined', _UserId) -> 'false';
is_valid_caller_identity(_Name, _Number, 'undefined') -> 'false';
is_valid_caller_identity(_Name, _Number, _UserId) -> 'true'.
