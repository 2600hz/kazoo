%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Handles inspection of incoming caller id and branching to a child
%%% callflow node accordingly.
%%%
%%% "data":{
%%%   "use_absolute_mode":[true/false] // if true, direct call down a branch that
%%%        //     matches the incoming caller id.
%%%        // if false, use regex to determine whether incoming call should directed
%%%        // down the "match" or "nomatch" branch.  Default is false.
%%%   ,"regex": "^\\+15558881111" // regular expression used to determine match/nomatch
%%%        // when use_absolute_mode is false.  Default matches all incoming caller ids.
%%%   ,"caller_id": {      // optional caller id applied to incoming call when it goes down a
%%%       "external": {   // match branch, if specified.
%%%           "name": "Joseph",
%%%           "number": "+15558881122"
%%%       },
%%%       "internal": {
%%%           "name": "Joe",
%%%           "number": "+15558881122"
%%%       }
%%%    }
%%%   ,"user_id":[uuid of kazoo User] // optional user id applied as owner of incoming call when
%%%                                   // when a call goes down the match branch, if specified.
%%% },
%%% "children": {
%%%   "match": { [callflow node to branch to when absolute mode is false and regex matches] },
%%%   "nomatch": { [callflow node to branch to when regex does not match or no child node defined for incoming caller id] },
%%%   "+15558881111": { [callflow node to branch to absolute mode is true and caller id matches +15558881111)] }
%%% }
%%% @end
%%% @contributors
%%%   Brian Davis
%%%-------------------------------------------------------------------
-module(cf_check_cid).

-export([handle/2]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CallerIdNumber = whapps_call:caller_id_number(Call),
    Regex = wh_json:get_value(<<"regex">>, Data, <<".*">>),

    lager:debug("comparing caller id ~s against regex ~s", [CallerIdNumber, Regex]),

    case re:run(CallerIdNumber, Regex) of
        {'match', _} -> handle_match(Data, Call, CallerIdNumber);
        'nomatch' -> handle_no_match(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle a caller id "match" condition
%% @end
%%--------------------------------------------------------------------
-spec handle_match(wh_json:object(), whapps_call:call(), ne_binary()) -> 'ok'.
handle_match(Data, Call, CallerIdNumber) ->
    case wh_json:is_true(<<"use_absolute_mode">>, Data, 'false') of
        'true' -> maybe_branch_on_caller_id(Data, Call, CallerIdNumber);
        'false' -> maybe_branch_on_regex(Data, Call)
    end.

-spec maybe_branch_on_caller_id(wh_json:object(), whapps_call:call(), ne_binary()) -> 'ok'.
maybe_branch_on_caller_id(Data, Call, CallerIdNumber) ->
    case is_callflow_child(CallerIdNumber, Call) of
        'true' -> update_caller_identity(Data, Call);
        'false' -> cf_exe:continue(Call)
    end.

-spec maybe_branch_on_regex(wh_json:object(), whapps_call:call()) -> 'ok'.
maybe_branch_on_regex(Data, Call) ->
    case is_callflow_child(<<"match">>, Call) of
        'true' -> update_caller_identity(Data, Call);
        'false' -> cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle a caller id "no match" condition
%% @end
%%--------------------------------------------------------------------
-spec handle_no_match(whapps_call:call()) -> 'ok'.
handle_no_match(Call) ->
    case is_callflow_child(<<"nomatch">>, Call) of
        'true' -> 'ok';
        'false' -> cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the given node name is a callflow child
%% @end
%%--------------------------------------------------------------------
-spec is_callflow_child(ne_binary(), whapps_call:call()) -> boolean().
is_callflow_child(Name, Call) ->
    lager:debug("Looking for callflow child ~s", [Name]),
    case cf_exe:attempt(Name, Call) of
        {'attempt_resp', 'ok'} ->
            lager:debug("found callflow child"),
            'true';
        {'attempt_resp', {'error', _}} ->
            lager:debug("failed to find callflow child"),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc update the caller id and owner information for this call
%% @end
%%--------------------------------------------------------------------
-spec update_caller_identity(wh_json:object(), whapps_call:call()) -> 'ok'.
update_caller_identity(Data, Call) ->
    Name = wh_json:get_ne_value([<<"caller_id">>, <<"external">>, <<"name">>], Data),
    Number = wh_json:get_ne_value([<<"caller_id">>, <<"external">>, <<"number">>], Data),
    UserId =  wh_json:get_ne_value([<<"user_id">>], Data),
    case is_valid_caller_identity(Name, Number, UserId) of
        'true' ->
            lager:info("setting caller id to ~s <~s>", [Number, Name]),
            lager:info("setting owner id to ~s", [UserId]),
            Updates = [fun(C) -> whapps_call:set_caller_id_number(Number, C) end
                       ,fun(C) -> whapps_call:set_caller_id_name(Name, C) end
                       ,fun(C) -> whapps_call:kvs_store('owner_id', UserId, C) end
                      ],
            {'ok', C} = cf_exe:get_call(Call),
            cf_exe:set_call(whapps_call:exec(Updates, C));
        'false' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc validate that all required parameters are defined
%% @end
%%--------------------------------------------------------------------
-spec is_valid_caller_identity(api_binary(), api_binary(), api_binary()) -> boolean().
is_valid_caller_identity('undefined', _Number, _UserId) -> 'false';
is_valid_caller_identity(_Name, 'undefined', _UserId) -> 'false';
is_valid_caller_identity(_Name, _Number, 'undefined') -> 'false';
is_valid_caller_identity(_Name, _Number, _UserId) -> 'true'.
