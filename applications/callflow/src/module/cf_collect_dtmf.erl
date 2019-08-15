%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Collect DTMF into an optional key for later retrieval.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`max_digits'</dt>
%%%   <dd>Maximum digits to collect. Default is to collect one digit.</dd>
%%%
%%%   <dt>`max_digits'</dt>
%%%   <dd>How long to wait for first DTMF, in milliseconds</dd>
%%%
%%%   <dt>`terminator'</dt>
%%%   <dd>What DTMF stops collection (and aren't included). Possible values are [0-9*#]. Default is `#'.</dd>
%%%
%%%   <dt>`terminators'</dt>
%%%   <dd>What DTMFs stops collection (and aren't included). Possible values are [0-9*#]. Default is `#'.</dd>
%%%
%%%   <dt>`interdigit_timeout'</dt>
%%%   <dd>How long to wait for the next DTMF, in milliseconds</dd>
%%%
%%%   <dt>`collection_name'</dt>
%%%   <dd>The name of the collection to store collected numbers for later processing</dd>
%%% </dl>
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_collect_dtmf).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    kapps_call_command:answer(Call),

    AlreadyCollected =
        case kapps_call:get_dtmf_collection(Call) of
            'undefined' -> <<>>;
            <<_/binary>> = D -> D
        end,
    AlreadyCollected1 = truncate_after_terminator(AlreadyCollected, terminators(Data)),

    maybe_collect_more_digits(Data, kapps_call:set_dtmf_collection('undefined', Call), AlreadyCollected1).

-spec maybe_collect_more_digits(kz_json:object(), kapps_call:call(), binary()) -> 'ok'.
maybe_collect_more_digits(Data, Call, AlreadyCollected) ->
    AlreadyCollectedSize = byte_size(AlreadyCollected),
    MaxDigits = max_digits(Data),

    maybe_collect_more_digits(Data, Call, AlreadyCollected, AlreadyCollectedSize, MaxDigits),
    cf_exe:continue(Call).

-spec maybe_collect_more_digits(kz_json:object(), kapps_call:call(), binary(), non_neg_integer(), pos_integer()) -> 'ok'.
maybe_collect_more_digits(Data, Call, AlreadyCollected, ACS, Max) when ACS >= Max ->
    lager:debug("early DTMF met collection criteria, not collecting any more digits"),
    <<Head:Max/binary, _/binary>> = AlreadyCollected,
    CollectionName = collection_name(Data),

    cf_exe:set_call(kapps_call:set_dtmf_collection(Head, CollectionName, Call));
maybe_collect_more_digits(Data, Call, AlreadyCollected, ACS, Max) ->
    collect_more_digits(Data, Call, AlreadyCollected, Max-ACS).

-spec collect_more_digits(kz_json:object(), kapps_call:call(), binary(), pos_integer()) -> 'ok'.
collect_more_digits(Data, Call, AlreadyCollected, MaxDigits) ->
    case kapps_call_command:collect_digits(MaxDigits
                                          ,collect_timeout(Data)
                                          ,interdigit(Data)
                                          ,'undefined'
                                          ,terminators(Data)
                                          ,Call
                                          )
    of
        {'ok', Ds} ->
            CollectionName = collection_name(Data),
            lager:debug("collected ~s~s for ~s", [AlreadyCollected, Ds, CollectionName]),

            cf_exe:set_call(
              kapps_call:set_dtmf_collection(<<AlreadyCollected/binary, Ds/binary>>, CollectionName, Call)
             );
        {'error', _E} ->
            lager:debug("failed to collect DTMF: ~p", [_E])
    end.

-spec truncate_after_terminator(binary(), kz_term:ne_binaries()) -> binary().
truncate_after_terminator(AlreadyCollected, Terminators) ->
    hd(binary:split(AlreadyCollected, Terminators)).

-ifdef(TEST).
truncate_after_terminator_test_() ->
    [?_assertEqual(<<"1234">>, truncate_after_terminator(<<"1234#456#789">>, [<<"#">>, <<"*">>]))
    ,?_assertEqual(<<"1234">>, truncate_after_terminator(<<"1234">>, [<<"#">>]))
    ,?_assertEqual(<<"123">>, truncate_after_terminator(<<"123#">>, [<<"#">>, <<"*">>]))
    ,?_assertEqual(<<"1">>, truncate_after_terminator(<<"1*2#3">>, [<<"#">>, <<"*">>]))
    ,?_assertEqual(<<>>, truncate_after_terminator(<<"#234">>, [<<"#">>]))
    ].
-endif.

-spec collection_name(kz_json:object()) -> kz_term:ne_binary().
collection_name(Data) ->
    case kz_json:get_value(<<"collection_name">>, Data) of
        <<_/binary>> = Name -> Name;
        'undefined' -> <<"default">>
    end.

-spec max_digits(kz_json:object()) -> pos_integer().
max_digits(Data) ->
    case kz_json:get_integer_value(<<"max_digits">>, Data) of
        'undefined' -> 1;
        N when N > 0 -> N
    end.

-spec collect_timeout(kz_json:object()) -> pos_integer().
collect_timeout(Data) ->
    case kz_json:get_integer_value(<<"timeout">>, Data) of
        'undefined' -> 5000;
        N when N > 0 -> N
    end.

-spec interdigit(kz_json:object()) -> pos_integer().
interdigit(Data) ->
    case kz_json:get_integer_value(<<"interdigit_timeout">>, Data) of
        'undefined' -> kapps_call_command:default_interdigit_timeout();
        N when N > 0 -> N
    end.

-spec terminators(kz_json:object()) -> kz_term:ne_binaries().
terminators(Data) ->
    case kz_json:get_first_defined([<<"terminator">>, <<"terminators">>], Data) of
        'undefined' -> [<<"#">>];
        <<_/binary>> = T ->
            'true' = lists:member(T, ?ANY_DIGIT),
            [T];
        [_|_] = Ts ->
            'true' = lists:all(fun(T) -> lists:member(T, ?ANY_DIGIT) end, Ts),
            lists:usort(Ts)
    end.
