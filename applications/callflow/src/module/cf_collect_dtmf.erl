%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%% Collect DTMF into an optional key for later retrieval
%%% "data":{
%%%   "max_digits":4
%%%   ,"timeout":5000 // milliseconds, how long to wait for first DTMF
%%%   ,"terminator":"#" // what DTMF stops collection (and aren't included)
%%%   ,"terminators":["#","*"] // what DTMFs stop collection (and aren't included)
%%%   ,"interdigit_timeout":2000 // milliseconds, how long to wait for the next DTMF
%%%   ,"collection_name":"your_name_here" // name the collection for later processing
%%% }
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_collect_dtmf).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),

    AlreadyCollected =
        case whapps_call:get_dtmf_collection(Call) of
            'undefined' -> <<>>;
            <<_/binary>> = D -> D
        end,

    maybe_collect_more_digits(Data, whapps_call:set_dtmf_collection('undefined', Call), AlreadyCollected).

-spec maybe_collect_more_digits(wh_json:object(), whapps_call:call(), binary()) -> 'ok'.
maybe_collect_more_digits(Data, Call, AlreadyCollected) ->
    AlreadyCollectedSize = byte_size(AlreadyCollected),
    MaxDigits = max_digits(Data),

    maybe_collect_more_digits(Data, Call, AlreadyCollected, AlreadyCollectedSize, MaxDigits),
    cf_exe:continue(Call).

-spec maybe_collect_more_digits(wh_json:object(), whapps_call:call(), binary(), non_neg_integer(), pos_integer()) -> 'ok'.
maybe_collect_more_digits(Data, Call, AlreadyCollected, ACS, Max) when ACS >= Max ->
    lager:debug("early DTMF met collection criteria, not collecting any more digits"),
    <<Head:Max/binary, _/binary>> = AlreadyCollected,
    CollectionName = collection_name(Data),

    cf_exe:set_call(whapps_call:set_dtmf_collection(Head, CollectionName, Call));
maybe_collect_more_digits(Data, Call, AlreadyCollected, ACS, Max) ->
    collect_more_digits(Data, Call, AlreadyCollected, Max-ACS).

-spec collect_more_digits(wh_json:object(), whapps_call:call(), binary(), pos_integer()) -> 'ok'.
collect_more_digits(Data, Call, AlreadyCollected, MaxDigits) ->
    case whapps_call_command:collect_digits(MaxDigits
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
              whapps_call:set_dtmf_collection(<<AlreadyCollected/binary, Ds/binary>>, CollectionName, Call)
             );
        {'error', _E} ->
            lager:debug("failed to collect DTMF: ~p", [_E])
    end.

-spec collection_name(wh_json:object()) -> ne_binary().
collection_name(Data) ->
    case wh_json:get_value(<<"collection_name">>, Data) of
        <<_/binary>> = Name -> Name;
        'undefined' -> <<"default">>
    end.

-spec max_digits(wh_json:object()) -> pos_integer().
max_digits(Data) ->
    case wh_json:get_integer_value(<<"max_digits">>, Data) of
        'undefined' -> 1;
        N when N > 0 -> N
    end.

-spec collect_timeout(wh_json:object()) -> pos_integer().
collect_timeout(Data) ->
    case wh_json:get_integer_value(<<"timeout">>, Data) of
        'undefined' -> 5000;
        N when N > 0 -> N
    end.

-spec interdigit(wh_json:object()) -> pos_integer().
interdigit(Data) ->
    case wh_json:get_integer_value(<<"interdigit_timeout">>, Data) of
        'undefined' -> whapps_call_command:default_interdigit_timeout();
        N when N > 0 -> N
    end.

-spec terminators(wh_json:object()) -> ne_binaries().
terminators(Data) ->
    case wh_json:get_first_defined([<<"terminator">>, <<"terminators">>], Data) of
        'undefined' -> [<<"#">>];
        <<_/binary>> = T ->
            'true' = lists:member(T, ?ANY_DIGIT),
            [T];
        [_|_] = Ts ->
            'true' = lists:all(fun(T) -> lists:member(T, ?ANY_DIGIT) end, Ts),
            lists:usort(Ts)
    end.
