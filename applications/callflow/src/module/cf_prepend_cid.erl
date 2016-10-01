%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% This callflow module can be used to prepend a value (or values) to the caller-id-name and caller-id-number of a call.
%%% @end
%%%
%%% @contributors
%%%   Jon Blanton
%%%   Mark Magnusson
%%%-------------------------------------------------------------------
-module(cf_prepend_cid).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    Call1 = kapps_call:exec([
                             fun maybe_set_orig_name/1
                            ,fun maybe_set_orig_number/1
                            ], Call),
    handle(kz_json:get_value(<<"action">>, Data, <<"prepend">>), Data, Call1).

-spec handle(binary(), kz_json:object(), kapps_call:call()) -> any().
handle(<<"reset">>, _Data, Call) ->
    Name   = kapps_call:kvs_fetch('original_cid_name', Call),
    Number = kapps_call:kvs_fetch('original_cid_number', Call),

    set_values(Name, Number, Call);

handle(<<"prepend">>, Data, Call) ->
    NamePre = kz_json:get_value(<<"caller_id_name_prefix">>, Data, <<"">>),
    NumberPre = kz_json:get_value(<<"caller_id_number_prefix">>, Data, <<"">>),

    {Name, Number} = case kz_json:get_value(<<"apply_to">>, Data, <<"original">>) of
                         <<"original">> -> {
                             <<NamePre/binary, (kapps_call:kvs_fetch('original_cid_name', Call))/binary>>
                                           ,<<NumberPre/binary, (kapps_call:kvs_fetch('original_cid_number', Call))/binary>>
                            };

                         <<"current">> -> {
                             <<NamePre/binary, (kapps_call:caller_id_name(Call))/binary>>
                                          ,<<NumberPre/binary, (kapps_call:caller_id_number(Call))/binary>>
                            }
                     end,

    set_values(Name, Number, Call).

-spec maybe_set_orig_name(kapps_call:call()) -> kapps_call:call().
maybe_set_orig_name(Call) ->
    case kapps_call:kvs_fetch('original_cid_name', Call) of
        'undefined' -> kapps_call:kvs_store('original_cid_name', kapps_call:caller_id_name(Call), Call);
        _Exists     -> Call
    end.

-spec maybe_set_orig_number(kapps_call:call()) -> kapps_call:call().
maybe_set_orig_number(Call) ->
    case kapps_call:kvs_fetch('original_cid_number', Call) of
        'undefined' -> kapps_call:kvs_store('original_cid_number', kapps_call:caller_id_number(Call), Call);
        _Exists     -> Call
    end.

-spec set_values(binary(), binary(), kapps_call:call()) -> any().
set_values(Name, Number, Call) ->
    Call1 = kapps_call:exec([
                             fun(C) -> kapps_call:set_caller_id_name(Name, C) end
                            ,fun(C) -> kapps_call:set_caller_id_number(Number, C) end
                            ], Call),

    cf_exe:continue(cf_exe:set_call(Call1)).
