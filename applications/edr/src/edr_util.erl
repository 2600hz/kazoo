%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Utils for backends
%%% @author SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%% @author Conversant Ltd (Max Lay)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_util).

-include("edr.hrl").

-export([backend_from_json/1
        ,formatter/2
        ,formatter_options/1
        ]).

-spec formatter(kz_json:object(), module()) -> module().
formatter(Options, Default) ->
    case kz_json:get_value([<<"formatter">>, <<"type">>], Options) of
        'undefined' -> Default;
        Type -> kz_term:to_atom("edr_fmt_" ++ binary_to_list(Type))
    end.

-spec formatter_options(kz_json:object()) -> kz_json:object().
formatter_options(Options)->
    kz_json:get_value([<<"formatter">>, <<"options">>], Options, kz_json:new()).

-spec backend_from_json(kz_json:object()) -> backend().
backend_from_json(JObj) ->
    #backend{name=kz_json:get_binary_value(<<"name">>, JObj)
            ,type=kz_json:get_ne_binary_value(<<"type">>, JObj)
            ,enabled=kz_json:is_true(<<"enabled">>, JObj, 'false')
            ,options=kz_json:get_json_value(<<"options">>, JObj, {})
            ,bindings=edr_bindings:bindings_from_json(kz_json:get_list_value(<<"bindings">>, JObj, []))
            }.
