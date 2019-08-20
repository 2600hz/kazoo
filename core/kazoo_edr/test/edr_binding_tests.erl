%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc
%%% @author Conversant Ltd (Max Lay)
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_binding_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("edr.hrl").

binding_keys_test() ->
    ?assertEqual([<<"edr.warning.error.abc.app">>
                 ,<<"edr.warning.fatal.abc.app">>
                 ,<<"edr.critical.error.abc.app">>
                 ,<<"edr.critical.fatal.abc.app">>
                 ]
                ,edr_bindings:binding_keys(#edr_binding{severity='warning'
                                                       ,verbosity='error'
                                                       ,account_id= <<"abc">>
                                                       ,app_name= <<"app">>
                                                       })),
    ?assertEqual([<<"edr.critical.fatal.abc.*">>]
                ,edr_bindings:binding_keys(#edr_binding{account_id= <<"abc">>
                                                       ,severity='critical'
                                                       ,verbosity='fatal'
                                                       })),
    ?assertEqual([<<"edr.warning.fatal.abc.*">>
                 ,<<"edr.warning.fatal.def.*">>
                 ,<<"edr.critical.fatal.abc.*">>
                 ,<<"edr.critical.fatal.def.*">>
                 ]
                ,edr_bindings:binding_keys(#edr_binding{account_id=[<<"abc">>, <<"def">>]
                                                       ,severity='warning'
                                                       ,verbosity='fatal'
                                                       })).
binding_to_from_json_test() ->
    Binding = #edr_binding{},
    ?assertEqual(Binding, edr_bindings:bindings_from_json(edr_bindings:bindings_to_json(Binding))),
    ?assertEqual([Binding], edr_bindings:bindings_from_json(edr_bindings:bindings_to_json([Binding]))).
