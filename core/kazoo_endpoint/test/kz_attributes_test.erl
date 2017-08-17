%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_attributes_test).

-include_lib("eunit/include/eunit.hrl").

process_dynamic_flags_test_() ->
    Call = create_kapps_call(),
    [{"verify that dynamic CCVs can be fetched and are converted to binary"
     ,?_assertEqual([<<"true">>], kz_attributes:process_dynamic_flags([<<"custom_channel_vars.ccv_dynamic_flag">>], Call))
     }
    ,{"verify that exported kapps_call functions can be used"
     ,?_assertEqual([<<"nouser">>], kz_attributes:process_dynamic_flags([<<"to_user">>], Call))
     }
    ,{"verify that non-exported kapps_call functions dont crash"
     ,?_assertEqual([], kz_attributes:process_dynamic_flags([<<"not_exported">>], Call))
     }
    ,{"verify that the zone name can be resolved"
     ,?_assertEqual([<<"local">>], kz_attributes:process_dynamic_flags([<<"zone">>], Call))
     }
    ].

create_kapps_call() ->
    Routines = [{fun kapps_call:insert_custom_channel_var/3, <<"CCV-Dynamic-Flag">>, 'true'}],
    kapps_call:exec(Routines, kapps_call:new()). 
