%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014 2600Hz INC
%%% @doc
%%% Dialplan API commands
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_fax).

-export([receive_fax/3]).

receive_fax(Node, UUID, JObj) ->
    Sets = props:filter_undefined(
             lists:foldl(fun(Header, Acc) ->
                                 case wh_json:get_value(Header, JObj) of
                                     'undefined' -> Acc;
                                     Value -> [header_to_fs_var(Header, Value) | Acc]
                                 end
                         end
                        ,[]
                        ,[<<"Enable-T38-Fax">>
                          ,<<"Enable-T38-Fax-Request">>
                          ,<<"Enable-T38-Passthrough">>
                          ,<<"Enable-T38-Gateway">>
                         ])),
    ecallmgr_util:set(Node, UUID, Sets),
    
    [{<<"playback">>, <<"silence_stream://2000">>}
    ,{<<"rxfax">>, ecallmgr_util:fax_filename(UUID)}
    ].

header_to_fs_var(<<"Enable-T38-Fax">>, Value ) ->
    case wh_util:is_true(Value) of
        'true' -> {<<"fax_enable_t38">>, <<"true">> };
        'false' -> {<<"fax_enable_t38">>, 'undefined'}
    end;
header_to_fs_var(<<"Enable-T38-Fax-Request">>, Value ) ->
    case wh_util:is_true(Value) of
        'true' -> {<<"fax_enable_t38_request">>, <<"true">> };
        'false' -> {<<"fax_enable_t38_request">>, 'undefined'}
    end;
header_to_fs_var(<<"Enable-T38-Passthrough">>, Value ) ->
    case wh_util:is_true(Value) of
        'true' -> {<<"t38_passthru">>, <<"true">> };
        'false' -> {<<"t38_passthru">>, 'undefined'}
    end;
header_to_fs_var(<<"Enable-T38-Gateway">>, Direction) ->    
    {<<"execute_on_answer">>, <<"t38_gateway ", Direction/binary>>}.
