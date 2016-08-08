%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cf_privacy).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    AccountId = kapps_call:account_id(Call),
    case cf_flow:lookup(CaptureGroup, AccountId) of
        {'ok', CallFlow, _} ->
            Mode = kz_json:get_value(<<"mode">>, Data),
            kapps_call_command:privacy(Mode, Call),
            update_call(CaptureGroup
                       ,cf_exe:get_call(Call)
                       ),
            cf_exe:branch(kz_json:get_value(<<"flow">>, CallFlow), Call);
        {'error', _} ->
            cf_exe:stop(Call)
    end.

-spec update_call(ne_binary(), {'ok', kapps_call:call()}) -> 'ok'.
update_call(CaptureGroup, {'ok', Call}) ->
    Routines = [{fun kapps_call:set_request/2
                ,<<CaptureGroup/binary, "@"
                   ,(kapps_call:request_realm(Call))/binary
                 >>
                }
               ,{fun kapps_call:kvs_store/3, ?MODULE, 'true'}
               ,{fun kapps_call:set_caller_id_name/2
                ,kapps_config:get_non_empty(?CF_CONFIG_CAT
                                           ,<<"privacy_name">>
                                           ,kz_util:anonymous_caller_id_name()
                                           )
                }
               ,{fun kapps_call:set_caller_id_number/2
                ,kapps_config:get_non_empty(?CF_CONFIG_CAT
                                           ,<<"privacy_number">>
                                           ,kz_util:anonymous_caller_id_number()
                                           )
                }
               ],
    cf_exe:set_call(kapps_call:exec(Routines, Call)).
