%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_privacy).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    AccountId = kapps_call:account_id(Call),
    Number = cf_util:normalize_capture_group(kapps_call:kvs_fetch('cf_capture_group', Call), AccountId),
    case Number =/= 'undefined'
        andalso cf_flow:lookup(Number, AccountId)
    of
        'false' ->
            lager:debug("capture group is empty and can not be set as destination, hanging up."),
            cf_exe:stop_bad_destination(Call);
        {'ok', CallFlow, _} ->
            update_call(Number, Data, cf_exe:get_call(Call)),
            cf_exe:branch(kz_json:get_value(<<"flow">>, CallFlow), Call);
        {'error', _} ->
            cf_exe:stop(Call)
    end.

-spec update_call(kz_term:ne_binary(), kz_json:object(), {'ok', kapps_call:call()}) -> 'ok'.
update_call(Number, Data, {'ok', Call}) ->
    Mode = kz_json:get_ne_binary_value(<<"mode">>, Data, <<"full">>),
    Strategy = kz_json:get_ne_binary_value(<<"endpoint_strategy">>, Data, <<"overwrite">>),

    lager:debug("setting privacy mode for number ~s to ~s. use endpoint privacy: ~s"
               ,[Number, Mode, Strategy]
               ),
    Routines = [{fun kapps_call:set_request/2
                ,list_to_binary([Number, "@", kapps_call:request_realm(Call)])
                }
               ,{fun kapps_call:set_custom_channel_vars/2
                ,kz_privacy:flags_by_mode(Mode)
                }
               ,{fun kapps_call:kvs_store/3
                ,<<"use_endpoint_privacy">>
                ,should_use_endpoint_privacy(Strategy)
                }
               ],
    cf_exe:set_call(kapps_call:exec(Routines, Call)).

-spec should_use_endpoint_privacy(kz_term:ne_binary()) -> boolean().
should_use_endpoint_privacy(<<"overwrite">>) -> 'false';
should_use_endpoint_privacy(<<"merge">>) -> 'true'.
