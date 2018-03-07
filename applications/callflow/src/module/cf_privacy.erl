%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
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
            Mode = kz_json:get_ne_binary_value(<<"mode">>, Data, <<"full">>),
            kapps_call_command:privacy(Mode, Call),
            update_call(CaptureGroup
                       ,cf_exe:get_call(Call)
                       ,Mode
                       ,should_use_endpoint_privacy(Data)
                       ),
            cf_exe:branch(kz_json:get_value(<<"flow">>, CallFlow), Call);
        {'error', _} ->
            cf_exe:stop(Call)
    end.

-spec update_call(kz_term:ne_binary(), {'ok', kapps_call:call()}, kz_term:ne_binary(), boolean()) -> 'ok'.
update_call(CaptureGroup, {'ok', Call}, Mode, Overwrite) ->
    Normalize = knm_converters:normalize(CaptureGroup),
    CCVs = ccvs_by_privacy_mode(Mode),
    Routines = [{fun kapps_call:set_request/2
                ,list_to_binary([Normalize, "@", kapps_call:request_realm(Call)])
                }
               ,{fun kapps_call:set_custom_channel_vars/2, CCVs}
               ,{fun kapps_call:kvs_store/3, <<"use_endpoint_privacy">>, Overwrite}
               ],
    cf_exe:set_call(kapps_call:exec(Routines, Call)).

-spec ccvs_by_privacy_mode(kz_term:api_ne_binary()) -> kz_term:proplist().
ccvs_by_privacy_mode(Mode) ->
    cf_util:ccvs_by_privacy_mode(Mode).

-spec should_use_endpoint_privacy(kz_json:object()) -> boolean().
should_use_endpoint_privacy(Data) ->
    case kz_json:get_ne_binary_value(<<"endpoint_strategy">>, Data, <<"overwrite">>) of
        <<"overwrite">> -> 'false';
        <<"merge">> -> 'true'
    end.
