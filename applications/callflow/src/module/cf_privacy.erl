%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
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
            Mode = kz_json:get_ne_binary_value(<<"mode">>, Data, <<"full">>),
            kapps_call_command:privacy(Mode, Call),
            update_call(CaptureGroup
                       ,cf_exe:get_call(Call)
                       ,Mode
                       ),
            cf_exe:branch(kz_json:get_value(<<"flow">>, CallFlow), Call);
        {'error', _} ->
            cf_exe:stop(Call)
    end.

-spec update_call(ne_binary(), {'ok', kapps_call:call()}, ne_binary()) -> 'ok'.
update_call(CaptureGroup, {'ok', Call}, Mode) ->
    Normalize = knm_converters:normalize(CaptureGroup),
    CCVs = ccvs_by_privacy_mode(Mode),
    Routines = [{fun kapps_call:set_request/2
                ,<<Normalize/binary, "@"
                   ,(kapps_call:request_realm(Call))/binary
                 >>
                }
               ,{fun kapps_call:set_custom_channel_vars/2, CCVs}
               ],
    cf_exe:set_call(kapps_call:exec(Routines, Call)).

-spec ccvs_by_privacy_mode(api_ne_binary()) -> kz_proplist().
ccvs_by_privacy_mode('undefined') ->
    ccvs_by_privacy_mode(<<"full">>);
ccvs_by_privacy_mode(<<"full">>) ->
    [{<<"Caller-Screen-Bit">>, 'true'}
    ,{<<"Caller-Privacy-Hide-Number">>, 'true'}
    ,{<<"Caller-Privacy-Hide-Name">>, 'true'}
    ];
ccvs_by_privacy_mode(<<"yes">>) ->
    ccvs_by_privacy_mode(<<"full">>);
ccvs_by_privacy_mode(<<"name">>) ->
    [{<<"Caller-Screen-Bit">>, 'true'}
    ,{<<"Caller-Privacy-Hide-Name">>, 'true'}
    ];
ccvs_by_privacy_mode(<<"number">>) ->
    [{<<"Caller-Screen-Bit">>, 'true'}
    ,{<<"Caller-Privacy-Hide-Number">>, 'true'}
    ];
ccvs_by_privacy_mode(_Else) ->
    lager:debug("unsupported privacy mode ~s, forcing full privacy", [_Else]),
    ccvs_by_privacy_mode(<<"full">>).
