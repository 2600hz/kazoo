%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, SIPLABS, LLC
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2016
%%%-------------------------------------------------------------------
-module(cf_edr).

-include("../callflow.hrl").

-export([handle/2]).

-define(SPECIAL_VARS, [<<"Caller-ID-Name">>
                      ,<<"Caller-ID-Number">>
                      ,<<"Account-ID">>
                      ,<<"Owner-ID">>
                      ,<<"Fetch-ID">>
                      ,<<"Bridge-ID">>
                      ,<<"Authorizing-ID">>
                      ,<<"Authorizing-Type">>
                      ,<<"Bridge-ID">>
                      ,<<"Other-Leg-Call-ID">>
                      ]).
%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CallProps = kapps_call:to_proplist(Call),
    Tags = [{Key, _Val} || {Key, _Val} <- CallProps, lists:member(Key, ?SPECIAL_VARS)],
    AdditionalTags = [{<<"Custom-Data">>, Data}
                     ,{<<"Callflow-ID">>, props:get_value([<<"Key-Value-Store">>, <<"cf_flow_id">>], CallProps)}
                     ],
    JTags = kz_json:from_list(props:filter_undefined(Tags ++ AdditionalTags)),
    kz_edr:event(?APP_NAME, ?APP_VERSION, 'ok', 'info', JTags, kapps_call:account_id(Call)),
    cf_exe:continue(Call).
