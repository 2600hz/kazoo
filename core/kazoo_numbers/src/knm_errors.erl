%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_errors).

-export([unspecified/2
        ,unauthorized/0
        ,number_exists/1
        ,invalid_state_transition/3
        ,no_change_required/1
        ,service_restriction/2
        ,carrier_not_specified/1
        ,billing_issue/2
        ,invalid/2
        ,multiple_choice/2
        ,assign_failure/2
        ,database_error/2
        ,number_is_porting/1
        ,by_carrier/3
        ]).

-export([to_json/1, to_json/2, to_json/3
        ,code/1
        ,error/1
        ,cause/1
        ,message/1
        ]).

-export([failed_to_proplist/1
        ,failed_to_json/1
        ]).

-include("knm.hrl").

-define(CODE, <<"code">>).
-define(ERROR, <<"error">>).
-define(CAUSE, <<"cause">>).
-define(MESSAGE, <<"message">>).

-type binatom_reason() :: atom() | kz_term:ne_binary().
-type error() :: kz_json:object().

%% used by knm_pipe
-type reason() :: error() | atom().
-type reasons() :: [reason()].
-type failed() :: #{kz_term:ne_binary() => reason()}.
-type proplist() :: [{kz_term:ne_binary(), reason()}].

-type thrown_error() :: {'error', atom()} |
                        {'error', atom(), any()} |
                        {'error', atom(), any(), any()}.

-export_type([error/0
             ,failed/0
             ,proplist/0
             ,reason/0, reasons/0
             ,thrown_error/0
             ]).

%%------------------------------------------------------------------------------
%% @doc Convert knm_pipe failed map to proplist.
%% @end
%%------------------------------------------------------------------------------
-spec failed_to_proplist(failed()) -> proplist().
failed_to_proplist(Failed) ->
    maps:to_list(Failed).

%%------------------------------------------------------------------------------
%% @doc Convert knm_pipe failed map to proplist.
%% @end
%%------------------------------------------------------------------------------
-spec failed_to_json(failed()) -> kz_json:object().
failed_to_json(Failed) ->
    kz_json:from_map(Failed).

-spec unspecified(any(), knm_phone_number:record() | kz_term:ne_binary()) -> no_return().
unspecified(Error, PN) ->
    throw({'error', Error, PN}).

-spec unauthorized() -> no_return().
unauthorized() ->
    throw({'error', 'unauthorized'}).

-spec number_exists(kz_term:ne_binary()) -> no_return().
number_exists(DID) ->
    throw({'error', 'number_exists', DID}).

-spec invalid_state_transition(knm_phone_number:record() | knm_phone_number:record(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> no_return().
invalid_state_transition(PN, undefined, ToState) ->
    invalid_state_transition(PN, <<"(nothing)">>, ToState);
invalid_state_transition(PN, FromState, ToState) ->
    Reason = <<"from ", FromState/binary, " to ", ToState/binary>>,
    throw({'error', 'invalid_state_transition', PN, Reason}).

-spec no_change_required(knm_phone_number:record()) -> no_return().
no_change_required(PN) ->
    throw({'error', 'no_change_required', PN}).

-spec service_restriction(knm_phone_number:record(), kz_term:ne_binary()) -> no_return().
service_restriction(PN, Message) ->
    throw({'error', 'service_restriction', PN, Message}).

-spec carrier_not_specified(knm_phone_number:record()) -> no_return().
carrier_not_specified(PN) ->
    throw({'error', 'carrier_not_specified', PN}).

-spec billing_issue(kz_term:ne_binary(), kz_json:object()) -> no_return().
billing_issue(AccountId, Reason) ->
    throw({'error', 'billing_issue', AccountId, Reason}).

-spec invalid(knm_phone_number:record(), kz_term:ne_binary()) -> no_return().
invalid(PN, Reason) ->
    throw({'error', 'invalid', PN, Reason}).

-spec multiple_choice(knm_phone_number:record(), kz_json:object()) -> no_return().
multiple_choice(PN, Update) ->
    throw({'error', 'multiple_choice', PN, Update}).

-spec assign_failure(knm_phone_number:record(), any()) -> no_return().
assign_failure(PN, E) ->
    throw({'error', 'assign_failure', PN, E}).

-spec database_error(kz_data:data_errors(), knm_phone_number:record()) -> no_return().
database_error(E, PN) ->
    throw({'error', 'database_error', PN, E}).

-spec number_is_porting(kz_term:ne_binary()) -> no_return().
number_is_porting(Num) ->
    throw({'error', 'number_is_porting', Num}).

-spec by_carrier(module(), binatom_reason(), kz_term:api_ne_binary() | knm_phone_number:record()) -> no_return().
by_carrier(Carrier, E, 'undefined') ->
    throw_by_carrier(Carrier, E, <<"unknown">>);
by_carrier(Carrier, E, <<Num/binary>>) ->
    throw_by_carrier(Carrier, E, Num);
by_carrier(Carrier, E, PN) ->
    throw_by_carrier(Carrier, E, knm_phone_number:number(PN)).

-spec throw_by_carrier(module(), binatom_reason(), kz_term:ne_binary()) -> no_return().
throw_by_carrier(Carrier, E, Num) ->
    throw({'error', 'by_carrier', Num, {Carrier, E}}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec to_json(binatom_reason()) -> error().
to_json(Reason)->
    to_json(Reason, 'undefined').

-spec to_json(binatom_reason(), kz_term:api_ne_binary()) -> error().
to_json(Reason, Num)->
    to_json(Reason, Num, 'undefined').

-spec to_json(binatom_reason(), kz_term:api_ne_binary() | kz_term:ne_binaries(), atom() | kz_term:ne_binary() | any()) -> error().
to_json('number_is_porting', Num=?NE_BINARY, _) ->
    Message = <<"number ", Num/binary, " is porting">>,
    build_error(400, 'number_is_porting', Message, Num);
to_json('number_exists', Num=?NE_BINARY, _) ->
    Message = <<"number ", Num/binary, " already exists">>,
    build_error(409, 'number_exists', Message, Num);
to_json('not_found', Num=?NE_BINARY, _) ->
    Message = <<"number ", Num/binary, " not found">>,
    build_error(404, 'not_found', Message, Num);
to_json('not_reconcilable', Num=?NE_BINARY, _) ->
    Message = <<"number ", Num/binary, " is not reconcilable">>,
    build_error(404, 'not_reconcilable', Message, Num);
to_json('unauthorized', _, Cause) ->
    Message = <<"requestor is unauthorized to perform operation">>,
    build_error(403, 'forbidden', Message, Cause);
to_json('service_restriction', Num=?NE_BINARY, Cause) ->
    build_error(402, 'service_restriction', Cause, Num);
to_json('no_change_required', _, Cause) ->
    Message = <<"no change required">>,
    build_error(400, 'no_change_required', Message, Cause);
to_json('invalid_state_transition', _, Cause) ->
    Message = <<"invalid state transition">>,
    build_error(400, 'invalid_state_transition', Message, Cause);
to_json('assign_failure', _, Cause) ->
    Message = <<"invalid account to assign to">>,
    build_error(400, 'assign_failure', Message, Cause);
to_json(Reason='invalid', _, Cause) ->
    Message = <<"invalid">>,
    build_error(400, Reason, Message, Cause);
to_json('by_carrier', Num, {_Carrier,_Cause}) ->
    lager:error("carrier ~s fault: ~p", [_Carrier, _Cause]),
    build_error(500, 'unspecified_fault', <<"fault by carrier">>, Num);
to_json('not_enough_credit', AccountId, Reason) ->
    Message = io_lib:format("account ~s does not have enough credit to perform the operation"
                           ,[AccountId]
                           ),
    build_error(402, 'not_enough_credit', kz_term:to_binary(Message), Reason);
to_json(Reason='internal_error', _, _Cause) ->
    lager:error("internal error: ~p", [_Cause]),
    build_error(500, Reason, 'internal_error', 'undefined');
to_json(Reason, _, Cause) ->
    ?LOG_ERROR("funky 500 error: ~p/~p", [Reason, Cause]),
    build_error(500, 'unspecified_fault', Reason, Cause).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec build_error(integer(), atom(), binatom_reason(), binatom_reason()) ->
          error().
build_error(Code, Error, Message, Cause) ->
    kz_json:from_list(
      [{?CODE, Code}]
      ++ [{K, kz_term:to_binary(V)}
          || {K, V} <- [{?ERROR, Error}
                       ,{?CAUSE, Cause}
                       ,{?MESSAGE, Message}
                       ],
             V =/= 'undefined'
         ]
     ).

-spec code(error()) -> kz_term:api_integer().
code(JObj) ->
    kz_json:get_value(?CODE, JObj).

-spec error(error()) -> kz_term:api_binary().
error(JObj) ->
    kz_json:get_value(?ERROR, JObj).

-spec cause(error()) -> kz_term:api_binary().
cause(JObj) ->
    kz_json:get_value(?CAUSE, JObj).

-spec message(error()) -> kz_term:api_binary().
message(JObj) ->
    kz_json:get_value(?MESSAGE, JObj).
