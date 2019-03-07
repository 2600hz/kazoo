%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
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

-include("knm.hrl").

-define(CODE, <<"code">>).
-define(ERROR, <<"error">>).
-define(CAUSE, <<"cause">>).
-define(MESSAGE, <<"message">>).

-type reason() :: atom() | kz_term:ne_binary().
-type error() :: kz_json:object().

-type kn() :: knm_number:knm_number().
-type kpn() :: knm_phone_number:knm_phone_number().

-type thrown_error() :: {'error', atom()} |
                        {'error', atom(), any()} |
                        {'error', atom(), any(), any()}.

-export_type([error/0
             ,thrown_error/0
             ]).

-spec unspecified(any(), kn() | kz_term:ne_binary()) -> no_return().
unspecified(Error, Number) ->
    throw({'error', Error, Number}).

-spec unauthorized() -> no_return().
unauthorized() ->
    throw({'error', 'unauthorized'}).

-spec number_exists(kz_term:ne_binary()) -> no_return().
number_exists(DID) ->
    throw({'error', 'number_exists', DID}).

-spec invalid_state_transition(kn() | kpn(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> no_return().
invalid_state_transition(Number, undefined, ToState) ->
    invalid_state_transition(Number, <<"(nothing)">>, ToState);
invalid_state_transition(Number, FromState, ToState) ->
    Reason = <<"from ", FromState/binary, " to ", ToState/binary>>,
    throw({'error', 'invalid_state_transition', Number, Reason}).

-spec no_change_required(kn()) -> no_return().
no_change_required(Number) ->
    throw({'error', 'no_change_required', Number}).

-spec service_restriction(kn(), kz_term:ne_binary()) -> no_return().
service_restriction(Number, Message) ->
    throw({'error', 'service_restriction', Number, Message}).

-spec carrier_not_specified(kn()) -> no_return().
carrier_not_specified(Number) ->
    throw({'error', 'carrier_not_specified', Number}).

-spec billing_issue(kz_term:ne_binary(), kz_json:object()) -> no_return().
billing_issue(AccountId, Reason) ->
    throw({'error', 'billing_issue', AccountId, Reason}).

-spec invalid(kn(), kz_term:ne_binary()) -> no_return().
invalid(Number, Reason) ->
    throw({'error', 'invalid', Number, Reason}).

-spec multiple_choice(kn(), kz_json:object()) -> no_return().
multiple_choice(Number, Update) ->
    throw({'error', 'multiple_choice', Number, Update}).

-spec assign_failure(knm_phone_number:knm_phone_number(), any()) -> no_return().
assign_failure(PhoneNumber, E) ->
    throw({'error', 'assign_failure', PhoneNumber, E}).

-spec database_error(kz_data:data_errors(), knm_phone_number:knm_phone_number()) -> no_return().
database_error(E, PhoneNumber) ->
    throw({'error', 'database_error', PhoneNumber, E}).

-spec number_is_porting(kz_term:ne_binary()) -> no_return().
number_is_porting(Num) ->
    throw({'error', 'number_is_porting', Num}).

-spec by_carrier(module(), kz_term:ne_binary() | atom(), kz_term:ne_binary() | kn()) -> no_return().
by_carrier(Carrier, E, Num) when is_binary(Num) ->
    throw({'error', 'by_carrier', Num, {Carrier,E}});
by_carrier(Carrier, E, Number) ->
    Num = knm_phone_number:number(knm_number:phone_number(Number)),
    by_carrier(Carrier, E, Num).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec to_json(reason()) -> error().
to_json(Reason)->
    to_json(Reason, 'undefined').

-spec to_json(reason(), kz_term:api_ne_binary()) -> error().
to_json(Reason, Num)->
    to_json(Reason, Num, 'undefined').

-spec to_json(reason(), kz_term:api_ne_binary() | kz_term:ne_binaries(), atom() | kz_term:ne_binary() | any()) -> error().
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
-spec build_error(integer(), atom(), kz_term:api_binary(), atom() | kz_term:ne_binary()) ->
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
