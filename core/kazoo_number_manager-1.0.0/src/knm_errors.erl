%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_errors).

-export([unauthorized/0
         ,number_exists/1
         ,invalid_state_transition/3
         ,no_change_required/1
         ,service_restriction/1
        ]).

-export([to_json/1, to_json/2, to_json/3]).

-include("knm.hrl").

-type reason() :: atom() | ne_binary().
-type error() :: wh_json:object().

-export_type([error/0]).

-spec unauthorized() -> no_return().
unauthorized() ->
    throw({'error', 'unauthorized'}).

-spec number_exists(ne_binary()) -> no_return().
number_exists(DID) ->
    throw({'error', 'number_exists', DID}).

-spec invalid_state_transition(knm_number:knm_number(), ne_binary(), ne_binary()) ->
                                      no_return().
invalid_state_transition(Number, FromState, ToState) ->
    throw({'error'
           ,'invalid_state_transition'
           ,Number
           ,iolist_to_binary(["from ", FromState
                              ," to ", ToState
                             ])
          }).

-spec no_change_required(knm_number:knm_number()) -> no_return().
no_change_required(Number) ->
    throw({'error', 'no_change_required', Number}).

-spec service_restriction(ne_binary()) -> no_return().
service_restriction(Message) ->
    throw({'error', 'service_restriction', Message}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_json(reason()) ->
                     error().
-spec to_json(reason(), api_binary()) ->
                     error().
-spec to_json(reason(), api_binary(), api_binary()) ->
                     error().
to_json(Reason)->
    to_json(Reason, 'undefined').

to_json(Reason, Num)->
    to_json(Reason, Num, 'undefined').

to_json('not_found', Num, _Cause) ->
    Message = <<"number ", Num/binary, " not found">>,
    build_error(404, 'not_found', Message, Num);
to_json('not_reconcilable', Num, _Cause) ->
    Message = <<"number ", Num/binary, " is not reconcilable">>,
    build_error(404, 'not_found', Message, Num);
to_json('unauthorized', Num, Cause) ->
    Message = <<"operation on ", Num/binary, " unauthorized">>,
    build_error(403, 'forbidden', Message, Cause);
to_json('no_change_required', _Num, Cause) ->
    Message = <<"no change required">>,
    build_error(400, 'no_change_required', Message, Cause);
to_json('invalid_state_transition', _Num, Cause) ->
    Message = <<"invalid state transition">>,
    build_error(400, 'invalid_state_transition', Message, Cause);
to_json(Reason, _Num, Cause) ->
    build_error(500, 'unspecified_fault', Reason, Cause).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec build_error(integer(), atom(), api_binary(), api_binary()) ->
                         error().
build_error(Code, Error, Message, Cause) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"code">>, Code}
         ,{<<"error">>, Error}
         ,{<<"cause">>, Cause}
         ,{<<"message">>, Message}
        ])
     ).
