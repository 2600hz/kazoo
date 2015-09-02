%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_errors).

-export([to_json/1, to_json/2, to_json/3]).

-include("knm.hrl").

-type reason() :: atom() | ne_binary().
-type num() :: api_binary() | knm_phone_number:knm_number().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_json(reason()) -> wh_json:object().
-spec to_json(reason(), num()) ->
                     wh_json:object().
to_json(Reason)->
    to_json(Reason, 'undefined').

to_json(Reason, Num)->
    to_json(Reason, Num, 'undefined').

-spec to_json(reason(), num(), api_binary()) ->
                     wh_json:object().
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
-spec build_error(integer(), atom(), ne_binary(), api_binary()) -> wh_json:object().
build_error(Code, Error, Message, Cause) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"code">>, Code}
         ,{<<"error">>, to_binary(Error)}
         ,{<<"cause">>, to_binary(Cause)}
         ,{<<"message">>, to_binary(Message)}
        ])
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_binary(any()) -> binary().
to_binary('undefined') -> 'undefined';
to_binary(Any) -> wh_util:to_binary(Any).
