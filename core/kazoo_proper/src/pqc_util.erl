-module(pqc_util).

-export([transition_if/2]).

-include("kazoo_proper.hrl").

-spec transition_if(pqc_kazoo_model:model(), [{fun(), list()}]) -> pqc_kazoo_model:model().
transition_if(CurrentModel, Checks) ->
    case lists:foldl(fun transition_if_fold/2, {'true', CurrentModel}, Checks) of
        {'true', UpdatedModel} -> UpdatedModel;
        {'false', _} -> CurrentModel
    end.

-spec transition_if_fold({fun(), list()}, {boolean(), pqc_kazoo_model:model()}) ->
                                {boolean(), pqc_kazoo_model:model()}.
transition_if_fold({_Fun, _Args}, {'false', _}=False) -> False;
transition_if_fold({Fun, Args}, {'true', Model}) ->
    case apply(Fun, [Model | Args]) of
        'false' -> {'false', Model};
        'true' -> {'true', Model};
        {'true', _NewState}=True -> True;
        NewModel -> {'true', NewModel}
    end.
