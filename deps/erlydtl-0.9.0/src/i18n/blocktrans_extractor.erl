-module(blocktrans_extractor).

-export([extract/1]).

extract(Path) when is_list(Path) ->
    {ok, Contents} = file:read_file(Path),
    extract(Contents);

extract(Contents) when is_binary(Contents) ->
    case erlydtl_compiler:parse(Contents) of
        {ok, ParseTree} ->
            Blocks = process_tree(ParseTree),
            {ok, Blocks};
        Error ->
            Error
    end.

process_tree(ParseTree) ->
    process_tree(ParseTree, []).

process_tree([], Acc) ->
    lists:reverse(Acc);
process_tree([{'autoescape', _, Contents}|Rest], Acc) ->
    process_tree(Rest, lists:reverse(process_tree(Contents), Acc));
process_tree([{'block', _, Contents}|Rest], Acc) ->
    process_tree(Rest, lists:reverse(process_tree(Contents), Acc));
process_tree([{'blocktrans', _, Contents}|Rest], Acc) ->
    process_tree(Rest, [lists:flatten(erlydtl_unparser:unparse(Contents))|Acc]); % <-- where all the action happens
process_tree([{'filter', _, Contents}|Rest], Acc) ->
    process_tree(Rest, lists:reverse(process_tree(Contents), Acc));
process_tree([{'for', _, Contents}|Rest], Acc) ->
    process_tree(Rest, lists:reverse(process_tree(Contents), Acc));
process_tree([{'for', _, Contents, EmptyPartContents}|Rest], Acc) ->
    process_tree(Rest, lists:reverse(process_tree(Contents) ++ process_tree(EmptyPartContents), Acc));
process_tree([{Instruction, _, Contents}|Rest], Acc) when Instruction =:= 'if'; 
                                                          Instruction =:= 'ifequal'; 
                                                          Instruction =:= 'ifnotequal' ->
    process_tree(Rest, lists:reverse(process_tree(Contents), Acc));
process_tree([{Instruction, _, IfContents, ElseContents}|Rest], Acc) when Instruction =:= 'ifelese'; 
                                                                          Instruction =:= 'ifequalelse'; 
                                                                          Instruction =:= 'ifnotequalelse' ->
    process_tree(Rest, lists:reverse(process_tree(IfContents) ++ process_tree(ElseContents), Acc));
process_tree([{'spaceless', Contents}|Rest], Acc) ->
    process_tree(Rest, lists:reverse(process_tree(Contents), Acc));
process_tree([{'with', _, Contents}|Rest], Acc) ->
    process_tree(Rest, lists:reverse(process_tree(Contents), Acc));
process_tree([_|Rest], Acc) ->
    process_tree(Rest, Acc).
