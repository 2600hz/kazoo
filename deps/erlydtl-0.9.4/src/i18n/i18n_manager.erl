%% Author: dave
%% Created: Feb 26, 2010
%% Description: TODO: Add description to dets_generator
-module(i18n_manager).

%%
%% Include files
%%

%% Exported Functions
%%
-export([generate_pos/1]).
-define(EPOT_TABLE,epos).
-define(EPOT_TABLE_FUZZY,epos_fuzzy).

%%
%% API Functions
%%

generate_pos([Lang,Files])->
    io:format("~s -> ~s ~n",[Lang,Files]),
    SplittedLocales = string:tokens(Lang,","),
    SplittedFiles = string:tokens(Files, ","),
    ProcessedFiles = sources_parser:parse(SplittedFiles),
    io:format("Parsed tokens are ~p~n",[ProcessedFiles]),
    BaseDir = "lang/default/",

    PopulateTable = fun(Language) ->
                            io:format("-------------------------Generating po file for ~s-------------------------~n",[Language]),
                            ok = open_table(Language),
                            put(locale, Language),
                            insert_tokens(ProcessedFiles),

                            %%Recover already present translations
                            TranslationsForLanguage = po_scanner:scan(BaseDir ++ Language ++ "/gettext.po"),
                            io:format("Updating translations~n"),
                            insert_translations(TranslationsForLanguage),
                            Data = dets_data(),
                            io:format("Generating po file ~n"),
                            Fuzzy = dets_fuzzy(),
                            po_generator:generate_file(Language, Data, Fuzzy),
                            io:format("Closing files ~n"),
                            ok = close_tables(Language),
                            io:format("All files closed ~n")
                    end,

    lists:foreach(PopulateTable, SplittedLocales),
    init:stop().


%%
%% Local Functions
%%

%% Open a temporal table for a given locale
open_table(Locale)->
    Dir = "./lang/tmp/" ++ Locale,
    io:format("Creating dir ~s~n",[Dir]),
    ok = file:del_dir(Dir),
    ok = file:make_dir(Dir),
    OpenTable = fun({TableName, TableFile}, ok) ->
                        File = Dir ++ TableFile,
                        case dets:open_file(TableName, [{file, File}]) of
                            {ok,Ref} ->  io:format("Opened DETS ~p ~p~n",[TableName,Ref]);
                            _Error -> io:format("Error opening DETS~p~n",[_Error])
                        end
                end,

    lists:foldl(OpenTable, ok, [{?EPOT_TABLE,"/epot.dets"},{?EPOT_TABLE_FUZZY,"/epot_fuzzy.dets"}]).

%%TODO better way to do cleanup
close_tables(Locale) ->
    %%dets:delete_all_objects(?EPOT_TABLE),
    ok = dets:close(?EPOT_TABLE),
    ok = dets:close(?EPOT_TABLE_FUZZY),
    ok = file:delete("./lang/tmp/" ++ Locale ++ "/epot.dets"),
    file:delete("./lang/tmp/" ++ Locale ++ "/epot_fuzzy.dets").

%%Get all data from dets table
dets_data() -> dets:foldl(fun(E, Acc) -> [E|Acc] end, [], ?EPOT_TABLE).
dets_fuzzy() -> dets:foldl(fun(E, Acc) -> [E|Acc] end, [], ?EPOT_TABLE_FUZZY).

insert_tokens([]) -> noop;
insert_tokens([{Id,{Fname,Line,_Col}}|Tail]) ->
    ok = insert_token(Id, Id, Fname, Line),
    insert_tokens(Tail).

insert_token(Id, Translation,Fname,Line)->
    FileInfo = get_file_info(Id), %%File info are all files where this string is present
    AllFileReferences = lists:sort( [{Fname,Line} | FileInfo] ),
    dets:insert(?EPOT_TABLE, {Id, Translation,AllFileReferences}).

insert_translations([]) -> noop;
insert_translations(L = [H|T]) ->
    %%io:format("Remaining ~p~n",[L]),
    case H of
        {comment, _} ->
            %%Comments are skipped
            insert_translations(T);
        _Other ->
            [{id,Id}, {str,Str}|Tail] = L,
            ok = insert_translation(Id,Str),
            insert_translations(Tail)
    end.

insert_translation(Id, Translation) ->
    io:format("Updating translation for ~p to ~p ~n",[Id,Translation]),
    case Id of
        [] ->
            noop;
        Id ->
            case dets:lookup(?EPOT_TABLE,Id) of
                [] ->
                    %%Fuzzy translation!
                    dets:insert(?EPOT_TABLE_FUZZY, {Id, Translation,fuzzy});
                [{Id, _StoredTranslation,FileInfo}] ->
                    %%TODO check for translation unicity
                    io:format("Recovered translation for ~p ~p ~n",[Id,_StoredTranslation]),
                    dets:insert(?EPOT_TABLE, {Id, Translation,FileInfo})
            end
    end.

get_file_info(Key) ->
    case dets:lookup(?EPOT_TABLE, Key) of
        []            -> [];
        [{_,_,Finfo}|_] -> Finfo
    end.
