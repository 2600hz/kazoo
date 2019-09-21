%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc Ensure a "doc/maintenance.md" exists in each application
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(maintenance_docs).

-export([process/0, process/1
        ,to_docs/0, to_doc/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo_ast/src/kz_ast.hrl").

-type acc() :: {module() | 'undefined', #{}}.

-define(CONFIG, [{'module', fun is_sup_module/2}
                ,{'after_module', fun print_dot/2}
                ,{'export', fun handle_export/3}
                ,{'clause', fun handle_clause/4}
                ,{'accumulator', {'undefined', #{}}} %% {current module, all exports}
                ]).

-spec process() -> map().
process() ->
    {_, Acc} = kazoo_ast:walk_project(?CONFIG),
    io:format(" done.~n"),
    Acc.

-spec process(module()) -> acc().
process(Module) ->
    kazoo_ast:walk_modules([Module], ?CONFIG).

-spec to_docs() -> 'ok'.
to_docs() ->
    io:format("processing maintenance modules: "),
    to_docs(maps:to_list(process())),
    io:format("~n").

-spec to_doc(module()) -> 'ok'.
to_doc(Module) ->
    {Module, Acc} = process(Module),
    to_docs(maps:to_list(Acc)).

to_docs(Modules) ->
    [to_module_doc(Module, ExportedFuns) || {Module, ExportedFuns} <- Modules],
    'ok'.

to_module_doc(Module, ExportedFuns) ->
    RefDoc = module_to_ref_doc_path(Module),
    Content = exported_funs_to_doc(maps:to_list(ExportedFuns)),
    'ok' = file:write_file(RefDoc, Content).

-spec exported_funs_to_doc([{{atom(), arity()}, list()}]) -> iodata().
exported_funs_to_doc(ExportedFuns) ->
    [doc_header()
    ,lists:foldr(fun exported_fun_to_doc/2, [], lists:keysort(1, ExportedFuns))
    ].

doc_header() ->
    "## SUP-able functions\n\n"
        "| Function | Arguments | Description |\n"
        "| -------- | --------- | ----------- |\n"
        .

exported_fun_to_doc({{Function, Arity}, Args}, Acc) ->
    [["| `", kz_term:to_list(Function), "/", kz_term:to_list(Arity), "` "
      "| ", args_to_cell(Args), " "
      "| |\n"
     ]
     | Acc
    ].

args_to_cell([]) -> <<>>;
args_to_cell(Args) ->
    Cell = lists:foldr(fun arg_to_cell/2, [], lists:usort(Args)),
    ["`", kz_binary:join(Cell, <<" | ">>), "`"].

arg_to_cell([], Acc) -> [<<>> | Acc];
arg_to_cell(ArgList, Acc) ->
    [["(", kz_binary:join(ArgList, <<",">>), ")"] | Acc].

-spec module_to_ref_doc_path(module()) -> filename:filename_all().
module_to_ref_doc_path(Module) ->
    {'ok', App} = application:get_application(Module),
    AppDir = code:lib_dir(App),

    DocName = module_to_doc_name(App, Module),
    Path = filename:join([AppDir, "doc", "ref", <<DocName/binary, ".md">>]),
    'ok' = filelib:ensure_dir(Path),
    Path.

module_to_doc_name(<<App/binary>>, <<Module/binary>>) ->
    AppBytes = byte_size(App),
    case Module of
        <<App:AppBytes/binary, "_maintenance">> -> <<"maintenance">>;
        Module -> Module
    end;
module_to_doc_name(App, Module) ->
    module_to_doc_name(kz_term:to_binary(App), kz_term:to_binary(Module)).

-spec print_dot(module(), acc()) -> acc().
print_dot(M, Acc) ->
    case is_maintenance_module(M) of
        'false' -> io:format(".");
        'true' -> io:format("s")
    end,
    Acc.

-spec handle_export(atom(), arity(), {module(), map()}) -> {module(), map()}.
handle_export(Fun, Arity, {Module, Acc}) ->
    ModuleExports = maps:get(Module, Acc, #{}),
    MExps = maps:put({Fun, Arity}, [], ModuleExports),
    {Module, maps:put(Module, MExps, Acc)}.

-spec handle_clause(atom(), list(), list(), {module(), map()}) -> {module(), map()}.
handle_clause(_Function, [], _Guards, {Module, Acc}) -> {Module, Acc};
handle_clause(Function, Args, _Guards, {Module, Acc}) ->
    Arity = length(Args),
    ModuleExports = maps:get(Module, Acc),

    case maps:get({Function, Arity}, ModuleExports, 'undefined') of
        'undefined' -> {Module, Acc};
        FunVars ->
            Vars = args_to_vars(Args),
            MExps = maps:put({Function, Arity}, [Vars | FunVars], ModuleExports),
            {Module, maps:put(Module, MExps, Acc)}
    end.

args_to_vars(Args) ->
    lists:map(fun arg_to_var/1, Args).

arg_to_var(?VAR(Name)) -> kz_term:to_binary(Name);
arg_to_var(?ATOM(A)) ->   A;
arg_to_var(?EMPTY_LIST) -> <<"[]">>;
arg_to_var(?BINARY_MATCH([?BINARY_VAR(Name)])) ->
    kz_term:to_binary(Name);
arg_to_var(_Arg) -> <<"_">>.

-spec is_sup_module(module(), acc()) -> acc() | {'skip', acc()}.
is_sup_module('kapps_config'=M, {_, Acc}) -> {M, Acc};
is_sup_module('kapps_account_config'=M, {_, Acc}) -> {M, Acc};
is_sup_module(M, {_, Acc}) ->
    case is_maintenance_module(M) of
        'false' -> {'skip', {M, Acc}};
        'true' ->  {M, Acc}
    end.

-spec is_maintenance_module(module()) -> boolean().
is_maintenance_module(M) ->
    kz_binary:suffix(<<"_maintenance">>, kz_term:to_binary(M)).
