-mode(compile).
-compile([debug_info]).

-export([main/1]).

-define(API_PATHS, ['crossbar']).

%% API

main(_) ->
    [process_application(App) || App <- ?API_PATHS],
    'ok'.

process_application(App) ->
    EBinDir = code:lib_dir(App, 'ebin'),
    filelib:fold_files(EBinDir, "*.beam", 'true', fun process_module/2, 'ok').

process_module(File, 'ok') ->
    {'ok', {Module, [{'exports', Fs}]}} = beam_lib:chunks(File, ['exports']),

    process_exports(File, Module, Fs).

is_api_function({'allowed_methods', _Arity}) -> 'true';
is_api_function({'resource_exists', _Arity}) -> 'true';
is_api_function(_) ->  'false'.

process_exports(File, Module, Fs) ->
    case lists:any(fun is_api_function/1, Fs) of
        'false' -> io:format("skipping non-api module ~s~n", [Module]);
        'true' ->
            process_api_module(File, Module)
    end.

process_api_module(File, Module) ->
    {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(F, ['abstract_code']),
    process_api_ast(Module, AST).

process_api_ast(Module, {'raw_abstract_v1', Attributes}) ->
    APIFunctions = [{F, A, Clauses}
                    || {'function', _Line, F, A, Clauses} <- Attributes,
                       is_api_function({F, A})
                   ],
    process_api_ast_functions(Module, APIFunctions).

process_api_ast_functions(Module, Functions) ->
    [process_api_ast_function(Module, F, A, Cs) || {F, A, Cs} <- Functions],
    'ok'.

process_api_ast_function(Module, 'allowed_methods', Arity, Clauses) ->
    Methods = find_http_methods(Clauses),

find_http_methods(Clauses) ->
    lists:foldl(fun find_http_methods_from_clause/2, [], Clauses).

find_http_methods_from_clause({'clause', _Line, ArgsList, _, ClauseBody}, Methods) ->
    [{args_list_to_path(ArgsList), find_methods(ClauseBody)}
     | Methods
    ].

args_list_to_path(Args) ->
    lists:foldl(fun arg_to_path/2, [<<"/">>], Args).

arg_to_path({'var', _, Name}, Acc) ->
    [atom_to_binary(Name) | Acc].

find_methods(ClauseBody) ->
    find_methods(ClauseBody, []).
find_methods(ClauseBody, Acc) ->
    lists:foldl(fun find_methods_in_clause/2, Acc, ClauseBody).

{cons,86,
          {bin,86,[{bin_element,86,{string,86,"GET"},default,default}]},
          {cons,86,
           {bin,86,[{bin_element,86,{string,86,"PUT"},default,default}]},
           {cons,86,
            {bin,86,[{bin_element,86,{string,86,"POST"},default,default}]},
            {cons,86,
             {bin,86,[{bin_element,86,{string,86,"PATCH"},default,default}]},
             {nil,86}}}}}
find_methods_in_clause({'cons',_
                       ,{'bin',_
                        ,[{'bin_element',_,{'string',_,Method},'default','default'}]
                        }
                        ,{'nil',_}
                       }
                      ,Acc
                      ) ->
    [list_to_binary(Method) | Acc];
find_methods_in_clause({'cons',_
                       ,{'bin',_
                        ,[{'bin_element',_,{'string',_,Method},'default','default'}]
                        }
                        ,Cons
                       }
                      ,Acc
                      ) ->
    [list_to_binary(Method) | find_methods_in_clause(Cons, Acc)];
find_methods_in_clause({'case', 83
                        ,_CaseConditional
                        ,CaseClauses
                       }
                       ,Acc0
                      ) ->
    lists:foldl(fun({'clause', _Line, _Args, _, ClauseBody}, Acc1) ->
                        find_methods(ClauseBody, Acc1)
                end
                ,Acc0
                ,CaseClauses
               ).

find_modules(Path) ->
    case filelib:is_dir(Path) of
        'false' -> [];
        'true' ->
            AccFiles = fun(File, Acc) -> [File|Acc] end,
            filelib:fold_files(Path, "cb_.+", 'true', AccFiles, [])
    end.

pp(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
pp({atom,_,Atom}) ->
    "'" ++ pp(Atom) ++ "'";
pp({var,_,Atom}) ->
    pp(Atom);
pp({bin,_,[{bin_element,_,{var,_,Atom},_,_}]}) ->
    pp(Atom);
pp({bin,_,[{bin_element,_,{string,_,Str},_,_}]}) ->
    "\"" ++ Str ++ "\"";

%% MAY hide those (as its internals)
pp({nil,_}) ->
    "[]";
pp({cons,_,H,T}) ->
    "[" ++ pp(H) ++ "|" ++ pp(T) ++ "]";
pp(_E) ->
    "PLACEHOLDER".


%% End of Module

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

allowed_methods_parsing_test() ->
    AMs = [{function,79,allowed_methods,0,
  [{clause,79,[],[],
    [{cons,80,
      {bin,80,[{bin_element,80,{string,80,"PUT"},default,default}]},
      {nil,80}}]}]},
 {function,82,allowed_methods,1,
  [{clause,82,
    [{var,82,'AccountId'}],
    [],
    [{'case',83,
      {call,83,
       {remote,83,{atom,83,whapps_util},{atom,83,get_master_account_id}},
       []},
      [{clause,84,
        [{tuple,84,[{atom,84,ok},{var,84,'AccountId'}]}],
        [],
        [{'case',85,
          {tuple,85,
           [{call,85,{atom,85,whereis},[{atom,85,lager_event}]},
            {call,85,
             {remote,85,{atom,85,lager_config},{atom,85,get}},
             [{atom,85,loglevel},{tuple,85,[{integer,85,0},{nil,85}]}]}]},
          [{clause,85,
            [{tuple,85,[{atom,85,undefined},{var,85,'_'}]}],
            [],
            [{call,85,
              {'fun',85,
               {clauses,
                [{clause,85,[],[],
                  [{tuple,85,
                    [{atom,85,error},{atom,85,lager_not_running}]}]}]}},
              []}]},
           {clause,85,
            [{tuple,85,
              [{var,85,'__Pidcb_accounts85'},
               {tuple,85,
                [{var,85,'__Levelcb_accounts85'},
                 {var,85,'__Tracescb_accounts85'}]}]}],
            [[{op,85,'orelse',
               {op,85,'/=',
                {op,85,'band',
                 {var,85,'__Levelcb_accounts85'},
                 {integer,85,128}},
                {integer,85,0}},
               {op,85,'/=',{var,85,'__Tracescb_accounts85'},{nil,85}}}]],
            [{call,85,
              {remote,85,{atom,85,lager},{atom,85,do_log}},
              [{atom,85,debug},
               {cons,85,
                {tuple,85,[{atom,85,module},{atom,85,cb_accounts}]},
                {cons,85,
                 {tuple,85,
                  [{atom,85,function},
                   {call,85,
                    {remote,85,{atom,85,erlang},{atom,85,get}},
                    [{atom,85,callid}]}]},
                 {cons,85,
                  {tuple,85,[{atom,85,line},{integer,85,85}]},
                  {cons,85,
                   {tuple,85,
                    [{atom,85,pid},
                     {call,85,
                      {atom,85,pid_to_list},
                      [{call,85,{atom,85,self},[]}]}]},
                   {cons,85,
                    {tuple,85,[{atom,85,node},{call,85,{atom,85,node},[]}]},
                    {call,85,{remote,85,{atom,85,lager},{atom,85,md}},[]}}}}}},
               {string,85,"accessing master account, disallowing DELETE"},
               {atom,85,none},
               {integer,85,4096},
               {integer,85,128},
               {var,85,'__Levelcb_accounts85'},
               {var,85,'__Tracescb_accounts85'},
               {var,85,'__Pidcb_accounts85'}]}]},
           {clause,85,[{var,85,'_'}],[],[{atom,85,ok}]}]},
         {cons,86,
          {bin,86,[{bin_element,86,{string,86,"GET"},default,default}]},
          {cons,86,
           {bin,86,[{bin_element,86,{string,86,"PUT"},default,default}]},
           {cons,86,
            {bin,86,[{bin_element,86,{string,86,"POST"},default,default}]},
            {cons,86,
             {bin,86,[{bin_element,86,{string,86,"PATCH"},default,default}]},
             {nil,86}}}}}]},
       {clause,87,
        [{tuple,87,[{atom,87,ok},{var,87,'_MasterId'}]}],
        [],
        [{cons,88,
          {bin,88,[{bin_element,88,{string,88,"GET"},default,default}]},
          {cons,88,
           {bin,88,[{bin_element,88,{string,88,"PUT"},default,default}]},
           {cons,88,
            {bin,88,[{bin_element,88,{string,88,"POST"},default,default}]},
            {cons,88,
             {bin,88,[{bin_element,88,{string,88,"PATCH"},default,default}]},
             {cons,88,
              {bin,88,[{bin_element,88,{string,88,"DELETE"},default,default}]},
              {nil,88}}}}}}]},
       {clause,89,
        [{tuple,89,[{atom,89,error},{var,89,'_E'}]}],
        [],
        [{'case',90,
          {tuple,90,
           [{call,90,{atom,90,whereis},[{atom,90,lager_event}]},
            {call,90,
             {remote,90,{atom,90,lager_config},{atom,90,get}},
             [{atom,90,loglevel},{tuple,90,[{integer,90,0},{nil,90}]}]}]},
          [{clause,90,
            [{tuple,90,[{atom,90,undefined},{var,90,'_'}]}],
            [],
            [{call,90,
              {'fun',90,
               {clauses,
                [{clause,90,[],[],
                  [{tuple,90,
                    [{atom,90,error},{atom,90,lager_not_running}]}]}]}},
              []}]},
           {clause,90,
            [{tuple,90,
              [{var,90,'__Pidcb_accounts90'},
               {tuple,90,
                [{var,90,'__Levelcb_accounts90'},
                 {var,90,'__Tracescb_accounts90'}]}]}],
            [[{op,90,'orelse',
               {op,90,'/=',
                {op,90,'band',
                 {var,90,'__Levelcb_accounts90'},
                 {integer,90,128}},
                {integer,90,0}},
               {op,90,'/=',{var,90,'__Tracescb_accounts90'},{nil,90}}}]],
            [{call,90,
              {remote,90,{atom,90,lager},{atom,90,do_log}},
              [{atom,90,debug},
               {cons,90,
                {tuple,90,[{atom,90,module},{atom,90,cb_accounts}]},
                {cons,90,
                 {tuple,90,
                  [{atom,90,function},
                   {call,90,
                    {remote,90,{atom,90,erlang},{atom,90,get}},
                    [{atom,90,callid}]}]},
                 {cons,90,
                  {tuple,90,[{atom,90,line},{integer,90,90}]},
                  {cons,90,
                   {tuple,90,
                    [{atom,90,pid},
                     {call,90,
                      {atom,90,pid_to_list},
                      [{call,90,{atom,90,self},[]}]}]},
                   {cons,90,
                    {tuple,90,[{atom,90,node},{call,90,{atom,90,node},[]}]},
                    {call,90,{remote,90,{atom,90,lager},{atom,90,md}},[]}}}}}},
               {string,90,"failed to get master account id: ~p"},
               {cons,90,{var,90,'_E'},{nil,90}},
               {integer,90,4096},
               {integer,90,128},
               {var,90,'__Levelcb_accounts90'},
               {var,90,'__Tracescb_accounts90'},
               {var,90,'__Pidcb_accounts90'}]}]},
           {clause,90,[{var,90,'_'}],[],[{atom,90,ok}]}]},
         {'case',91,
          {tuple,91,
           [{call,91,{atom,91,whereis},[{atom,91,lager_event}]},
            {call,91,
             {remote,91,{atom,91,lager_config},{atom,91,get}},
             [{atom,91,loglevel},{tuple,91,[{integer,91,0},{nil,91}]}]}]},
          [{clause,91,
            [{tuple,91,[{atom,91,undefined},{var,91,'_'}]}],
            [],
            [{call,91,
              {'fun',91,
               {clauses,
                [{clause,91,[],[],
                  [{tuple,91,
                    [{atom,91,error},{atom,91,lager_not_running}]}]}]}},
              []}]},
           {clause,91,
            [{tuple,91,
              [{var,91,'__Pidcb_accounts91'},
               {tuple,91,
                [{var,91,'__Levelcb_accounts91'},
                 {var,91,'__Tracescb_accounts91'}]}]}],
            [[{op,91,'orelse',
               {op,91,'/=',
                {op,91,'band',{var,91,'__Levelcb_accounts91'},{integer,91,64}},
                {integer,91,0}},
               {op,91,'/=',{var,91,'__Tracescb_accounts91'},{nil,91}}}]],
            [{call,91,
              {remote,91,{atom,91,lager},{atom,91,do_log}},
              [{atom,91,info},
               {cons,91,
                {tuple,91,[{atom,91,module},{atom,91,cb_accounts}]},
                {cons,91,
                 {tuple,91,
                  [{atom,91,function},
                   {call,91,
                    {remote,91,{atom,91,erlang},{atom,91,get}},
                    [{atom,91,callid}]}]},
                 {cons,91,
                  {tuple,91,[{atom,91,line},{integer,91,91}]},
                  {cons,91,
                   {tuple,91,
                    [{atom,91,pid},
                     {call,91,
                      {atom,91,pid_to_list},
                      [{call,91,{atom,91,self},[]}]}]},
                   {cons,91,
                    {tuple,91,[{atom,91,node},{call,91,{atom,91,node},[]}]},
                    {call,91,{remote,91,{atom,91,lager},{atom,91,md}},[]}}}}}},
               {string,91,
                "disallowing DELETE while we can't determine the master account id"},
               {atom,91,none},
               {integer,91,4096},
               {integer,91,64},
               {var,91,'__Levelcb_accounts91'},
               {var,91,'__Tracescb_accounts91'},
               {var,91,'__Pidcb_accounts91'}]}]},
           {clause,91,[{var,91,'_'}],[],[{atom,91,ok}]}]},
         {cons,92,
          {bin,92,[{bin_element,92,{string,92,"GET"},default,default}]},
          {cons,92,
           {bin,92,[{bin_element,92,{string,92,"PUT"},default,default}]},
           {cons,92,
            {bin,92,[{bin_element,92,{string,92,"POST"},default,default}]},
            {cons,92,
             {bin,92,[{bin_element,92,{string,92,"PATCH"},default,default}]},
             {nil,92}}}}}]}]}]}]},
 {function,95,allowed_methods,2,
  [{clause,95,
    [{var,95,'_'},
     {bin,95,[{bin_element,95,{string,95,"move"},default,default}]}],
    [],
    [{cons,96,
      {bin,96,[{bin_element,96,{string,96,"POST"},default,default}]},
      {nil,96}}]},
   {clause,97,
    [{var,97,'_'},{var,97,'Path'}],
    [],
    [{match,98,
      {var,98,'Paths'},
      {cons,98,
       {bin,98,[{bin_element,98,{string,98,"children"},default,default}]},
       {cons,99,
        {bin,99,[{bin_element,99,{string,99,"descendants"},default,default}]},
        {cons,100,
         {bin,100,[{bin_element,100,{string,100,"siblings"},default,default}]},
         {cons,101,
          {bin,101,[{bin_element,101,{string,101,"api_key"},default,default}]},
          {cons,102,
           {bin,102,[{bin_element,102,{string,102,"tree"},default,default}]},
           {cons,103,
            {bin,103,
             [{bin_element,103,{string,103,"parents"},default,default}]},
            {nil,104}}}}}}}},
     {'case',105,
      {call,105,
       {remote,105,{atom,105,lists},{atom,105,member}},
       [{var,105,'Path'},{var,105,'Paths'}]},
      [{clause,106,
        [{atom,106,true}],
        [],
        [{cons,106,
          {bin,106,[{bin_element,106,{string,106,"GET"},default,default}]},
          {nil,106}}]},
       {clause,107,[{atom,107,false}],[],[{nil,107}]}]}]}]}].

-endif.
