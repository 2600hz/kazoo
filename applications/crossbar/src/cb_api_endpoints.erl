-module(cb_api_endpoints).
-compile([debug_info]).

-export([get/0]).

%% API

get() ->
    process_application('crossbar').

process_application(App) ->
    EBinDir = code:lib_dir(App, 'ebin'),
    io:format("looking in ~p~n", [EBinDir]),
    filelib:fold_files(EBinDir, "^cb_.*.beam$", 'false', fun process_module/2, []).

process_module(File, Acc) ->
    io:format("processing file ~p~n", [File]),
    {'ok', {Module, [{'exports', Fs}]}} = beam_lib:chunks(File, ['exports']),

    case process_exports(File, Module, Fs) of
        'undefined' -> Acc;
        Exports -> [Exports | Acc]
    end.

is_api_function({'allowed_methods', _Arity}) -> 'true';
is_api_function({'content_types_provided', _Arity}) -> 'true';
is_api_function(_) ->  'false'.

process_exports(_File, 'api_resource', _) -> [];
process_exports(_File, 'cb_context', _) -> [];
process_exports(File, Module, Fs) ->
    case lists:any(fun is_api_function/1, Fs) of
        'false' ->
            io:format("skipping non-api module ~s~n", [Module]),
            'undefined';
        'true' ->
            process_api_module(File, Module)
    end.

process_api_module(File, Module) ->
    {'ok', {Module, [{'abstract_code', AST}]}} = beam_lib:chunks(File, ['abstract_code']),
    process_api_ast(Module, AST).

process_api_ast(Module, {'raw_abstract_v1', Attributes}) ->
    APIFunctions = [{F, A, Clauses}
                    || {'function', _Line, F, A, Clauses} <- Attributes,
                       is_api_function({F, A})
                   ],
    process_api_ast_functions(Module, APIFunctions).

process_api_ast_functions(Module, Functions) ->
    {Module
      ,[process_api_ast_function(Module, F, A, Cs) || {F, A, Cs} <- Functions]
     }.

process_api_ast_function(_Module, 'allowed_methods', _Arity, Clauses) ->
    Methods = find_http_methods(Clauses),
    io:format("found in ~p: ~p~n", [_Module, Methods]),
    {'allowed_methods', Methods};
process_api_ast_function(_Module, 'content_types_provided', _Arity, Clauses) ->
    ContentTypes = find_http_methods(Clauses),
    io:format("found ctp in ~p: ~p~n", [_Module, ContentTypes]),
    {'content_types_provided', ContentTypes}.

find_http_methods(Clauses) ->
    lists:foldl(fun find_http_methods_from_clause/2, [], Clauses).

find_http_methods_from_clause({'clause', _Line, ArgsList, _, ClauseBody}, Methods) ->
    [{args_list_to_path(ArgsList), find_methods(ClauseBody)}
     | Methods
    ].

args_list_to_path([]) ->
    <<"/">>;
args_list_to_path(Args) ->
    wh_util:join_binary(
      lists:reverse(lists:foldl(fun arg_to_path/2, [<<>>], Args))
      ,<<"/">>
     ).

-define(BINARY(Value), {'bin',_, [{'bin_element', _, {'string', _, Value}, 'default', 'default'}]}).
-define(VAR_NAME(Name), {'var', _, Name}).

arg_to_path(?BINARY(Name), Acc) ->
    [wh_util:to_binary(Name) | Acc];
arg_to_path(?VAR_NAME('Context'), Acc) ->
    Acc;
arg_to_path(?VAR_NAME(Name), Acc) ->
    [wh_util:to_binary(Name) | Acc];
arg_to_path({'match', _, {'bin', _, _}, ?VAR_NAME(Name)}, Acc) ->
    [wh_util:to_binary(Name) | Acc].

find_methods(ClauseBody) ->
    find_methods(ClauseBody, []).
find_methods(ClauseBody, Acc) ->
    lists:usort(lists:foldl(fun find_methods_in_clause/2, Acc, ClauseBody)).

-define(LAGER_CALL, {'remote', _, {'atom', _, 'lager'}, _}).
-define(CB_CONTEXT_CALL(Fun)
        ,{'remote', _
         ,{'atom', _, 'cb_context'}
         ,{'atom', _, Fun}
         }
       ).

find_methods_in_clause(?VAR_NAME('Context'), Acc) ->
    Acc;
find_methods_in_clause(?VAR_NAME('Context1'), Acc) ->
    Acc;
find_methods_in_clause({'call', _, ?LAGER_CALL, _}, Acc) ->
    Acc;
find_methods_in_clause({'call', _, ?CB_CONTEXT_CALL('add_content_types_provided')
                        ,[?VAR_NAME('Context')
                          ,Args
                         ]
                       }, Acc) ->
    find_methods_in_clause(Args, Acc);
find_methods_in_clause({'call', _, ?CB_CONTEXT_CALL('set_content_types_provided')
                        ,[?VAR_NAME(_)
                          ,Args
                         ]
                       }, Acc) ->
    find_methods_in_clause(Args, Acc);
find_methods_in_clause({'call', _, {'fun', _, _}, []}, Acc) ->
    Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_fax'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_faxes:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_media'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_media:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_notifications'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_notifications:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_attachments'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_whitelabel:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_domain_attachments'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_whitelabel:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_provisioner'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_global_provisioner_templates:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_for_vm_download'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_vmboxes:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'call', _, {'atom', _, 'content_types_provided_get'}, _Args}, Acc) ->
    [wh_util:join_binary([Type, SubType], <<"/">>)
     || {Type, SubType} <- cb_port_requests:acceptable_content_types()
    ] ++ Acc;
find_methods_in_clause({'atom', _, 'ok'}, Acc) ->
    Acc;
find_methods_in_clause({'match', _, _, _}, Acc) ->
    Acc;
find_methods_in_clause({'nil', _}, Acc) ->
    [<<"nil">> | Acc];
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

%% Matches the content_types_provided to_json list
find_methods_in_clause({'cons', _
                        ,{'tuple', _
                          ,[{'atom', _, 'to_json'}
                            ,JSONList
                           ]
                         }
                        ,Rest
                       }
                      ,Acc) ->
    CTPs = find_content_types_in_clause(JSONList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_csv list
find_methods_in_clause({'cons', _
                        ,{'tuple', _
                          ,[{'atom', _, 'to_csv'}
                            ,CSVList
                           ]
                         }
                        ,Rest
                       }
                      ,Acc) ->
    CTPs = find_content_types_in_clause(CSVList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_binary list
find_methods_in_clause({'cons', _
                        ,{'tuple', _
                          ,[{'atom', _, 'to_binary'}
                            ,BinaryList
                           ]
                         }
                        ,Rest
                       }
                      ,Acc) ->
    CTPs = find_content_types_in_clause(BinaryList, Acc),
    find_methods_in_clause(Rest, CTPs);
%% Matches the content_types_provided to_pdf list
find_methods_in_clause({'cons', _
                        ,{'tuple', _
                          ,[{'atom', _, 'to_pdf'}
                            ,PDFList
                           ]
                         }
                        ,Rest
                       }
                      ,Acc) ->
    CTPs = find_content_types_in_clause(PDFList, Acc),
    find_methods_in_clause(Rest, CTPs);

find_methods_in_clause({'case', _
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

-define(CONTENT_TYPE_BINS(Type, SubType), [?BINARY(Type), ?BINARY(SubType)]).
-define(CONTENT_TYPE_VARS(Type, SubType), [?VAR_NAME(Type), ?VAR_NAME(SubType)]).

find_content_types_in_clause({'nil', _}, Acc) -> Acc;
find_content_types_in_clause({'cons', _
                              ,{'tuple', _
                                ,?CONTENT_TYPE_VARS(_Type, _SubType)
                               }
                              ,Rest
                             }
                             ,Acc) ->
    find_content_types_in_clause(Rest, Acc);
find_content_types_in_clause({'cons', _
                             ,{'tuple', _
                              ,?CONTENT_TYPE_BINS(Type, SubType)
                              }
                             ,Rest
                             }, Acc) ->
    CT = wh_util:join_binary([Type, SubType], <<"/">>),
    find_content_types_in_clause(Rest, [CT | Acc]).


%% End of Module

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
