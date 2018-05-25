%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2001-2018, 2600Hz
%%% The origin of this file is the edoc module `edoc_layout.erl'
%%% written by Richard Carlsson and Kenneth Lundin.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%%% not use this file except in compliance with the License. You may obtain
%%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% Alternatively, you may use this file under the terms of the GNU Lesser
%%% General Public License (the "LGPL") as published by the Free Software
%%% Foundation; either version 2.1, or (at your option) any later version.
%%% If you wish to allow use of your version of this file only under the
%%% terms of the LGPL, you should delete the provisions above and replace
%%% them with the notice and other provisions required by the LGPL; see
%%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%%% above, a recipient may use your version of this file under the terms of
%%% either the Apache License or the LGPL.
%%%
%%%
%%% @doc This is the EDoc layout callback module for creating Kazoo
%%% documents in HTML format, and also application documents based on
%%% "overview.edoc". It parses and creates proplist of EDoc XML document
%%% and convert it to HTML using ErlyDTL template.
%%%
%%% @author Richard Carlsson <carlsson.richard@gmail.com>
%%% @author Hesaam Farhang <hesaam@2600hz.com>
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_edoc_layout).

-export([module/2
        ,overview/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec module(kz_types:xml_el(), map()) -> proplists:proplist().
module(#xmlElement{name = module, content = Es}=E
      ,#{sort_functions := SortFunctions}=Context
      ) ->
    Name = kz_xml:get_attrval(name, E),

    Functions = case [{function_name_arity(F, Context), F} || F <- kz_xml:get_content(functions, Es)] of
                    Funs when SortFunctions =:= true -> lists:sort(Funs);
                    Funs -> Funs
                end,
    Types = [{type_name(T, Context), T} || T <- kz_xml:get_content(typedecls, Es)],

    filter_empty(
      [{name, list_to_binary(Name)}
      ,{copyright, export_content(kz_xml:get_content(copyright, Es), Context)}
      ,{deprecated, deprecated(Es, Context)}
      ,{version, export_content(kz_xml:get_content(version, Es), Context)}
      ,{since, since(Es, Context)}
      ,{behaviours, behaviours_prop(Es, Name, Context)}
      ,{authors, authors(Es, Context)}
      ,{references, references(Es, Context)}
      ,{sees, sees(Es, Context)}
      ,{todos, todos(Es, Context)}
      ,{types, types(lists:sort(Types), Context)}
      ,{functions, functions(Functions, Context)}
       | description(both, Es, Context)
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec overview(kz_types:xml_el(), map()) -> proplists:proplist().
overview(#xmlElement{name = overview, content = Es}, Context) ->
    filter_empty(
      [{title, export_content(kz_xml:get_textval(title, Es), Context)}
      ,{copyright, export_content(kz_xml:get_content(copyright, Es), Context)}
      ,{version, export_content(kz_xml:get_content(version, Es), Context)}
      ,{since, since(Es, Context)}
      ,{authors, authors(Es, Context)}
      ,{references, references(Es, Context)}
      ,{sees, sees(Es, Context)}
      ,{todos, todos(Es, Context)}
      ,{full_desc, description(full, Es, Context)}
      ]
     ).

%%%=============================================================================
%%% Module Tags functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Behaviour tag to proplist.
%% @end
%%------------------------------------------------------------------------------
-spec behaviours_prop(kz_types:xml_els(), string(), map()) ->
                             Result when Result :: [{name, string()} |
                                                    {behaviours, [kz_html:exported()]} |
                                                    {callbacks, [kz_html:exported()]} |
                                                    {optional_callbacks, [kz_html:exported()]}
                                                   ].
behaviours_prop(Es, BehaviourName, Context) ->
    Behaviours = [export_content(behaviour(B, Context), Context) || B <- kz_xml:get_elem(behaviour, Es)],
    Required = [export_content(callback(C, Context), Context) || C <- kz_xml:get_content(callbacks, Es)],
    Optional = [export_content(callback(C, Context), Context) || C <- kz_xml:get_content(optional_callbacks, Es)],
    case Required =/= []
        orelse Optional =/= []
    of
        true ->
            filter_empty([{name, BehaviourName}
                         ,{behaviours, Behaviours}
                         ,{callbacks, Required}
                         ,{optional_callbacks, Optional}
                         ]);
        false ->
            filter_empty([{behaviours, Behaviours}])
    end.

-spec behaviour(kz_types:xml_el(), map()) -> kz_html:exporties().
behaviour(E=#xmlElement{content = Es}, Context) ->
    see(E, Es, Context).

-spec callback(kz_types:xml_el(), map()) -> [string()].
callback(E=#xmlElement{}, Context) ->
    Name = kz_xml:get_attrval(name, E),
    Arity = kz_xml:get_attrval(arity, E),
    [atom(Name, Context), "/", Arity].

%%------------------------------------------------------------------------------
%% @doc Author proplist.
%% @end
%%------------------------------------------------------------------------------

-type author() :: [{name, binary()} |
                   {email, binary()} |
                   {website, binary()}
                  ].
-spec authors(kz_types:xml_els(), map()) -> [{binary(), author()}].
authors(Es, Context) ->
    lists:usort([{proplists:get_value(name, Author), Author}
                 || A <- kz_xml:get_elem(author, Es),
                    Author <- [author(A, Context)],
                    Author =/= []
                ]).

-spec author(kz_types:xml_el(), map()) -> author().
author(E=#xmlElement{}, _Context) ->
    Email = iolist_to_binary(kz_xml:get_attrval(email, E)),
    URL = iolist_to_binary(kz_xml:get_attrval(website, E)),
    case kz_xml:get_attrval(name, E) of
        [] -> [];
        Name ->
            filter_empty([{name, iolist_to_binary(Name)}
                         ,{email, Email}
                         ,{website, URL}
                         ])
    end.

-spec references(kz_types:xml_els(), map()) -> [kz_html:exported()].
references(Es, Context) ->
    [export_content(normalize_description(C, Context), Context)
     || #xmlElement{content = C} <- kz_xml:get_elem(reference, Es)
    ].

%%%=============================================================================
%%% Type Tag functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec type_name(kz_types:xml_el(), map()) -> string().
type_name(#xmlElement{content = Es}, Context) ->
    t_name(kz_xml:get_elem(erlangName, kz_xml:get_content(typedef, Es)), Context).

%% <!ELEMENT typedecl (typedef, description?)>
%% <!ELEMENT typedef (erlangName, argtypes, type?, localdef*)>

-type typedef() :: [{def, kz_html:exported()} |
                    {localdefs, localdefs()} |
                    {abstract_datatype, boolean()}
                   ].

-type type_prop() :: [{id, binary()} |
                      {name, binary()} |
                      {label, string()} |
                      {typedef, typedef()} |
                      {full_desc, kz_html:exported()}
                     ].

-spec types(kz_types:xml_els(), map()) -> [type_prop()].
types(Ts, Context) ->
    [typedecl(Name, E, Context) || {Name, E} <- Ts].

-spec typedecl(string(), kz_types:xml_el(), map()) -> type_prop().
typedecl(Name, E=#xmlElement{content = Es}, Context) ->
    NameArgTypes = lists:append([Name, "("] ++ seq(t_utype_elem_fun(Context), kz_xml:get_content(argtypes, Es), [")"])),
    {Id, _} = anchor_id_label(Name ++ "()", E),
    filter_empty(
      [{id, Id}
      ,{name, iolist_to_binary(Name)}
      ,{label, NameArgTypes}
      ,{typedef, typedef(NameArgTypes, kz_xml:get_content(typedef, Es), Context)}
      ,{full_desc, description(full, Es, Context)}
      ]
     ).

-spec typedef(string(), kz_types:xml_els(), map()) -> typedef().
typedef(NameArgTypes, Es, Context) ->
    Typedef = filter_empty([{localdefs, local_defs(kz_xml:get_elem(localdef, Es), Context)}]),

    case kz_xml:get_elem(type, Es) of
        [] ->
            [{def, export_content([NameArgTypes], Context)}
            ,{abstract_datatype, true}
             | Typedef
            ];
        Type ->
            [{def, format_type(NameArgTypes, NameArgTypes, Type, Context)} | Typedef]
    end.

-spec format_type(string(), string(), kz_types:xml_els(), map()) -> kz_html:exported().
format_type(Prefix, NameArgTypes, Type, #{pretty_printer := erl_pp}=Context) ->
    try
        L = t_utype(Type, Context),
        O = pp_type(NameArgTypes, Type),
        {R, ".\n"} = etypef(L, O, Context),
        export_content([Prefix] ++ [" = "] ++ R, Context)
    catch _:_ ->
            %% Example: "t() = record(a)."
            ?DEV_LOG("wtf type ~p", [Prefix]),
            format_type(Prefix, NameArgTypes, Type, Context#{pretty_printer => ''})
    end;
format_type(Prefix, _Name, Type, Context) ->
    export_content([Prefix] ++ [" = "] ++ t_utype(Type, Context), Context).

-spec pp_type(string(), kz_types:xml_els()) -> string().
pp_type(Prefix, Type) ->
    Atom = list_to_atom(lists:duplicate(string:len(Prefix), $a)),
    Attr = {attribute, 0, type, {Atom, ot_utype(Type), []}},
    L1 = erl_pp:attribute(erl_parse:new_anno(Attr)
                         ,[{encoding, utf8}]
                         ),
    {L2,N} = case lists:dropwhile(fun(C) -> C =/= $: end, lists:flatten(L1)) of
                 ":: " ++ L3 -> {L3, 9}; %% compensation for extra "()" and ":"
                 "::\n" ++ L3 -> {"\n" ++ L3, 6}
             end,
    Ss = lists:duplicate(N, $\s),
    S1 = re:replace(L2, "\n" ++ Ss, "\n", [{return,list},global,unicode]),

    %% remove the extra tickie if the atom must have tickies to
    %% avoid escaped tickies like (#'\'queue.declare'\'{}).
    re:replace(S1, "\\\\'", "", [{return,list},global,unicode]).

-type localdef() :: kz_html:exported().
-type localdefs() :: [localdef()].

-spec local_defs(kz_types:xml_els(), map()) -> localdefs().
local_defs(Es, Context) ->
    [localdef(E1, Context) || E1 <- Es].

-spec localdef(kz_types:xml_el(), map()) -> localdef().
localdef(E = #xmlElement{content = Es}, Context) ->
    {{_, Name}, TypeName} =
        case kz_xml:get_elem(typevar, Es) of
            [] ->
                N0 = lists:append(t_abstype(kz_xml:get_content(abstype, Es), Context)),
                {anchor_id_label(N0, E), N0};
            [V] ->
                N0 = lists:append(t_var(V)),
                {{<<>>, N0}, N0}
        end,
    format_type(Name, TypeName, kz_xml:get_elem(type, Es), Context).

%%%=============================================================================
%%% Function Tags functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec function_name_arity(kz_types:xml_el(), map()) -> string().
function_name_arity(E, Context) ->
    atom(kz_xml:get_attrval(name, E), Context) ++ "/" ++ kz_xml:get_attrval(arity, E).

%% <!ELEMENT function (args, typespec?, returns?, throws?, equiv?,
%%                     description?, since?, deprecated?, see*, todo?)>
%% <!ATTLIST function
%%   name CDATA #REQUIRED
%%   arity CDATA #REQUIRED
%%   exported NMTOKEN(yes | no) #REQUIRED
%%   label CDATA #IMPLIED>
%% <!ELEMENT args (arg*)>
%% <!ELEMENT equiv (expr, see?)>
%% <!ELEMENT expr (#PCDATA)>

-type throws() :: [{type, kz_html:exported()} | {localdefs, localdefs()}].
-type params() :: [{string(), kz_html:exported()}].
-type function_props() :: [{id, binary()} |
                           {label, string()} |
                           {name, string()} |
                           {arity, string()} |
                           {is_exported, boolean()} |
                           {typespec, kz_html:exported()} |
                           {localdefs, localdefs()} |
                           {params, params()} |
                           {returns, kz_html:exported()} |
                           {throws, throws()} |
                           {equiv, kz_html:exported()} |
                           {deprecated, kz_html:exported()} |
                           {since, kz_html:exported()} |
                           {sees, [kz_html:exported()]} |
                           {todos, [kz_html:exported()]} |
                           {short_desc, kz_html:exported()} |
                           {full_desc, kz_html:exported()}
                          ].

-spec functions([{string(), kz_types:xml_el()}], map()) -> [function_props()].
functions(Fs, Context) ->
    [function(NameArity, E, Context) || {NameArity, E} <- Fs].

-spec function(string(), kz_types:xml_el(), map()) -> function_props().
function(NameArity, E=#xmlElement{content = Es}, Context) ->
    Name = kz_xml:get_attrval(name, E),
    Arity = kz_xml:get_attrval(arity, E),
    {Id, Label} = anchor_id_label(NameArity, E),
    filter_empty(
      [{id, Id}
      ,{label, Label}
      ,{name, Name}
      ,{arity, Arity}
      ,{is_exported, is_exported(E)}
      ,{typespec, typespec_signature(E, Context)}
      ,{localdefs, local_defs(kz_xml:get_elem(localdef, kz_xml:get_content(typespec, Es)), Context)}
      ,{params, params(Es, Context)}
      ,{returns, returns(Es, Context)}
      ,{throws, throws(Es, Context)}
      ,{equiv, equiv(Es, Context)}
      ,{deprecated, deprecated(Es, Context)}
      ,{since, since(Es, Context)}
      ,{sees, sees(Es, Context)}
      ,{todos, todos(Es, Context)}
       | description(both, Es, Context)
      ]
     ).

-spec is_exported(kz_types:xml_el()) -> boolean().
is_exported(E) ->
    case kz_xml:get_attrval(exported, E) of
        "yes" -> true;
        _ -> false
    end.

%% parameter descriptions (if any)
-spec params(kz_types:xml_els(), map()) -> params().
params(Es, Context) ->
    [{kz_xml:get_textval(argName, Es1), Desc}
     || #xmlElement{content = Es1} <- kz_xml:get_content(args, Es),
        Desc <- [description(full, Es1, Context)],
        Desc =/= <<>>
    ].

%% return value descriptions (if any)
-spec returns(kz_types:xml_els(), map()) -> kz_html:exported().
returns(Es, Context) ->
    description(full, kz_xml:get_content(returns, Es), Context).

%% <!ELEMENT throws (type, localdef*)>

-spec throws(kz_types:xml_els(), map()) -> throws().
throws(Es, Context) ->
    case kz_xml:get_content(throws, Es) of
        [] -> [];
        Es1 ->
            %% Don't use format_type; keep it short!
            [{type, export_content(t_utype(kz_xml:get_elem(type, Es1), Context), Context)}
            ,{localdefs, local_defs(kz_xml:get_elem(localdef, Es1), Context)}
            ]
    end.

-spec equiv(kz_types:xml_els(), map()) -> kz_html:exported().
equiv(Es, Context) ->
    Es1 = kz_xml:get_content(equiv, Es),
    case {kz_xml:get_content(expr, Es1)
         ,kz_xml:get_elem(see, Es1)
         }
    of
        {[], _} -> <<>>;
        {[Expr], []} ->
            export_content([Expr], Context);
        {[Expr], [E=#xmlElement{}]} ->
            export_content(see(E, [Expr], Context), Context)
    end.

-spec typespec_signature(kz_types:xml_el(), map()) -> kz_html:exported().
typespec_signature(E=#xmlElement{content = Es}, Context) ->
    case typespec(kz_xml:get_content(typespec, Es), Context) of
        [] ->
            export_content(signature(kz_xml:get_content(args, Es), atom(kz_xml:get_attrval(name, E), Context)), Context);
        Spec ->
            export_content(Spec, Context)
    end.

%% <!ELEMENT typespec (erlangName, type, localdef*)>

-spec typespec(kz_types:xml_els(), map()) -> [string()].
typespec([], _Context) -> [];
typespec(Es, Context) ->
    Name = t_name(kz_xml:get_elem(erlangName, Es), Context),
    [Type] = kz_xml:get_elem(type, Es),
    format_spec(Name, Type, Context).

%% <!ELEMENT args (arg*)>
%% <!ELEMENT arg (argName, description?)>
%% <!ELEMENT argName (#PCDATA)>

%% This is currently only done for functions without type spec.

-spec signature(kz_types:xml_els(), string()) -> [string()].
signature(Es, Name) ->
    [Name, "("] ++ seq(fun function_arg/1, Es) ++ [") -> any()"].

-spec function_arg(kz_types:xml_el()) -> [string()].
function_arg(#xmlElement{content = Es}) ->
    [kz_xml:get_textval(argName, Es)].

%% Use the default formatting of EDoc, which creates references, and
%% then insert newlines and indentation according to erl_pp (the
%% (fast) Erlang pretty printer).
-spec format_spec(string(), kz_types:xml_el(), map()) -> [string()].
format_spec(Name, Type, #{pretty_printer := erl_pp}=Context) ->
    try
        L = t_clause(Name, Type, Context),
        O = pp_clause(Name, Type, Context),
        {R, ".\n"} = etypef(L, O, Context),
        R
    catch _E:_T ->
            %% Should not happen.
            ?DEV_LOG("wtf spec ~p", [Name]),
            format_spec(Name, Type, Context#{pretty_printer => ''})
    end;
format_spec(Sep, Type, Context) ->
    %% Very limited formatting.
    t_clause(Sep, Type, Context).

-spec t_clause(string(), kz_types:xml_el(), map()) -> [string()].
t_clause(Name, Type, Context) ->
    #xmlElement{content = [#xmlElement{name = 'fun', content = C}]} = Type,
    [Name] ++ t_fun(C, Context).

-spec pp_clause(string(), kz_types:xml_el(), map()) -> string().
pp_clause(Pre, Type, _Context) ->
    Types = ot_utype([Type]),
    Atom = lists:duplicate(string:len(Pre), $a),
    Attr = {attribute, 0, spec, {{list_to_atom(Atom), 0}, [Types]}},
    L1 = erl_pp:attribute(erl_parse:new_anno(Attr)
                         ,[{encoding, utf8}
                          ]),
    "-spec " ++ L2 = lists:flatten(L1),
    L3 = Pre ++ lists:nthtail(length(Atom), L2),
    L4 = re:replace(L3, "\n      ", "\n", [{return,list},global,unicode]),

    %% remove the extra tickie if the atom must have tickies to
    %% avoid escaped tickies like (#'\'queue.declare'\'{}).
    re:replace(L4, "\\\\'", "", [{return,list},global,unicode]).

%%%=============================================================================
%%% Edoc typedef/spec_type Tags to HTML functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec t_utype(kz_types:xml_els(), map()) -> any().
t_utype([E], Context) ->
    t_utype_elem(E, Context).

-spec t_utype_elem_fun(map()) -> fun((kz_types:xml_el()) -> any()).
t_utype_elem_fun(Context) ->
    fun(E) -> t_utype_elem(E, Context) end.

-spec t_utype_elem(kz_types:xml_el(), map()) -> any().
t_utype_elem(E=#xmlElement{content = Es}, Context) ->
    case kz_xml:get_attrval(name, E) of
        "" -> t_type(Es, Context);
        Name ->
            T = t_type(Es, Context),
            case T of
                [Name] -> T; %% avoid generating "Foo::Foo"
                T -> [Name] ++ ["::"] ++ T
            end
    end.

t_type([E=#xmlElement{name = typevar}], _Context) ->
    t_var(E);
t_type([E=#xmlElement{name = atom}], Context) ->
    t_atom(E, Context);
t_type([E=#xmlElement{name = integer}], _Context) ->
    t_integer(E);
t_type([E=#xmlElement{name = range}], _Context) ->
    t_range(E);
t_type([E=#xmlElement{name = binary}], _Context) ->
    t_binary(E);
t_type([E=#xmlElement{name = float}], _Context) ->
    t_float(E);
t_type([#xmlElement{name = nil}], _Context) ->
    t_nil();
t_type([#xmlElement{name = paren, content = Es}], Context) ->
    t_paren(Es, Context);
t_type([#xmlElement{name = list, content = Es}], Context) ->
    t_list(Es, Context);
t_type([#xmlElement{name = nonempty_list, content = Es}], Context) ->
    t_nonempty_list(Es, Context);
t_type([#xmlElement{name = map, content = Es}], Context) ->
    t_map(Es, Context);
t_type([#xmlElement{name = tuple, content = Es}], Context) ->
    t_tuple(Es, Context);
t_type([#xmlElement{name = 'fun', content = Es}], Context) ->
    ["fun("] ++ t_fun(Es, Context) ++ [")"];
t_type([E = #xmlElement{name = record, content = Es}], Context) ->
    t_record(E, Es, Context);
t_type([E = #xmlElement{name = abstype, content = Es}], Context) ->
    t_abstype(E, Es, Context);
t_type([#xmlElement{name = union, content = Es}], Context) ->
    t_union(Es, Context).

t_var(E) ->
    [kz_xml:get_attrval(name, E)].

t_atom(E, Context) ->
    [atom(kz_xml:get_attrval(value, E), Context)].

t_integer(E) ->
    [kz_xml:get_attrval(value, E)].

t_range(E) ->
    [kz_xml:get_attrval(value, E)].

t_binary(E) ->
    [kz_xml:get_attrval(value, E)].

t_float(E) ->
    [kz_xml:get_attrval(value, E)].

t_nil() ->
    ["[]"].

t_paren(Es, Context) ->
    ["("] ++ t_utype(kz_xml:get_elem(type, Es), Context) ++ [")"].

t_list(Es, Context) ->
    ["["] ++ t_utype(kz_xml:get_elem(type, Es), Context) ++ ["]"].

t_nonempty_list(Es, Context) ->
    ["["] ++ t_utype(kz_xml:get_elem(type, Es), Context) ++ [", ...]"].

t_map(Es, Context) ->
    Fs = kz_xml:get_elem(map_field, Es),
    ["#{"] ++ seq(fun(E) -> t_map_field(E, Context) end, Fs, ["}"]).

t_map_field(#xmlElement{content = [K,V]}=E, Context) ->
    KElem = t_utype_elem(K, Context),
    VElem = t_utype_elem(V, Context),
    AS = case kz_xml:get_attrval(assoc_type, E) of
             "assoc" -> " => ";
             "exact" -> " := "
         end,
    KElem ++ [AS] ++ VElem.

t_tuple(Es, Context) ->
    ["{"] ++ seq(t_utype_elem_fun(Context), Es, ["}"]).

-spec t_fun(kz_types:xml_els(), map()) -> kz_html:exporties().
t_fun(Es, Context) ->
    ["("] ++ seq(t_utype_elem_fun(Context), kz_xml:get_content(argtypes, Es),
                 [") -> "] ++ t_utype(kz_xml:get_elem(type, Es), Context)).

t_record(E, Es, Context) ->
    Name = ["#"] ++ t_type(kz_xml:get_elem(atom, Es), Context),
    case kz_xml:get_elem(field, Es) of
        [] ->
            see(E, Name ++ ["{}"], Context);
        Fs ->
            see(E, Name, Context) ++ ["{"] ++ seq(fun(F) -> t_field(F, Context) end, Fs, ["}"])
    end.

t_field(#xmlElement{content = Es}, Context) ->
    t_type(kz_xml:get_elem(atom, Es), Context) ++ [" = "] ++ t_utype(kz_xml:get_elem(type, Es), Context).

t_abstype(E, Es, Context) ->
    Name = t_name(kz_xml:get_elem(erlangName, Es), Context),
    case kz_xml:get_elem(type, Es) of
        [] ->
            see(E, [Name, "()"], Context);
        Ts ->
            see(E, [Name], Context) ++ ["("] ++ seq(t_utype_elem_fun(Context), Ts, [")"])
    end.

t_abstype(Es, Context) ->
    [t_name(kz_xml:get_elem(erlangName, Es), Context), "("]
        ++ seq(t_utype_elem_fun(Context), kz_xml:get_elem(type, Es), [")"]).

t_union(Es, Context) ->
    seq(t_utype_elem_fun(Context), Es, " | ", []).

%%%=============================================================================
%%% Edoc typedef/spec_type Tags to Erlang AST functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec ot_utype(any()) -> any().
ot_utype([E]) ->
    ot_utype_elem(E).

ot_utype_elem(E=#xmlElement{content = Es}) ->
    case kz_xml:get_attrval(name, E) of
        "" -> ot_type(Es);
        N ->
            Name = {var,0,list_to_atom(N)},
            T = ot_type(Es),
            case T of
                Name -> T;
                T -> {ann_type,0,[Name, T]}
            end
    end.

ot_type([E=#xmlElement{name = typevar}]) ->
    ot_var(E);
ot_type([E=#xmlElement{name = atom}]) ->
    ot_atom(E);
ot_type([E=#xmlElement{name = integer}]) ->
    ot_integer(E);
ot_type([E=#xmlElement{name = range}]) ->
    ot_range(E);
ot_type([E=#xmlElement{name = binary}]) ->
    ot_binary(E);
ot_type([E=#xmlElement{name = float}]) ->
    ot_float(E);
ot_type([#xmlElement{name = nil}]) ->
    ot_nil();
ot_type([#xmlElement{name = paren, content = Es}]) ->
    ot_paren(Es);
ot_type([#xmlElement{name = list, content = Es}]) ->
    ot_list(Es);
ot_type([#xmlElement{name = nonempty_list, content = Es}]) ->
    ot_nonempty_list(Es);
ot_type([#xmlElement{name = tuple, content = Es}]) ->
    ot_tuple(Es);
ot_type([#xmlElement{name = map, content = Es}]) ->
    ot_map(Es);
ot_type([#xmlElement{name = 'fun', content = Es}]) ->
    ot_fun(Es);
ot_type([#xmlElement{name = record, content = Es}]) ->
    ot_record(Es);
ot_type([#xmlElement{name = abstype, content = Es}]) ->
    ot_abstype(Es);
ot_type([#xmlElement{name = union, content = Es}]) ->
    ot_union(Es).

ot_var(E) ->
    {var, 0, list_to_atom(kz_xml:get_attrval(name, E))}.

ot_atom(E) ->
    Name = list_to_atom(kz_xml:get_attrval(value, E)),
    {atom, erl_anno:new(0), Name}.

ot_integer(E) ->
    {integer, 0, list_to_integer(kz_xml:get_attrval(value, E))}.

ot_range(E) ->
    [I1, I2] = string:tokens(kz_xml:get_attrval(value, E), "."),
    {type, 0, range, [{integer, 0, list_to_integer(I1)}
                     ,{integer, 0, list_to_integer(I2)}
                     ]
    }.

ot_binary(E) ->
    {Base, Unit} =
        case string:tokens(kz_xml:get_attrval(value, E), ",:*><") of
            [] ->
                {0, 0};
            ["_", B] ->
                {list_to_integer(B), 0};
            ["_", "_", U] ->
                {0, list_to_integer(U)};
            ["_", B, _, "_", U] ->
                {list_to_integer(B), list_to_integer(U)}
        end,
    {type, 0, binary, [{integer, 0, Base}, {integer, 0, Unit}]}.

ot_float(E) ->
    {float, 0, list_to_float(kz_xml:get_attrval(value, E))}.

ot_nil() ->
    {nil, 0}.

ot_paren(Es) ->
    {paren_type, 0, [ot_utype(kz_xml:get_elem(type, Es))]}.

ot_list(Es) ->
    {type, 0, list, [ot_utype(kz_xml:get_elem(type, Es))]}.

ot_nonempty_list(Es) ->
    {type, 0, nonempty_list, [ot_utype(kz_xml:get_elem(type, Es))]}.

ot_tuple(Es) ->
    {type, 0, tuple, [ot_utype_elem(E) || E <- Es]}.

ot_map(Es) ->
    {type, 0, map, [ot_map_field(E) || E <- kz_xml:get_elem(map_field,Es)]}.

ot_map_field(#xmlElement{content=[K,V]}=E) ->
    A = case kz_xml:get_attrval(assoc_type, E) of
            "assoc" -> map_field_assoc;
            "exact" -> map_field_exact
        end,
    {type, 0, A, [ot_utype_elem(K), ot_utype_elem(V)]}.

ot_fun(Es) ->
    Range = ot_utype(kz_xml:get_elem(type, Es)),
    Args = [ot_utype_elem(A) || A <- kz_xml:get_content(argtypes, Es)],
    {type, 0, 'fun', [{type,0,product,Args},Range]}.

ot_record(Es) ->
    {type, 0, record, [ot_type(kz_xml:get_elem(atom, Es))
                       | [ot_field(F) || F <- kz_xml:get_elem(field, Es)]
                      ]
    }.

ot_field(#xmlElement{content = Es}) ->
    {type, 0, field_type, [ot_type(kz_xml:get_elem(atom, Es))
                          ,ot_utype(kz_xml:get_elem(type, Es))
                          ]
    }.

ot_abstype(Es) ->
    ot_name(kz_xml:get_elem(erlangName, Es)
           ,[ot_utype_elem(Elem) || Elem <- kz_xml:get_elem(type, Es)]
           ).

ot_union(Es) ->
    {type, 0, union, [ot_utype_elem(E) || E <- Es]}.

ot_name(Es, T) ->
    case ot_name(Es) of
        [Mod, ":", Atom] ->
            {remote_type, 0, [{atom, 0, list_to_atom(Mod)}
                             ,{atom,0,list_to_atom(Atom)},T
                             ]
            };
        "tuple" when T =:= [] ->
            {type, 0, tuple, any};
        Atom ->
            {type, 0, list_to_atom(Atom), T}
    end.

ot_name([E]) ->
    Atom = kz_xml:get_attrval(name, E),
    case kz_xml:get_attrval(module, E) of
        "" -> Atom;
        M ->
            case kz_xml:get_attrval(app, E) of
                "" -> [M, ":", Atom];
                A -> ["//" ++ A ++ "/" ++ M, ":", Atom] %% EDoc only!
            end
    end.

%%%=============================================================================
%%% Linkage functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sees(kz_types:xml_els(), map()) -> [kz_html:exported()].
sees(Es, Context) ->
    [export_content(see(E, Context), Context) || E <- kz_xml:get_elem(see, Es)].

-spec see(kz_types:xml_el(), map()) -> kz_html:exporties().
see(E=#xmlElement{content = Es}, Context) ->
    see(E, Es, Context).

-spec see(kz_types:xml_el(), kz_types:xml_els() | [string()], map()) -> kz_html:exporties().
see(E, Es, Context) ->
    case href(E, Context) of
        [] -> Es;
        Ref ->
            [{a, Ref, Es}]
    end.

-spec href(kz_types:xml_el(), map()) -> [{target, string()} | {href, string()}].
href(E, Context) ->
    case kz_fix_link(kz_xml:get_attrval(href, E), Context) of
        [] -> [];
        URI ->
            %% ?DEV_LOG("fixed link ~p", [URI]),
            T = case kz_xml:get_attrval(target, E) of
                    "" -> [];
                    S -> [{target, S}]
                end,
            [{href, URI} | T]
    end.

-spec anchor_id_label(string() | binary() | [string()] | [binary()], kz_types:xml_el()) -> {binary(), string() | binary() | [string()] | [binary()]}.
anchor_id_label(Content, E) ->
    case kz_xml:get_attrval(label, E) of
        "" -> {<<>>, Content};
        Ref -> {iolist_to_binary(Ref), Content}
    end.

%%%=============================================================================
%%% Common Tags functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Deprecated tag to proplist.
%% @end
%%------------------------------------------------------------------------------
-spec deprecated(kz_types:xml_els(), map()) -> kz_html:exported().
deprecated(Es, Context) ->
    description(full, kz_xml:get_content(deprecated, Es), Context).

-spec description(full | both, kz_types:xml_els(), map()) -> kz_html:exported() | [{short_desc | full_desc, kz_html:exported()}].
description(full, Es, Context) ->
    export_content(normalize_description(kz_xml:get_content(fullDescription, kz_xml:get_content(description, Es)), Context), Context);
%% description(short, Es, Context) ->
%%     export_content(normalize_description(kz_xml:get_content(briefDescription, kz_xml:get_content(description, Es)), Context), Context);
description(both, Es, Context) ->
    Desc = kz_xml:get_content(description, Es),
    Short = normalize_description(kz_xml:get_content(briefDescription, Desc), Context),
    Full = normalize_description(kz_xml:get_content(fullDescription, Desc), Context),
    %% ?DEV_LOG("Full ~p", [Full]),
    filter_empty(
      [{short_desc, export_content(Short, Context)}
      ,{full_desc, export_content(Full, Context)}
      ]
     ).

-spec since(kz_types:xml_els(), map()) -> kz_html:exported().
since(Es, Context) ->
    export_content(kz_xml:get_content(since, Es), Context).

-spec todos(kz_types:xml_els(), map()) -> [kz_html:exported()].
todos(Es, Context) ->
    [export_content(normalize_description(C, Context), Context)
     || #xmlElement{content = C} <- kz_xml:get_elem(todo, Es)
    ].

%%------------------------------------------------------------------------------
%% @doc Replaces types with their link.
%% @end
%%------------------------------------------------------------------------------
-spec etypef(any(), any(), any()) -> any().
etypef(L, O0, Context) ->
    {R, O} = etypef(L, [], O0, [], Context),
    {lists:reverse(R), O}.

etypef([C | L], St, [C | O], R, Context) ->
    etypef(L, St, O, [[C] | R], Context);
etypef(" "++L, St, O, R, Context) ->
    etypef(L, St, O, R, Context);
etypef("", [Cs | St], O, R, Context) ->
    etypef(Cs, St, O, R, Context);
etypef("", [], O, R, _Context) ->
    {R, O};
etypef(L, St, " "++O, R, Context) ->
    etypef(L, St, O, [" " | R], Context);
etypef(L, St, "\n"++O, R, Context) ->
    Ss = lists:takewhile(fun(C) -> C =:= $\s end, O),
    etypef(L, St, lists:nthtail(length(Ss), O), ["\n"++Ss | R], Context);
etypef([{a, HRef, S0} | L], St, O0, R, Context) ->
    {S, O} = etypef(S0, app_fix(O0, Context), Context),
    etypef(L, St, O, [{a, HRef, S} | R], Context);
etypef("="++L, St, "::"++O, R, Context) ->
    %% EDoc uses "=" for record field types; Erlang types use "::".
    %% Maybe there should be an option for this, possibly affecting
    %% other similar discrepancies.
    etypef(L, St, O, ["=" | R], Context);
etypef([Cs | L], St, O, R, Context) ->
    etypef(Cs, [L | St], O, R, Context).

app_fix(L, Context) ->
    try
        {"//" ++ R1,L2} = app_fix1(L, 1),
        [App, Mod] = string:tokens(R1, "/"),
        Res = "//" ++ atom(App, Context) ++ "/" ++ atom(Mod, Context) ++ L2,
        ?DEV_LOG("app_fix L ~p Res ~p", [L, Res]),
        Res
    catch _:_ -> L
    end.

app_fix1(L, I) -> % a bit slow
    {L1, L2} = lists:split(I, L),
    case erl_scan:tokens([], L1 ++ ". ", 1) of
        {done, {ok,[{atom,_,Atom}|_],_}, _} -> {atom_to_list(Atom), L2};
        _ -> app_fix1(L, I+1)
    end.

%%%=============================================================================
%%% XML Utility functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Export simple content of elements to HTML.
%% @end
%%------------------------------------------------------------------------------
-spec export_content(kz_html:exporty() | kz_html:exporties(), map()) -> kz_html:exported().
export_content(ErlHtml, #{kz_export_type := xmerl_html}) ->
    kz_html:export_content(ErlHtml, 'xmerl_html');
export_content(ErlHtml, #{kz_export_type := xmerl_xml}) ->
    kz_html:export_content(ErlHtml, 'xmerl_xml');
export_content(ErlHtml, _) ->
    kz_html:export_content(ErlHtml, 'xmerl_html').

-spec t_name(kz_types:xml_els(), map()) -> string().
t_name([E], Context) ->
    N = kz_xml:get_attrval(name, E),
    case kz_xml:get_attrval(module, E) of
        "" -> atom(N, Context);
        M ->
            S = atom(M, Context) ++ ":" ++ atom(N, Context),
            case kz_xml:get_attrval(app, E) of
                "" -> S;
                A -> "//" ++ atom(A, Context) ++ "/" ++ S
            end
    end.

-spec atom(string(), map()) -> string().
%% Commenting until switching to Erlang-20
%% atom(String, #opts{encoding = latin1}) ->
%%     io_lib:write_atom_as_latin1(list_to_atom(strip_tickie(String)));
atom(String, _Context) ->
    io_lib:write_atom(list_to_atom(strip_tickie(String))).

-spec strip_tickie(string()) -> string().
strip_tickie("'"++Rest) ->
    strip_tickie(Rest);
strip_tickie(String) ->
    case lists:last(String) of
        39 -> lists:droplast(String);
        _ -> String
    end.

%%------------------------------------------------------------------------------
%% @doc Why EDoc is so stupid to not make a paragraph of last text/markups
%% if they are not a block level elements?
%% This is a copy pasta code from `edoc_wiki' with a change to remove
%% stupid whitespace only `#xmlText{}'.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_description(kz_types:xml_els(), map()) -> kz_html:exporties().
normalize_description([], _) ->
    [];
normalize_description(Es, Context) ->
    normalize_links(kz_html:make_paraghraph(Es), Context).

%%%=============================================================================
%%% Utility functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Fix links to Kazoo applications, modules and types.
%% @end
%%------------------------------------------------------------------------------
-spec kz_fix_link(string(), map()) -> string().
kz_fix_link("", _) ->
    [];
kz_fix_link(URI, #{file_suffix := Suffix
                  ,kz_rel_path := RelPath
                  ,kz_apps_uri := AppsUri
                  ,kz_link_apps := Apps
                  ,kz_link_mods := Mods
                  }=Context) ->
    App = maps:get(kz_app_name, Context, undefined),
    Mod = maps:get(kz_mod_name, Context, undefined),

    %% ?DEV_LOG("checking link ~p", [URI]),

    case re:run(URI, "(([a-z_]+)/doc/)?([a-z_]+)\\" ++ Suffix ++ "(#.*)?", [{capture, [2, 3, 4], list}]) of
        nomatch ->
            URI;
        {match, [App, Mod, Fragment]} ->
            Mod ++ Suffix ++ Fragment;
        {match, [App, SomeFile, Fragment]} ->
            SomeFile ++ Suffix ++ Fragment;
        {match, [[], SomeFile, Fragment]} ->
            case get_kazoo_app_or_mod(SomeFile, Apps(SomeFile), Mods(SomeFile)) of
                undefined -> [];
                AppSlashMod ->
                    RelPath ++ AppsUri ++ "/" ++ AppSlashMod ++ Suffix ++ Fragment
            end;
        {match, [OtherApp, OtherMod, Fragment]} ->
            %% ?DEV_LOG("OtherApp ~p OtherMod ~p, KA ~p KM ~p", [OtherApp, OtherMod, Apps(OtherApp), Mods(OtherMod)]),
            case get_kazoo_app_or_mod(OtherMod, Apps(OtherApp), Mods(OtherMod)) of
                undefined -> [];
                AppSlashMod ->
                    RelPath ++ AppsUri ++ "/" ++ AppSlashMod ++ Suffix ++ Fragment
            end
    end.

-spec get_kazoo_app_or_mod(string(), kz_term:api_string(), kz_term:api_string()) -> kz_term:api_string().
get_kazoo_app_or_mod(_, undefined, undefined) ->
    %% don't create link for deps or erlang (mostly occurred for behaviour, e.g. gen_server) or other
    %% non-kazoo app.
    %% Also don't create links for kazoo apps/mods which we are not creating document for them (kz_docgen was called
    %% with specific app names)

    %% ?DEV_LOG("undefined undefined", []),
    undefined;
get_kazoo_app_or_mod(ModName, undefined, {AppName, _AppCat}) ->
    %% ?DEV_LOG("1 AppName ~p ModName ~p", [AppName, ModName]),
    AppName ++ "/" ++ ModName;
get_kazoo_app_or_mod(ModName, _AppCat, {AppName, _AppCat}) ->
    %% ?DEV_LOG("2 AppName ~p ModName ~p", [AppName, ModName]),
    AppName ++ "/" ++ ModName;
get_kazoo_app_or_mod(AppName, _AppCat, undefined) ->
    %% ?DEV_LOG("3 AppName ~p AppCat ~p", [AppName, _AppCat]),
    AppName ++ "/".

-spec normalize_links(kz_html:exporties(), map()) -> kz_html:exporties().
normalize_links(Es, Context) ->
    lists:reverse(normalize_links(Es, Context, [])).

-spec normalize_links(kz_types:xml_els(), map(), kz_html:exporties()) -> kz_html:exporties().
normalize_links([], _, Acc) ->
    Acc;
normalize_links([#xmlElement{name = a, attributes = Attributes}=E|Es], Context, Acc) ->
    normalize_links(Es, Context, normalize_link(E, kz_xml:get_attr(href, Attributes), Context) ++ Acc);
normalize_links([#xmlElement{content = Content}=E|Es], Context, Acc) ->
    normalize_links(Es, Context, [E#xmlElement{content = normalize_links(Content, Context)} | Acc]);
%% normalize_links([{a, Attributes, String}|Es], Context, Acc) ->
%%     URI = proplists:get_value(href, Attributes, undefined),
%%     case URI =/= undefined
%%         andalso kz_fix_link(URI, Context)
%%     of
%%         false ->
%%             normalize_links(Es, Context, [{a, Attributes, String} | Acc]);
%%         [] ->
%%             normalize_links(Es, Context, [String | Acc]);
%%         FixedURI ->
%%             normalize_links(Es, Context, [{a, [{href, FixedURI} | proplists:delete(href, Attributes)], String} | Acc])
%%     end;
normalize_links([E|Es], Context, Acc) ->
    normalize_links(Es, Context, [E | Acc]).

normalize_link(#xmlElement{content = Es}=E, [], Context) ->
    [E#xmlElement{content = normalize_links(Es, Context)}];
normalize_link(#xmlElement{}=E, [#xmlAttribute{value = URI}=Href], Context) ->
    fix_href_attribute(E, Href, kz_fix_link(URI, Context), Context).

fix_href_attribute(#xmlElement{content = Es}, _Href, [], Context) ->
    normalize_links(Es, Context);
fix_href_attribute(#xmlElement{content = Es, attributes = Attributes}=E, Href, URI, Context) ->
    [E#xmlElement{content = normalize_links(Es, Context)
                 ,attributes = [Href#xmlAttribute{value = URI}
                                | [Attr
                                   || #xmlAttribute{name = AttrName}=Attr <- Attributes,
                                      AttrName =/= href
                                  ]
                               ]
                 }
    ].

%%------------------------------------------------------------------------------
%% @doc Run a sequence of functions over elements, optionally adding
%% separator characters.
%% @end
%%------------------------------------------------------------------------------
-spec seq(fun((kz_types:xml_el()) -> kz_html:exporty()), kz_types:xml_els()) -> kz_html:exporties().
seq(F, Es) ->
    seq(F, Es, []).

-spec seq(fun((kz_types:xml_el()) -> kz_html:exporty()), kz_types:xml_els(), [string()]) -> kz_html:exporties().
seq(F, Es, Tail) ->
    seq(F, Es, ", ", Tail).

-spec seq(fun((kz_types:xml_el()) -> kz_html:exporty()), kz_types:xml_els(), string(), [string()]) -> kz_html:exporties().
seq(F, [E], _Sep, Tail) ->
    F(E) ++ Tail;
seq(F, [E | Es], Sep, Tail) ->
    F(E) ++ [Sep] ++ seq(F, Es, Sep, Tail);
seq(_F, [], _Sep, Tail) ->
    Tail.

-spec filter_empty([{atom(), any()}]) -> [{atom(), any()}].
filter_empty(Props) ->
    [P || P <- Props, is_not_empty(P)].

-spec is_not_empty({atom(), any()}) -> boolean().
is_not_empty({_, []}) -> 'false';
is_not_empty({_, <<>>}) -> 'false';
is_not_empty(_V) -> 'true'.
