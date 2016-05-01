%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------


-module(kz_perf_transform).

-export([parse_transform/2]).

-type form()    :: any().
-type forms()   :: [form()].
-type options() :: [{atom(), any()}].


-spec parse_transform(forms(), options()) ->
    forms().
parse_transform(Forms, Options) ->
    Trace = kz_trace_opt(Options, Forms),
    case parse_trans:depth_first(fun(T,F,C,A) ->
                     xform_fun(T,F,C,A,Forms, Trace)
                 end, [], Forms, Options) of
    {error, Es} ->
        Es ++ Forms;
    {NewForms, _} ->
        parse_trans:revert(NewForms)
    end.

kz_trace_opt(Options, Forms) ->
    case proplists:get_value(kz_expand_trace, Options) of
    undefined ->
        case [Opt || {attribute,_,kz_expand_trace,Opt} <- Forms] of
        [] ->
            [];
        [_|_] = L ->
            lists:last(L)
        end;
    Flags when is_list(Flags) ->
        Flags
    end.


tree(N, Mod, Fun, Ari, Args) ->
    {tree,application,
     {attr,N,[],none},
     {application,
      {tree,module_qualifier,
       {attr,N,[],none},
       {module_qualifier,{atom,N,kzs_perf},{atom,N,profile}}},
      [{tree,tuple,
        {attr,N,[],none},
        [{atom,N,Mod},{atom,N,Fun},{integer,N,Ari}]},
       {tree,list,
        {attr,N,[],none},
        {list, Args,
         none}}]}}. 

yup(Mod, Fun, Ari, Form) ->
    N = erl_syntax:get_pos(Form),
    Args = erl_syntax:application_arguments(Form),
    {'ok', tree(N, Mod, Fun, Ari, Args)}.

transform({kzs_view=Mod, {get_results=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_view=Mod, {get_results_count=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_doc=Mod, {open_doc=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_doc=Mod, {save_doc=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_doc=Mod, {save_docs=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_doc=Mod, {del_doc=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_doc=Mod, {del_docs=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_attachments=Mod, {fetch_attachment=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform(_X, _Form) -> ko.


xform_fun(application, Form, _Ctxt, Acc, _Forms, _Trace) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case transform(MFA, Form) of
        {'ok', NewForm} -> {NewForm, Acc};
        ko ->
            {Form, Acc}
    end;
xform_fun(_X, Form, _Ctxt, Acc, _, _) ->
    {Form, Acc}.
