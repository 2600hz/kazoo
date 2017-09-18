%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
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

-spec parse_transform(forms(), options()) -> forms().
parse_transform(Forms, Options) ->
    case parse_trans:depth_first(fun xform_fun/4
                                ,[]
                                ,Forms
                                ,Options
                                )
    of
        {error, Es} ->
            Es ++ Forms;
        {NewForms, _} ->
            parse_trans:revert(NewForms)
    end.

tree(N, Mod, Fun, Ari, Args) ->
    Attr = {attr,N,[],none},
    {tree,application,
     Attr,
     {application,
      {tree,module_qualifier,
       Attr,
       {module_qualifier,{atom,N,kzs_perf},{atom,N,profile}}},
      [{tree,tuple,Attr,[{atom,N,Mod},{atom,N,Fun},{integer,N,Ari}]},
       {tree,list, Attr,{list,Args,none}}]}}.

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
transform({kzs_doc=Mod, {ensure_saved=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_doc=Mod, {del_doc=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_doc=Mod, {del_docs=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform({kzs_attachments=Mod, {fetch_attachment=Fun, 4=Ari}}, Form) ->
    yup(Mod, Fun, Ari, Form);
transform(_X, _Form) -> ko.

-type context() :: tuple().
-spec xform_fun(atom(), form(), context(), Acc) ->
                       {forms(), Acc} |
                       {'error', list()}.
xform_fun(application, Form, _Ctxt, Acc) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case transform(MFA, Form) of
        {'ok', NewForm} -> {NewForm, Acc};
        ko ->
            {Form, Acc}
    end;
xform_fun(_Type, Form, _Ctxt, Acc) ->
    {Form, Acc}.
