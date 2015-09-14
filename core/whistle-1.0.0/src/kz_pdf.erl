%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015 2600Hz, INC
%%% @doc
%%% Builds an LOA doc using HTMLDoc (http://www.msweet.org/projects.php?Z1)
%%% @end
%%% @contributors:
%%%   Peter Defebvre
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_pdf).


-export([generate/2]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(CONFIG_CAT, <<"kazoo">>).

-define(TEMPLATE_ATTACHMENT_ID, <<"template">>).
-define(TEMPLATE_DOC_ID(Type), <<"template-to-pdf.", Type/binary>>).

-define(PDF_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".pdf">>).
-define(HTML_TO_PDF, <<"/usr/bin/htmldoc --quiet --webpage -f $pdf$ $html$">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec generate(ne_binary(), wh_proplist()) -> {'ok', ne_binary()} | {'error', any()}.
-spec generate(ne_binary(), wh_proplist(), ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
generate(AccountId, Props) ->
    DocType = props:get_first_defined([<<"type">>, <<"pvt_type">>], Props),
    case find_template(AccountId, DocType) of
        {'error', _R}=Error -> Error;
        {'ok', Template} ->
           generate(AccountId, Props, Template)
    end.

generate(AccountId, Props, Template) ->
    DocType = props:get_first_defined([<<"type">>, <<"pvt_type">>], Props),
    Renderer = wh_util:to_atom(<<"kz_pdf_", DocType/binary>>, 'true'),
    {'ok', Renderer} = erlydtl:compile_template(Template, Renderer, [{'out_dir', 'false'}]),

    {'ok', Rendered} = Renderer:render(Props),

    code:purge(Renderer),
    code:delete(Renderer),

    Prefix = <<AccountId/binary, "-", DocType/binary>>,
    HTMLFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".html">>]),
    PDFFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".pdf">>]),

    'ok' = file:write_file(HTMLFile, Rendered),

    RawCmd = whapps_config:get(?PDF_CONFIG_CAT, <<"html2pdf">>, ?HTML_TO_PDF),
    Cmd = lists:foldl(fun cmd_fold/2
                      ,RawCmd
                      ,[{<<"$pdf$">>, PDFFile}
                        ,{<<"$html$">>, HTMLFile}
                       ]
                     ),
    lager:debug("exec ~s", [Cmd]),
    case os:cmd(wh_util:to_list(Cmd)) of
        [] ->
            file:read_file(PDFFile);
        _R ->
            lager:error("failed to exec ~s: ~s", [Cmd, _R]),
            {'error', _R}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cmd_fold({ne_binary(), ne_binary()}, ne_binary()) -> ne_binary().
cmd_fold({Search, Replace}, Subject) ->
    binary:replace(Subject, Search, Replace).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_template(ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
find_template(AccountId, DocType) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:fetch_attachment(AccountDb, ?TEMPLATE_DOC_ID(DocType), ?TEMPLATE_ATTACHMENT_ID) of
        {'ok', _}=OK -> OK;
        {'error', _R} ->
            lager:error("failed to find template ~s in ~s: ~p", [DocType, AccountDb, _R]),
            default_template(DocType)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec default_template(ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
default_template(DocType) ->
    lager:debug("searching for default template ~s", [DocType]),
    case couch_mgr:fetch_attachment(?WH_CONFIG_DB, ?TEMPLATE_DOC_ID(DocType), ?TEMPLATE_ATTACHMENT_ID) of
        {'ok', _}=OK -> OK;
        {'error', _R} ->
            lager:error("failed to find  default template ~s : ~p", [DocType, _R]),
            create_default_template(DocType)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_default_template(ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
create_default_template(DocType) ->
    lager:debug("creating default template ~s", [DocType]),
    JObj =
        wh_json:from_list([
            {<<"template_name">>, DocType}
            ,{<<"_id">>, ?TEMPLATE_DOC_ID(DocType)}
        ]),
    case couch_mgr:save_doc(?WH_CONFIG_DB, JObj) of
        {'error', _R}=Error ->
            lager:error("failed to create default template doc for ~s : ~p", [DocType, _R]),
            Error;
        {'ok', _} ->
            save_default_template(DocType)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_default_template(ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
save_default_template(DocType) ->
    lager:debug("saving default template ~s", [DocType]),

    PrivDir = code:priv_dir('kazoo'),
    TemplateFile = filename:join([PrivDir, <<"couchdb">>, <<"templates">>, <<DocType/binary, ".tmpl">>]),
    lager:debug("loading template from ~s", [TemplateFile]),

    case file:read_file(TemplateFile) of
        {'error', _R}=Error ->
            lager:error("failed to find default template file ~s : ~p", [TemplateFile, _R]),
            _ = couch_mgr:del_doc(?WH_CONFIG_DB, ?TEMPLATE_DOC_ID(DocType)),
            Error;
        {'ok', Template} ->
            case
                couch_mgr:put_attachment(
                    ?WH_CONFIG_DB
                    ,?TEMPLATE_DOC_ID(DocType)
                    ,?TEMPLATE_ATTACHMENT_ID
                    ,Template
                )
            of
                {'error', _R}=Error ->
                    lager:error("failed to save default template attachment for ~s : ~p", [DocType, _R]),
                    _ = couch_mgr:del_doc(?WH_CONFIG_DB, ?TEMPLATE_DOC_ID(DocType)),
                    Error;
                {'ok', _} -> {'ok', Template}
            end
    end.
