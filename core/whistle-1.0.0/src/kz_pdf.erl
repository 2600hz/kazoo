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

-export([find_template/2
         ,find_template/3
        ]).
-export([generate/2
         ,generate/3
        ]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(CONFIG_CAT, <<"kazoo">>).
-define(TEMPLATE_DOC_ID(Type), <<"pdf.", Type/binary>>).

-define(PDF_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".pdf">>).
-define(HTML_TO_PDF, <<"/usr/bin/htmldoc --quiet --webpage -f $pdf$ $html$">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_template(ne_binary(), wh_proplist() | ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
find_template(AccountId, DocType) when is_binary(DocType)  ->
    find_template(AccountId, DocType, <<DocType/binary, ".tmpl">>);
find_template(AccountId, Props) ->
    DocType = props:get_first_defined([<<"type">>, <<"pvt_type">>], Props),
    find_template(AccountId, Props, <<DocType/binary, ".tmpl">>).

-spec find_template(ne_binary(), wh_proplist() | ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
find_template(AccountId, DocType, AttachmentId) when is_binary(DocType) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:fetch_attachment(AccountDb, ?TEMPLATE_DOC_ID(DocType), AttachmentId) of
        {'ok', _}=OK -> OK;
        {'error', _R} ->
            lager:error("failed to find template ~s/~s in ~s: ~p"
                       ,[?TEMPLATE_DOC_ID(DocType), AttachmentId, AccountDb, _R]),
            default_template(DocType, AttachmentId)
    end;
find_template(AccountId, Props, AttachmentId) ->
    DocType = props:get_first_defined([<<"type">>, <<"pvt_type">>], Props),
    find_template(AccountId, DocType, AttachmentId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec generate(ne_binary(), wh_proplist()) -> {'ok', ne_binary()} | {'error', any()}.
-spec generate(ne_binary(), wh_proplist(), ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
generate(AccountId, Props) ->
    case find_template(AccountId, Props) of
        {'error', _R}=Error -> Error;
        {'ok', Template} ->
           generate(AccountId, Props, Template)
    end.

generate(Account, Props, Template) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    DocType = props:get_first_defined([<<"type">>, <<"pvt_type">>], Props),

    Rand = wh_util:rand_hex_binary(5),
    Renderer = wh_util:to_atom(<<"kz_pdf_", DocType/binary, "_", Rand/binary>>, 'true'),
    {'ok', Renderer} = erlydtl:compile_template(Template, Renderer, [{'out_dir', 'false'}]),
    {'ok', Rendered} = Renderer:render(Props),

    code:purge(Renderer),
    code:delete(Renderer),

    Prefix = <<AccountId/binary, "-", DocType/binary, "-", Rand/binary>>,
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
        [] -> file:read_file(PDFFile);
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
-spec default_template(ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
default_template(DocType, AttachmentId) ->
    lager:debug("searching for default template ~s", [AttachmentId]),
    case couch_mgr:fetch_attachment(?WH_CONFIG_DB, ?TEMPLATE_DOC_ID(DocType), AttachmentId) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} -> maybe_create_default_template(DocType, AttachmentId);
        {'error', _R}=Error ->
            lager:error("failed to find  default template ~s/~s : ~p"
                        ,[?TEMPLATE_DOC_ID(DocType), AttachmentId, _R]
                       ),
            Error
    end.

-spec maybe_create_default_template(ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
maybe_create_default_template(DocType, AttachmentId) ->
    PrivDir = code:priv_dir('crossbar'),
    TemplateFile = filename:join([PrivDir, <<"couchdb">>, <<"templates">>, AttachmentId]),
    lager:debug("loading template from ~s", [TemplateFile]),
    case file:read_file(TemplateFile) of
        {'error', _R}=Error ->
            lager:error("failed to find default template file ~s : ~p", [TemplateFile, _R]),
            Error;
        {'ok', Template} -> create_default_template(Template, DocType, AttachmentId)
    end.

-spec create_default_template(binary(), ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
create_default_template(Template, DocType, AttachmentId) ->
    lager:debug("creating default template ~s", [DocType]),
    Default = wh_json:from_list([{<<"template_name">>, DocType}]),
    JObj =
        wh_doc:update_pvt_parameters(
          wh_json:from_list(
            [{<<"_id">>, ?TEMPLATE_DOC_ID(DocType)}
             ,{<<"default">>, Default}
            ]
           )
          ,?WH_CONFIG_DB
          ,[{'type', <<"config">>}]
         ),
    case couch_mgr:save_doc(?WH_CONFIG_DB, JObj) of
        {'ok', _} -> save_default_attachment(Template, DocType, AttachmentId);
        {'error', 'conflict'} -> save_default_attachment(Template, DocType, AttachmentId);
        {'error', _R}=Error ->
            lager:error("failed to create default template doc for ~s : ~p"
                         ,[?TEMPLATE_DOC_ID(DocType), _R]
                       ),
            Error
    end.

-spec save_default_attachment(binary(), ne_binary(), ne_binary()) -> {'ok', ne_binary()} | {'error', any()}.
save_default_attachment(Template, DocType, AttachmentId) ->
    lager:debug("saving default template ~s attachment", [DocType]),
    case
        couch_mgr:put_attachment(
          ?WH_CONFIG_DB
          ,?TEMPLATE_DOC_ID(DocType)
          ,AttachmentId
          ,Template
         )
    of
        {'error', _R}=Error ->
            lager:error("failed to save default template attachment for ~s : ~p"
                        ,[?TEMPLATE_DOC_ID(DocType), _R]
                       ),
            Error;
        {'ok', _} -> {'ok', Template}
    end.
