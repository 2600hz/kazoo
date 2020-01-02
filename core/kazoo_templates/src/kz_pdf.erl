%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Builds PDF from an HTML template using
%%% <a href="http://www.msweet.org/projects.php?Z1">HTMLDoc</a>.
%%% @author Peter Defebvre
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_pdf).

-export([find_template/2
        ,find_template/3
        ]).

-export([generate/2
        ,generate/3
        ]).

-export([error_empty/0]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

-define(CONFIG_CAT, <<"kazoo">>).
-define(TEMPLATE_DOC_ID(Type), <<"pdf.", Type/binary>>).
-define(ERROR_EMPTY, <<"error_empty.pdf">>).

-define(PDF_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".pdf">>).
-define(HTML_TO_PDF, <<"/usr/bin/htmldoc --quiet --webpage -f $pdf$ $html$">>).

-type ret() :: {'ok', kz_term:ne_binary()} | {'error', any()}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_template(kz_term:ne_binary(), kz_term:proplist() | kz_term:ne_binary()) -> ret().
find_template(AccountId, DocType) when is_binary(DocType)  ->
    find_template(AccountId, DocType, <<DocType/binary, ".tmpl">>);
find_template(AccountId, Props) ->
    DocType = props:get_first_defined([<<"type">>, <<"pvt_type">>], Props),
    find_template(AccountId, Props, <<DocType/binary, ".tmpl">>).

-spec find_template(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:proplist(), kz_term:ne_binary()) -> ret().
find_template(AccountId, DocType, AttachmentId) when is_binary(DocType) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    case kz_datamgr:fetch_attachment(AccountDb, ?TEMPLATE_DOC_ID(DocType), AttachmentId) of
        {'ok', _}=OK -> OK;
        {'error', _R} ->
            lager:error("failed to find template ~s/~s in ~s: ~p"
                       ,[?TEMPLATE_DOC_ID(DocType), AttachmentId, AccountDb, _R]),
            default_template(DocType, AttachmentId)
    end;
find_template(AccountId, Props, AttachmentId) ->
    DocType = props:get_first_defined([<<"type">>, <<"pvt_type">>], Props),
    find_template(AccountId, DocType, AttachmentId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec generate(kz_term:ne_binary(), kz_term:proplist()) -> ret().
generate(AccountId, Props) ->
    case find_template(AccountId, Props) of
        {'error', _R}=Error -> Error;
        {'ok', Template} ->
            generate(AccountId, Props, Template)
    end.

-spec generate(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binary()) -> ret().
generate(Account, Props, Template) ->
    AccountId = kzs_util:format_account_id(Account),
    DocType = props:get_first_defined([<<"type">>, <<"pvt_type">>], Props),

    Rand = kz_binary:rand_hex(5),
    %% TODO: fix that atom creation!
    Renderer = kz_term:to_atom(<<"kz_pdf_", DocType/binary, "_", Rand/binary>>, 'true'),
    {'ok', Renderer} = kz_template:compile(Template, Renderer),
    {'ok', Rendered} = kz_template:render(Renderer, Props),

    code:purge(Renderer),
    code:delete(Renderer),

    Prefix = <<AccountId/binary, "-", DocType/binary, "-", Rand/binary>>,
    HTMLFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".html">>]),
    PDFFile = filename:join([<<"/tmp">>, <<Prefix/binary, ".pdf">>]),

    'ok' = file:write_file(HTMLFile, Rendered),

    RawCmd = kapps_config:get_ne_binary(?PDF_CONFIG_CAT, <<"html2pdf">>, ?HTML_TO_PDF),
    Targets = [{<<"\$pdf\$">>, PDFFile}
              ,{<<"\$html\$">>, HTMLFile}
              ],
    Cmd = lists:foldl(fun cmd_fold/2, RawCmd, Targets),
    lager:debug("exec ~s", [Cmd]),
    case os:cmd(kz_term:to_list(Cmd)) of
        [] -> file:read_file(PDFFile);
        _R ->
            lager:error("failed to exec ~s: ~s", [Cmd, _R]),
            {'error', _R}
    end.

-spec error_empty() -> binary().
error_empty() ->
    PrivDir = code:priv_dir('crossbar'),
    FilePath = filename:join([PrivDir, <<"couchdb">>, <<"templates">>, ?ERROR_EMPTY]),
    case file:read_file(FilePath) of
        {'error', _R} -> <<>>;
        {'ok', File} -> File
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cmd_fold({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:ne_binary()) -> kz_term:ne_binary().
cmd_fold({Search, Replace}, Subject) ->
    binary:replace(Subject, Search, Replace).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec default_template(kz_term:ne_binary(), kz_term:ne_binary()) -> ret().
default_template(DocType, AttachmentId) ->
    lager:debug("searching for default template ~s", [AttachmentId]),
    case kz_datamgr:fetch_attachment(?KZ_CONFIG_DB, ?TEMPLATE_DOC_ID(DocType), AttachmentId) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} -> maybe_create_default_template(DocType, AttachmentId);
        {'error', _R}=Error ->
            lager:error("failed to find  default template ~s/~s : ~p"
                       ,[?TEMPLATE_DOC_ID(DocType), AttachmentId, _R]),
            Error
    end.

-spec maybe_create_default_template(kz_term:ne_binary(), kz_term:ne_binary()) -> ret().
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

-spec create_default_template(binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> ret().
create_default_template(Template, DocType, AttachmentId) ->
    lager:debug("creating default template ~s", [DocType]),
    Default = kz_json:from_list([{<<"template_name">>, DocType}]),
    JObj =
        kz_doc:update_pvt_parameters(kz_json:from_list(
                                       [{<<"_id">>, ?TEMPLATE_DOC_ID(DocType)}
                                       ,{<<"default">>, Default}
                                       ]
                                      )
                                    ,?KZ_CONFIG_DB
                                    ,[{'type', <<"config">>}]
                                    ),
    case kz_datamgr:save_doc(?KZ_CONFIG_DB, JObj) of
        {'ok', _} -> save_default_attachment(Template, DocType, AttachmentId);
        {'error', 'conflict'} -> save_default_attachment(Template, DocType, AttachmentId);
        {'error', _R}=Error ->
            lager:error("failed to create default template doc for ~s : ~p"
                       ,[?TEMPLATE_DOC_ID(DocType), _R]),
            Error
    end.

-spec save_default_attachment(binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> ret().
save_default_attachment(Template, DocType, AttachmentId) ->
    lager:debug("saving default template ~s attachment", [DocType]),
    case
        kz_datamgr:put_attachment(?KZ_CONFIG_DB
                                 ,?TEMPLATE_DOC_ID(DocType)
                                 ,AttachmentId
                                 ,Template
                                 )
    of
        {'error', _R}=Error ->
            lager:error("failed to save default template attachment for ~s : ~p"
                       ,[?TEMPLATE_DOC_ID(DocType), _R]),
            Error;
        {'ok', _} -> {'ok', Template}
    end.
