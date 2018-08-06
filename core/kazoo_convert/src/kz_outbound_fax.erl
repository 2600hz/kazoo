%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_outbound_fax).

-export([save/4]).


%%------------------------------------------------------------------------------
%% @doc Faxes pre-convert attachment documents to tiff/pdf on ingress and save
%% all these formats to the db as attachments.
%%
%% If configured, the fax document for outbound faxes will contain a copy of the
%% post conversion fax tiff file and a pdf representation of this file.
%%
%% @end
%%------------------------------------------------------------------------------
-spec save(kz_term:ne_binary(), kz_json:object(), kz_term:api_binary(), kz_term:api_binary()) ->
                               {'ok', kz_json:object()} |
                               {'error', any()}.
save(Db, Doc, Original, ContentType) ->
    kzd_fax:save_outbound_fax(Db, Doc, Original, ContentType).

