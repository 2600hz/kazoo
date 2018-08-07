%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fax_attachment).

-export([save/4, fetch/3]).


%%------------------------------------------------------------------------------
%% @doc wrapper function for saving fax attachments
%%
%% @end
%%------------------------------------------------------------------------------
-spec save(kz_term:ne_binary(), kz_json:object(), kz_term:api_binary(), kz_term:api_binary()) ->
                  {'ok', kz_json:object()} |
                  {'error', any()}.
save(Db, Doc, Original, ContentType) ->
    kzd_fax:save_outbound_fax(Db, Doc, Original, ContentType).

%%------------------------------------------------------------------------------
%% @doc wrapper function for fetching attachments
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                   {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                   {'error', kz_term:ne_binary()}.
fetch(Format, Db, Doc) ->
    kzd_fax:fetch_attachment_format(Format, Db, Doc).
