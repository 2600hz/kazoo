-ifndef(KZ_CONVERT_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(CHUNKSIZE, 24576).
-define(APP_NAME, <<"kazoo_convert">>).
-define(APP_VERSION, <<"1.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-define(TIFF_MIME, <<"image/tiff">>).
-define(PDF_MIME, <<"application/pdf">>).
-define(IMAGE_MIME_PREFIX, <<"image/">>).
-define(OPENXML_MIME_PREFIX, "application/vnd.openxmlformats-officedocument.").
-define(OPENOFFICE_MIME_PREFIX, "application/vnd.oasis.opendocument.").
-define(OPENOFFICE_COMPATIBLE(CT)
       ,(CT =:= <<"application/msword">>
             orelse CT =:= <<"application/vnd.ms-excel">>
             orelse CT =:= <<"application/vnd.ms-powerpoint">>
        )).

-define(TMP_DIR
       ,kapps_config:get_binary(?CONFIG_CAT, <<"file_cache_path">>, <<"/tmp/">>)).

-define(KZ_CONVERT_HRL, 'true').
-endif.
