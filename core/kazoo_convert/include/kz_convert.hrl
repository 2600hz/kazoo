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
-define(TIFF_TO_PDF_CMD, <<"tiff2pdf -o $TO $FROM">>).
-define(CONVERT_PDF_CMD
       ,<<"/usr/bin/gs -q "
          "-r204x98 "
          "-g1728x1078 "
          "-dNOPAUSE "
          "-dBATCH "
          "-dSAFER "
          "-sDEVICE=tiffg4 "
          "-sOutputFile=$TO -- $FROM"
        >>
       ).
-define(CONVERT_IMAGE_CMD, <<"convert $FROM "
                             "-resample 204x98 "
                             "-units PixelsPerInch "
                             "-resize 1728x1078\! "
                             "-compress group4 $TO"
                           >>
       ).

-define(CONVERT_OPENOFFICE_CMD, <<"libreoffice "
                                  "--headless "
                                  "--convert-to pdf $FROM "
                                  "--outdir $WORKDIR "
                                  " 2>&1 "
                                  "|egrep 'parser error|Error' "
                                  "&& exit 1 || exit 0"
                                >>
       ).

-define(COUNT_TIFF_PAGES_CMD, <<"echo -n `tiffinfo $FILE | grep 'Page Number' | grep -c 'P'`">>).

-define(VALIDATE_PDF_CMD, <<"gs -dNOPAUSE -dBATCH -sDEVICE=nullpage $FILE">>).
-define(VALIDATE_TIFF_CMD, <<"tiffinfo $FILE">>).

-define(CONVERT_IMAGE_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"convert_image_command">>, ?CONVERT_IMAGE_CMD)).
-define(CONVERT_PDF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"convert_pdf_command">>, ?CONVERT_PDF_CMD)).
-define(VALIDATE_PDF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"validate_pdf_command">>, ?VALIDATE_PDF_CMD)).
-define(CONVERT_TIFF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"convert_tiff_command">>, ?TIFF_TO_PDF_CMD)).
-define(VALIDATE_TIFF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"validate_tiff_command">>, ?VALIDATE_TIFF_CMD)).
-define(CONVERT_OPENOFFICE_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, <<"convert_openoffice_command">>, ?CONVERT_OPENOFFICE_CMD)).

-define(TMP_DIR
       ,kapps_config:get_binary(?CONFIG_CAT, <<"file_cache_path">>, <<"/tmp/">>)).

-define(SERIALIZE_OPENOFFICE
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"serialize_openoffice">>, true)).

-define(ENABLE_OPENOFFICE
       ,kapps_config:get_is_true(?CONFIG_CAT, <<"enable_openoffice">>, true)).

-define(CONVERT_TIMEOUT
       ,kapps_config:get_integer(?CONFIG_CAT,<<"convert_command_timeout">> ,120 * ?MILLISECONDS_IN_SECOND)).

-define(KZ_CONVERT_HRL, 'true').
-endif.
