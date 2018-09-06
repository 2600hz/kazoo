-ifndef(KZ_FAX_CONVERTER_HRL).

-include_lib("kazoo_convert/include/kz_convert.hrl").

-define(TIFF_TO_PDF_CMD, <<"tiff2pdf -o $TO $FROM">>).
-define(CONVERT_PDF_CMD
       ,<<"/usr/bin/gs -q "
          "-r200x200 "
          "-g1728x2200 "
          "-dPDFFitPage "
          "-dNOPAUSE "
          "-dBATCH "
          "-dSAFER "
          "-sDEVICE=tiffg4 "
          "-sOutputFile=$TO -- $FROM"
        >>
       ).
-define(RESAMPLE_IMAGE_CMD, <<"convert $FROM "
                              "-resample 200x200 "
                              "-page +0+0 "
                              "-compress group4 $TO"
                            >>
       ).
-define(CONVERT_IMAGE_CMD, <<"convert $FROM "
                             "-resample 200x200 "
                             "-units PixelsPerInch "
                             "-size 1728x2200 "
                             "-compress group4 $TO"
                           >>
       ).
-define(RESIZE_TIFF_CMD, <<"convert $FROM "
                           "-resample 200x200 "
                           "-units PixelsPerInch "
                           "-resize 1728\\!x2200 "
                           "-compress group4 $TO"
                         >>
       ).
-define(EMBIGGEN_TIFF_CMD, <<"convert $FROM "
                             "-gravity center "
                             "-resample 200x200 "
                             "-units PixelsPerInch "
                             "-extent 1728x2200 "
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

-define(TIFF_INFO_CMD, <<"tiffinfo $FILE "
                         "|egrep 'Page|Width|Resolution|Compression' "
                         "|sed 's|Image Width: \\([0-9]*\\) Image Length: \\([0-9]*\\)|Width: \\1\\nLength: \\2\\n|g' "
                         "|sed 's|Resolution: \\([0-9]*\\), \\([0-9]*\\)|X: \\1\\nY: \\2\\n|g' "
                         "|sed 's/^[ \\t]*//g'"
                       >>).

-define(COUNT_TIFF_PAGES_CMD, <<"echo -n `tiffinfo $FILE | grep 'Page Number' | grep -c 'P'`">>).

-define(VALIDATE_PDF_CMD, <<"gs -dNOPAUSE -dBATCH -sDEVICE=nullpage $FILE">>).

-define(VALIDATE_TIFF_CMD, <<"tiffinfo $FILE">>).

-define(RESAMPLE_IMAGE_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"fax">>, <<"resample_image_command">>], ?RESAMPLE_IMAGE_CMD)).
-define(CONVERT_IMAGE_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"fax">>, <<"convert_image_command">>], ?CONVERT_IMAGE_CMD)).
-define(LARGE_TIFF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"fax">>, <<"large_tiff_command">>], ?RESIZE_TIFF_CMD)).
-define(SMALL_TIFF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"fax">>, <<"small_tiff_command">>], ?EMBIGGEN_TIFF_CMD)).
-define(CONVERT_PDF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"fax">>, <<"convert_pdf_command">>], ?CONVERT_PDF_CMD)).
-define(VALIDATE_PDF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"fax">>, <<"validate_pdf_command">>], ?VALIDATE_PDF_CMD)).
-define(CONVERT_TIFF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"fax">>, <<"convert_tiff_command">>], ?TIFF_TO_PDF_CMD)).
-define(VALIDATE_TIFF_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"fax">>, <<"validate_tiff_command">>], ?VALIDATE_TIFF_CMD)).
-define(CONVERT_OPENOFFICE_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"fax">>, <<"convert_openoffice_command">>], ?CONVERT_OPENOFFICE_CMD)).

-define(SERIALIZE_OPENOFFICE
       ,kapps_config:get_is_true(?CONFIG_CAT, [<<"fax">>, <<"serialize_openoffice">>], true)).

-define(ENABLE_OPENOFFICE
       ,kapps_config:get_is_true(?CONFIG_CAT, [<<"fax">>, <<"enable_openoffice">>], true)).

-define(CONVERT_TIMEOUT
       ,kapps_config:get_integer(?CONFIG_CAT, [<<"fax">>, <<"convert_command_timeout">>], 180 * ?MILLISECONDS_IN_SECOND)).

-define(KZ_FAX_CONVERTER_HRL, 'true').
-endif.
