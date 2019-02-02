-ifndef(KZ_AUDIO_CONVERTER_HRL).

-include_lib("kazoo_convert/include/kz_convert.hrl").

-define(CONVERT_AUDIO_CMD, <<"ffmpeg -n -loglevel warning -i $FROM $TO">>).
-define(READ_METADATA_CMD, <<"ffmpeg -y -loglevel error -i $FROM -f ffmetadata /dev/stdout">>).
-define(VALIDATE_AUDIO_CMD, <<"ffmpeg -y -loglevel error -i $FROM -f ffmetadata /dev/stdout">>).

-define(CONVERT_AUDIO_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"audio">>, <<"convert_audio_command">>], ?CONVERT_AUDIO_CMD)).
-define(READ_METADATA_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"audio">>, <<"read_metadata_command">>], ?READ_METADATA_CMD)).
-define(VALIDATE_AUDIO_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"audio">>, <<"validate_audio_command">>], ?VALIDATE_AUDIO_CMD)).

-define(AUDIO_CONVERT_TIMEOUT
       ,kapps_config:get_integer(?CONFIG_CAT, [<<"audio">>, <<"convert_command_timeout">>], 180 * ?MILLISECONDS_IN_SECOND)).

-define(KZ_AUDIO_CONVERTER_HRL, 'true').
-endif.
