-ifndef(KZ_AUDIO_CONVERTER_HRL).

-include_lib("kazoo_convert/include/kz_convert.hrl").

-define(MP3_MIME, <<"audio/x-wav">>).
-define(WAV_MIME, <<"audio/x-wav">>).

-define(WAV_COMPATIBLE(CT)
       ,(CT =:= <<"audio/wav">>
             orelse CT =:= <<"audio/wave">>
             orelse CT =:= <<"audio/x-pn-wav">>
        )).

-define(CONVERT_AUDIO_CMD, <<"ffmpeg -i $FROM $TO">>).
-define(VALIDATE_AUDIO_CMD, <<"ffmpeg -i $FILE">>).

-define(CONVERT_AUDIO_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"audio">>, <<"convert_audio_command">>], ?CONVERT_AUDIO_CMD)).
-define(VALIDATE_AUDIO_COMMAND
       ,kapps_config:get_binary(?CONFIG_CAT, [<<"audio">>, <<"validate_audio_command">>], ?VALIDATE_AUDIO_CMD)).

-define(CONVERT_TIMEOUT
       ,kapps_config:get_integer(?CONFIG_CAT, [<<"audio">>, <<"convert_command_timeout">>], 180 * ?MILLISECONDS_IN_SECOND)).

-define(KZ_AUDIO_CONVERTER_HRL, 'true').
-endif.
