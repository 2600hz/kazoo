# Kazoo IM

This application provides functionality related to tracking a IM's metadata, executing commands and routing to external providers.

## kapps_im

Record containing information about a IM. Generally populated from `sms_inbound` and `mms_inbound` AMQP payloads. It can be serialized and passed to other Kazoo applications for tranferring control of the IM processing.

## kapps_im_command

Set of convenience functions for executing IM commands. A thin wrapper around kapi_sms / kapi_mms with argument defaults, as well as async and sync versions of some command (sync noted by the `b_` prefix for blocking).
