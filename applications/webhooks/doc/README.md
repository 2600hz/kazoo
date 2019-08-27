# Webhooks *Event driven HTTP*

Smee: I've just had an apostrophe.
Captain Hook: I think you mean an epiphany.
Smee: Lightning has stuck my brain
Captain Hook: That must've hurt

## Get notified when things happen

KAZOO, internally, runs messages through a message broker; in this case RabbitMQ via the AMQP protocol. These messages are flowing in a protected, high-trust environment which necessitates no 3rd party client access.

However, KAZOO also strives to provide 3rd parties (resellers or integrators, for instance) access to the goings on of the system.

Webhooks effectively provides an AMQP->HTTP bridge to allow events in KAZOO to flow from the AMQP message bus to the HTTP server of the 3rd party.
