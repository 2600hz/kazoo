# Webhooks

## Initialization

When the webhooks app starts up for the first time, two initialization processes will run:

1. Loading webhook metadata into master account (if missing)
2. Loading configured webhooks into memory for routing

### Webhook Metadata

Similar to teletype and other apps that have different templates or events available, depending on the version of Kazoo running, the number of webhook events exposed will depend on the version of Kazoo. This process will check the available webhook events compiled into the running instance and load them into the master account's database.

The structure of each metadata package will include:

    {"id":"channel_create"
     ,"name":"Channel Create"
     ,"description":"Event fires when a new call is begun"
    }

These metadata packages can then be accessed via Crossbar to allow REST clients to present the latest available webhooks for selection.

See the [webhooks_skel](applications/webhooks/src/modules/webhooks_skel.erl) for creating new webhook modules.

### Webhook Modules

These modules define a webhook available for accounts to create and for the webhooks app to fire. They expose three functions:

1. `init/0` - load metadata document into the master account database
2. `bindings_and_routings/0` - load binding keys and routing rules into the `webhooks_shared_listener` process.
3. `handle_event/2` - `gen_listener` callback to process the Kazoo event for distribution

### Configured Webhooks

When an account creates a webhook, it is stored in the `webhooks` database for easy discovery by the webhooks application. These documents are then processed and loaded into an ETS table to be queried when events occur within Kazoo.

## Startup

Webhooks starts up two listeners:

1. `webhooks_listener`: listens for webhook configuration changes and adjusts the ETS table appropriately.
2. `webhooks_shared_listener`: listens for Kazoo events (based on loaded webhook modules) and distributes the events to the handlers.

Webhooks also starts up a server to periodically check configured webhooks for too many failures, automatically disabling requests to those servers deemed broken.
