# Webhooks

## Initialization

When the webhooks app starts up for the first time, two initialization processes will run:

1. Loading webhook metadata into master account (if missing)
2. Loading configured webhooks into memory for responders

### Webhook Metadata

Similar to teletype and other apps that have different templates or events available, depending on the version of Kazoo running, the number of webhook events exposed will depend on the version of Kazoo. This process will check the available webhook events compiled into the running instance and load them into the master account's database.

The structure of each metadata package will include:

    {"id":"channel_create"
     ,"name":"Channel Create"
     ,"description":"Event fires when a new call is begun"
     ,"modifiers":{...}
    }

These metadata packages can then be accessed via Crossbar to allow REST clients to present the latest available webhooks for selection.

See the [webhooks_skel](https://github.com/2600hz/kazoo/blob/master/applications/webhooks/src/modules/webhooks_skel.erl) for creating new webhook modules.

#### Modifiers

Some webhooks, like `doc`, have a `modifiers` object to fine-tune the behaviour of the webhook. `objects`, for instance, allows the creator to specify a subset of object types to listen for and publish (versus getting everything).

### Webhook Modules

These modules define a webhook available for accounts to create and for the webhooks app to fire. They expose three functions:

1. `init/0` - load metadata document into the master account database
2. `bindings_and_responders/0` - load binding keys and responders rules into the `webhooks_shared_listener` process.
3. `handle_event/2` - `gen_listener` callback to process the Kazoo event for distribution

### Configured Webhooks

When an account creates a webhook, it is stored in the `webhooks` database for easy discovery by the webhooks application. These documents are then processed and loaded into an ETS table to be queried when events occur within Kazoo.

## Startup

The startup process tree in `webhooks_sup` looks like:

    [webhooks_sup]
          |
          -- [webhooks_cache]
          -- [kazoo_etsmgr_srv]
          -- [webhooks_init]
          -- [webhooks_disabler]
          -- [webhooks_listener]
          -- [webhooks_shared_listener]

### `webhooks_cache`

The cache is used to track failed hook firing attempts.

### `kazoo_etsmgr_srv`

Manages the ETS table used to store active hooks. Cedes control to `webhooks_listener` once the listener is ready.

### `webhooks_init`

Finds available webhook modules and initializes them. Does not continue running afterwards (returns 'ignore').

### `webhooks_disabler`

Starts up a server to periodically check configured webhooks for too many failures, automatically disabling requests to those servers deemed broken.

### Listeners

Webhooks starts up two listeners:

1. `webhooks_listener`: listens for webhook configuration changes and adjusts the ETS table appropriately.
2. `webhooks_shared_listener`: listens for Kazoo events (based on loaded webhook modules) and distributes the events to the handlers.
