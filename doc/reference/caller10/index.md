### Caller 10

This series of docs is meant to serve as an introduction to building on the core of Kazoo.

We'll take a look at how the various technologies are used, including:

* SIP
* AMQP
* HTTP/REST
* Erlang

We'll take a look at the REST API server, `Crossbar`, and how to add functionality. We'll look at the main call processing app, `Callflow`, and how to add new actions to perform on a call. And finally, we'll look at building our own custom application, `Caller10`, to implement the new functionality. All while having a great time!

#### Let's Take a Journey

Too often guides stay theoretical, so we'll be sure to root any theory into practical, concrete code examples. We'll reference existing Kazoo code as well as code for our new application. Hopefully this will provide the framework for navigating Kazoo's code, making it easier to understand what is being done.

Let's go!

#### Caller 10 - The Introduction

Introducing the newest, latest, fantastic-ist app to hit Kazoo: CallerTen! Enabling this application will allow your customers the ability to hold promotional call-ins, similar to how radio stations allow callers to call in to win concert tickets and other prizes.

Okay, the idea is set, we've got management's approval to build the application, let's roll! What's the plan?

The following is a proof of concept implementation, without bells and whistles, and some design decisions will need to be made to properly distribute this application across multiple application servers, and across multiple zones (if you've enabled federation).

#### Identifying Resources

What is it that will represent the CallerTen application? Well, using more generic language, we might say we want to create contests. Let's define contests using a [JSON schema](http://json-schema.org/).

`applications/crossbar/priv/couchdb/schemas/contests.json`

By convention, resources are the plural version (`contests` instead of `contest`, in this case).

Now that we have an idea what our resource looks like, let's get it loaded into Kazoo and create the REST endpoint.

Follow along in the [crossbar](./crossbar.md) section. Crossbar is the HTTP REST API server (and yes, you must yell it when you say that). It is built on top of Cowboy and provides many utilities and functionality to make adding resource endpoints easy and convenient.

#### Routing calls

Now that we can create contests, we'd like to be able to send calls to our to-be-built application. We'll want to create a callflow action that we can put in a callflow document that will pass the caller along to our application. You can look in the [callflow](./callflow.md) section to learn about that process.

#### Building the application

Now that callers are being sent to our app, we should probably actually create the app! This will clone the skel application, introduce the common design patterns used by most applications, and get you on your way to building other, more amazing applications to wow your users with. Follow along in the [caller10](./caller10.md) section.
