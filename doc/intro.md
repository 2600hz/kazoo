# Intro and Background on KAZOO

## What is KAZOO?

KAZOO is an API-based platform that lets you use your existing phones, programming languages and IT skills to build voice, video and SMS services. We focus on building a simple, powerful communications platform and let you focus on marketing, servicing and integrating communications with your clients systems.

KAZOO leverages several open source technologies such as FREESWITCH, Kamailio, RabbitMQ, and CouchDB in order to provide a robust set of telephony features. You as the developer can start here to learn how to use KAZOO to build your own communications applications on top of the existing core.

## How do I Get Started?

KAZOO is open-core, meaning that you are free to clone, modify, or otherwise utilize the [kazoo-core](https://www.github.com/2600hz/kazoo-core) source code. If you’re interested in building KAZOO from source, this [guide](https://github.com/2600hz/kazoo/blob/master/doc/installation.md) is an appropriate jump-off point. Otherwise, visit our [website](http://2600hz.com) to get in contact about scheduling a demo.

We at 2600Hz also offer production-grade applications as well as KAZOO cluster hosting and management as services. Visit our [website](http://2600hz.com) for more information. All our guides will assume that you have a KAZOO environment to develop against, whether you’re an open source user or a 2600Hz customer.

## What can I Build with KAZOO?

The sky is truly the limit when it comes to building atop KAZOO: full telephony applications such as call center software, visibility dashboards, customized billing engines, and more are very much possible to create—in fact, many of these have been created by us at 2600Hz, our partners, or hobbyist developers with great ideas!

As part of our developer documentation, we’ll walk through writing applications that leverage KAZOO in different ways. Given how robust the platform already is from a feature perspective, many people are now interested in how to leverage KAZOO’s Webhooks and Websockets functionality, integrations with different CRMs, and other concepts related to integrating KAZOO with other services. We’ll also talk about how to get started on some of these hot topics.

## Basic API Information

Almost all requests and responses to the KAZOO API are serialized using JSON, unless otherwise specified in the documentation. You may find, for example, an endpoint will respond with XML instead of JSON.

The KAZOO API is built on widely-adopted architectures and specifications such as REST (for API consistency) or [JSON-Schema](http://json-schema.org/) (for data validation). You can use any HTTP client to interact with the API, whether via a programming language’s HTTP library, a HTTP GUI like Postman, or just plain old cURL in the terminal. There is very little overhead in order to get started making requests against your KAZOO account.

## KAZOO Versioning and API Consistency

Expect to encounter inconsistencies with official documentation material if you are using features or KAZOO versions that are outside of what we use in our own stable commercial environments. This means that, if you are for example running an older (or newer experimental) version of KAZOO, that this documentation may not be accurate for your given environment. We recommend using our [community forums](https://forums.2600hz.com/forums/) to ask questions.
