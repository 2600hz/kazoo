# How to Write Documentation for Kazoo!

Consistentcy in documentation is a win for everyone, including your future self. Write the docs! We're here to help.

## Rubric

What we're looking for when evaluating documentation:

### Top-level

When writing top-level application docs, we're looking for:

* Basics of API usage
* Commonalities among all endpoints
* ASCII Art of various Erlang process trees, FSM transitions, etc
* SUP commands available

### Per-endpoint

When writing docs for API endpoints, we'd like to strive for:

* Example request format
* Example response format
* Suggeested use cases (brief)
* Links to user guides or tutorials of why to use this endpoint
* Common pitfalls when using the endpoint
* Conflicting settings to watch out for

## Template Rules

When creating a new markdown document, consider the following rules:

1. Headings should only be h3 (`###`) or bigger, as h1 and h2 headers are used in the doc-generating code.

You can take a look at the [index](https://raw.githubusercontent.com/tripit/slate/master/source/index.md) page and the [error](https://raw.githubusercontent.com/tripit/slate/master/source/includes/_errors.md) include from the slate repo for some of the syntax for creating code samples, tables, etc.

### API Descriptions

It would be great to have:

* An `About` section briefly describing the endpoint
* Descriptions of the relevant fields (defined in the schema for the resource)
* Listing of URI segments available, with HTTP methods
* Code sample for **at least** a cURL command; bonus points if you include PHP SDK equivalents.
* differences between versions of the endpoint, if any
