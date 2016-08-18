# How to Write Documentation for Kazoo!

Consistency in documentation is a win for everyone, including your future self. Write the docs! We're here to help.

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

* Example request/response formats
  * As many as needed to demostrate usage
* Keep it terse - no storytelling needed. Just the facts ma'am!
* Suggested use cases (brief)
  * Bullet list of common ways to use the API
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
* Differences between versions of the endpoint, if any

## Tools used

Catologuing some of the tools used:

* Multi-line search/replace of comment sections: `find . -name "*.md" -print | xargs perl -i -pe 'BEGIN {undef $/;} s/\/\*\n.+?\*\///smg'`
