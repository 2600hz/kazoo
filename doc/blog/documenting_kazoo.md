# Documenting Kazoo


## The Problem

There's no such thing as docs!

Every request we get for documentation is different and typically asking something else than what is said.


### Customer request

**What they said**: We need documentation

**What they meant**: We need to know how to install doorbells at our new client site


### 2600Hz Engineers

**What they said**: We’d write docs if there was a place to put them

**What they meant**: We’d write docs if the place to put them is:

-   Incredibly convenient for us
-   Didn’t require us to login to any other websites
-   Doesn't use Confluence
-   Works in Emacs
-   Is version controlled


### Developers

**What they said**: I need API reference documentation.

**What they meant**: I need copy/paste sample copy so I don’t have to read the docs.


### Sales

**What they said**: The customer needs docs

**What they meant**: The customer needs thorough development documentation


### Sales (30 minutes later)

**What they said**: The customer needs docs

**What they meant**: The customer needs a user manual on how to use the GUI because they’ve never managed a phone system before


## Where We Are


### We Have Different Audiences!

1.  Backend Kazoo developers (2600Hz Core and community)
2.  Frontend Kazoo developers (2600Hz UI, 2600Hz Aux, community)
3.  Sysadmins (2600Hz OPS and community)
4.  Resellers (2600Hz Sales, business/sales-type people)
5.  End users (UI to check voicemail, simply provision a phone, etc)


### We need faster ramp up

People in each audience are generally looking for a quick way to get started with minimal effort. They want to test the power of the system.

**We need to base this on behaviours we see already, not what they ask for!**

Only after that do they want more thorough documentation and reference, and they seem to be willing to help build it!


### Requirements

1.  Structure
2.  A way to participate
3.  The basics done for them
4.  A list of what’s left and someone to manage and distribute the work


### What we have

Site dedicated to the different audiences: [docs.2600hz.com](https://docs.2600hz.com)

-   Kazoo APIs: [Dev](https://docs.2600hz.com/dev)
-   Kazoo UI: [Monster UI](https://docs.2600hz.com/ui/)
-   Sysadmins: [Sysadmin](https://docs.2600hz.com/sysadmin)
-   Resellers: [User Guides](https://docs.2600hz.com/user_guides/)

Each site is backed by a git repo.

Each page has a link to a GitHub editor with the document in question.

The site installs webhooks with GitHub so that when pull requests are accepted into the master branch, the site fetches the new version of the pages and rebuilds the site in question (typically a couple seconds after PR is accepted).

The app that builds the doc sites and manages the webhooks also files an issue on the GitHub repo if the site fails to build. We also add CircleCI checks in each repo to try to minimize build errors for the docs site.


### What we want

-   WYSIWYG editor: we use [mkdocs](http://www.mkdocs.org/) which uses the Python [Markdown](https://pypi.python.org/pypi/Markdown) engine which can differ from GitHub Flavored Markdown.
    -   Also handling of images is&#x2026;tedious using the GitHub UI.
-   Back the editor with our GitHub bot to remove any Git interactions for those that don't want to deal with it
-   Versioned site
-   Internationalization


## Automating Kazoo doc generation


### Docs go stale

Even when they're lifted from source code comments, developers don't think about and aren't reminded to update appropriate sections of documentation.


### Do better

We starting thinking of how we could use our source code (not comments) to alert us to documentation-requiring changes?

How could we automate that process so that a harried PR review didn't gloss over the need?

How could community contributors run a make target locally to let them know they needed to add/update documentation?


### API Endpoints

The first effort was providing automated documentation of our APIs (that UIs and such are built on).

We use [Cowboy](https://github.com/ninenines/cowboy) as our HTTP server and the [rest handlers](https://github.com/ninenines/cowboy/blob/master/doc/src/guide/rest_handlers.asciidoc) to build each endpoint. Most endpoints are only accessed via an account, so most URLs are of the structure `/v2/accounts/{ACCOUNT_ID}/endpoints/{ENDPOINT_ID}` where the collection is followed by an entity.

All endpoint modules expose an `allowed_methods` function with varying arities, determined by the number of parameters in the URL. So `/v2/accounts/{ACCOUNT_ID}/vmboxes` would call [`cb_vmboxes:allowed_methods/0`](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_vmboxes.erl#L77-L78), `/vmboxes/{VMBOX_ID}` would call [`cb_vmboxes:allowed_methods/1`](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/src/modules/cb_vmboxes.erl#L79-L82), etc.

We can exploit the clauses in the `allowed_methods` functions to map out exactly what URI structures are accessible and by what HTTP verbs. If we saw `_` or `_ID` or the like, we would assume it was an entity ID.

The first full run found us with endpoints that made no sense but were technically reachable. A great cleanup occurred to tighten up the `allowed_methods` functions, to explicitly define various static path parameters, and to rename all ignored variables to better reflect the contents of that variable (used in other functions later in the processing).

This work created what we call ["ref" docs](https://github.com/2600hz/kazoo/tree/master/applications/crossbar/doc/ref), or bare-bone versions of endpoint documentation, as best as we could do given the constraints of the code.

1.  JSON Schemas

    Each entity has a backing JSON schema document that informs the client of what fields are available for them to manipulate. It turns out, people like tables more than JSON when trying to figure out what can be put on the data payload.

    So we wrote a JSON schema to markdown table generator and added it as part of the ref doc build process. Now any schema updates will be reflected in the ref doc's schema table.

    A script was then written to automatically migrate the ref doc's table to the "real" doc to keep it up to date as well.

    Finally, we also update a [swagger.json](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/priv/api/swagger.json) file because one of our devs thought it was the trendy thing to do.

2.  Build tools

    Make targets were added to do all this doc-building automatically. If an endpoint is changed or schema updated, these targets will take care of building the appropriate changes in the docs and show up as unstaged changes. If the developer runs them locally, they can commit the changes as well as hopefully update the "real" version of the docs (or the PR reviewer should see a change to a "ref" doc without a corresponding "real" doc change).

    These are run on every pull request and commit to master as well; if the auto-changes are not committed, unstaged changes will appear in the CI job and cause the build to fail.

3.  The code

    The code is found in the [cb\_api\_endpoints](https://github.com/2600hz/kazoo/blob/master/core/kazoo_ast/src/cb_api_endpoints.erl) module (part of the [kazoo\_ast](https://github.com/2600hz/kazoo/blob/master/core/kazoo_ast/) core application). It finds all the Crossbar endpoint modules and parses the `allowed_methods` AST of each to find the list of HTTP methods allowed and what parameters are passed to the function (from the URI path).


### Callflow action data

Each action in a callflow has an associated data JSON object. If the API endpoint docs were sad, the callflow action docs were non-existent.

This work was much different from the Crossbar doc work; in Crossbar's case, we had a known function name and return (list of binaries) to work with. In callflows, we have a `Data` variable passed to the `handle/2` function in each callflow action. From there, we need to trace through all functions where `Data` is accessed using the `kz_json` module to get values out. We collect the keys used, as well as defaults if found, to create the JSON schema.

We create/merge ['ref" docs](https://github.com/2600hz/kazoo/blob/master/applications/callflow/doc/ref/) and [schemas](https://github.com/2600hz/kazoo/tree/master/applications/crossbar/priv/couchdb/schemas) for the actions' `Data` payload.

1.  Challenges

    -   `Data` is passed to a function and called something else in the args list.
    -   Modules in other applications are called with `Data` (or aliased) as a parameter, so we need to find and parse the AST for those modules.
    -   Recursive calls that include `Data` were blowing the tool up, so some memoization was needed to know if an `M:F(A)` had been visited already.
    -   Detecting the type of the value (or default value) for inclusion in the JSON schema

2.  Improvements

    Besides the obvious auto-generation of JSON schemas and "ref" docs, we saw numerous improvements in the code:

    -   Using more specific get functions in the `kz_json` module to ensure the value was of the right form
    -   Behaviour was defined to more formally build callflow actions.
    -   Caught improper uses of the JSON type (proplist in a 1-tuple) where complex Erlang terms were stored in `Data` for convenience of the programmer.

3.  Future

    There are still some keys that are "hidden" from the AST walker. We define lists of functions to fold over and include the `Data` as a parameter.

    ```erlang
    build_it(Call, Data) ->
        lists:foldl(fun(F, Acc) -> F(Call, Data, Acc) end
                    ,kz_json:new()
                    ,[fun m/3, fun n/3, fun o/3]
                   ).
    ```

4.  The code

    The [cf\_data\_usage](https://github.com/2600hz/kazoo/blob/master/core/kazoo_ast/src/cf_data_usage.erl) module takes care of this work.


### Conference Schema

The conference entity was undocumented and had no schema doc to guide developers. We have a module, [kapps\_conference](https://github.com/2600hz/kazoo/blob/master/core/kazoo_call/src/kapps_conference.erl), that represents a conference and all its attributes, including a JSON->record function to build the `#kapps_conference{}` record.

Again, we just look for usages of the JSON object `JObj` in that function (and any called functions where `JObj` is passed). The code is in the [conference\_schema\_builder](https://github.com/2600hz/kazoo/blob/master/core/kazoo_ast/src/conference_schema_builder.erl) module.


### FreeSWITCH properties

We rely on [FreeSWITCH](https://freeswitch.org/) to handle much of our SIP and audio/video around active calls. We have a module, [`mod_kazoo`](https://freeswitch.org/stash/projects/FS/repos/freeswitch/browse/src/mod/event_handlers/mod_kazoo), that presents FreeSWITCH as a [C-Node](http://erlang.org/doc/tutorial/cnode.html) to Kazoo's `ecallmgr` application. Each event that happens in FreeSWITCH has various key-value pairs which mod\_kazoo filters to only include the fields we care about.

We wanted to build this list dynamically so that we could change/update mod\_kazoo's filter list form Kazoo (instead of having to update XML files on all FreeSWITCH servers and reloading mod\_kazoo, impacting the servers' ability to process calls for Kazoo while the reload occurred.

Similar to tracing usage of the `kz_json` module in callflows or conferences, we trace usage of our proplist module, [props](https://github.com/2600hz/kazoo/blob/master/core/kazoo/src/props.erl), for the keys used to extract values from the FreeSWITCH-supplied proplist.

This information is used to generate a header file, [fs\_event\_filters.hrl](https://github.com/2600hz/kazoo/blob/master/applications/ecallmgr/src/fs_event_filters.hrl), used within the ecallmgr application. If a developer accesses new keys in the proplist, this header file should be updated to reflect that (and code can inform mod\_kazoo of the new filter list).

The code is in the [fs\_prop\_usage](https://github.com/2600hz/kazoo/blob/master/core/kazoo_ast/src/fs_prop_usage.erl) module.


### System configuration

Kazoo uses a database in [CouchDB](https://couchdb.apache.org/) called `system_config`. It contains JSON objects related to configuration data for the cluster, for each zone, or for an individual node in the cluster. Again, there was no listing of what fields were possible in Kazoo for these documents. Data would be added when code accessed the config data for the first time, populating defaults when the code supplied them.

As we built cluster manager, our Aux team was reading through the Kazoo source trying to find all the possible data stored in these documents and manually creating schemas to reflect their findings.

We built [kapps\_config\_usage](https://github.com/2600hz/kazoo/blob/master/core/kazoo_ast/src/kapps_config_usage.erl) to walk the entire project looking for places where the [kapps\_config](https://github.com/2600hz/kazoo/blob/master/core/kazoo_config/src/kapps_config.erl) module was used to access config data and generate appropriate schemas.

1.  Account configuration

    Some configuration allow accounts to override system settings; these are accessed with the [kapps\_account\_config](https://github.com/2600hz/kazoo/blob/master/core/kazoo_config/src/kapps_account_config.erl) module. `kapps_config_usage` was extended to include creating account-specific config JSON schemas as well.


### Raw JSON usage

Back when Kazoo was Whistle, [mochijson2](https://github.com/mochi/mochiweb/blob/master/src/mochijson2.erl) was the most popular JSON encoder/decoder. It represented JSON objects as `{struct, [{Key, Value},...}]}`. More recent libraries mostly settled on `{[Key, Value},...]}` as the Erlang encoding of choice. And now with Erlang supporting maps, we are seeing options to decode JSON strings to maps.

All of this to say, the representation of JSON in Erlang evolves. We wrote [kz\_json](https://github.com/2600hz/kazoo/blob/master/core/kazoo_json/src/kz_json.erl) to abstract the representation away, as well as build a plethora of functionality around the data type.

At one point, we manually went through an remove all "raw" Erlang JSON terms during the `{struct, []}` to `{[]}` conversion but still found places in the code (and in our community's code independently-developed) that broke.

So we built [raw\_json\_usage](https://github.com/2600hz/kazoo/blob/master/core/kazoo_ast/src/raw_json_usage.erl) to detect where, outside of `kz_json`, the raw representation of JSON was being used and error on it.

One caveat is that the `kazoo_json` application has an include-able header file that has a macro for matching both empty JSON object and the JSON wrapper of choice. These are useful when pattern matching values that might be scalars or a nested JSON object.

So we added checks to read the actual Erlang source and see if the macros are in use vs the literal characters of the JSON representation.


### Future Efforts

Right now, the modules in the `kazoo_ast` application are rather ad-hoc and a shameful amount of copy-pasta exists. Refactoring the walker into [kazoo\_ast](https://github.com/2600hz/kazoo/blob/master/core/kazoo_ast/src/kazoo_ast.erl) and providing hooks for the above modules is underway. All the AST forms, expressions, clauses, functions, etc, are defined in an [include file](https://github.com/2600hz/kazoo/blob/master/core/kazoo_ast/include/kz_ast.hrl) for all to share.

We have a layer of internal JSON payloads that are passed over [AMQP](https://en.wikipedia.org/wiki/Advanced_Message_Queuing_Protocol) right now that are undocumented and no JSON schema exists. Up to this point, very few people outside 2600Hz were interested in building their own applications that would sit on the AMQP bus - this has changed and more people are asking to tie in custom services (like managing phone credentials in LDAP for instance) in their private Kazoo installations.

The AST walkers are fairly naive, as mentioned in the callflow walker. Improving what code paths can be followed will help find more nooks and crannies to expose.

A new-ish websockets interface exists that takes AMQP-like bindings from the client to register for events on the cluster. These bindings are not well documented, and as new modules are added to expose more functionality, this gulf will widen.

Similarly, a webhooks interface lacks details on what will be included in the various payloads.

A real-time call control application, [Pivot](https://github.com/2600hz/kazoo/blob/master/applications/pivot/doc/README.md), also doesn't have a proper listing of possible request data that could be present.

JSON schemas continue to evolve as Kazoo grows; new fields allowed in the various entities' data objects aren't automatically checked to exist in the corresponding schema and still relies on the developer to include an updated schema in the PR.

Currently, the way Crossbar endpoint modules work, it is hard to extract the content types accepted and provided by the various URI paths of an endpoint. Either refactoring all modules to be easier to parse, or improving parsing of the existing code, would allow us to create better ref docs and fill in the swagger JSON file more authoritatively.


## Lessons Learned

Developers have lots of time constraints; building tools to help them (or the PR reviewer) know when a change necessitates doc changes keeps everyone honest about keeping docs up to date (including our community!).

Moving the docs out of Confluence and into the git repo was a big move. Each developer can update the docs in their editor of choice and the tooling enforces some measure of strictness on what that looks like.

Speaking of, consistent looking code is a Godsend! While style guides may irk some, we've found overall that adherence to a style produces more benefits than drawbacks, especially once you get past the [bike-shedding](https://en.wikipedia.org/wiki/Law_of_triviality) phase (**cough** [atom ticky policy](https://github.com/2600hz/kazoo/pull/2724) **cough**). `make fmt` enforces a style across all Erlang code in Kazoo, regardless of what editor is used.

Management has to buy-in to the importance of docs. We had a 3-day retreat to Tahoe for skiing and documentation writing with the two co-founders and CTO present and writing docs. The message is clear to all - long live documentation!

We've adjusted the CI tests to include all these AST walkers and fail builds if unstaged changes occur (or raw JSON is detected). If the CI builds aren't passing, you aren't going to get a review.

All of this tooling is available to run locally; no excuses when CI fails because a developer didn't run the checks locally.

This, in turn, informs the community and let's them submit easier-to-review PRs that we can more quickly provide feedback and merge. The playing field is level in that regard between 2600Hz and our community developers.

Auto-generated docs will never be enough. User guides and tutorials still require work and effort. We haven't yet figured out how to link those to the development process to know when an article should be updated.


# Thank you!

If you made it this far, thanks for reading! We hope this has caused some reflection on documenting your work for others, even if only internally useful.

We want to hear your ideas! We don't claim any of this to be novel but we also haven't really seen this parsing of the source code AST touted as a strategy for documentation. Further improvements and refinements welcome!
