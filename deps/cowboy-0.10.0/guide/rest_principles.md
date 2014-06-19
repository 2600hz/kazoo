REST principles
===============

This chapter will attempt to define the concepts behind REST
and explain what makes a service RESTful.

REST is often confused with performing a distinct operation
depending on the HTTP method, while using more than the GET
and POST methods. That's highly misguided at best.

We will first attempt to define REST and will look at what
it means in the context of HTTP and the Web.
For a more in-depth explanation of REST, you can read
[Roy T. Fielding's dissertation](http://www.ics.uci.edu/~fielding/pubs/dissertation/top.htm)
as it does a great job explaining where it comes from and
what it achieves.

REST architecture
-----------------

REST is a *client-server* architecture. The client and the server
both have a different set of concerns. The client cares about how
it will display information to the user. The server cares about
how to store that information and how to make it available to
users efficiently. This separation of concerns allows both the
client and the server to evolve independently as it only requires
that the interface stays the same.

REST is *stateless*. That means the communication between the
client and the server always contains all the information needed
to perform the request. There is no session state in the server,
it is kept entirely on the client's side. If access to a resource
requires authentication, then the client needs to authenticate
itself with every request.

REST is *cacheable*. The client, the server and any intermediary
components can all cache resources in order to improve performance.

REST provides a *uniform interface* between components. This
simplifies the architecture, as all components follow the same
rules to speak to one another. It also makes it easier to understand
the interactions between the different components of the system.
A number of constraints are required to achieve this. They are
covered in the rest of the chapter.

REST is a *layered system*. Individual components cannot see
beyond the immediate layer with which they are interacting. This
means that a client connecting to an intermediate component, like
a proxy, has no knowledge of what lies beyond. This allows
components to be independent and thus easily replaceable or
extendable.

REST optionally provides *code on demand*. Code may be downloaded
to extend client functionality. This is optional however because
the client may not be able to download or run this code, and so
a REST component cannot rely on it being executed.

Resources and resource identifiers
----------------------------------

A resource is an abstract concept. In a REST system, any information
that can be named can be a resource. This includes documents, images,
a collection of resources and any other information. Any information
that can be the target of an hypertext link can be a resource.

A resource is a conceptual mapping to a set of entities. The set of
entities evolves over time; a resource doesn't. For example a resource
can map to "users who have logged in this past month" and another
to "all users". At some point in time they may map to the same set of
entities, because all users logged in this past month. But they are
still different resources. Similarly, if nobody logged in recently,
then the first resource may map to the empty set. This resource exists
regardless of the information it maps to.

Resources are identified by uniform resource identifiers, also known
as URIs. Sometimes internationalized resource identifiers, or IRIs,
may also be used, but these can be directly translated into a URI.

In practice we will identify two kinds of resources. Individual
resources map to a set of one element, for example "user Joe".
Collection of resources map to a set of 0 to N elements,
for example "all users".

Resource representations
------------------------

The representation of a resource is a sequence of bytes associated
with metadata.

The metadata comes as a list of key-value pairs, where the name is
corresponds to a standard that defines the value's structure and
semantics. In HTTP the metadata comes in the form of HTTP headers
which are well defined by the HTTP standard. Metadata includes
representation metadata, resource metadata and control data.

The representation metadata gives additional information about
the representation, such as its media type, the last date of
modification, or even an Etag.

Resource metadata could be link to related resources or
information about additional representations of the resource.

Control data allows parameterizing the request or response.
For example, we may only want the representation returned if
it is more recent than the one we have in cache. Similarly,
we may want to instruct the client about how it should cache
the representation. This isn't restricted to caching. We may
for example want to store a new representation of a resource
only if it wasn't modified since we first retrieved it.

The data format of a representation is also known as the media
type. Some media types are intended for direct rendering to the
user, while others are intended for automated processing. The
media type is a key component of the REST architecture.

Self-descriptive messages
-------------------------

Messages must be self-descriptive. That means that the data
format of a representation must always come with its media
type (and similarly requesting a resource involves choosing
the media type of the representation returned). If you are
sending HTML, then you must say it is HTML by sending the
media type with the representation. In HTTP this is done
using the content-type header.

The media type is often an IANA registered media type, like
`text/html` or `image/png`, but does not need to be. Exactly
two things are important for respecting this constraint: that
the media type is well specified, and that the sender and
recipient agree about what the media type refers to.

This means that you can create your own media types, like
`application/x-mine`, and that as long as you write the
specifications for it and that both endpoints agree about
it then the constraint is respected.

Hypermedia as the engine of application state
---------------------------------------------

The last constraint is generally where services that claim
to be RESTful fail. Interactions with a server must be
entirely driven by hypermedia. The client does not need
any prior knowledge of the service in order to use it,
other than an entry point and of course basic understanding
of the media type of the representations, at the very least
enough to find and identify hyperlinks and link relations.

To give a simple example, if your service only works with
the `application/json` media type then this constraint
cannot be respected (as there are no concept of links in
JSON) and thus your service isn't RESTful. This is the case
for the majority of self-proclaimed REST services.

On the other hand if you create a JSON based media type
that has a concept of links and link relations, then
your service might be RESTful.

Respecting this constraint means that the entirety of the
service becomes self-discoverable, not only the resources
in it, but also the operations you can perform on it. This
makes clients very thin as there is no need to implement
anything specific to the service to operate on it.
