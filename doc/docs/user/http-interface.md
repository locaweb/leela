LEELA HTTP INTERFACE
====================

Leela has an HTTP interface that provides a RESTful API to query
data. Notice at this point this API is read only.

/v2
---

This entrypoint allows you to issue lql queries to leela.

Query string:

  * `q`      : The well-defined lql query
  * `format` : json|text [default: json]

Examples:

    /v2?q=using (leela::sandbox) guid (machine::warp0013);

/v2/kind/name
-------------

This entrypoint issues a `path` command. It retrieves all edges a
given vertex may have.

Query string:

  * `tree`   : the namespace;
  * `format` : json|text [default: json]

Examples:

```
  /v2/machine/warp0013?tree=leela::sandbox
```

N.B.: Remember to encode properly the query parameters!!!


/v2/kind/name/label1/label2/.../labelN
--------------------------------------

This entrypoint issuse a `path` command. It retrieves all edges a
given vertex may have. This is the same as the previous endpoint but
you can provide some labels to navigate further down in the graph.

Examples:

```
  /v2/machine/warp0013/server/nic?tree=leela::sandbox
```

The above query issues the lql command:

```
  using (leela::sandbox) path GUID -[server]> () -[nic]> ();
```

The GUID is the guid of `machine::warp0013`.

Query string

  * `tree`   : the namespace;
  * `format` : json|text [default: json]

/v2/last/*
----------

This entrypoint returns the last known value of a metric up to
24h. Notice this is read from a cache and as such it may may return
everything that exists in the database. That said, it is a good way to
fetch data from multiple sources efficently.

Parameters:

* `attr`   : The attribute to retrieve [eg: *cpu-0/cpu-idle*]. Notice you
  can use also use glob syntax;
* `tree`   : the namespace;
* `format` : json|text [default: json]

The following example retrieves everything from `machine::warp0013`.

```
  /v2/last/machine/warp0013?tree=locaweb::sandbox&format=text&attr=*
```
