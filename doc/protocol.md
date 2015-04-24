# transforming requests

A request has the following structure:

```
{ "meth": [METHOD, URI]    -- resource location
, "meta": HASH             -- metadata information
, "data": DATA             -- message payload
, "load": BINARY           -- persistent state
}
```

And the response:

```
{ "code": [CODE, LINE]     -- status code and human message
, "meta": HASH             -- metadata information
, "data": DATA             -- message payload
, "stor": [BINARY, SCOPE]  -- persistent state
}
```

This is very similar to a HTTP request/response and this is on
purpose. The idea is to make it easy to transform HTTP requests into
this format and vice-versa.

For instance, consider this HTTP request:

```
    GET /foo/bar?foo=bar&bar=foo HTTP/1.1
    host: foobar
    user-agent: emacs
    content-type: application/json
    content-length: 13

    {"foo":"bar"}
```

and its encoded version:

```
{ "meth": ["GET", "/foo/bar", ["bar", "foo"]]
, "meta": { "host": "foobar"
          , "user-agent": "emacs"
          , "content-type": "application/json"
          , "content-length": 13
          }
, "data": {"foo":"bar"}
}
```

# request model

Applications are stateless, i.e. all information required to process
the request must be present in the request itself. Also each request
must produce a response. Notice this differs sensibly from the HTTP
model.
