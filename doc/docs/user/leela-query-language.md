LEELA QUERY LANGUAGE
====================

Leela Query Language or simply LQL is the gateway to insert data or
query the database.

This document is divided into three sections. The first part describes
the structure of the language. The second one is graph manipulation
and querying [edges and vertexes] and the final part is property
manipulation and querying.

LQL
---

The language is built on the idea of commands with subcommands and
arguments. For instance, consider this example:

```
  make (foo::bar)
```

The sintax above is used to create a vertex with name
`foo::bar`. There are commands that takes no arguments:

```
  stat
```

And there commands with subcommands like attr:

```
  attr get 1d1a751c-622a-11e5-a7b4-efda49744bfe
  ^    ^   ^
  ^    ^   target/object
  ^    subcommand
  command
```

The language has a few types as well:

```
  "lql"         : a string literal;
  (double 42.0) : a double value [64 bits float type]
  (int64 42)    : 64 bits signed integer;
  (int32 42)    : 32 bits signed integer;
  (uint64 42)   : 64 bits unsigned integer;
  (uint32 42)   : 32 bits unsigned integer;
  (bool true)   : boolean value;
```

Lastly, leela objects have namespaces. Namespaces completely isolates
objects and properties, so that you can use the same identifier in two
different namespaces without worries.

Every query must provide the namespace it is working on. Following is
a complete example of what a lql query looks like:

```
  using (foo::bar) stat;
  ^     ^          ^   ^
  ^     ^          ^   query terminator
  ^     ^          command + arguments
  ^     ^
  ^     namespace to use
  ^
  command
```

Every command starts with a `using` command following by the namespace
enclosed by parenthesis. The namespace is composed by two components,
separated from `::`. In the above case the first component is `foo`
and the second is `bar`. More about this in the `NAMING` section.

NAMING
------

The identifiers in Leela are composed by two components. The idea
behind this is that the first component serves as category or class
for the name in question. Components are separated by `::`. Now,
consider the example:

```
  machine::warp0013
```

The first component is `machine` and the second is
`warp0013`. Similarly:

```
  nic::00-01-02-03-04-05
```

This represents a network interface and its mac address.

GRAPH MANIPULATION
------------------

There are two commands that deal with the graph structure:

  * `make` : create vertex and edges;
  * `path` : navigate the graph;
  * `kill` : destroy vertex and edges;

### MAKE ###

```
  QUERY: make (kind::name);
  REPLY: NAME
```

Creates a new vertex identified by NAME. Is replies with a `NAME`
message. For example:

```
  > using (leela::sandbox) make (machine::warp0013);
  < user  | tree    | kind    | name     | guid
  < leela | sandbox | machine | warp0013 | cd8a5e22-622e-11e5-bb66-fb5334d472bc
```

The lines above need a little bit of explanation. The first line is
the LQL query. In the examples we will use the `>` character to
identify lines that are sent to the server. Following there is one or
more response lines, identified by the `<` character. More information
about the different types of answers at the final section of this
document.

Let's now create an edge:

```
  QUERY: make GUID -[LABEL]> GUID;
  REPLY: DONE
```

This command requires vertex ids instead of names. Then, you can link
two vertex with a given label which can be later used to navigate
through the graph. Let's see an example:

```
  > using (leela::sandbox) make (machine::warp0013);
  < user  | tree    | kind    | name     | guid
  < leela | sandbox | machine | warp0013 | cd8a5e22-622e-11e5-bb66-fb5334d472bc

  > using (leela::sandbox) make (nic::01-02-03-04-05);
  < user  | tree    | kind | name           | guid
  < leela | sandbox | nic  | 01-02-03-04-05 | 1d8bd666-6230-11e5-a3d6-7b6f71a2eebc

  > using (leela::sandbox) make cd8a5e22-622e-11e5-bb66-fb5334d472bc -[nic]> 1d8bd666-6230-11e5-a3d6-7b6f71a2eebc;
  < ()

  > using (leela::sandbox) make cd8a5e22-622e-11e5-bb66-fb5334d472bc <[machine]- 1d8bd666-6230-11e5-a3d6-7b6f71a2eebc;
  < ()
```

The first two queries just register the vertexes, nothing new
here. The last query is the one that creates the edge. Differently
from vertexes, labels are just strings so there is no need to register
them. Labels are enclosed by `-[]>`, the reason for this is that that
string looks like an arrow. You can also change direction by using
`<[]-` which creates an edge with the inverse direction.

The `DONE` reply is here represented by the `()` string because it has
no payload.

### PATH ###

The path command allows you to navigate through the graph. Creating
edges and navigating through them are very similar. As a matter effect
the syntax is almost the same:

    QUERY: path GUID
    QUERY: path GUID -[LABEL-GLOB]> ()...
    REPLY: PATH

Path takes a variable number of arguments. It can be used with no
arguments in which case returns all edges a vertex may have. Or you
can provide some edges in which case the system will use information
to navigate the structure. Examples:

```
  > using (leela::sandbox) path cd8a5e22-622e-11e5-bb66-fb5334d472bc;
  < label | guid
  < nic   | 1d8bd666-6230-11e5-a3d6-7b6f71a2eebc

  > using (leela::sandbox) path cd8a5e22-622e-11e5-bb66-fb5334d472bc -[n*]> ();
  < label | guid
  < nic   | 1d8bd666-6230-11e5-a3d6-7b6f71a2eebc

  > using (leela::sandbox) path cd8a5e22-622e-11e5-bb66-fb5334d472bc -[foo]> ();
  < ()
```

The first query retrieves all edges. The second one all edges that
have a label starting with n. The last one attempts to retrieve edges
with a `foo` label, which does not exists, thus an empty reply.

### KILL ###

Kill commands are the dual of make commands. They share the same
syntax:

```
  QUERY: kill GUID
  QUERY: kill GUID -[LABEL]> ()...
  REPLY: DONE
```

The second form destroy edges. The second form destroy vertexes and
all information related [like properties and edges]. Sadly, deleting a
vertex is not implemented.

PROPERTY MANIPULATION
---------------------

Vertexes may have properties and you can change or retrieve them using
the `attr` command. Properties comes in two flavors:

  
  * kv: key-value property;
  * ts: time-series property;

The difference between the two is that the former is indexed by
time. You may think the `kv` property as a simple cell whereas the
`ts` as an array in which the indexes are timestamps.

There are four operations you can do with `kv` properties, namely
`put`, `get`, `del` and `kls`. The last one is used for enumerating
the attributes. The same commands can be used for `ts` properties but
the last one which is `tls` instead. Next we provide details about
every one of them.

### ATTR GET ###

You can use this command to retrieve both `kv` and `ts` properties.

```
  QUERY: attr get GUID "NAME" [WITH OPTIONS];
  REPLY: K-ATTR
```

This syntax is used to fetch `kv` properties. Similarly:

```
  QUERY: attr get GUID "NAME" [S:E]
  REPLY: T-ATTR
```

Fetches `ts` properties. The only difference is the last bit which
contains the start and finish of the time-series. For example:

```
  > using (leela::sandbox) attr get cd8a5e22-622e-11e5-bb66-fb5334d472bc "foobar" [0:3600];
  < guid                                 | name   | series
  < 4eb3de70-cb80-11e3-b4dc-0f48fe0268e8 | foobar | (1443043943.0, 19.95), (1443044003.0, 20.25), (1443044063.0, 16.5), (1443044123.0, 16.2334), (1443044183.0, 19.9333)
```

#### FUNCTIONS ####

There are a few functions that can be applied to transform the
data. `map` functions apply to each datapoint, which does not change
the size of the series. `reduce` functions on the other hand are
blocking and consume all data before yielding a value
[but you can use window functions to control how much data]. Following
a list of available functions:

* `map (+ N)`              : for each datapoint, datapoint + N;
* `map (* N)`              : for each datapoint, datapoint * N;
* `map (/ N)`              : for each datapoint, datapoint / N;
* `map (- N)`              : for each datapoint, datapoint - N;
* `map (N -)`              : for each datapoint, N - datapoint;
* `map max`                : for each datapoint yields the maximum found so far;
* `map min`                : for each datapoint yields the mimimum found so far;
* `map (ewma R)`           : for each point yields the exponential moving average using R as the alpha parameter;
* `map mean`               : for each point yields the current mean value;
* `map count`              : for each point yields the current size of the series;
* `map hmean`              : for each point yields the harmonic mean value;
* `map abs`                : absolute function [eg.: abs -9 = 9];
* `map ceil`               : ceiling function [eg.: ceil 2.1 = 3];
* `map floor`              : floor function [eg.: floor 2.9 = 2];
* `map round`              : rounding function [eg.: round 2.5 = 3];
* `map truncate`           : truncate function [ex.: truncate 2.3 = 2];
* `map sqrt`               : square root function;
* `map (log R)`            : logarithm base R;
* `reduce (+)`             : summation of the datapoints;
* `reduce (*)`             : product of the datapoints;
* `reduce min`             : the minimum value of the series;
* `reduce max`             : the maximum value of the series;
* `reduce count`           : the size of the series;
* `reduce mean`            : the mean value of the series;
* `reduce hmean`           : the harmonic mean value of the series;
* `reduce (ewma R)`        : the exponential moving average using R as the alpha parameter;
* `filter (> N)`           : yields datapoints with value > N;
* `filter (< N)`           : yields datapoints with value < N;
* `filter (>= N)`          : yields datapoints with value >= N;
* `filter (<= N)`          : yields datapoints with value <= N;
* `filter (N >)`           : yields datapoints with N > value;
* `filter (N <)`           : yields datapoints with N < value;
* `filter (N >=)`          : yields datapoints with N >= value;
* `filter (N <=)`          : yields datapoints with N <= value;
* `filter (N ==)`          : yields datapoints with values == N;
* `filter (== N)`          : yields datapoints with values == N;
* `window N max`           : groups N datapoints and yields the maximum;
* `window N min`           : groups N datapoints and yields the maximum;
* `window N (*)`           : groups N datapoints and yields the product of the series;
* `window N (+)`           : groups N datapoints and yields the summation of the series;
* `window N (ewma R)`      : groups N datapoints and yields the exponential moving average;
* `window N mean`          : groups N datapoints and yields the mean value;
* `window N hmean`         : groups N datapoints and yields the harmonic mean value;
* `time-window N max`      : groups N seconds of data and yields the maximum;
* `time-window N min`      : groups N seconds of data and yields the minimum;
* `time-window N (*)`      : groups N seconds of data and yields the product of the series;
* `time-window N (+)`      : groups N seconds of data and yields the summation of the series;
* `time-window N mean`     : groups N seconds of data and yields then mean value;
* `time-window N hmean`    : groups N seconds of data and yields then harmonic mean value;
* `time-window N (ewma R)` : groups N seconds of data and yields the exponential moving average;

### ATTR PUT ###

Use this command to insert/modify data. `Kv` properties looks like:

```
  QUERY: attr put GUID "NAME" VALUE [WITH OPTIONS];
  REPLY: ()
```

The value can be any value as described in the beginning of this
document. Example:

```
  > using (leela::sandbox) attr put cd8a5e22-622e-11e5-bb66-fb5334d472bc "foo" "bar";
  < ()
```

Notice that put either inserts or updates the value. Now, for `ts` properties:

```
  QUERY: attr put GUID "NAME" [TIMESTAMP] VALUE;
  REPLY: ()
```

The syntax is the same but the `[TIMESTAMP]` bit. This should contain
a timestamp, i.e, the number of seconds since Jan/01/1970. For instance:

```
  > using (leela::sandbox) attr put cd8a5e22-622e-11e5-bb66-fb5334d472bc "foo" [0] "bar";
  < ()
```

This inserts the value `"bar"` at the given timestamp.

#### OPTIONS ####

  * ttl:N sets an expiration time of N seconds for this data point. Example, an ttl of 60s:

```
  > using (leela::sandbox) attr put cd8a5e22-622e-11e5-bb66-fb5334d472bc "foo" [0] "bar" with ttl:60;
  < ()
```
### ATTR DEL ###

Use this command to remove data. To remove `kv` properties:

```
  QUERY: attr del GUID "NAME";
  REPLY: ()
```

Similarly, to remove `ts` properties:

```
  QUERY: attr del GUID "NAME" [S:E]
  REPLY: ()
```

The only difference is the timestamp bit. This is an interval, and all
the values from S to E [inclusive] will be removed.

Examples:

```
  > using (leela::sandbox) attr del cd8a5e22-622e-11e5-bb66-fb5334d472bc "foo";
  < ()
  > using (leela::sandbox) attr del cd8a5e22-622e-11e5-bb66-fb5334d472bc "foo" [0:3600];
  < ()
```

### ATTR KLS/TLS ###

This allows you to enumerate properties by name using a glob-like
syntax. The `kls` command is used to list `kv`
properties. Unsurprisingly, `tls` is used to list `ts` properties.

```
  QUERY: attr kls GUID "NAME-GLOB"
  REPLY: NATTR

  QUERY: attr tls GUID "NAME-GLOB"
  REPLY: TATTR
```

A few examples:

```
  > using (leela::sandbox) attr kls cd8a5e22-622e-11e5-bb66-fb5334d472bc "*"
  < guid`                                 | names
  < cd8a5e22-622e-11e5-bb66-fb5334d472bc | cpu-idle | memory-free | swap-free ...

  > using (leela::sandbox) attr tls cd8a5e22-622e-11e5-bb66-fb5334d472bc "c*"
  < guid                                 | names
  < cd8a5e22-622e-11e5-bb66-fb5334d472bc | cpu-idle
```
