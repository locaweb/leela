=========
 Web api
=========

This exposes data via a *REST* interface. The following should apply
to all resources:

* All resources support the *JSON-P* protocol by appending the
  ``callback`` parameter to the URL;
* Currently only JSON format is supported;

Resources
=========

/v1/:key/:year/:month
---------------------

Retrieves data from the given month.

:key: the event to load [e.g. localhost.cpu.idle];
:year: the year [4 digits];
:month: the month [numeric, starts with 1];

/v1/:key/:year/:month/:day
--------------------------

Retrieves data from the given day.

:key: the event to load [e.g. localhost.cpu.idle];
:year: the year [4 digits];
:month: the month [numeric, starts with 1];
:day: the day of month [numeric, starts with 1];

/v1/:key/past24
---------------

Retrieves data from the past 24 hours.

:key: the event to load [e.g. localhost.cpu.idle];

/v1/:key/pastweek
-----------------

Retrieves data from the past week (7 days).

:key: the event to load [e.g. localhost.cpu.idle];

Response Codes
==============

:200: Success;

:404: the requested data could not be found (invalid range, missing
      event etc.);

:500: internal server error;

:400: you did something wrong;

Payload: failure case
~~~~~~~~~~~~~~~~~~~~~
::

  {"code": int, "message": string}

:code: the http response code (e.g. 200, 400);
:message: a very short description of what went wrong;

Example:
::

  {"code": 404, "message": "no event found"}

Payload: success case
~~~~~~~~~~~~~~~~~~~~~
::

  {"request.uri": string,
   "results": {key: timeseries}
  }

:request.uri: the resource used to retrieve this data;
:key: the event requested;
:timeseries: A list with a 2-tuple ``[timestamp, value]``;

Example:
::

  {"request.uri": "/v1/localhost.cpu.idle/past24",
   "results": {"localhost.cpu.idle": [ [0,  0],
                                       [60, 12.5]
                                     ]}
  }
