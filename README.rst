ringpoprop
==========

Property-based tests for ringpop.

Build, compile and run tests
----------------------------

::

    $ make setup  ## requires `virtualenv` in `$PATH`).
    $ make

TODO
----

Things to do before any kind of announcement of this experiment:

* Documentation generator.
* Elvis.

TChannel
========

This module implements a small enough subset of tchannel, just enough to be
able to establish a connection, keep it open, send and receive simple (<64KB)
JSON payloads.

API
---

The sketch of API is as follows.

See tchannel:option/0 for more options::

  Opts = [{tcp_connect_timeout, 500},
          {init_timeout, 500}],

Establishes the TCP connection and initializes tchannel state::

  {ok, Channel} = tchannel:connect("172.16.0.1:3001", <<"hello">>, Options),

Constructing headers for ``tchannel:send/3``. See tchannel spec for details::

  Headers = [{as, <<"json">>},   % required
             {cas, undefined},   % optional
             {cn, <<"echoer">>}, % required
             {re, undefined},    % optional
             {se, undefined},    % optional
             {fd, undefined},    % optional
             {sk, undefined}     % optional
             {rd, undefined}],   % optional

Contstructing outgoing message::

  MsgOpts = [{ttl, 1000},
             {tracing, undefined}, % not supported
             {service, <<"echo_service">>},
             {headers, Headers}],

Sending the actual message::

  {ok, Msg} = tchannel:send(Channel, <<"ping">>, MsgOpts),

Wait for the response (init_res) to that message::

  {ok, Reply} = tchannel:recv(Msg).

TODO: what to do with that message? Get this stuff:

1. headers.
2. response code.
3. response payload.
