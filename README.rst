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
  Headers = tchannel_resp:headers(),
  Code = tchannel_resp:code(),
  {Arg1, Arg2, Arg3} = tchannel_resp:payload().

TODO
----

The API and implementation are explicitly minimalistic, because the intention
is to implement as less as possible without overthinking. It is better to make
big changes when the codebase is small and limited, rather than big and
complete. With a good introduction, we lack:

1. Receiving a request without sending a message.
2. Node and Go have sub-channels. In node/go, a sub-channel can have multiple
   peers. Do we need subchannels at all?
3. Initial version will definitely not implement >64K requests and responses.
4. In Erlang, a TChannel instance maps 1:1 to the underlying TCP connection. It
   is not true in Go/Node APIs, but is not mandated by the protocol. We'll know
   if we need to do that after actually using it for some time.
