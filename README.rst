ringpoprop
==========

Property-based tests for ringpop.

Build, compile and run tests
----------------------------

    $ make setup  ## requires ``virtualenv`` in ``$PATH``).
    $ make

TODO
----

Things to do before any kind of announcement of this experiment:

# Documentation generator.
# Elvis.

TChannel
--------

This module implements a small enough subset of tchannel, just enough to be
able to establish a connection, keep it open, send and receive simple (<64KB)
JSON payloads.
