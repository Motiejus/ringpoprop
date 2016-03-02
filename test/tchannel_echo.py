#!/usr/bin/env python

import sys

import tornado

from tornado import ioloop
from tchannel import TChannel

tchannel = TChannel('echo-server', hostport='127.0.0.1:0')

def on_stdin(fd, events):
    fd.readlines()
    print events

@tornado.gen.coroutine
def close_callback(*args, **kwargs):
    print args, kwargs

@tchannel.json.register('/echo')
def handler(request):
    return request.body

if __name__ == '__main__':
    tchannel.listen()
    print(tchannel.hostport)
    sys.stdout.flush()
    # TODO: http://www.tornadoweb.org/en/stable/iostream.html
    # set_close_callback
    #ioloop.IOLoop.current().add_handler(sys.stdin, on_stdin, ioloop.IOLoop.ERROR)

    stdin = tornado.iostream.PipeIOStream(sys.stdin.fileno())
    stdin.set_close_callback(close_callback)

    ioloop.IOLoop.current().start()
