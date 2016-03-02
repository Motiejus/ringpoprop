#!/usr/bin/env python
import tornado
from tornado.ioloop import IOLoop
from tornado.web import Application, RequestHandler
import sys

class MainHandler(RequestHandler):
    def get(self):
        self.finish("foo")
application = Application([ (r"/", MainHandler), ])

@tornado.gen.coroutine
def close_callback(*args, **kwargs):
    print args, kwargs

if __name__ == "__main__":
    application.listen(8888)

    stdin = tornado.iostream.PipeIOStream(sys.stdin.fileno())
    stdin.set_close_callback(close_callback)

    IOLoop.instance().start()
