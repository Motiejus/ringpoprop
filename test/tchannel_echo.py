#!/usr/bin/env python

import sys

from tornado import ioloop
from tchannel import TChannel


tchannel = TChannel('echo-server', hostport='127.0.0.1:0')


@tchannel.json.register('/echo')
def handler(request):
    return request.body

if __name__ == '__main__':
    tchannel.listen()
    print(tchannel.hostport)
    sys.stdout.flush()
    ioloop.IOLoop.current().start()
