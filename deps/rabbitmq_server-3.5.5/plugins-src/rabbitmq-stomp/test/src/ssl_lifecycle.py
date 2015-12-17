import unittest
import os
import os.path
import sys

import stomp
import base
import ssl


base_path = os.path.dirname(sys.argv[0])

ssl_key_file = os.path.abspath(base_path + "/../certs/client/key.pem")
ssl_cert_file = os.path.abspath(base_path + "/../certs/client/cert.pem")
ssl_ca_certs = os.path.abspath(base_path  + "/../certs/testca/cacert.pem")

class TestSslClient(unittest.TestCase):

    def __ssl_connect(self):
        conn = stomp.Connection(host_and_ports = [ ('localhost', 61614) ],
                                use_ssl = True, ssl_key_file = ssl_key_file,
                                ssl_cert_file = ssl_cert_file,
                                ssl_ca_certs = ssl_ca_certs)
        print "FILE: ", ssl_cert_file
        conn.start()
        conn.connect("guest", "guest")
        return conn

    def __ssl_auth_connect(self):
        conn = stomp.Connection(host_and_ports = [ ('localhost', 61614) ],
                                use_ssl = True, ssl_key_file = ssl_key_file,
                                ssl_cert_file = ssl_cert_file,
                                ssl_ca_certs = ssl_ca_certs)
        conn.start()
        conn.connect()
        return conn

    def test_ssl_connect(self):
        conn = self.__ssl_connect()
        conn.disconnect()

    def test_ssl_auth_connect(self):
        conn = self.__ssl_auth_connect()
        conn.disconnect()

    def test_ssl_send_receive(self):
        conn = self.__ssl_connect()
        self.__test_conn(conn)

    def test_ssl_auth_send_receive(self):
        conn = self.__ssl_auth_connect()
        self.__test_conn(conn)

    def __test_conn(self, conn):
        try:
            listener = base.WaitableListener()

            conn.set_listener('', listener)

            d = "/topic/ssl.test"
            conn.subscribe(destination=d, ack="auto", id="ctag", receipt="sub")

            self.assertTrue(listener.await(1))

            self.assertEquals("sub",
                              listener.receipts[0]['headers']['receipt-id'])

            listener.reset(1)
            conn.send(body="Hello SSL!", destination=d)

            self.assertTrue(listener.await())

            self.assertEquals("Hello SSL!", listener.messages[0]['message'])
        finally:
            conn.disconnect()
