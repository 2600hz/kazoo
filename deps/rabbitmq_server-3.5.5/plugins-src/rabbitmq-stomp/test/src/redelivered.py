import unittest
import stomp
import base
import time

class TestRedelivered(base.BaseTest):

    def test_redelivered(self):
        destination = "/queue/redelivered-test"

        # subscribe and send message
        self.subscribe_dest(self.conn, destination, None, ack='client')
        self.conn.send(destination, "test1")
        self.assertTrue(self.listener.await(4), "initial message not received")
        self.assertEquals(1, len(self.listener.messages))
        self.assertEquals('false', self.listener.messages[0]['headers']['redelivered'])

        # disconnect with no ack
        self.conn.disconnect()

        # now reconnect
        conn2 = self.create_connection()
        try:
            listener2 = base.WaitableListener()
            listener2.reset(1)
            conn2.set_listener('', listener2)
            self.subscribe_dest(conn2, destination, None, ack='client')
            self.assertTrue(listener2.await(), "message not received again")
            self.assertEquals(1, len(listener2.messages))
            self.assertEquals('true', listener2.messages[0]['headers']['redelivered'])
        finally:
            conn2.disconnect()
