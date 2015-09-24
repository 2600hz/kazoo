import unittest
import stomp
import base
import time

class TestTransactions(base.BaseTest):

    def test_tx_commit(self):
        ''' Test TX with a COMMIT and ensure messages are delivered '''
        destination = "/exchange/amq.fanout"
        tx = "test.tx"

        self.listener.reset()
        self.subscribe_dest(self.conn, destination, None)
        self.conn.begin(transaction=tx)
        self.conn.send(destination, "hello!", transaction=tx)
        self.conn.send(destination, "again!")

        ## should see the second message
        self.assertTrue(self.listener.await(3))
        self.assertEquals(1, len(self.listener.messages))
        self.assertEquals("again!", self.listener.messages[0]['message'])

        ## now look for the first message
        self.listener.reset()
        self.conn.commit(transaction=tx)
        self.assertTrue(self.listener.await(3))
        self.assertEquals(1, len(self.listener.messages),
                          "Missing committed message")
        self.assertEquals("hello!", self.listener.messages[0]['message'])

    def test_tx_abort(self):
        ''' Test TX with an ABORT and ensure messages are discarded '''
        destination = "/exchange/amq.fanout"
        tx = "test.tx"

        self.listener.reset()
        self.subscribe_dest(self.conn, destination, None)
        self.conn.begin(transaction=tx)
        self.conn.send(destination, "hello!", transaction=tx)
        self.conn.send(destination, "again!")

        ## should see the second message
        self.assertTrue(self.listener.await(3))
        self.assertEquals(1, len(self.listener.messages))
        self.assertEquals("again!", self.listener.messages[0]['message'])

        ## now look for the first message to be discarded
        self.listener.reset()
        self.conn.abort(transaction=tx)
        self.assertFalse(self.listener.await(3))
        self.assertEquals(0, len(self.listener.messages),
                          "Unexpected committed message")

