package com.rabbitmq.examples;

import com.rabbitmq.client.AMQP.BasicProperties;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.DefaultConsumer;
import com.rabbitmq.client.Envelope;
import com.rabbitmq.client.MessageProperties;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

public class PriorityQueue {
    private static final String QUEUE = "my-priority-queue";

    public static void main(String[] argv) throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        Connection conn = factory.newConnection();
        Channel ch = conn.createChannel();

        Map<String, Object> args = new HashMap<String, Object>();
        args.put("x-max-priority", 10);
        ch.queueDeclare(QUEUE, true, false, false, args);

        publish(ch, 0);
        publish(ch, 5);
        publish(ch, 10);

        final CountDownLatch latch = new CountDownLatch(3);
        ch.basicConsume(QUEUE, true, new DefaultConsumer(ch) {
            @Override
            public void handleDelivery(String consumerTag, Envelope envelope, BasicProperties properties, byte[] body) throws IOException {
                System.out.println("Received " + new String(body));
                latch.countDown();
            }
        });

        latch.await();
        conn.close();
    }

    private static void publish(Channel ch, int priority) throws IOException {
        BasicProperties props = MessageProperties.PERSISTENT_BASIC.builder().priority(priority).build();
        String body = "message with priority " + priority;
        System.out.println("Sent " + body);
        ch.basicPublish("", QUEUE, props, body.getBytes());
    }
}
