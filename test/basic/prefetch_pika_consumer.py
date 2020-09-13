#!/usr/bin/env python3
import pika
import time
from datetime import datetime


def callback(ch, method, properties, body):
    
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S.%f")
    print("Now: ", current_time, " [x] Received ", body)
    time.sleep(1)
    ch.basic_ack(delivery_tag = method.delivery_tag)

def main():

    conn = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
    channel = conn.channel()
    channel.basic_qos(prefetch_count=10)

    exchange_name = "topic-routing-test"
    channel.exchange_declare(exchange_name, exchange_type="topic")

    args = {}
    args['x-last-value-per-topic'] = 'enabled'
    channel.queue_declare(queue='test_queue2', durable=False, auto_delete=True, exclusive=True, arguments=args)
    for key in ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o']:
        channel.queue_bind('test_queue2', exchange_name, key)

    channel.basic_consume(on_message_callback=callback, queue='test_queue2', auto_ack=False)

    print(' [*] Waiting for messages. To exit press CTRL+C')
    channel.start_consuming()


if __name__ == "__main__":
    main()
