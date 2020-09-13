#!/usr/bin/env python3
import asyncio
from aio_pika import connect, ExchangeType, IncomingMessage
import time
from datetime import datetime
import sys
import json

loop = asyncio.get_event_loop()
PREFETCH = 10

async def on_message(message: IncomingMessage):
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S.%f")
    payload = message.body.decode()
   
    print("Now: ", current_time, " Received ", payload)

    '''
    try:
        value_data = (json.loads(payload))
        if value_data['message_type'] == 'liquidation_price' or value_data['message_type'] == 'liquidation_distance':
            print("skpping")
        else:
            value_body = value_data['message_body']
            val = value_body['value']
            timestamp = value_body['timestamp']
            ts = datetime.fromtimestamp(float(timestamp)/1000/1000/1000).strftime('%Y-%m-%d %H:%M:%S.%f')
            entity = value_body['entity']
            print("Now: ", current_time, " Received " + ts + " [" + entity + "] = " + val)
    except Exception as e:
        print(str(e))
        print()
        print("Input was: " + payload)
    '''

    await asyncio.sleep(sleep_time)
    await message.ack()

async def main(queue_name, exchange_name, sleep_time, last_value):
    connection = await connect(
        "amqp://localhost/", loop=loop
    )

    channel = await connection.channel()
    await channel.set_qos(prefetch_count=PREFETCH)
    exchange = await channel.declare_exchange(exchange_name, ExchangeType.TOPIC)

    args = {}
    if last_value:
        args['x-last-value-per-topic'] = 'enabled'
    queue = await channel.declare_queue(queue_name, durable=False, auto_delete=True, exclusive=True, arguments=args)

    routing_key = '1.0.value.OKEX.#'
    print('Binding queue to key: ' + routing_key)
    await queue.bind(exchange, routing_key)

    print(' [*] Waiting for messages. To exit press CTRL+C')
    await queue.consume(on_message)


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: consumer.py <exchange_name> <queue_name> <sleep_time> [--last-value]")
        sys.exit(1)
    queue_name = sys.argv[1]
    exchange_name = sys.argv[2]
    last_value = False

    sleep_time = float(sys.argv[3])

    if len(sys.argv) > 4:
        if sys.argv[4] == '--last-value':
            last_value = True
    loop = asyncio.get_event_loop()
    loop.create_task(main(queue_name, exchange_name, sleep_time, last_value))
    loop.run_forever()
