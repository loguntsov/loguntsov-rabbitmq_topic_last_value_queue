#!/usr/bin/env python3
import asyncio
from aio_pika import connect, ExchangeType, IncomingMessage
import time
from datetime import datetime

global_sleep = 1.0

async def on_message(message: IncomingMessage):
    now = datetime.now()
    current_time = now.strftime("%H:%M:%S.%f")
    print("Now: ", current_time, " Received ", message.body.decode())
    await asyncio.sleep(global_sleep)
    await message.ack()

async def consume(loop, queue_name, exchange_name, exchange_type, sleep_seconds, routing_keys, last_value):
    global global_sleep
    global_sleep = sleep_seconds

    connection = await connect(
        "amqp://localhost/", loop=loop
    )

    channel = await connection.channel()
    await channel.set_qos(prefetch_count=1)
    exchange = await channel.declare_exchange(exchange_name, exchange_type)

    args = {}
    if last_value:
        args['x-last-value-per-topic'] = 'enabled'
        print("Creating ltv queue: " + queue_name)
    else:
        print("Creating normal queue: " + queue_name)
    queue = await channel.declare_queue(queue_name, durable=False, auto_delete=True, exclusive=True, arguments=args)

    for routing_key in routing_keys:
        print('Binding queue to key: ' + routing_key)
        await queue.bind(exchange, routing_key)

    print(' [*] Waiting for messages. To exit press CTRL+C')
    await queue.consume(on_message)
