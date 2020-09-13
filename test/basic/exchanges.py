import sys
from aio_pika import ExchangeType

exchanges = {}
exchanges['topic'] = {'name': 'topic-routing-test', 'type': ExchangeType.TOPIC}
exchanges['direct'] = {'name': 'direct-routing-test', 'type': ExchangeType.DIRECT}
exchanges['fanout'] = {'name': 'fanout-routing-test', 'type': ExchangeType.FANOUT}


def get_exchange_from_cmdline(exchange_name_str):
    if exchange_name_str not in ['topic', 'direct', 'fanout']:
        print("Usage: exchange type must be topic, direct or fanout")
        sys.exit(2)
        
    return exchanges[exchange_name_str]['type'], exchanges[exchange_name_str]['name']
