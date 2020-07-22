import boto3
import requests
import time
import os
import json
import dotenv

if __name__ == '__main__':
    dotenv.load_dotenv()

ssm = boto3.client('ssm')
cf = boto3.client('cloudformation')

ENV = os.environ.get('ENV', 'prod')
SERVICES_STACK = "%s-services" % ENV
MACHINES_STACK = "%s-machines" % ENV

api_token_parameter_name = "/restyled/%s/restyled-api-token" % ENV
api_token_parameter = ssm.get_parameter(Name=api_token_parameter_name)
RESTYLED_API_HOST = os.environ.get('RESTYLED_API_HOST', 'https://restyled.io')
RESTYLED_API_TOKEN = api_token_parameter['Parameter']['Value']


def handler(_event, _context):
    try:
        machines_state = get_machines_state()
        return scale(machines_state['depth'], machines_state['machines'])
    except Exception as err:
        return {'ok': False, 'error': err}


def scale(depth, machines):
    def result_ok(x): return {'ok': True, 'result': x,
                              'depth': depth, 'machines': machines}

    if depth > 40 and machines < 4:
        update_stack_parameter(MACHINES_STACK, 'DesiredCapacity', 4)
        update_stack_parameter(SERVICES_STACK, 'AppsWebhooksDesiredCount', 8)
        return result_ok('ScaledUp')

    if depth < 10 and machines > 1:
        update_stack_parameter(SERVICES_STACK, 'AppsWebhooksDesiredCount', 2)
        update_stack_parameter(MACHINES_STACK, 'DesiredCapacity', 1)
        return result_ok('ScaledDown')

    return result_ok('NoChange')


def get_machines_state():
    url = "%s%s" % (RESTYLED_API_HOST, '/admin/machines/state')
    headers = {
        'Accept': 'application/json',
        'Content-type': 'application/json',
        'Authorization': "token %s" % RESTYLED_API_TOKEN
    }
    body = requests.get(url, headers=headers).json()

    return {
        'depth': body['queueDepth'],
        'machines': len(body['jobCounts'])
    }


def update_stack_parameter(stack_name, key, value):
    print("Updating %s Stack's Parameter: %s to %s" % (stack_name, key, value))
    cf.update_stack(StackName=stack_name,
                    UsePreviousTemplate=True,
                    Parameters=[
                        {'ParameterKey': str(key),
                         'ParameterValue': str(value)
                         }
                    ],
                    Capabilities='CAPABILITY_NAMED_IAM')
    waiter = client.get_waiter('stack_update_complete')
    waiter.wait(StackName=stack_name)


if __name__ == '__main__':
    while True:
        print(handler(None, None))
        time.sleep(30)
