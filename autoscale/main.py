import boto3
import dotenv
import json
import os
import requests
import time

dotenv.load_dotenv()

ENV = os.environ.get('ENV', 'prod')

STACK_NAMES = {
    'webhooks': '%s-services' % ENV,
    'machines': '%s-machines' % ENV
}

STACK_PARAMS = {
    'webhooks': 'AppsWebhooksDesiredCount',
    'machines': 'DesiredCapacity'
}

cf = boto3.client('cloudformation')
ssm = boto3.client('ssm')
api_token_parameter_name = "/restyled/%s/restyled-api-token" % ENV
api_token_parameter = ssm.get_parameter(Name=api_token_parameter_name)
RESTYLED_API_HOST = os.environ.get('RESTYLED_API_HOST', 'https://restyled.io')
RESTYLED_API_TOKEN = api_token_parameter['Parameter']['Value']

RESTYLED_API_URL = "%s%s" % (RESTYLED_API_HOST, '/admin/machines/state')
RESTYLED_API_HEADERS = {
    'Accept': 'application/json',
    'Content-type': 'application/json',
    'Authorization': "token %s" % RESTYLED_API_TOKEN
}


def handler(_event, _context):
    ok = True
    events = []

    def append_event(x):
        if x:
            events.append(x)

    try:
        resp = requests.get(RESTYLED_API_URL, headers=RESTYLED_API_HEADERS)
        depth = resp.json()['queueDepth']
        append_event({'depth': depth, 'high': depth > 30, 'low': depth < 1})

        if depth > 30:
            append_event(scale('webhooks', 8))
            append_event(scale('machines', 4))

        if depth < 1:
            append_event(scale('webhooks', 2))
            append_event(scale('machines', 1))

    except Exception as err:
        ok = False
        append_event({'error': err})

    return {'ok': ok, 'events': events}


def scale(service, desired):
    stack_name = STACK_NAMES[service]
    param = STACK_PARAMS[service]
    current = int(get_stack_parameter(stack_name, param))

    if current != desired:
        update_stack_parameter(stack_name, param, str(desired))
        return {'update': "%s[%s] %d => %d" % (stack_name, param, current, desired)}

    return None


def get_stack_parameter(stack_name, key):
    stacks = cf.describe_stacks(StackName=stack_name)['Stacks']

    if len(stacks) == 0:
        raise Exception("Stack %s not found" % stack_name)

    parameters = stacks[0]['Parameters']
    parameter = next(p for p in parameters if p['ParameterKey'] == key)

    if not parameter:
        raise Exception("%s is not a Parameter in %s" % (key, stack_name))

    return parameter['ParameterValue']


def update_stack_parameter(stack_name, key, value):
    parameter = [{'ParameterKey': key, 'ParameterValue': value}]
    capabilities = ['CAPABILITY_NAMED_IAM']
    cf.update_stack(StackName=stack_name,
                    UsePreviousTemplate=True,
                    Parameters=parameters,
                    Capabilities=capabilities)
