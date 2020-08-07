import boto3
import os
import requests

ENV = os.environ['ENV'] # required

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

cw = boto3.client('cloudwatch')

def handler(_event, _context):
    resp = requests.get(RESTYLED_API_URL, headers=RESTYLED_API_HEADERS)
    depth = resp.json()['queueDepth']
    cw.put_metric_data(
        Namespace='Restyled',
        MetricData=[{
            'MetricName': 'QueueDepth',
            'Dimensions': [{'Name': 'Environment', 'Value': ENV}],
            'Value': depth
        }]
    )
    return {'ok': True, 'depth': depth}
