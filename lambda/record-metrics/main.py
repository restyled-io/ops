import boto3
import os
import requests
import json


def handler(_event, _context):
    env = os.environ.get('ENV', 'dev')

    def add_env_dimension(metric):
        env_dimension = {'Name': 'Environment', 'Value': env}
        metric['Dimensions'] = metric.get('Dimensions', [])
        metric['Dimensions'].append(env_dimension)
        return metric

    host = get_restyled_host(env)
    token = get_restyled_token(env)

    metrics = get_system_metrics(host, token)
    metric_data = list(map(add_env_dimension, metrics))

    cw = boto3.client('cloudwatch')
    cw.put_metric_data(Namespace='Restyled', MetricData=metric_data)

    metric_names = list(map(lambda x: x['MetricName'], metric_data))
    return {'ok': True, 'env': env, 'host': host, 'recorded': metric_names}


def get_restyled_host(env):
    overrides = {'dev': 'https://restyled.ngrok.io',
                 'test': 'https://restyled.io',
                 'prod': 'https://restyled.io'}
    return overrides.get(env, "https://%s.restyled.io" % env)


def get_restyled_token(env):
    ssm = boto3.client('ssm')
    token_parameter_name = "/restyled/%s/restyled-api-token" % env
    token_parameter = ssm.get_parameter(Name=token_parameter_name)
    return token_parameter['Parameter']['Value']


def get_system_metrics(host, token):
    resp = requests.get("%s/system/metrics" % host,
                        params={'since-minutes': 1},
                        headers={
                            'Accept': 'application/json',
                            'Content-type': 'application/json',
                            'Authorization': "token %s" % token
                        })
    return resp.json()['metrics']


if __name__ == '__main__':
    result = handler(None, None)
    print(json.dumps(result, indent=2))
