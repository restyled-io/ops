#!/bin/bash
exec > >(tee /var/log/user-data.log|logger -t user-data -s 2>/dev/console) 2>&1
set -ex

yum update -y
yum install -y aws-cfn-bootstrap awslogs

# shellcheck disable=SC2016
sed -i 's/region =.*/region = ${AWS::Region}/g' /etc/awslogs/awscli.conf

cat >/etc/awslogs/awslogs.conf <<'EOM'
[general]
state_file = /var/lib/awslogs/agent-state

[/var/log/secure]
file = /var/log/secure
log_group_name = ${App}${Environment}AppsInstance
log_stream_name = secure/{instance_id}
datetime_format = %b %d %H:%M:%S

[/var/log/dmesg]
file = /var/log/dmesg
log_group_name = ${App}${Environment}AppsInstance
log_stream_name = dmesg/{instance_id}

[/var/log/messages]
file = /var/log/messages
log_group_name = ${App}${Environment}AppsInstance
log_stream_name = messages/{instance_id}
datetime_format = %b %d %H:%M:%S

[/var/log/docker]
file = /var/log/docker
log_group_name = ${App}${Environment}AppsInstance
log_stream_name = docker/{instance_id}
datetime_format = %Y-%m-%dT%H:%M:%S.%f

[/var/log/ecs/ecs-init.log]
file = /var/log/ecs/ecs-init.log*
log_group_name = ${App}${Environment}AppsInstance
log_stream_name = ecs/ecs-init/{instance_id}
datetime_format = %Y-%m-%dT%H:%M:%SZ

[/var/log/ecs/ecs-agent.log]
file = /var/log/ecs/ecs-agent.log*
log_group_name = ${App}${Environment}AppsInstance
log_stream_name = ecs/ecs-agent/{instance_id}
datetime_format = %Y-%m-%dT%H:%M:%SZ

[/var/log/ecs/audit.log]
file = /var/log/ecs/audit.log*
log_group_name = ${App}${Environment}AppsInstance
log_stream_name = ecs/audit/{instance_id}
datetime_format = %Y-%m-%dT%H:%M:%SZ
EOM

service awslogs start

# shellcheck disable=SC2086
# shellcheck disable=SC2154
echo ECS_CLUSTER=${App}${Environment}Apps >> /etc/ecs/ecs.config
echo ECS_AVAILABLE_LOGGING_DRIVERS='["json-file","awslogs"]' >> /etc/ecs/ecs.config

/opt/aws/bin/cfn-signal \
  --success true \
  --region "${AWS::Region}" \
  --stack "${AWS::StackName}" \
  --resource AppsAutoScalingGroup
