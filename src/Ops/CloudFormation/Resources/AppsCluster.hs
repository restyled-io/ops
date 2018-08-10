{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Ops.CloudFormation.Resources.AppsCluster
    ( appsClusterResources
    ) where

import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text)
import Data.Text.Internal.Builder
import Data.Text.Lazy (toStrict)
import Ops.CloudFormation.Parameters
import Stratosphere
import Text.Shakespeare.Text (textFile)

appsClusterResources :: Resources
appsClusterResources =
    [ resource "AppsSecurityGroup"
        $ EC2SecurityGroupProperties
        $ ec2SecurityGroup (prefixRef "Apps")
        & ecsgVpcId ?~ Ref "Vpc"
        & ecsgTags ?~ tag "Name" (prefixRef "Apps") : defaultTags
        & ecsgSecurityGroupIngress ?~
            [ ec2SecurityGroupIngressProperty "tcp"
                & ecsgipFromPort ?~ Literal 31000
                & ecsgipToPort ?~ Literal 61000
                & ecsgipSourceSecurityGroupId ?~ Ref "ALBSecurityGroup"
            ]
    , resource "AppsInstanceRole"
        $ IAMRoleProperties
        $ iamRole (toObject [aesonQQ|
        {
            "Version": "2012-10-17",
            "Statement": [{
                "Principal": {"Service": "ec2.amazonaws.com"},
                "Action": "sts:AssumeRole",
                "Effect": "Allow"
            }]
        }
        |])
        & iamrRoleName ?~ prefixRef "InstanceRole"
        & iamrManagedPolicyArns ?~
            [ "arn:aws:iam::aws:policy/service-role/AmazonEC2ContainerServiceforEC2Role"
            ]
        & iamrPolicies ?~
            [ iamRolePolicy
                (toObject [aesonQQ|
                    {
                        "Version": "2012-10-17",
                        "Statement": [{
                            "Effect": "Allow",
                            "Action": [
                                "logs:CreateLogGroup",
                                "logs:CreateLogStream",
                                "logs:PutLogEvents",
                                "logs:DescribeLogStreams"
                            ],
                            "Resource": ["arn:aws:logs:*:*:*"]
                        }]
                    }
                    |]
                )
                (prefixRef "AppsLogsPolicy")
            ]
    , resource "AppsInstanceProfile"
        $ IAMInstanceProfileProperties
        $ iamInstanceProfile [Ref "AppsInstanceRole"]
    , resource "AppsLaunchConfiguration"
        $ AutoScalingLaunchConfigurationProperties
        $ autoScalingLaunchConfiguration
            (Ref "AppsClusterAMI")
            (Ref "AppsClusterInstanceType")
        & aslcIamInstanceProfile ?~ Ref "AppsInstanceProfile"
        & aslcSecurityGroups ?~ [Ref "AppsSecurityGroup"]
        & aslcUserData ?~ Base64 (Sub userDataText Nothing)
    , resource "AppsAutoScalingGroup"
        ( AutoScalingAutoScalingGroupProperties
        $ autoScalingAutoScalingGroup "5" "1" -- [sic]
        & asasgLaunchConfigurationName ?~ Ref "AppsLaunchConfiguration"
        & asasgVPCZoneIdentifier ?~
            [ Ref "PrivateSubnet1"
            , Ref "PrivateSubnet2"
            , Ref "PrivateSubnet3"
            ]
        & asasgDesiredCapacity ?~ Ref "AppsClusterSize"
        & asasgTags ?~
            [ autoScalingAutoScalingGroupTagProperty
                "Name" (Literal True) (prefixRef "Apps")
            , autoScalingAutoScalingGroupTagProperty
                "App" (Literal True) (Ref "App")
            , autoScalingAutoScalingGroupTagProperty
                "Environment" (Literal True) (Ref "Environment")
            ]
        )
        & resCreationPolicy ?~ creationPolicy
            (resourceSignal
                & rsCount ?~ Ref "AppsClusterSize"
                & rsTimeout ?~ "PT60S")
        & resUpdatePolicy ?~ (updatePolicy
            & upAutoScalingRollingUpdate ?~ (autoScalingRollingUpdatePolicy
                & asrupMaxBatchSize ?~ Literal 1
                & asrupMinInstancesInService ?~ Literal 2
                & asrupPauseTime ?~ "PT60S"
                & asrupWaitOnResourceSignals ?~ Literal True))
    , resource "AppsCluster"
        $ ECSClusterProperties
        $ ecsCluster
        & ecscClusterName ?~ prefixRef "Apps"
    , resource "AppsClusterLogGroup"
        $ LogsLogGroupProperties
        $ logsLogGroup
        & llgLogGroupName ?~ prefixRef "Apps"
        & llgRetentionInDays ?~ Literal 7
    ]

userDataText :: Text
userDataText = fromTextFile $(textFile "files/user-data.sh")

fromTextFile :: (Text -> Builder) -> Text
fromTextFile q = toStrict $ toLazyText $ q ""
