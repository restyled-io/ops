{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.TaskDefinitions
    ( taskDefinitionResources
    ) where

import Data.Aeson (toJSON)
import Data.Text (Text)
import Ops.CloudFormation.Parameters
import Stratosphere

-- brittany-disable-next-binding

taskDefinitionResources :: Resources
taskDefinitionResources =
    [ resource "AppTaskDefinition"
        ( ECSTaskDefinitionProperties
        $ ecsTaskDefinition
        & ecstdFamily ?~ prefixRef "App"
        & ecstdContainerDefinitions ?~
            [ ecsTaskDefinitionContainerDefinition
                (Join ":" [Ref "AppsImageName", Ref "AppsImageTag"])
                (prefixRef "App")
                & ecstdcdCommand ?~ ["/app/restyled.io"]
                & ecstdcdEnvironment ?~
                    [ ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "APPROOT"
                        & ecstdkvpValue ?~ Join "" ["https://", fqdnRef]
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "DATABASE_URL"
                        & ecstdkvpValue ?~ Ref "DatabaseUrl"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "REDIS_URL"
                        & ecstdkvpValue ?~ Ref "RedisUrl"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_APP_ID"
                        & ecstdkvpValue ?~ Ref "GitHubAppId"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_APP_KEY_BASE64"
                        & ecstdkvpValue ?~ Ref "GitHubAppKeyBase64"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_OAUTH_CLIENT_ID"
                        & ecstdkvpValue ?~ Ref "GitHubAppOAuthClientId"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_OAUTH_CLIENT_SECRET"
                        & ecstdkvpValue ?~ Ref "GitHubAppOAuthClientSecret"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "ADMIN_EMAILS"
                        & ecstdkvpValue ?~ Ref "AppsAdminEmails"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "LOG_LEVEL"
                        & ecstdkvpValue ?~ Ref "AppsLogLevel"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "RESTYLER_TAG"
                        & ecstdkvpValue ?~ Ref "RestylerTag"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "SESSION_KEY"
                        & ecstdkvpValue ?~ Ref "SessionKey"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "CLOUDWATCH_EKG"
                        & ecstdkvpValue ?~ Ref "AppsCloudWatchEKG"
                    ]
                & ecstdcdCpu ?~ Literal 50
                & ecstdcdMemory ?~ Literal appMemory
                & ecstdcdMemoryReservation ?~ Literal appMemoryReservation
                & ecstdcdPortMappings ?~
                    [ ecsTaskDefinitionPortMapping
                        & ecstdpmContainerPort ?~ Literal 3000
                        & ecstdpmHostPort ?~ Literal 0
                    ]
                & ecstdcdLogConfiguration ?~ (ecsTaskDefinitionLogConfiguration "awslogs"
                    & ecstdlcOptions ?~
                        [ ("awslogs-group", prefixJSON "Apps")
                        , ("awslogs-region", toJSON (Ref "AWS::Region" :: Val Text))
                        , ("awslogs-stream-prefix", "App")
                        ])
            ]
        )
        & dependsOn ?~ ["AppsClusterLogGroup"]

    , resource "BackendTaskDefinition"
        ( ECSTaskDefinitionProperties
        $ ecsTaskDefinition
        & ecstdFamily ?~ prefixRef "Backend"
        & ecstdVolumes ?~
            [ ecsTaskDefinitionVolume
                & ecstdvName ?~ "tmp"
                & ecstdvHost ?~ (ecsTaskDefinitionHostVolumeProperties
                    & ecstdhvpSourcePath ?~ "/tmp")
            , ecsTaskDefinitionVolume
                & ecstdvName ?~ "docker-socket"
                & ecstdvHost ?~ (ecsTaskDefinitionHostVolumeProperties
                    & ecstdhvpSourcePath ?~ "/var/run/docker.sock")
            ]
        & ecstdContainerDefinitions ?~
            [ ecsTaskDefinitionContainerDefinition
                (Join ":" [Ref "AppsImageName", Ref "AppsImageTag"])
                (prefixRef "Backend")
                & ecstdcdUser ?~ "root" -- access to Docker deamon
                & ecstdcdCommand ?~ ["/app/restyled.io-backend"]
                & ecstdcdEnvironment ?~
                    [ ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "APPROOT"
                        & ecstdkvpValue ?~ Join "" ["https://", fqdnRef]
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "DATABASE_URL"
                        & ecstdkvpValue ?~ Ref "DatabaseUrl"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "REDIS_URL"
                        & ecstdkvpValue ?~ Ref "RedisUrl"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_APP_ID"
                        & ecstdkvpValue ?~ Ref "GitHubAppId"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_APP_KEY_BASE64"
                        & ecstdkvpValue ?~ Ref "GitHubAppKeyBase64"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_OAUTH_CLIENT_ID"
                        & ecstdkvpValue ?~ Ref "GitHubAppOAuthClientId"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_OAUTH_CLIENT_SECRET"
                        & ecstdkvpValue ?~ Ref "GitHubAppOAuthClientSecret"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "ADMIN_EMAILS"
                        & ecstdkvpValue ?~ Ref "AppsAdminEmails"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "LOG_LEVEL"
                        & ecstdkvpValue ?~ Ref "AppsLogLevel"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "RESTYLER_TAG"
                        & ecstdkvpValue ?~ Ref "RestylerTag"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "CLOUDWATCH_EKG"
                        & ecstdkvpValue ?~ Ref "AppsCloudWatchEKG"
                    ]
                & ecstdcdCpu ?~ Literal 50
                & ecstdcdMemory ?~ Literal backendMemory
                & ecstdcdMemoryReservation ?~ Literal backendMemoryReservation
                & ecstdcdMountPoints ?~
                    [ ecsTaskDefinitionMountPoint
                        & ecstdmpSourceVolume ?~ "tmp"
                        & ecstdmpContainerPath ?~ "/tmp"
                    , ecsTaskDefinitionMountPoint
                        & ecstdmpSourceVolume ?~ "docker-socket"
                        & ecstdmpContainerPath ?~ "/var/run/docker.sock"
                    ]
                & ecstdcdLogConfiguration ?~ (ecsTaskDefinitionLogConfiguration "awslogs"
                    & ecstdlcOptions ?~
                        [ ("awslogs-group", prefixJSON "Apps")
                        , ("awslogs-region", toJSON (Ref "AWS::Region" :: Val Text))
                        , ("awslogs-stream-prefix", "Backend")
                        ])
            ]
        )
        & dependsOn ?~ ["AppsClusterLogGroup"]
    ]

instanceLimit :: Integer
instanceLimit = 959 - 128

appMemory :: Integer
appMemory = instanceLimit

appMemoryReservation :: Integer
appMemoryReservation = 64

backendMemory :: Integer
backendMemory = instanceLimit

backendMemoryReservation :: Integer
backendMemoryReservation = 392

-- brittany-disable-next-binding

_databaseURL :: Val Text
_databaseURL = Join ""
    [ "postgres://"
    , Ref "DBUsername", ":"
    , Ref "DBPassword", "@"
    , GetAtt "DB" "Endpoint.Address", ":"
    , GetAtt "DB" "Endpoint.Port", "/"
    , "restyled"
    ]

-- brittany-disable-next-binding

_redisURL :: Val Text
_redisURL = Join ""
    [ "redis://"
    , GetAtt "Cache" "RedisEndpoint.Address", ":"
    , GetAtt "Cache" "RedisEndpoint.Port", "/"
    , Literal "0"
    ]
