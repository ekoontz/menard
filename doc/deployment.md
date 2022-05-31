# Menard-as-a-service

This document describes the Menard API and how to deploy it as a
standalone service or in AWS Lambda.

# Standalone

See the [server](https://github.com/ekoontz/menard/tree/main/server) directory.

# AWS Lambda

See the [lambda](https://github.com/ekoontz/menard/tree/main/lambda)
directory. 

## CodeBuild

### Project Configuration

|Key|Value|
|-|-|
|Project Name|menard|

### Source

|Key|Value|
|-|-|
|Source Provider | Github |
|Repository|Public repository|


#### Service role permissions

|Key|Value|
|-|-|
|Allow AWS CodeBuild to modify this service role so it can be used with this build project|Yes|

## IAM

Create the role `new-deployer` with this [IAM Policy](https://github.com/ekoontz/menard/tree/main/lambda/code-pipeline-iam-policy.json).

## CodePipeline

### Stage: Source

There is a single action:

|Key|Value|
|-|-|
|Action name|Source|
|Action provider |  GitHub (Version 2)|
|Repository name |  ekoontz/menard|
|Branch name |  main|
|Start the pipeline on source code change?| Yes|
|Output artifact format |  CodePipeline default|
|Variable namespace | SourceVariables|
|Output artifacts | SourceArtifact |

### Stage: Build

There is a single action:

|Key|Value|
|-|-|
|Action name |  Build|
|Action provider |  AWS CodeBuild|
|Region |  any should work but I chose Europe/Frankfurt|
|Input artifacts |  SourceArtifact|
|Project name |  menard (see above in the CodeBuild section of the docs)|
|Build type |  Single build|
|Variable namespace |  BuildVariables|
|Output artifacts |  BuildArtifact|

### Deploy stage

There is a single action:


|Key|Value|
|-|-|
|Action name |  deploy|
|Action provider |  AWS CloudFormation|
|Region |  any should work but I chose Europe/Frankfurt|
|Input artifacts |  BuildArtifact|
|Action mode |  Create or update a stack|
|Stack name |  menard|


#### Template

|Key|Value|
|-|-|
|Artifact Name |  BuildArtifact|
|File name |  [/template-native.yml](https://github.com/ekoontz/menard/tree/main/lambda/template-native.yml)|
|Template file path |  BuildArtifact::/template-native.yml (this value will be auto-filled in by the above two fields (Artifact name and File name)|

#### Template configuration (optional)


|Key|Value|
|-|-|
|Use configuration file |  (leave this unchecked)
|Artifact name |  (leave blank)
|File name |  (leave blank)
|Template configuration file path |  (leave blank)


#### Capabilities (optional)

Add the following capabilities:

- `CAPABILITY_IAM`
- `CAPABILITY_AUTO_EXPAND`

#### Role name

`arn:aws:iam::(your-aws-account-id):role/new-deployer`

This role was created above in the IAM section.

## API Gateway

## 
