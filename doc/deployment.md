# Menard-as-a-service

This document describes the Menard API and how to deploy it as a
standalone service or in AWS Lambda.

# Standalone

See the [server](https://github.com/ekoontz/menard/tree/main/server) directory.

# AWS Lambda

See the [lambda](https://github.com/ekoontz/menard/tree/main/lambda)
directory. 

## ECR

Set up an ECR repository as described in [this README.md](https://github.com/ekoontz/menard/tree/main/lambda/README.md)

## S3

Create a bucket called `menard-lambda`.

## IAM

### CodeBuild

Create the role `codebuild-menard-service-role` with this IAM Policy (TODO).

### Deployer

Create the role `new-deployer` with this [IAM Policy](https://github.com/ekoontz/menard/tree/main/lambda/code-pipeline-iam-policy.json).

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

### Environment

|Key|Value|
|-|-|
|Image Type|Custom Image|
|Environment type|ARM (but "Linux" or "Linux GPU" might work too)|
|Image Registry|Amazon ECR|
|ECR Account|My ECR Account|
|Amazon ECR Repository| (use the one you created above in the "ECR" section) |
|Amazon ECR image|(use the one you created above in the "ECR" section) |
|Image Pull Credentials|AWS CodeBuild Credentials|
|Service role|`arn:aws:iam::<YOUR AWS ACCOUNT ID>:role/service-role/codebuild-menard-service-role`|
|Allow AWS CodeBuild to modify this service role so it can be used with this build project|Yes|

The rest of this form can be left with the default values.

### Buildspec

|Key|Value|
|-|-|
|Use a buildspec file|Yes - choose this option|
|Buildspec Name|[`lambda/buildspec.yml`](https://github.com/ekoontz/menard/tree/main/lambda/buildspec.yml)

### Batch Configuration

|Key|Value|
|-|-|
|New Service Role|Yes - choose this option|

### Artifacts

|Key|Value|
|-|-|
|Type|Amazon S3|
|Bucket Name|menard-lambda|
|Name|menard.zip|
|Artifact packaging|Zip|
|Allow AWS CodeBuild to modify this service role so it can be used with this build project|Yes|

### CloudWatch Logs

|Key|Value|
|-|-|
|CloudWatch logs - optional|I chose yes, but I don't find I need to look at the logs, but it's good for debugging especially when trying to get started|
|Allow AWS CodeBuild to modify this service role so it can be used with this build project|Yes|


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

There is a single action, which uses the
[`buildspec.yml`](https://github.com/ekoontz/menard/tree/main/lambda/buildspec.yml)
to build the following artifacts, defined in `buildspec.yml`'s
`artifacts/files`:

- [`/bootstrap`](https://github.com/ekoontz/menard/tree/main/lambda/bootstrap)
- `/menard_lambda`: This is the artifact of running `native-image` as specified in the `buildpsec.yml`.
- [`/template-native.yml`](https://github.com/ekoontz/menard/tree/main/lambda/template-native.yml).

These artifacts are created by doing to following steps defined in `buildspec.yml`'s `phases/build/commands`:

1. install [Leiningen](https://leiningen.org/)
2. `cd` to the [lambda](https://github.com/ekoontz/menard/tree/main/lambda) subdirectory of the menard source tree.
3. create the uberjar `menard-lambda.jar` as configured by [lambda/project.clj](https://github.com/ekoontz/menard/tree/main/lambda/project.clj)
4. run [`native-image`](https://www.graalvm.org/22.1/reference-manual/native-image/) on the `menard-lambda.jar` file created in step 3.
5. copy the `menard_lambda` binary to the top-level directory `/`, along with `bootstrap` and `template-native.yml` as mentioned above.

Create the following build configuration in the build stage:

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

There is a single action, which is to create or update the AWS CloudFormation Stack 'menard' as specified by [/template-native.yml](https://github.com/ekoontz/menard/tree/main/lambda/template-native.yml).

Use the following for the form that lets you add a new action:

|Key|Value|
|-|-|
|Action name |  deploy|
|Action provider |  AWS CloudFormation|
|Region |  any should work but I chose Europe/Frankfurt|
|Input artifacts |  BuildArtifact|
|Action mode |  Create or update a stack|
|Stack name | menard|

#### Template

[/template-native.yml](https://github.com/ekoontz/menard/tree/main/lambda/template-native.yml) creates the various AWS Lambda functions via AWS CloudFormation. Each function defines the following:

- a _Handler_, e.g. `menard.lambda.def.AnalyzeEN` created via [`fierycod.holy-lambda.core/deflambda`](https://github.com/FieryCod/holy-lambda/blob/0.0.7/src/clj/fierycod/holy_lambda/core.clj#L249). Each handler name has the prefix [`menard.lambda.def`](http://github.com/ekoontz/menard/tree/main/lambda/src/menard/lambda/def.clj).
- a _Path_, used to route requests from the API gateway to the given lambda function.
- a _Method_, the HTTP method to be used. Currently only `get` is used by [/template-native.yml](https://github.com/ekoontz/menard/tree/main/lambda/template-native.yml).
- a _FunctionName_, e.g. `AnalyzeEN`. This is always simply the last
  segment of the dotted name in the _Handler_.

Use the following for the form that lets you add a Template:

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

#### Advanced

For 'Parameter Overrides' use:

```
{ "BucketName": { "Fn::GetArtifactAtt" : [ "BuildArtifact", "BucketName" ] },
  "ObjectKey": { "Fn::GetArtifactAtt" : [ "BuildArtifact", "ObjectKey" ] }}
```

This is needed to supply the values `BucketName` and `ObjectKey` in
the `CodeUri` for each function defined in `template-native.yml`.

## API Gateway

When the build pipeline runs, it should have either created a new [API
Gateway
API](https://eu-central-1.console.aws.amazon.com/apigateway) called `menard`. Locate
the `menard` API within the [AWS UI](https://eu-central-1.console.aws.amazon.com/apigateway) and modify the Custom Domains to add the domain name `menard.hiro-tan.org`.

Make a note of the API Gateway domain name within the API's
configuration; it will be a name within AWS's namespace, such as
`a-b2cde44f22.execute-api.eu-central-1.amazonaws.com`.

## DNS Settings

Define an ALIAS within the domain `hiro-tan.org` (this is not
managed by AWS currently, but a different company,
[dnssimple.com](https://dnssimple.com).

The ALIAS fields should be:

|Key|Value|
|-|-|
|Name|`menard.hiro-tan.org`|
|Content|`a-b2cde44f22.execute-api.eu-central-1.amazonaws.com`|

The actual value of `Content` will be different than this example value.




