# Docker image

Retrieve an authentication token and authenticate your Docker client
to your registry.  Use the AWS CLI:

```
aws ecr get-login-password --region ${REGION} | docker login --username AWS --password-stdin ${AWS_ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com
```

Note: If you receive an error using the AWS CLI, make sure that you
have the latest version of the AWS CLI and Docker installed.  Build
your Docker image using the following command. For information on
building a Docker file from scratch see the instructions here . You
can skip this step if your image is already built:

```
docker build -t menard-lambda-deployer .
```

After the build completes, tag your image so you can push the image to this repository:

```
docker tag menard-lambda-deployer:latest ${AWS_ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com/menard-lambda-deployer:latest
```

Run the following command to push this image to your newly created AWS repository:

```
docker push ${AWS_ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com/menard-lambda-deployer:latest
```

