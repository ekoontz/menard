# See also

 [doc/deployment.md](https://github.com/ekoontz/menard/tree/main/doc/deployment.md), which references (in the ECR section) these steps.

# Publishing the Docker image

These steps build, tag and push the `menard-lambda-deployer` Docker image.

Retrieve an authentication token and authenticate your Docker client
to your registry.  Use the AWS CLI:

```
aws ecr get-login-password --region ${REGION} | docker login --username AWS --password-stdin ${AWS_ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com
```

Note: If you receive an error using the AWS CLI, make sure that you
have the latest version of the AWS CLI and Docker installed.  Build
your Docker image using the following command:

```
make image
```

After the build completes, tag your image so you can push the image to this repository:

```
make tag
```

Run the following command to push this image to your newly created AWS repository:

```
make push
```

# Manually building `menard_lambda`

The image `menard_lambda` is a native image that contains the individual AWS Lambdas that are run against requests as described in `template-native.yml`.

1. Start a Docker process for `menard-lambda-deployer` with:

```
cd ~/menard # one level above the directory `lambda` where this `README.md` is located.
docker run -v ${PWD}:/project -t -i menard-lambda-deployer:latest bash
```

2. Set up the environment

```
java -jar /leiningen-2.9.10-standalone.jar -m leiningen.core.main install
```

3. Build the uberjar

```
cd lambda
java -jar /leiningen-2.9.10-standalone.jar -m leiningen.core.main uberjar
```

4. Build the native image

```
gu install native-image
native-image -jar target/menard-lambda.jar menard_lambda \
    --report-unsupported-elements-at-runtime --no-fallback --verbose \
	--enable-url-protocols=http,https --no-server  --initialize-at-build-time \
	--initialize-at-build-time=org.apache.log4j.CategoryKey \
	--trace-object-instantiation=java.lang.Thread
```
