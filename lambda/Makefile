.PHONY: all check-service clean deploy deploy-native destroy destroy-bucket dry-api dry-api-native logs-tail make-bucket image tag push
BUCKET_NAME=menard-lambda
STACK_NAME=menard
APP_REGION=eu-central-1
PWD=$$(pwd)

native_image_cmd=docker run -v ${PWD}:/project -it ghcr.io/graalvm/graalvm-ce:latest bash -c "gu install native-image && cd /project && native-image -jar target/menard-lambda.jar --report-unsupported-elements-at-runtime --no-fallback --verbose --enable-url-protocols=http,https --no-server  --initialize-at-build-time --initialize-at-build-time=org.apache.log4j.CategoryKey --trace-object-instantiation=java.lang.Thread"

all: deploy-native

# <deploy-native>

deploy-native: packaged-native.yml menard-lambda
	sam deploy --template-file packaged-native.yml --stack-name $(STACK_NAME) --capabilities CAPABILITY_IAM --region $(APP_REGION)

packaged-native.yml: make-bucket
	sam package --template-file template-native.yml --output-template-file packaged-native.yml --s3-bucket $(BUCKET_NAME)

latest.zip: output
	zip $@ bootstrap output

menard-lambda: target/menard-lambda.jar
	${native_image_cmd}

target/menard-lambda.jar: src/menard/lambda/def.clj
	lein uberjar

# </deploy-native>

image: leiningen-2.9.10-standalone.jar
	docker build . -t menard-lambda-deployer

tag:
	docker tag menard-lambda-deployer:latest ${AWS_ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com/menard-lambda-deployer:latest

push:
	docker push ${AWS_ACCOUNT_ID}.dkr.ecr.${REGION}.amazonaws.com/menard-lambda-deployer:latest


leiningen-2.9.10-standalone.jar:
	curl https://github.com/technomancy/leiningen/releases/download/2.9.10/leiningen-2.9.10-standalone.jar -L > $@

check-service:
	@(nc -z localhost 3000) || echo "AWS API service is not running: run 'make dry-api-native &'."

clean:
	-rm -rf server target/ packaged.yml packaged-native.yml output latest.zip

deploy: packaged.yml
	sam deploy --template-file packaged.yml --stack-name $(STACK_NAME) --capabilities CAPABILITY_IAM --region $(APP_REGION)

destroy:
	aws cloudformation delete-stack --stack-name $(STACK_NAME) --region $(APP_REGION)

destroy-bucket:
	aws s3 rb s3://$(BUCKET_NAME) --force --region $(APP_REGION)

dry-api: target/menard-lambda.jar
	sam local start-api --template template.yml

dry-api-native: latest.zip
	sam local start-api --template template-native.yml

local-test: check-service
	curl "http://127.0.0.1:3000/parse-start/nl?q=de+kat"
	curl "http://127.0.0.1:3000/generate/nl?q=%5B%5B%5B%5D%20%7B%3Acat%20%3Anoun%2C%20%3Asubcat%20%5B%5D%2C%20%3Asem%20%7B%3Aquant%20%3Athe%7D%2C%20%3Aagr%20%7B%3Agender%20%3Acommon%2C%20%3Anumber%20%3Asing%7D%2C%20%3Aphrasal%20true%2C%20%3Ahead%20%7B%3Aphrasal%20false%7D%7D%5D%5D"
	curl 'http://127.0.0.1:3000/generate-with-alts?spec=%5B%5B%5B%5D%20%7B%3Acat%20%3Anoun%2C%20%3Amod%20nil%2C%20%3Asem%20%7B%3Aquant%20%3Athe%7D%2C%20%3Aphrasal%20true%2C%20%3Ahead%20%7B%3Aphrasal%20false%2C%20%3Ainflection%20%3Aheid%7D%7D%5D%5D&alts=(%5B%5B%5B%5D%20%7B%3Asem%20%7B%3Aref%20%7B%3Anumber%20%3Asing%7D%7D%7D%5D%5D%20%5B%5B%5B%5D%20%7B%3Asem%20%7B%3Aref%20%7B%3Anumber%20%3Aplur%7D%7D%7D%5D%5D)'

make-bucket:
	aws s3 ls s3://$(BUCKET_NAME) || aws s3 mb s3://$(BUCKET_NAME)

packaged.yml: target/menard-lambda.jar make-bucket
	sam package --template-file template.yml        --output-template-file packaged.yml        --s3-bucket $(BUCKET_NAME) --s3-prefix "menard-lambda"

