# IosValidator

This project is an Erlang-based server that validates iOS in-app purchase receipts using AWS SQS and DynamoDB.

## Prerequisites

1. **Erlang/OTP**: Ensure you have Erlang installed. [Download Erlang](https://www.erlang.org/downloads)
2. **Rebar3**: Install `rebar3`, the build tool for Erlang projects.
   ```bash
   curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && mv rebar3 /usr/local/bin/

3. **Run Server Locally** :
   Clean previous build artifacts
   rebar3 clean
   
   Fetch project dependencies
   rebar3 get-deps
   
   Compile the project
   rebar3 compile
   
   Start the server
   rebar3 shell

4. **Env vars** :
   export AWS_ACCESS_KEY_ID="<your_access_key>"
   export AWS_SECRET_ACCESS_KEY="<your_secret_key>"
   export AWS_REGION="us-east-1"
   export SQS_QUEUE_URL="https://sqs.us-east-1.amazonaws.com/<your_account_id>/<your_queue_name>"

5. **Run with Docker Locally** :

   docker build -t ios-validator .

   docker run -it --name ios-validator \
  -e AWS_ACCESS_KEY_ID="<your_access_key>" \
  -e AWS_SECRET_ACCESS_KEY="<your_secret_key>" \
  -e AWS_REGION="us-east-1" \
  -e SQS_QUEUE_URL="https://sqs.us-east-1.amazonaws.com/<your_account_id>/<your_queue_name>" \
  ios-validator

5. **Deploying with CloudFormation** :

   Upload the CloudFormation Template:
   
   Navigate to the AWS Management Console.
   Go to the CloudFormation service.
   Select Create Stack and upload the template.yaml file provided with this project.
   Provide Necessary Parameters: When prompted:
   
   text
   Copiar código
   - Enter your AWS_ACCESS_KEY_ID.
   - Enter your AWS_SECRET_ACCESS_KEY.
   - Enter the SQS_QUEUE_URL for your setup.
   Launch the Stack:
   
   Review the stack configuration and ensure the parameters are correct.
   Launch the stack and wait for the resources (SQS, DynamoDB, ECS) to be provisioned.

