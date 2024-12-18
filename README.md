# IosValidator

This project is an Erlang-based server that validates iOS in-app purchase receipts using AWS SQS and DynamoDB.

## Prerequisites

1. **Erlang/OTP**: Ensure you have Erlang installed. [Download Erlang](https://www.erlang.org/downloads)
2. **Rebar3**: Install `rebar3`, the build tool for Erlang projects.
   ```bash
   curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && mv rebar3 /usr/local/bin/

3. **Run Server** :
   # Clean previous build artifacts
   rebar3 clean
   
   # Fetch project dependencies
   rebar3 get-deps
   
   # Compile the project
   rebar3 compile
   
   # Start the server
   rebar3 shell

4. **Env vars** :
   export AWS_ACCESS_KEY_ID="<your_access_key>"
   export AWS_SECRET_ACCESS_KEY="<your_secret_key>"
   export AWS_REGION="us-east-1"
   export SQS_QUEUE_URL="https://sqs.us-east-1.amazonaws.com/<your_account_id>/<your_queue_name>"
