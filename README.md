# IosValidator

This project is an Erlang-based server that validates iOS in-app purchase receipts using AWS SQS and DynamoDB.

## Prerequisites

1. **Erlang/OTP**: Ensure you have Erlang installed. [Download Erlang](https://www.erlang.org/downloads)
2. **Rebar3**: Install `rebar3`, the build tool for Erlang projects.
   ```bash
   curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && mv rebar3 /usr/local/bin/

