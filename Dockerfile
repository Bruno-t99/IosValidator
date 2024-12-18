FROM erlang:alpine

# Install dependencies including git
RUN apk update && apk add --no-cache git curl

# Install rebar3
RUN curl -O https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Set the working directory
WORKDIR /app

# Copy your source code to the container
COPY . /app

# Install dependencies and compile the project
RUN rebar3 deps get && rebar3 compile

# Default command to run your Erlang application
ENTRYPOINT ["rebar3", "shell"]
