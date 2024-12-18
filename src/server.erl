-module(server).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Set environment variables for AWS
    application:set_env(erlcloud, aws_access_key_id, os:getenv("AWS_ACCESS_KEY_ID")),
    application:set_env(erlcloud, aws_secret_access_key, os:getenv("AWS_SECRET_ACCESS_KEY")),
    application:set_env(erlcloud, aws_region, os:getenv("AWS_REGION")),

    % Retrieve the queue URL from environment
    QueueUrl = os:getenv("SQS_QUEUE_URL"),

    % Ensure necessary applications are started
    application:ensure_all_started(erlcloud),
    application:ensure_all_started(lhttpc),

    % Start the loop to fetch messages
    spawn(fun() -> loop(QueueUrl) end),
    {ok, self()}.

stop(_State) ->
    ok.

loop(QueueUrl) ->
    io:format("Fetching messages from queue: ~s~n", [QueueUrl]),

    % Fetch messages from the queue
    {ReceiptHandle, Receipt, PostQueue, UserId} = sqs_handler:handle_sqs_request(QueueUrl),
    
    io:format("Pre status: ~s~n", [Receipt]),
    Status = receipt_validator:validate_receipt(Receipt),

    case Status of
        {ok, Result} when Result == "OK" ->
            io:format("Status: ~s~n", [Result]),
            erlcloud_sqs:delete_message(QueueUrl, ReceiptHandle),
            DynamoResult = dynamodb_handler:query_dynamodb(Receipt),
            io:format("DynamoDB Result: ~p~n", [DynamoResult]),

            case DynamoResult of
                ok ->
                    PostMessage = jsx:encode(#{ 
                        <<"user_id">> => UserId, 
                        <<"transaction_id">> => Receipt, 
                        <<"status">> => <<"OK">>
                    }),
                    erlcloud_sqs:send_message(PostQueue, PostMessage),
                    io:format("Message sent to PostQueue: ~s~n", [PostQueue]);
                invalid ->
                    io:format("Skipping due to invalid DynamoDB result.~n")
            end;
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            erlcloud_sqs:delete_message(QueueUrl, ReceiptHandle)
    end,

    % Sleep for a bit and repeat the loop
    timer:sleep(5000),
    loop(QueueUrl).
