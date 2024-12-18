-module(server).
-behaviour(application).  % Indicate that this is an OTP application
-export([start/2, stop/1, loop/1, handle_sqs_request/1]).

start(_StartType, _StartArgs) ->
    application:set_env(erlcloud, aws_access_key_id, os:getenv("AWS_ACCESS_KEY_ID")),
    application:set_env(erlcloud, aws_secret_access_key, os:getenv("AWS_SECRET_ACCESS_KEY")),
    application:set_env(erlcloud, aws_region, os:getenv("AWS_REGION")),

    QueueUrl = os:getenv("SQS_QUEUE_URL"),
    application:ensure_all_started(erlcloud),
    application:ensure_all_started(lhttpc),
    spawn(fun() -> loop(QueueUrl) end),
    {ok, self()}.

stop(_State) ->
    ok.
loop(QueueUrl) ->
    io:format("Fetching messages from queue: ~s~n", [QueueUrl]),
    handle_sqs_request(QueueUrl),
    timer:sleep(5000),  
    loop(QueueUrl).

handle_sqs_request(QueueUrl) ->
    io:format("Processing messages from queue: ~s~n", [QueueUrl]),
    
    % Retrieving the response from the SQS service
    Response = erlcloud_sqs:receive_message(QueueUrl),
    {messages, NestedMessages} = hd(Response),
    
    case NestedMessages of
        [] ->
            % No messages to process
            io:format("Queue is empty. No messages to process.~n");
        _ ->
            % Process found messages
            io:format("Messages found, processing...~n"),

            % Extract required fields
            ReceiptHandle = extract_handle(NestedMessages),
            Receipt = extract_field(<<"receipt">>, NestedMessages),
            PostQueue = extract_field(<<"post_queue">>, NestedMessages),
            UserId = extract_field(<<"user_id">>, NestedMessages),
            Status = validate_receipt(Receipt),
            
            case Status of
                {ok, Result} when Result == "OK"->
                    io:format("Status: ~s~n", [Result]),
                    erlcloud_sqs:delete_message(QueueUrl, ReceiptHandle),
                    DynamoResult = query_dynamodb(Receipt),
                    io:format("DynamoDB Result: ~p~n", [DynamoResult]),
                    case DynamoResult of
                        ok ->
                            PostMessage = jsx:encode(#{ 
                                <<"user_id">> => UserId, 
                                <<"transaction_id">> => Receipt, 
                                <<"status">> => <<"OK">>
                            }),
                            % Send message to PostQueue
                            erlcloud_sqs:send_message(PostQueue, PostMessage),
                            io:format("Message sent to PostQueue: ~s~n", [PostQueue]);
                        invalid ->
                            io:format("Skipping due to invalid DynamoDB result.~n")
                    end;
                    
                {error, Reason} ->
                    io:format("Error: ~s~n", [Reason]),
                    erlcloud_sqs:delete_message(QueueUrl, ReceiptHandle)
            end
    end.


extract_field(FieldName, NestedMessages) ->
     io:format("Extracting field : ~s~n", [FieldName]),

    [FirstMessageList | _] = NestedMessages,
    [FirstMessage | _] = FirstMessageList,
    {body, Body} = FirstMessage,
    BinaryBody = unicode:characters_to_binary(Body),
    Decoded = jsx:decode(BinaryBody),
    Value = maps:get(FieldName, Decoded),
    io:format("~p: ~p~n", [FieldName, Value]),
    Value.


extract_handle(NestedMessages) ->
    [FirstMessageList | _] = NestedMessages,
    io:format("FirstMessageList: ~p~n", [FirstMessageList]),

    ReceiptHandle = proplists:get_value(receipt_handle, FirstMessageList),
    io:format("Receipt Handle: ~s~n", [ReceiptHandle]),

    ReceiptHandle.

validate_receipt(_Base64Receipt) ->
    io:format("Validating receipt: ~s~n", [_Base64Receipt]),
    
    % Real API call simulation (wrong token)
    % Url = "https://api.storekit.itunes.apple.com/inApps/v1/verifyReceipt",
    % Headers = [
    %    {"Authorization", "wrong-jwt-token"},
    %    {"Content-Type", "application/json"}
    %],
    %Payload = jsx:encode(#{receipt => _Base64Receipt}),
    
    % Simulate the API request with an invalid token
    %case httpc:request(post, {Url, Headers, "application/json", Payload}, [], []) of
    %    {ok, {{_, 401, _}, _, Body}} ->  % Simulating a 401 Unauthorized error
    %        io:format("Error: Unauthorized - Invalid token. Response: ~s~n", [Body]);
    %    {ok, {{_, 400, _}, _, Body}} ->  % Simulating a 400 Bad Request error
    %        io:format("Error: Bad Request - Invalid receipt. Response: ~s~n", [Body]);
    %    {ok, {{_, 500, _}, _, Body}} ->  % Simulating a 500 Internal Server Error
    %        io:format("Error: Internal Server Error. Response: ~s~n", [Body]);
    %    {error, Reason} ->  % Catching other errors like connection failure
    %        io:format("Request failed: ~p~n", [Reason])
    %end.

    %Simulate a 50/50 chance of validity
    case rand:uniform(2) of
       1 -> {ok, "OK"};
       2 -> {error, "INVALID"}
    end.


query_dynamodb(Receipt) ->
    io:format("Receipt being queried: ~p~n", [Receipt]),

    % Query DynamoDB to check if the item exists
    {ok, QueryResponse} =
    erlcloud_ddb2:batch_get_item(
      [
        {<<"Transactions">>, 
         [[{<<"transactionId">>, {s, Receipt}}]],
         []}  
      ],
      [{return_consumed_capacity, total}, {out, record}]
    ),

    io:format("Query Response: ~p~n", [QueryResponse]),

    % Handle the response to check for existence
    case QueryResponse of
        {ddb2_batch_get_item, _, [{ddb2_batch_get_item_response, _, []}], _} ->
            io:format("No matching item found. Inserting new item...~n"),
            insert_receipt(Receipt),
            ok; 
        {ddb2_batch_get_item, _, [{ddb2_batch_get_item_response, _, _}], _} ->
            io:format("Item already exists, skipping...~n"),
            invalid; 
        _ ->
            io:format("Unexpected query response format.~n"),
            invalid
    end.

insert_receipt(Receipt) ->
    Item = [
        {<<"transactionId">>, {s, Receipt}} 
    ],
    {ok, Response} = erlcloud_ddb2:put_item(<<"Transactions">>, Item, []),
    io:format("New item inserted: ~p~n", [Response]),
    Response.








