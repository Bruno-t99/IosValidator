-module(sqs_handler).
-export([handle_sqs_request/1, extract_field/2, extract_handle/1]).

handle_sqs_request(QueueUrl) ->
    io:format("Processing messages from queue: ~s~n", [QueueUrl]),
    
    % Retrieve the response from the SQS service
    Response = erlcloud_sqs:receive_message(QueueUrl),
    {messages, NestedMessages} = hd(Response),
    
    case NestedMessages of
        [] -> io:format("Queue is empty. No messages to process.~n");
        _ -> process_messages(NestedMessages)  % QueueUrl is no longer needed here
    end.

process_messages(NestedMessages) ->
    io:format("Messages found, processing...~n"),
    
    % Extract required fields
    ReceiptHandle = extract_handle(NestedMessages),
    Receipt = extract_field(<<"receipt">>, NestedMessages),
    PostQueue = extract_field(<<"post_queue">>, NestedMessages),
    UserId = extract_field(<<"user_id">>, NestedMessages),
    
    % Return the extracted fields
    {ReceiptHandle, Receipt, PostQueue, UserId}.

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
    ReceiptHandle = proplists:get_value(receipt_handle, FirstMessageList),
    io:format("Receipt Handle: ~s~n", [ReceiptHandle]),
    ReceiptHandle.
