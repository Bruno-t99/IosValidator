-module(dynamodb_handler).
-export([query_dynamodb/1, insert_receipt/1]).

query_dynamodb(Receipt) ->
    io:format("Receipt being queried: ~p~n", [Receipt]),
    
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
