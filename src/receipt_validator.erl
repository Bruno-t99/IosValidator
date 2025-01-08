-module(receipt_validator).
-export([validate_receipt/1]).

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

validate_receipt(_Base64Receipt) ->
    io:format("Validating receipt: ~s~n", [_Base64Receipt]),
    
    % Simulate a 50/50 chance of validity
    case rand:uniform(2) of
       1 -> {ok, "OK"};
       2 -> {error, "INVALID"}
    end.
