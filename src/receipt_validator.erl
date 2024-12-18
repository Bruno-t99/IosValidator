-module(receipt_validator).
-export([validate_receipt/1]).

validate_receipt(_Base64Receipt) ->
    io:format("Validating receipt: ~s~n", [_Base64Receipt]),
    
    % Simulate a 50/50 chance of validity
    case rand:uniform(2) of
       1 -> {ok, "OK"};
       2 -> {error, "INVALID"}
    end.
