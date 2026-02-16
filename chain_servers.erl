%% Team Members: Daniel, John, Clay

-module(chain_servers).
-export([start/0]).

%% Main entry point
start() ->
    io:format("~nStarting chain of server processes...~n"),
    
    %% Spawn the three server processes in reverse order
    Pid3 = spawn(fun() -> serv3_init() end),
    Pid2 = spawn(fun() -> serv2(Pid3) end),
    Pid1 = spawn(fun() -> serv1(Pid2) end),
    
    io:format("Server chain initialized.~n"),
    io:format("Type messages to send (or 'all_done' to quit)~n~n"),
    
    %% Start the message loop
    message_loop(Pid1).

%% Message loop - reads user input and sends to serv1
message_loop(Pid1) ->
    io:format("Enter message: "),
    case io:read("") of
        {ok, all_done} ->
            io:format("~nShutting down...~n"),
            Pid1 ! halt,
            timer:sleep(100), %% Give processes time to finish
            io:format("~nAll done!~n");
        {ok, Message} ->
            Pid1 ! Message,
            message_loop(Pid1);
        {error, _} ->
            io:format("Invalid input. Try again.~n"),
            message_loop(Pid1)
    end.

%% SERV1 - Handles arithmetic operations

serv1(NextPid) ->
    receive
        %% Halt message - forward and stop
        halt ->
            io:format("(serv1) Forwarding halt message...~n"),
            NextPid ! halt,
            io:format("(serv1) Halting.~n");
        
        %% Three-element tuple arithmetic operations
        {add, X, Y} when is_number(X), is_number(Y) ->
            Result = X + Y,
            io:format("(serv1) Addition: ~p + ~p = ~p~n", [X, Y, Result]),
            serv1(NextPid);
        
        {sub, X, Y} when is_number(X), is_number(Y) ->
            Result = X - Y,
            io:format("(serv1) Subtraction: ~p - ~p = ~p~n", [X, Y, Result]),
            serv1(NextPid);
        
        {mult, X, Y} when is_number(X), is_number(Y) ->
            Result = X * Y,
            io:format("(serv1) Multiplication: ~p * ~p = ~p~n", [X, Y, Result]),
            serv1(NextPid);
        
        {divide, X, Y} when is_number(X), is_number(Y), Y =/= 0 ->
            Result = X / Y,
            io:format("(serv1) Division: ~p / ~p = ~p~n", [X, Y, Result]),
            serv1(NextPid);
        
        {divide, X, 0} when is_number(X) ->
            io:format("(serv1) Division by zero error: ~p / 0~n", [X]),
            serv1(NextPid);
        
        %% Two-element tuple unary operations
        {neg, X} when is_number(X) ->
            Result = -X,
            io:format("(serv1) Negation: -~p = ~p~n", [X, Result]),
            serv1(NextPid);
        
        {sqrt, X} when is_number(X), X >= 0 ->
            Result = math:sqrt(X),
            io:format("(serv1) Square root: sqrt(~p) = ~p~n", [X, Result]),
            serv1(NextPid);
        
        {sqrt, X} when is_number(X), X < 0 ->
            io:format("(serv1) Square root of negative number: sqrt(~p) [invalid]~n", [X]),
            serv1(NextPid);
        
        %% Any other message - forward to serv2
        Msg ->
            NextPid ! Msg,
            serv1(NextPid)
    end.

%% SERV2 - Handles lists with numeric heads

serv2(NextPid) ->
    receive
        %% Halt message - forward and stop
        halt ->
            io:format("(serv2) Forwarding halt message...~n"),
            NextPid ! halt,
            io:format("(serv2) Halting.~n");
        
        %% List with integer head - compute sum
        [H|_] = List when is_integer(H) ->
            Sum = sum_numbers(List),
            io:format("(serv2) Sum of list ~p = ~p~n", [List, Sum]),
            serv2(NextPid);
        
        %% List with float head - compute product
        [H|_] = List when is_float(H) ->
            Product = product_numbers(List),
            io:format("(serv2) Product of list ~p = ~p~n", [List, Product]),
            serv2(NextPid);
        
        %% Any other message - forward to serv3
        Msg ->
            NextPid ! Msg,
            serv2(NextPid)
    end.

%% Helper function to sum all numbers in a list
sum_numbers(List) ->
    sum_numbers(List, 0).

sum_numbers([], Acc) ->
    Acc;
sum_numbers([H|T], Acc) when is_number(H) ->
    sum_numbers(T, Acc + H);
sum_numbers([_H|T], Acc) ->
    sum_numbers(T, Acc).

%% Helper function to compute product of all numbers in a list
product_numbers(List) ->
    product_numbers(List, 1).

product_numbers([], Acc) ->
    Acc;
product_numbers([H|T], Acc) when is_number(H) ->
    product_numbers(T, Acc * H);
product_numbers([_H|T], Acc) ->
    product_numbers(T, Acc).

%% SERV3 - Handles error messages and counts unhandled messages

serv3_init() ->
    serv3(0).

serv3(Count) ->
    receive
        %% Halt message - print count and stop
        halt ->
            io:format("(serv3) Total unhandled messages: ~p~n", [Count]),
            io:format("(serv3) Halting.~n");
        
        %% Error message pattern
        {error, ErrorMsg} ->
            io:format("(serv3) Error: ~p~n", [ErrorMsg]),
            serv3(Count);
        
        %% Any other message - count as unhandled
        Msg ->
            io:format("(serv3) Not handled: ~p~n", [Msg]),
            serv3(Count + 1)
    end.