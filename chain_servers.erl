%% Team Members: Daniel, John, Clay

-module(chain_servers).
-export([start/0]).

start() ->
    io:format("~nStarting chain of server processes...~n"),
    
    Pid3 = spawn(fun() -> serv3(0) end),
    Pid2 = spawn(fun() -> serv2(Pid3) end),
    Pid1 = spawn(fun() -> serv1(Pid2) end),
    
    io:format("Server chain initialized.~n"),
    io:format("Type messages to send (or 'all_done' to quit)~n~n"),
    
    message_loop(Pid1).


message_loop(Pid1) ->
    io:format("Enter message: "),
    case io:read("") of
        {ok, all_done} ->
            io:format("~nShutting down...~n"),
            Pid1 ! halt,
            timer:sleep(100), 
            io:format("~nAll done!~n");
        {ok, Message} ->
            Pid1 ! Message,
            message_loop(Pid1);
        {error, _} ->
            io:format("Invalid input. Try again.~n"),
            message_loop(Pid1)
    end.


serv1(NextPid) ->
    receive
        halt ->
            io:format("(serv1) Forwarding halt message...~n"),
            NextPid ! halt,
            io:format("(serv1) Halting.~n");
        
        {add, X, Y} when is_number(X), is_number(Y) ->
            io:format("(serv1) ~p + ~p = ~p~n", [X, Y, X + Y]),
            serv1(NextPid);
        
        {sub, X, Y} when is_number(X), is_number(Y) ->
            io:format("(serv1) ~p - ~p = ~p~n", [X, Y, X - Y]),
            serv1(NextPid);
        
        {mult, X, Y} when is_number(X), is_number(Y) ->
            io:format("(serv1) ~p * ~p = ~p~n", [X, Y, X * Y]),
            serv1(NextPid);
        
        {divide, X, Y} when is_number(X), is_number(Y), Y =/= 0 ->
            io:format("(serv1) ~p / ~p = ~p~n", [X, Y, X / Y]),
            serv1(NextPid);
        
        {divide, X, 0} when is_number(X) ->
            io:format("(serv1) Division by zero error: ~p / 0~n", [X]),
            serv1(NextPid);
        
        {neg, X} when is_number(X) ->
            io:format("(serv1) neg(~p) = ~p~n", [X, -X]),
            serv1(NextPid);
        
        {sqrt, X} when is_number(X), X >= 0 ->
            io:format("(serv1) sqrt(~p) = ~p~n", [X, math:sqrt(X)]),
            serv1(NextPid);
        
        {sqrt, X} when is_number(X), X < 0 ->
            io:format("(serv1) sqrt of negative number ~p is undefined~n", [X]),
            serv1(NextPid);
        
        Msg ->
            NextPid ! Msg,
            serv1(NextPid)
    end.


serv2(NextPid) ->
    receive
        halt ->
            io:format("(serv2) Forwarding halt message...~n"),
            NextPid ! halt,
            io:format("(serv2) Halting.~n");
        
        [H|_] = List when is_integer(H) ->
            Sum = sum_numbers(List),
            io:format("(serv2) Sum of list ~p = ~p~n", [List, Sum]),
            serv2(NextPid);
        
        [H|_] = List when is_float(H) ->
            Product = product_numbers(List),
            io:format("(serv2) Product of list ~p = ~p~n", [List, Product]),
            serv2(NextPid);
        
        Msg ->
            NextPid ! Msg,
            serv2(NextPid)
    end.

sum_numbers(List) ->
    sum_numbers(List, 0).

sum_numbers([], Acc) ->
    Acc;
sum_numbers([H|T], Acc) when is_number(H) ->
    sum_numbers(T, Acc + H);
sum_numbers([_H|T], Acc) ->
    sum_numbers(T, Acc).

product_numbers(List) ->
    product_numbers(List, 1).

product_numbers([], Acc) ->
    Acc;
product_numbers([H|T], Acc) when is_number(H) ->
    product_numbers(T, Acc * H);
product_numbers([_H|T], Acc) ->
    product_numbers(T, Acc).


serv3(Count) ->
    receive
        halt ->
            io:format("(serv3) Total unhandled messages: ~p~n", [Count]),
            io:format("(serv3) Halting.~n");
        
        {error, ErrorMsg} ->
            io:format("(serv3) Error: ~p~n", [ErrorMsg]),
            serv3(Count);
        
        Msg ->
            io:format("(serv3) Not handled: ~p~n", [Msg]),
            serv3(Count + 1)
    end.