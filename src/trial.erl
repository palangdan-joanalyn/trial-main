-module(trial).


add_two_numbers(Num1, Num2) ->
    Num1 + Num2.

multiply_by_three(Num) ->

    Num*3.
main() -> io:fwrite('Displaying the sum and product of random numbers.'),Sum=add_two_numbers(30,87),Product=multiply_by_three(Sum),io:format("~nThese are the following results after the manipulation of numbers: ~nSum: ~p~n Product:~p~n",Sum,Product).



-export([run/0]).
run() ->
    main().




string_display()->
    io:format("~nHello to Neptune and back..... Here I am again!!!").