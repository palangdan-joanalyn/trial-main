%%%-------------------------------------------------------------------
%%% @author TEAM08_ERLANG
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% A module to perform operations on fractions: addition, subtraction, multiplication, division, and simplification.
%%% @end
%%% Created : 08. Dec 2024
%%%-------------------------------------------------------------------
-module(fraction_operations).
-author("TEAM08_ERLANG").

%% API
-export([add_fractions/2, subtract_fractions/2, multiply_fractions/2, divide_fractions/2, simplify_fraction/1]).

%% Function to add two fractions
add_fractions({N1, D1}, {N2, D2}) ->
  {Num, Denom} = {N1 * D2 + N2 * D1, D1 * D2},
  simplify_fraction({Num, Denom}).

%% Function to subtract two fractions
subtract_fractions({N1, D1}, {N2, D2}) ->
  {Num, Denom} = {N1 * D2 - N2 * D1, D1 * D2},
  simplify_fraction({Num, Denom}).

%% Function to multiply two fractions
multiply_fractions({N1, D1}, {N2, D2}) ->
  {Num, Denom} = {N1 * N2, D1 * D2},
  simplify_fraction({Num, Denom}).

%% Function to divide two fractions
divide_fractions({N1, D1}, {N2, D2}) when N2 /= 0 ->
  {Num, Denom} = {N1 * D2, D1 * N2},
  simplify_fraction({Num, Denom});
divide_fractions(_, {0, _}) ->
  throw({error, division_by_zero}).

%% Function to simplify a fraction
simplify_fraction({Num, Denom}) when Denom < 0 ->
  %% Ensure the denominator is always positive
  simplify_fraction({-Num, -Denom});
simplify_fraction({Num, Denom}) ->
  GCD = gcd(erlang:abs(Num), erlang:abs(Denom)),
  {Num div GCD, Denom div GCD}.

%% Helper function to compute the greatest common divisor (GCD)
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

%% Helper function to compute the absolute value of a number
abs(X) when X < 0 -> -X;
abs(X) -> X.
