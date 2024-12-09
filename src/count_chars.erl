%%%-------------------------------------------------------------------
%%% @author TEAM08_ERLANG
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% A module to count letters, numbers, and non-character symbols in a string.
%%% @end
%%% Created : 08. Dec 2024 5:57 pm
%%%-------------------------------------------------------------------
-module(count_chars).
-author("TEAM08_ERLANG").

%% API
-export([count_letters/1, count_lowercase_letters/1, count_uppercase_letters/1, count_numbers/1, count_non_characters/1]).

%% Main function to count the number of letters in a string
count_letters(String) when is_list(String) ->
  count_letters_internal(String, 0).

%% Main function to count the number of lowercase letters in a string
count_lowercase_letters(String) when is_list(String) ->
  count_lowercase_internal(String, 0).

%% Main function to count the number of uppercase letters in a string
count_uppercase_letters(String) when is_list(String) ->
  count_uppercase_internal(String, 0).

%% Main function to count the number of numeric characters in a string
count_numbers(String) when is_list(String) ->
  count_numbers_internal(String, 0).

%% Main function to count the number of non-character symbols in a string
count_non_characters(String) when is_list(String) ->
  count_non_characters_internal(String, 0).

%% Helper function to traverse the string and count letters
count_letters_internal([], Count) ->
  Count;
count_letters_internal([H|T], Count) when (H >= $a andalso H =< $z) orelse (H >= $A andalso H =< $Z) ->
  count_letters_internal(T, Count + 1);
count_letters_internal([_|T], Count) ->
  count_letters_internal(T, Count).

%% Helper function to traverse the string and count lowercase letters
count_lowercase_internal([], Count) ->
  Count;
count_lowercase_internal([H|T], Count) when (H >= $a andalso H =< $z) ->
  count_lowercase_internal(T, Count + 1);
count_lowercase_internal([_|T], Count) ->
  count_lowercase_internal(T, Count).

%% Helper function to traverse the string and count uppercase letters
count_uppercase_internal([], Count) ->
  Count;
count_uppercase_internal([H|T], Count) when (H >= $A andalso H =< $Z) ->
  count_uppercase_internal(T, Count + 1);
count_uppercase_internal([_|T], Count) ->
  count_uppercase_internal(T, Count).

%% Helper function to traverse the string and count numeric characters
count_numbers_internal([], Count) ->
  Count;
count_numbers_internal([H|T], Count) when (H >= $0 andalso H =< $9) ->
  count_numbers_internal(T, Count + 1);
count_numbers_internal([_|T], Count) ->
  count_numbers_internal(T, Count).

%% Helper function to traverse the string and count non-character symbols
count_non_characters_internal([], Count) ->
  Count;
count_non_characters_internal([H|T], Count) when not ((H >= $a andalso H =< $z) orelse (H >= $A andalso H =< $Z) orelse (H >= $0 andalso H =< $9)) ->
  count_non_characters_internal(T, Count + 1);
count_non_characters_internal([_|T], Count) ->
  count_non_characters_internal(T, Count).
