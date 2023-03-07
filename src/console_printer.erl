-module(console_printer).

-include_lib("approximation.hrl").

-export([console_printer/0]).

print_solution(Solution) ->
  lists:foreach(
    fun (Point) -> io:fwrite(" ~.4f ~.4f~n", [Point#point.x, Point#point.y]) end,
    Solution
  ).

print_message(Message) -> io:format("~s~n", [Message]).

console_printer() ->
  receive
    {print_solution, Method, Points} ->
      print_message("-------------------------------"),
      print_message(string:concat("Result of ", Method)),
      print_solution(Points),
      print_message(""),
      console_printer();

    {print_message, Message} ->
      print_message(Message),
      console_printer();

    {close} -> closed
  end.
