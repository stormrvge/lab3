-module(file_reader).

-include("approximation.hrl").

-import(csv_parser, [parse_csv/1]).

-export([read_points_from_file/1]).

fill_points(Points, []) -> lists:reverse(Points);

fill_points(Points, [Field | Tail]) ->
  NewPoint = #point{x = list_to_float(lists:nth(1, Field)), y = list_to_float(lists:nth(2, Field))},
  fill_points([NewPoint | Points], Tail).

read_points_from_file(File) ->
  case parse_csv(File) of
    {ok, Fields} -> {ok, fill_points([], Fields)};
    {_, Error} -> {error, Error}
  end.
