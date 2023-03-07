-module(approximator).

-include("approximation.hrl").

-export([approximator/1]).

generate_points(LeftPoint, RightPoint, Step, Points) ->
  NewPoint = LeftPoint + Step,
  case NewPoint >= RightPoint of
    true -> lists:reverse(Points);
    false -> generate_points(NewPoint, RightPoint, Step, [NewPoint | Points])
  end.

take_points(Points, Step) ->
  [RightPoint, LeftPoint | _] = Points,
  generate_points(LeftPoint, RightPoint, Step, [LeftPoint]).

approximator(ApproximationMethod) ->
  receive
    {approximate, SenderPid, Points, Step} ->
      XValues = lists:foldl(fun (Point, Acc) -> [Point#point.x | Acc] end, [], Points),
      NewXValues = take_points(XValues, Step),
      Solution = ApproximationMethod(Points, NewXValues),
      SenderPid ! {ok, Solution},
      approximator(ApproximationMethod);

    close -> closed
  end.
