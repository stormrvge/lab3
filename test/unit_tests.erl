-module(unit_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("src/approximation.hrl").

-import(
  approximation_methods,
  [
    linear_approximation/2,
    power_approximation/2,
    exponential_approximation/2,
    logarithmic_approximation/2
  ]
).

-define(deviation, 0.01).

linear_approximation_test() ->
  Points =
    [
      #point{x = 1.3, y = 1.7},
      #point{x = 3.5, y = 5.1},
      #point{x = 9.7, y = 16.2},
      #point{x = 12.8, y = 9.1},
      #point{x = 20.2, y = 3.7}
    ],
  XValues = [1.3, 3.5, 9.7, 12.8, 20.2],
  {_, Solution} = linear_approximation(Points, XValues),
  [FirstPoint, SecondPoint, ThirdPoint, FourthPoint, FifthPoint] = Solution,
  ?assert((6.144 - ?deviation < FirstPoint#point.y) and (6.144 + ?deviation > FirstPoint#point.y)),
  ?assert((6.416 - ?deviation < SecondPoint#point.y) and (6.416 + ?deviation > SecondPoint#point.y)),
  ?assert((7.184 - ?deviation < ThirdPoint#point.y) and (7.184 + ?deviation > ThirdPoint#point.y)),
  ?assert((7.568 - ?deviation < FourthPoint#point.y) and (7.568 + ?deviation > FourthPoint#point.y)),
  ?assert((8.485 - ?deviation < FifthPoint#point.y) and (8.485 + ?deviation > FifthPoint#point.y)).

power_approximation_test() ->
  Points =
    [
      #point{x = 1.3, y = 1.7},
      #point{x = 3.5, y = 5.1},
      #point{x = 9.7, y = 16.2},
      #point{x = 12.8, y = 9.1},
      #point{x = 20.2, y = 3.7}
    ],
  XValues = [1.3, 3.5, 9.7, 12.8, 20.2],
  {_, Solution} = power_approximation(Points, XValues),
  [FirstPoint, SecondPoint, ThirdPoint, FourthPoint, FifthPoint] = Solution,
  ?assert((2.58 - ?deviation < FirstPoint#point.y) and (2.58 + ?deviation > FirstPoint#point.y)),
  ?assert((4.083 - ?deviation < SecondPoint#point.y) and (4.083 + ?deviation > SecondPoint#point.y)),
  ?assert((6.548 - ?deviation < ThirdPoint#point.y) and (6.548 + ?deviation > ThirdPoint#point.y)),
  ?assert((7.446 - ?deviation < FourthPoint#point.y) and (7.446 + ?deviation > FourthPoint#point.y)),
  ?assert((9.199 - ?deviation < FifthPoint#point.y) and (9.199 + ?deviation > FifthPoint#point.y)).

exponential_approximation_test() ->
  Points =
    [
      #point{x = 1.3, y = 1.7},
      #point{x = 3.5, y = 5.1},
      #point{x = 9.7, y = 16.2},
      #point{x = 12.8, y = 9.1},
      #point{x = 20.2, y = 3.7}
    ],
  XValues = [1.3, 3.5, 9.7, 12.8, 20.2],
  {_, Solution} = exponential_approximation(Points, XValues),
  [FirstPoint, SecondPoint, ThirdPoint, FourthPoint, FifthPoint] = Solution,
  ?assert((4.118 - ?deviation < FirstPoint#point.y) and (4.118 + ?deviation > FirstPoint#point.y)),
  ?assert((4.436 - ?deviation < SecondPoint#point.y) and (4.436 + ?deviation > SecondPoint#point.y)),
  ?assert((5.468 - ?deviation < ThirdPoint#point.y) and (5.468 + ?deviation > ThirdPoint#point.y)),
  ?assert((6.071 - ?deviation < FourthPoint#point.y) and (6.071 + ?deviation > FourthPoint#point.y)),
  ?assert((7.794 - ?deviation < FifthPoint#point.y) and (7.794 + ?deviation > FifthPoint#point.y)).

logarithmic_approximation_test() ->
  Points =
    [
      #point{x = 1.3, y = 1.7},
      #point{x = 3.5, y = 5.1},
      #point{x = 9.7, y = 16.2},
      #point{x = 12.8, y = 9.1},
      #point{x = 20.2, y = 3.7}
    ],
  XValues = [1.3, 3.5, 9.7, 12.8, 20.2],
  {_, Solution} = logarithmic_approximation(Points, XValues),
  [FirstPoint, SecondPoint, ThirdPoint, FourthPoint, FifthPoint] = Solution,
  ?assert((3.514 - ?deviation < FirstPoint#point.y) and (3.514 + ?deviation > FirstPoint#point.y)),
  ?assert((5.762 - ?deviation < SecondPoint#point.y) and (5.762 + ?deviation > SecondPoint#point.y)),
  ?assert((8.076 - ?deviation < ThirdPoint#point.y) and (8.076 + ?deviation > ThirdPoint#point.y)),
  ?assert((8.705 - ?deviation < FourthPoint#point.y) and (8.705 + ?deviation > FourthPoint#point.y)),
  ?assert((9.741 - ?deviation < FifthPoint#point.y) and (9.741 + ?deviation > FifthPoint#point.y)).
