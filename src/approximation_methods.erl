-module(approximation_methods).

-include("approximation.hrl").

-export(
  [
    linear_approximation/2,
    power_approximation/2,
    exponential_approximation/2,
    logarithmic_approximation/2
  ]
).

-import(linear_system_solver, [solve_linear_system/3]).
-import(math, [pow/2, log/1]).

-define(E_COEFFICIENT, 2.718).

calculate_a_b_coefficients(Coefficients, PointsAmount) ->
  {SX, SY, SXX, SXY} = Coefficients,
  Det = SXX * PointsAmount - SX * SX,
  Det_1 = SXY * PointsAmount - SX * SY,
  Det_2 = SXX * SY - SX * SXY,
  case (Det) of
    0 -> {error, incomputable};
    _ -> {Det_1 / Det, Det_2 / Det}
  end.


calculate_coefficients([], _, Coefficients) -> Coefficients;
calculate_coefficients(Points, Function, Coefficients) ->
  [Point | Tail] = Points,
  NewCoefficients = Function(Coefficients, Point),
  calculate_coefficients(Tail, Function, NewCoefficients).

calculate_coefficients(Points, Function) -> calculate_coefficients(Points, Function, {0, 0, 0, 0}).

fill_points([], _, _, _, Acc) -> Acc;

fill_points([Point | Tail], MathFunction, A, B, Acc) ->
  NewPoint = #point{x = Point, y = MathFunction(Point, A, B)},
  fill_points(Tail, MathFunction, A, B, [NewPoint | Acc]).

fill_points(Points, MathFunction, A, B) -> fill_points(Points, MathFunction, A, B, []).

linear_approximation(Points, XValues) ->
  CoefficientsFunction =
    fun
      (Coefficients, Point) ->
        {SX, SY, SXX, SXY} = Coefficients,
        {
          SX + Point#point.x,
          SY + Point#point.y,
          SXX + pow(Point#point.x, 2),
          SXY + Point#point.x * Point#point.y
        }
    end,
  Coefficients = calculate_coefficients(Points, CoefficientsFunction),
  PointsAmount = length(Points),
  case calculate_a_b_coefficients(Coefficients, PointsAmount) of
    {error, incomputable} -> {error, incomputable};

    {CoeffA, CoeffB} ->
      {
        "Linear Approximation",
        lists:reverse(fill_points(XValues, fun (X, A, B) -> A * X + B end, CoeffA, CoeffB))
      }
  end.

power_approximation(Points, XValues) ->
  Computable =
    lists:foldl(
      fun
        (Point, Computable) ->
          case Computable of
            true -> (Point#point.x >= 0) and (Point#point.y >= 0);
            false -> false
          end
      end,
      true,
      Points
    ),
  case Computable of
    false -> {error, incomputable};

    true ->
      CoefficientsFunction =
        fun
          (Coefficients, Point) ->
            {SX, SY, SXX, SXY} = Coefficients,
            {
              SX + log(Point#point.x),
              SY + log(Point#point.y),
              SXX + pow(log(Point#point.x), 2),
              SXY + log(Point#point.x) * log(Point#point.y)
            }
        end,
      Coefficients = calculate_coefficients(Points, CoefficientsFunction),
      PointsAmount = length(Points),
      case calculate_a_b_coefficients(Coefficients, PointsAmount) of
        {error, incomputable} -> {error, incomputable};

        {CoeffA, CoeffB} ->
          {
            "Power Approximation",
            lists:reverse(
              fill_points(
                XValues,
                fun (X, A, B) -> A * pow(X, B) end,
                pow(?E_COEFFICIENT, CoeffB),
                CoeffA
              )
            )
          }
      end
  end.

exponential_approximation(Points, XValues) ->
  Computable =
    lists:foldl(
      fun
        (Point, Computable) ->
          case Computable of
            true -> Point#point.y >= 0;
            false -> false
          end
      end,
      true,
      Points
    ),
  case Computable of
    false -> {error, incomputable};

    true ->
      CoefficientsFunction =
        fun
          (Coefficients, Point) ->
            {SX, SY, SXX, SXY} = Coefficients,
            {
              SX + Point#point.x,
              SY + log(Point#point.y),
              SXX + pow(Point#point.x, 2),
              SXY + Point#point.x * log(Point#point.y)
            }
        end,
      Coefficients = calculate_coefficients(Points, CoefficientsFunction),
      PointsAmount = length(Points),
      case calculate_a_b_coefficients(Coefficients, PointsAmount) of
        {error, incomputable} -> {error, incomputable};

        {CoeffA, CoeffB} ->
          {
            "Exponential Approximation",
            lists:reverse(
              fill_points(
                XValues,
                fun (X, A, B) -> A * pow(?E_COEFFICIENT, B * X) end,
                pow(?E_COEFFICIENT, CoeffB),
                CoeffA
              )
            )
          }
      end
  end.

logarithmic_approximation(Points, XValues) ->
  Computable =
    lists:foldl(
      fun
        (Point, Computable) ->
          case Computable of
            true -> Point#point.x >= 0;
            false -> false
          end
      end,
      true,
      Points
    ),
  case Computable of
    false -> {error, incomputable};

    true ->
      CoefficientsFunction =
        fun
          (Coefficients, Point) ->
            {SX, SY, SXX, SXY} = Coefficients,
            {
              SX + log(Point#point.x),
              SY + Point#point.y,
              SXX + pow(log(Point#point.x), 2),
              SXY + log(Point#point.x) * Point#point.y
            }
        end,
      Coefficients = calculate_coefficients(Points, CoefficientsFunction),
      PointsAmount = length(Points),
      case calculate_a_b_coefficients(Coefficients, PointsAmount) of
        {error, incomputable} -> {error, incomputable};

        {CoeffA, CoeffB} ->
          {
            "Logarithmic Approximation",
            lists:reverse(fill_points(XValues, fun (X, A, B) -> A * log(X) + B end, CoeffA, CoeffB))
          }
      end
  end.
