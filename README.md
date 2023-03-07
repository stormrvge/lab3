# Approximation

## Ключевые моменты реализации

### Линейная аппроксимация
```erlang
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
```

### Степенная аппроксимация
```erlang
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
```

### Экспоненциальная аппроксимация
```erlang
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
```

### Логарифмическая аппроксимация
```erlang
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
```

### Процесс, отвечающий за аппроксимацию
```erlang
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
```

### Обработчик пользовательского ввода
```erlang
input_stream_handler(Approximators, PrinterPid, Points, Config) ->
  case read_point() of
    {ok, NewPoint} ->
      LastPoint =
        case Points of
          [] -> #point{x = NewPoint#point.x - 1, y = NewPoint#point.y};
          _ -> lists:last(Points)
        end,
      case LastPoint#point.x >= NewPoint#point.x of
        true ->
          PrinterPid ! {print_message, "New point must be greater than previous."},
          input_stream_handler(Approximators, PrinterPid, Points, Config);

        false ->
          case length(Points) >= Config#config.window - 1 of
            true ->
              NewPoints = shift_window(lists:append(Points, [NewPoint]), Config#config.window),
              Solutions = get_solutions(Approximators, NewPoints, Config#config.step),
              print_solutions(Solutions, PrinterPid),
              input_stream_handler(Approximators, PrinterPid, NewPoints, Config);

            false ->
              input_stream_handler(
                Approximators,
                PrinterPid,
                lists:append(Points, [NewPoint]),
                Config
              )
          end
      end;

    {error, _} ->
      PrinterPid ! {print_message, "Invalid input. Try again."},
      input_stream_handler(Approximators, PrinterPid, Points, Config)
  end.
```


## Пример работы

```
2.1, 3.5
5.4, 6.1
6.8, 20.1
-------------------------------
Result of Linear Approximation
 5.4000 11.8250
 5.6000 12.4329
 5.8000 13.0408
 6.0000 13.6487
 6.2000 14.2566
 6.4000 14.8645
 6.6000 15.4724

-------------------------------
Result of Power Approximation
 5.4000 10.1000
 5.6000 10.5608
 5.8000 11.0254
 6.0000 11.4937
 6.2000 11.9655
 6.4000 12.4408
 6.6000 12.9194

-------------------------------
Result of Exponential Approximation
 5.4000 9.3256
 5.6000 9.9725
 5.8000 10.6643
 6.0000 11.4040
 6.2000 12.1951
 6.4000 13.0411
 6.6000 13.9457

-------------------------------
Result of Logarithmic Approximation
 5.4000 12.4773
 5.6000 12.8712
 5.8000 13.2512
 6.0000 13.6184
 6.2000 13.9735
 6.4000 14.3173
 6.6000 14.6506

7.9, 16.1
-------------------------------
Result of Linear Approximation
 6.8000 14.5268
 7.0000 15.3803
 7.2000 16.2338
 7.4000 17.0873
 7.6000 17.9408
 7.8000 18.7943

-------------------------------
Result of Exponential Approximation
 6.8000 13.0645
 7.0000 14.1776
 7.2000 15.3856
 7.4000 16.6965
 7.6000 18.1191
 7.8000 19.6629

-------------------------------
Result of Logarithmic Approximation
 6.8000 14.8840
 7.0000 15.7301
 7.2000 16.5523
 7.4000 17.3520
 7.6000 18.1304
 7.8000 18.8886

-------------------------------
Result of Power Approximation
 6.8000 13.5179
 7.0000 14.6489
 7.2000 15.8386
 7.4000 17.0884
 7.6000 18.3995
 7.8000 19.7731
```

