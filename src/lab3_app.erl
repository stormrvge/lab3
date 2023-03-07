-module(lab3_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

-include("approximation.hrl").

-import(file_reader, [read_points_from_file/1]).
-import(
approximation,
[
linear_approximation/1,
power_approximation/1,
exponential_approximation/1,
logarithmic_approximation/1,
square_approximation/1
]
).
-import(console_printer, [console_printer/0]).
-import(approximator, [approximator/1]).
-import(input_stream, [input_stream_handler/4]).

init_default_config() -> #config{step = 0.5, window = 3, file = none, methods = [linear]}.

init_config(Step, Window, File, Methods) ->
  #config{step = Step, window = Window, file = File, methods = Methods}.

parse_methods([], Methods) -> {[], Methods};

parse_methods([Value | Args], Methods) ->
  case Value of
    "linear" -> parse_methods(Args, lists:append(Methods, [linear]));
    "exponential" -> parse_methods(Args, lists:append(Methods, [exponential]));
    "power" -> parse_methods(Args, lists:append(Methods, [power]));
    "logarithmic" -> parse_methods(Args, lists:append(Methods, [logarithmic]));
    _ -> {Args, Methods}
  end.

parse_args(Config, [], _) -> Config;
parse_args(Config, [Arg, Value | Args], PrinterPid) ->
  case Arg of
    "-step" ->
      case string:to_float(Value) of
        {error, _} ->
          PrinterPid ! {print_message, "Invalid step value."},
          parse_args(Config, Args, PrinterPid);

        {Step, _} ->
          NewConfig =
            init_config(Step, Config#config.window, Config#config.file, Config#config.methods),
          parse_args(NewConfig, Args, PrinterPid)
      end;

    "-window" ->
      case string:to_integer(Value) of
        {error, _} ->
          PrinterPid ! {print_message, "Invalid window value."},
          parse_args(Config, Args, PrinterPid);

        {Window, _} ->
          NewConfig =
            init_config(Config#config.step, Window, Config#config.file, Config#config.methods),
          parse_args(NewConfig, Args, PrinterPid)
      end;

    "-file" ->
      NewConfig =
        init_config(Config#config.step, Config#config.window, Value, Config#config.methods),
      parse_args(NewConfig, Args, PrinterPid);

    "-methods" ->
      {RemainingArgs, Methods} = parse_methods([Value | Args], []),
      NewConfig =
        init_config(Config#config.step, Config#config.window, Config#config.file, Methods),
      parse_args(NewConfig, RemainingArgs, PrinterPid);

    _ -> PrinterPid ! {print_message, "Invalid argument."}
  end.

start() ->
  Args = init:get_arguments(),
  File = init:get_argument("file"),
  Methods = init:get_argument("methods"),
  Params = case File of
             undefined -> case Methods of
                            undefined -> ["-step", init:get_argument("step"), "-window", init:get_argument("window")];
                            Value ->
                              ["-step", init:get_argument("step"), "-window", init:get_argument("window"), "-methods", Value]
                          end;
             FileValue -> case Methods of
                            undefined ->
                              ["-step", init:get_argument("step"), "-window", init:get_argument("window"), "-file", init:get_argument("file")];
                            Value ->
                              ["-step", init:get_argument("step"), "-window", init:get_argument("window"), "-methods", Value, "-file", FileValue]
                          end
           end,
  start([], Params).

start(_StartType, _StartArgs) ->
  PrinterPid = create_console_printer(),
  Config = parse_args(init_default_config(), _StartArgs, PrinterPid),
  Approximators = create_approximators(Config#config.methods),
  case Config#config.file of
    none -> input_stream_handler(Approximators, PrinterPid, [], Config);
    File ->
      case read_points_from_file(File) of
        {ok, Points} ->
          SortedPoints =
            lists:sort(
              fun(FirstPoint, SecondPoint) -> FirstPoint#point.x =< SecondPoint#point.x end,
              Points
            ),
          input_stream_handler(Approximators, PrinterPid, SortedPoints, Config);

        {error, _} ->
          PrinterPid ! {print_message, "Unable to open file"},
          input_stream_handler(Approximators, PrinterPid, [], Config)
      end
  end,
  finish(Approximators, PrinterPid),
  ok.

create_approximators(Methods) ->
  Pids =
    lists:foldl(
      fun
        (Method, Pids) ->
          case Method of
            linear ->
              MethodPid =
                spawn(fun() -> approximator(fun approximation_methods:linear_approximation/2) end),
              [MethodPid | Pids];

            power ->
              MethodPid =
                spawn(fun() -> approximator(fun approximation_methods:power_approximation/2) end),
              [MethodPid | Pids];

            exponential ->
              MethodPid =
                spawn(
                  fun() -> approximator(fun approximation_methods:exponential_approximation/2) end
                ),
              [MethodPid | Pids];

            logarithmic ->
              MethodPid =
                spawn(
                  fun() -> approximator(fun approximation_methods:logarithmic_approximation/2) end
                ),
              [MethodPid | Pids]
          end
      end,
      [],
      Methods
    ),
  lists:reverse(Pids).

stop_approximators(Approximators) ->
  lists:foreach(fun(Approximator) -> Approximator ! close end, Approximators).

create_console_printer() -> spawn(fun() -> console_printer() end).

stop_console_printer(PrinterPid) -> PrinterPid ! close.

finish(Approximators, PrinterPid) ->
  stop_approximators(Approximators),
  stop_console_printer(PrinterPid).

stop(_State) -> ok.
