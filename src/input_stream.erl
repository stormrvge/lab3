-module(input_stream).

-export([input_stream_handler/4]).

-import(point_generator, [generate_points/3]).
-import(console_printer, [print_solution/1]).

-include("approximation.hrl").

read_point() ->
  case io:fread("", "~f,~f") of
    {ok, [X, Y]} -> {ok, #point{x = X, y = Y}};
    {error, Reason} -> {error, Reason}
  end.

request_solutions(Approximators, Points, Step) ->
  lists:foreach(
    fun (Approximator) -> Approximator ! {approximate, self(), Points, Step} end,
    Approximators
  ).

receive_solutions([], Solutions) -> Solutions;
receive_solutions([_ | Approximators], Solutions) ->
  receive
    {ok, {Method, Solution}} ->
      receive_solutions(Approximators, lists:append(Solutions, [{Method, Solution}]))
  after
    5000 -> exit(timeout)
  end.

get_solutions(Approximators, Points, Step) ->
  request_solutions(Approximators, Points, Step),
  receive_solutions(Approximators, []).

print_solutions(Solutions, PrinterPid) ->
  lists:map(
    fun
      (Solution) ->
        case Solution of
          {error, incomputable} -> PrinterPid ! {print_message, "Method is not applicable"};
          {Method, Points} -> PrinterPid ! {print_solution, Method, Points}
        end
    end,
    Solutions
  ).

shift_window(Points, WindowSize) ->
  case length(Points) > WindowSize of
    true ->
      [_ | Tail] = Points,
      Tail;

    false -> Points
  end.

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
