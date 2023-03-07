-module(csv_parser).

-export([parse_csv/1]).

parse_csv(File) ->
  try
    {ok, Bin} = file:read_file(File),
    {ok, parse(binary_to_list(Bin), [], [], [])}
  catch
    Class:Error -> {Class, Error}
  end.

parse([], _FBuff, _RBuff, Result) -> lists:reverse([lists:reverse([_FBuff | _RBuff]) | Result]);
parse([$,, $\s | Rest], FBuff, RBuff, Result) ->
  parse(Rest, [], [lists:reverse(FBuff) | RBuff], Result);
parse([$\r, $\n | Rest], FBuff, RBuff, Result) ->
  parse(Rest, [], [], [lists:reverse([lists:reverse(FBuff) | RBuff]) | Result]);
parse([A | Rest], FBuff, RBuff, Result) -> parse(Rest, [A | FBuff], RBuff, Result).
