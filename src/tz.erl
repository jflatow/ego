-module(tz).

-export([nautical/1,
         solar/1]).

nautical({_, Lng}) when Lng > 0 ->
    trunc(Lng / 15 + 0.5);
nautical({_, Lng}) ->
    trunc(Lng / 15 - 0.5).

solar({_, Lng}) ->
    Lng / 15.
