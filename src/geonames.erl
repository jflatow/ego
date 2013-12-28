-module(geonames).

-export([index/0,
         index/1,
         index/3,
         fetch/2,
         place/2]).

-record(geoname, {country_code,
                  postal_code,
                  place_name,
                  admin,
                  latlng,
                  accuracy}).

num(<<>>) ->
    undefined;
num(Bin) when is_binary(Bin) ->
    num(binary_to_list(Bin));
num(List) ->
    try list_to_float(List) catch error:badarg -> list_to_integer(List) end.

box(Point) ->
    box(Point, 1).

box({Ly, Lx}, W) ->
    [{Ly + I, Lx + J} || I <- lists:seq(-W, W), J <- lists:seq(-W, W)].

key({Ly, Lx}) ->
    io_lib:format("~p,~p", [Ly, Lx]).

tile({Lat, Lng}) ->
    {trunc(Lat * 10), trunc(Lng * 10)}.

encode(Val) ->
    term_to_binary(Val).

decode(Bin) ->
    binary_to_term(Bin).

index() ->
    index(ego:priv("geonames/allCountries.txt")).

index(Path) ->
    discodb:finalize(
      index(Path,
            fun (#geoname{place_name=(<<>>)}, Cons) ->
                    Cons;
                (#geoname{latlng={Lat, Lng}}, Cons) when Lat =:= undefined;
                                                         Lng =:= undefined ->
                    Cons;
                (#geoname{latlng=LatLng} = Val, Cons) ->
                    discodb:add(Cons, key(tile(LatLng)), encode(Val))
            end, discodb:cons()), [unique_items]).

index(Path, Fun, Acc) ->
    path:foldlines(Path,
                   fun (Line, A) ->
                           case binary:split(Line, <<"\t">>, [global]) of
                               [CountryCode, PostalCode, PlaceName,
                                AdminName1, AdminCode1,
                                AdminName2, AdminCode2,
                                AdminName3, AdminCode3,
                                Lat, Lng, Accuracy] ->
                                   Fun(#geoname{country_code=CountryCode,
                                                postal_code=PostalCode,
                                                place_name=PlaceName,
                                                admin=[{AdminName1, AdminCode1},
                                                       {AdminName2, AdminCode2},
                                                       {AdminName3, AdminCode3}],
                                                latlng={num(Lat), num(Lng)},
                                                accuracy=Accuracy}, A)
                           end
                   end, Acc, [trim]).

fetch(DB, Query) ->
    lists:usort(
      discodb:fold(discodb:q(DB, [[key(P) || P <- box(tile(Query))]]),
                   fun (V, A) ->
                           case decode(V) of
                               #geoname{latlng=LatLng} = Val ->
                                   [{geo:haversine(Query, LatLng), Val}|A]
                           end
                   end, [])).

place(DB, Query) ->
    case fetch(DB, Query) of
        [{_, #geoname{place_name=Place}}|_] ->
            Place;
        [] ->
            undefined
    end.
