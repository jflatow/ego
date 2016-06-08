-module(geo).
-define(REarth, 6371).
-export([haversine/2, dist/2]).

rad(X) -> math:pi() / 180 * X.
cos(X) -> math:cos(rad(X)).
sin(X) -> math:sin(rad(X)).
sqr(X) -> X * X.

haversin(X) ->
    sqr(sin(X / 2)).

haversine({Lat, Lng}, {Lat_, Lng_}) ->
    math:asin(math:sqrt(haversin(Lat_ - Lat) + cos(Lat) * cos(Lat_) * haversin(Lng_ - Lng))).



dist({Lat, Lng}, {Lat_, Lng_}) ->
    Dist = haversine({PictLat, PictLon},
                     {Lat , Lon}) * ?REarth * 2.
