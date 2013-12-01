-module(maxminddb).

-export([open/0,
         open/1,
         load/1,
         find/2,
         find/3]).

-record(db, {root,
             meta,
             ip_version,
             node_count,
             record_size,
             record_half,
             record_left,
             tree_size,
             data_offs}).

-define(MetaMagic, <<16#abcdef:24, "MaxMind.com">>).
-define(MetaMax, 20000).

open() ->
    open(priv(?MODULE)).

open(Path) ->
    load(read(Path)).

load(Root) ->
    Meta = meta(Root),
    IPVersion = proplists:get_value(<<"ip_version">>, Meta),
    NodeCount = proplists:get_value(<<"node_count">>, Meta),
    RecordSize = proplists:get_value(<<"record_size">>, Meta),
    RecordHalf = RecordSize rem 8,
    TreeSize = NodeCount * RecordSize div 4,
    #db{root=Root,
        meta=Meta,
        ip_version=IPVersion,
        node_count=NodeCount,
        record_size=RecordSize,
        record_half=RecordHalf,
        record_left=RecordSize - RecordHalf,
        tree_size=TreeSize,
        data_offs=TreeSize + 16}.

priv(Name) ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    filename:join(PrivDir, Name).

read(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            case filename:extension(Path) of
                ".gz" ->
                    zlib:gunzip(Data);
                _ ->
                    Data
            end
    end.

meta(Root) ->
    {Pos, Len} = binary:match(Root, ?MetaMagic, [{scope, {size(Root), -?MetaMax}}]),
    value(#db{root=Root}, Pos + Len).

find(#db{ip_version=6} = G, {A, B, C, D}) ->
    find(G, {0, 0, 0, 0, 0, 16#ffff, (A bsl 8) bor B, (C bsl 8) bor D});
find(G, Addr) when is_binary(Addr) ->
    find(G, binary_to_list(Addr));
find(G, Addr) when is_list(Addr) ->
    {ok, IP} = inet:parse_address(Addr),
    find(G, IP);
find(G, IP) ->
    find(G, path(IP), node(G, 0)).

find(_, _, {stop, Value}) ->
    {ok, Value};
find(_, [], Node) ->
    {error, Node};
find(G, [0|Path], {L, _}) ->
    find(G, Path, node(G, L));
find(G, [1|Path], {_, R}) ->
    find(G, Path, node(G, R)).

node(#db{node_count=N}, N) ->
    {stop, undefined};
node(#db{node_count=N, tree_size=T} = G, K) when K > N ->
    {stop, value(G, K - N + T)};
node(#db{root=Root, record_size=S, record_half=H, record_left=L}, K) ->
    <<Low:L, High:H, Right:S>> = binary:part(Root, K * S div 4, S div 4),
    {Low + (High bsl L), Right}.

decode(G, <<1:3, S:2, R/bitstring>>) ->
    decode(G, {pointer, S}, R);
decode(G, <<2:3, S:5, R/binary>>) ->
    decode(G, {binary, S}, R);
decode(_, <<3:3, 8:5, D:64/float, R/binary>>) ->
    {D, R};
decode(G, <<4:3, S:5, R/binary>>) ->
    decode(G, {binary, S}, R);
decode(G, <<5:3, S:5, R/binary>>) ->
    decode(G, {uint, S}, R);
decode(G, <<6:3, S:5, R/binary>>) ->
    decode(G, {uint, S}, R);
decode(G, <<7:3, S:5, R/binary>>) ->
    decode(G, {map, S}, R);

decode(G, <<0:3, S:5, 1, R/binary>>) ->
    decode(G, {int, S}, R);
decode(G, <<0:3, S:5, 2, R/binary>>) ->
    decode(G, {uint, S}, R);
decode(G, <<0:3, S:5, 3, R/binary>>) ->
    decode(G, {uint, S}, R);
decode(G, <<0:3, S:5, 4, R/binary>>) ->
    decode(G, {array, S}, R);
decode(_, <<0:3, _:5, 5, R/binary>>) ->
    {cache, R};
decode(_, <<0:3, 0:5, 6, R/binary>>) ->
    {eod, R};
decode(_, <<0:3, 1:5, 7, R/binary>>) ->
    {true, R};
decode(_, <<0:3, 0:5, 7, R/binary>>) ->
    {false, R};
decode(_, <<0:3, 4:5, 8, F:32/float, R/binary>>) ->
    {F, R}.

decode(_, {len, S}, Data) when S < 29 ->
    {S, Data};
decode(_, {len, S}, Data) ->
    Size = S - 29,
    <<L:Size, R/binary>> = Data,
    {L + 29, R};

decode(G, {binary, S}, Data) ->
    {Len, Rest} = decode(G, {len, S}, Data),
    <<B:Len/binary, R/binary>> = Rest,
    {B, R};

decode(_, {int, S}, Data) ->
    <<I:S/signed-integer-unit:8, R/binary>> = Data,
    {I, R};
decode(_, {uint, S}, Data) ->
    <<U:S/unsigned-integer-unit:8, R/binary>> = Data,
    {U, R};

decode(G, {pointer, 0}, Data) ->
    <<P:11, R/binary>> = Data,
    {value(G, G#db.data_offs + P), R};
decode(G, {pointer, 1}, Data) ->
    <<P:19, R/binary>> = Data,
    {value(G, G#db.data_offs + P + 2048), R};
decode(G, {pointer, 2}, Data) ->
    <<P:27, R/binary>> = Data,
    {value(G, G#db.data_offs + P + 526336), R};
decode(G, {pointer, 3}, Data) ->
    <<_:3, P:32, R/binary>> = Data,
    {value(G, G#db.data_offs + P), R};

decode(G, {map, S}, Data) ->
    {Len, Rest} = decode(G, {len, S}, Data),
    decode(G, {map, Len, []}, Rest);

decode(_, {map, 0, Acc}, Data) ->
    {Acc, Data};
decode(G, {map, N, Acc}, Data) ->
    {Key, R0} = decode(G, Data),
    {Val, R1} = decode(G, R0),
    decode(G, {map, N - 1, [{Key, Val}|Acc]}, R1);

decode(G, {array, S}, Data) ->
    {Len, Rest} = decode(G, {len, S}, Data),
    decode(G, {array, Len, []}, Rest);

decode(_, {array, 0, Acc}, Data) ->
    {lists:reverse(Acc), Data};
decode(G, {array, N, Acc}, Data) ->
    {Val, Rest} = decode(G, Data),
    decode(G, {array, N - 1, [Val|Acc]}, Rest).

value(#db{root=Root} = G, Offs) ->
    <<_:Offs/binary, Rest/binary>> = Root,
    {Val, _} = decode(G, Rest),
    Val.

bits(I, Size) ->
    [B || <<B:1>> <= <<I:Size>>].

path({A, B, C, D}) ->
    bits((A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D, 32);
path({A, B, C, D, E, F, G, H}) ->
    bits((A bsl 112) bor (B bsl 96) bor (C bsl 80) bor (D bsl 64) bor (E bsl 48) bor (F bsl 32) bor (G bsl 16) bor H, 128).
