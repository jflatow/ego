-module(maxminddb).

%% @doc Library to search MaxMind format binary databases
%% @reference See <a href="http://maxmind.github.io/MaxMind-DB/">the data spec</a>

-export([open/0,
         open/1,
         load/1,
         find/2,
         find/3]).

-record(db, {root :: binary(),         % file data
             meta,                     % file level metadata, proplist
             ip_version  :: integer(), % 4 or 6
             node_count  :: integer(), % number of index nodes
             record_size :: integer(), % record size in bits
             node_size   :: integer(), % node size in bytes (2 x record size)
             tree_size   :: integer(), % tree size in bytes
             data_offs   :: integer()  % offset of data section in file
            }).

% Pattern which indicates start of metadata
-define(MetaMagic, <<16#abcdef:24, "MaxMind.com">>).
% 128kb is the max size of the metadata section
-define(MetaMax, 131072).

%% @doc Open and load default file (geoip) from priv dir
-spec open() -> #db{}.
open() ->
    open(ego:priv(geoip)).

%% @doc Open and load specific file by path
-spec open(Path) -> #db{} when
      Path :: list().
open(Path) ->
    load(ego:read(Path)).

%% @doc Parse data file
-spec load(Root) -> #db{} when
      Root :: binary().
load(Root) ->
    Meta = meta(Root),
    IPVersion = proplists:get_value(<<"ip_version">>, Meta),
    NodeCount = proplists:get_value(<<"node_count">>, Meta),
    RecordSize = proplists:get_value(<<"record_size">>, Meta),

    % NodeSize in bytes = (RecordSize in bits * 2) / 8 bits per byte
    NodeSize = RecordSize div 4,

    % TreeSize in bytes = NodeCount * NodeSize
    TreeSize = NodeCount * NodeSize,
    DataOffset = TreeSize + 16,

    #db{root=Root,
        meta=Meta,
        ip_version=IPVersion,
        node_count=NodeCount,
        record_size=RecordSize,
        node_size=NodeSize,
        tree_size=TreeSize,
        data_offs=DataOffset}.

%% @doc Decode metadata
-spec meta(Root) -> list(Metadata) when
        Root :: binary(),
        Metadata :: {binary(), any()}.
meta(Root) ->
    Matches = binary:matches(Root, ?MetaMagic, [{scope, {size(Root), -?MetaMax}}]),
    {Pos, Len} = lists:last(Matches),
    value(#db{root=Root}, Pos + Len).

%% @doc Find data for IP
-spec find(G, Addr) -> Value when
        G :: #db{},
        Addr :: inet:ip_address() | binary() | list(),
        Value :: any(). % TODO: tighten this up
find(#db{ip_version=6} = G, {A, B, C, D}) ->
    find(G, {0, 0, 0, 0, 0, 16#ffff, (A bsl 8) bor B, (C bsl 8) bor D});
% TODO: support 6to4 addresses from 2002::/16
find(G, Addr) when is_binary(Addr) ->
    find(G, binary_to_list(Addr));
find(G, Addr) when is_list(Addr) ->
    {ok, IP} = inet:parse_address(Addr),
    find(G, IP);
find(G, IP) ->
    find(G, path(IP), node(G, 0)).

-spec find(G, PathList, NodeValue) -> Result when
        G :: #db{},
        PathList :: list(integer()),
        NodeValue :: {stop, undefined} | {stop, any()} | {integer(), integer()}, % TODO: tighten up any()
        Result :: {ok, any()} | {error, any()}. % TODO: tighten up any()
find(_, _, {stop, Value}) ->
    {ok, Value};
find(_, [], Node) ->
    {error, Node};
find(G, [0|Path], {L, _}) ->
    find(G, Path, node(G, L));
find(G, [1|Path], {_, R}) ->
    find(G, Path, node(G, R)).

%% @doc Handle node value
-spec node(G, N) -> Value when
        G :: #db{},
        N :: integer(),
        Value :: {stop, undefined} | {stop, any()} | {integer(), integer()}. % TODO: tighten up any()
% No data for this IP
node(#db{node_count=N}, N) ->
    {stop, undefined};
% Pointer into data section
node(#db{node_count=N, tree_size=T} = G, K) when K > N ->
    {stop, value(G, K - N + T)};
% Node
node(#db{root=Root, record_size=S, node_size=N}, K) ->
    Offs = K * N,
    {Left, Right} = case S of
        24 ->
            <<L:S, R:S>> = binary:part(Root, Offs, N),
            {L, R};
        28 ->
            % The layout of 28 bit databases is weird
            <<LL:24, LH:4, RH:4, RL:24>> = binary:part(Root, Offs, N),
            {(LH bsl 4) + LL, (RH bsl 4) + RL};
        32 ->
            <<L:S, R:S>> = binary:part(Root, Offs, N),
            {L, R}
    end,
    {Left, Right}.

%% @doc Decode data field
-spec decode(G, R) -> {Value, R} when
        G :: #db{},
        R :: binary(),
        Value :: number() | binary() | list({binary(), any()}) | list(any()).
% Type 1, pointer
decode(G, <<1:3, S:2, R/bitstring>>) ->
    decode(G, {pointer, S}, R);
% Type 2, UTF-8 string TODO: use different 'string' type?
decode(G, <<2:3, S:5, R/binary>>) ->
    decode(G, {binary, S}, R);
% Type 3, IEEE-754 double
decode(_, <<3:3, 8:5, D:64/float, R/binary>>) ->
    {D, R};
% Type 4, bytes
decode(G, <<4:3, S:5, R/binary>>) ->
    decode(G, {binary, S}, R);
% Type 5, unsigned 16-bit integer
decode(G, <<5:3, S:5, R/binary>>) ->
    decode(G, {uint, S}, R);
% Type 6, unsigned 32-bit integer
decode(G, <<6:3, S:5, R/binary>>) ->
    decode(G, {uint, S}, R);
% Type 7, map
decode(G, <<7:3, S:5, R/binary>>) ->
    decode(G, {map, S}, R);

% Extended types
% Type 8, signed 32-bit integer
decode(G, <<0:3, S:5, 1, R/binary>>) ->
    decode(G, {int, S}, R);
% Type 9, unsigned 64-bit integer
decode(G, <<0:3, S:5, 2, R/binary>>) ->
    decode(G, {uint, S}, R);
% Type 10, unsigned 128-bit integer
decode(G, <<0:3, S:5, 3, R/binary>>) ->
    decode(G, {uint, S}, R);
% Type 11, array
decode(G, <<0:3, S:5, 4, R/binary>>) ->
    decode(G, {array, S}, R);
% Type 12, data cache container
decode(_, <<0:3, _:5, 5, R/binary>>) ->
    {cache, R};
% Type 13, end marker
decode(_, <<0:3, 0:5, 6, R/binary>>) ->
    {eod, R};
% Type 14, boolean true
decode(_, <<0:3, 1:5, 7, R/binary>>) ->
    {true, R};
% Type 14, boolean false
decode(_, <<0:3, 0:5, 7, R/binary>>) ->
    {false, R};
% Type 15, IEEE-754 32-bit float
decode(_, <<0:3, 4:5, 8, F:32/float, R/binary>>) ->
    {F, R}.

decode(_, {len, S}, Data) when S < 29 ->
    {S, Data};
decode(_, {len, 29}, Data) ->
    <<L:1/unsigned-integer-unit:8, R/binary>> = Data,
    {L + 29, R};
decode(_, {len, 30}, Data) ->
    <<L:2/unsigned-integer-unit:8, R/binary>> = Data,
    {L + 285, R};
decode(_, {len, 31}, Data) ->
    <<L:3/unsigned-integer-unit:8, R/binary>> = Data,
    {L + 65821, R};

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

%% @doc Get value at offset into file
-spec value(G, Offs) -> Value when
        G :: #db{},
        Offs :: integer(),
        Value :: number() | binary() | list({binary(), any()}) | list(any()).
value(#db{root=Root} = G, Offs) ->
    <<_:Offs/binary, Rest/binary>> = Root,
    {Val, _} = decode(G, Rest),
    Val.

%% @doc Get integer as a list of bits
-spec bits(I, Size) -> Value when
        I :: integer(),
        Size :: integer(),
        Value :: list(integer()).
bits(I, Size) ->
    [B || <<B:1>> <= <<I:Size>>].

%% @doc Convert inet:inet:ip_address() to list of ints
-spec path(Addr) -> Value when
        Addr :: inet:ip_address(),
        Value :: list(integer()).
path({A, B, C, D}) ->
    bits((A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D, 32);
path({A, B, C, D, E, F, G, H}) ->
    bits((A bsl 112) bor (B bsl 96) bor (C bsl 80) bor (D bsl 64) bor (E bsl 48) bor (F bsl 32) bor (G bsl 16) bor H, 128).
