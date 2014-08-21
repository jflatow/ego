-module(ego).

-export([priv/1,
         read/1]).

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
            end;

        {error, Error} ->
            {error, Error}
    end.
