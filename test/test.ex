
defmodule ASd do

    @spec szigetek(xs::[integer]) :: rss::[[integer]]

    @spec szfold(zs::[integer]) :: ys::[ {hossz::integer, csucs::integer} ]

    @spec max(list::[integer], index::integer, currentMax::integer) :: integer

    def szigetek([]) do
        []
    end

    def szigetek(xs) do
        list = xs |> Enum.chunk_by(fn(x) -> x > 0 end) |> Enum.reject(fn(x) -> [head | tail] = x; head <= 0 end)
    end

    def max(list, index, currentMax) do
        if Enum.at(list, index) > currentMax do
            Enum.at(list, index)
        else
            currentMax
        end
    end

    def max(list, index, currentMax) when index > 0 do
        if Enum.at(list, index) > currentMax do
            max(list, index - 1, Enum.at(list, index))
        else
            max(list, index - 1, currentMax)
        end
    end

    def szfold(zs) do
        szfolds = szigetek(zs)

        for(
            list <- szfolds,
            length(list) > 0,
            do: (
                {length(list), max(list, length(list) - 1, Enum.at(list, 0))}
            )
        )
    end
end

IO.inspect Integer.to_string(1353, 16)

IO.inspect ASd.szfold([0,0,0])